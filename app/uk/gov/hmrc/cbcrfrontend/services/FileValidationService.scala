/*
 * Copyright 2021 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.cbcrfrontend.services
import cats.data.{EitherT, NonEmptyList}
import cats.instances.all._
import play.api.i18n.Messages
import play.api.mvc.{AnyContent, Request}
import play.api.{Configuration, Environment, Logger}
import uk.gov.hmrc.auth.core.AffinityGroup
import uk.gov.hmrc.auth.core.retrieve.Credentials
import uk.gov.hmrc.cbcrfrontend.connectors.UpscanConnector
import uk.gov.hmrc.cbcrfrontend.controllers.right
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.model.requests.IdentifierRequest
import uk.gov.hmrc.cbcrfrontend.model.upscan.{FileValidationResult, UploadId, UploadSessionDetails, UploadedSuccessfully}
import uk.gov.hmrc.cbcrfrontend.util.ErrorUtil
import uk.gov.hmrc.cbcrfrontend.util.ModifySize.calculateFileSize
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.cache.client.CacheMap

import java.io.File
import java.time.{Duration, LocalDateTime}
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}
import scala.math.BigDecimal.int2bigDecimal
import scala.util.{Failure, Success}

class FileValidationService @Inject()(
  val schemaValidator: CBCRXMLValidator,
  val businessRuleValidator: CBCBusinessRuleValidator,
  val xmlExtractor: XmlInfoExtract,
  val env: Environment,
  val upscanConnector: UpscanConnector,
  fileService: FileService,
  val auditService: AuditService)(implicit ec: ExecutionContext, cache: CBCSessionCache, val config: Configuration) {

  lazy val logger: Logger = Logger(this.getClass)

  def fileValidate()(
    implicit hc: HeaderCarrier,
    messages: Messages,
    request: IdentifierRequest[AnyContent]): EitherT[Future, CBCErrors, FileValidationResult] = {

    val fileDtls: Future[(UploadedSuccessfully, UploadId)] = for {
      uploadId       <- getUploadId()
      uploadSessions <- upscanConnector.getUploadDetails(uploadId)
      uploadDetails = getUploadDetails(uploadSessions)
    } yield (uploadDetails, uploadId)

    for {
      file_meta <- right[(UploadedSuccessfully, UploadId)](fileDtls)
      file      <- fileService.getFile(file_meta._2, file_meta._1.downloadUrl)
      _         <- right(cache.save[UploadedSuccessfully](file_meta._1))
      _         <- EitherT.cond[Future](file_meta._1.name.toLowerCase endsWith ".xml", (), InvalidFileType(file_meta._1.name))

      schemaErrors: XmlErrorHandler = schemaValidator.validateSchema(file)
      xmlErrors = XMLErrors.errorHandlerToXmlErrors(schemaErrors)
      schemaSize = if (xmlErrors.errors.nonEmpty) Some(getErrorFileSize(List(xmlErrors))) else None
      _ <- EitherT.right[Future, CBCErrors, CacheMap](cache.save(XMLErrors.errorHandlerToXmlErrors(schemaErrors)))
      _ <- if (!schemaErrors.hasFatalErrors) EitherT.pure[Future, CBCErrors, Unit](())
          else
            auditService
              .auditFailedSubmission("schema validation errors")
              .flatMap(_ => EitherT.left[Future, CBCErrors, Unit](Future.successful(FatalSchemaErrors(schemaSize))))
      _ = println("\n\n\n\nBefore business rules")
      result <- validateBusinessRules(file, file_meta._1.name, request.enrolment, request.affinityGroup)
      _ = println(s"\n\n\n\nafter business rules $result")
      businessSize = result.fold(e => Some(getErrorFileSize(e.toList)), _ => None)
      _ <- if (schemaErrors.hasErrors)
            auditService.auditFailedSubmission("schema validation errors")
          else if (result.isLeft)
            auditService.auditFailedSubmission("business rules errors")
          else EitherT.pure[Future, CBCErrors, Unit](())
      _ = fileService.deleteFile(file)
    } yield {
      FileValidationResult(
        request.affinityGroup,
        Some(file_meta._1.name),
        file_meta._1.size,
        schemaSize,
        businessSize,
        result.map(_.reportingEntity.reportingRole).toOption)
    }
  }

  def validateBusinessRules(
    xmlFile: File,
    fileName: String,
    enrolment: Option[CBCEnrolment],
    affinityGroup: Option[AffinityGroup])(
    implicit hc: HeaderCarrier): ServiceResponse[Either[NonEmptyList[BusinessRuleErrors], CompleteXMLInfo]] = {
    val startValidation = LocalDateTime.now()

    val rawXmlInfo = xmlExtractor.extract(xmlFile)

    val result = for {
      xmlInfo <- EitherT(
                  businessRuleValidator
                    .validateBusinessRules(rawXmlInfo, fileName = fileName, enrolment, affinityGroup)
                    .map(_.toEither))
      completeXI <- EitherT(businessRuleValidator.recoverReportingEntity(xmlInfo).map(_.toEither))
    } yield completeXI

    result.value.onComplete {
      case Failure(_) =>
        val endValidation = LocalDateTime.now()
        logger.info(
          s"File validation failed for file in ${Duration.between(startValidation, endValidation).toMillis} milliseconds ")
      case Success(_) =>
        val endValidation = LocalDateTime.now()
        logger.info(
          s"File validation succeeded for file in ${Duration.between(startValidation, endValidation).toMillis} milliseconds ")

    }

    EitherT.right[Future, CBCErrors, Either[NonEmptyList[BusinessRuleErrors], CompleteXMLInfo]](
      result
        .fold(
          errors => cache.save(AllBusinessRuleErrors(errors.toList)).map(_ => Left(errors)),
          info => cache.save(info).flatMap(_ => cache.save(Hash(fileService.sha256Hash(xmlFile))).map(_ => Right(info)))
        )
        .flatten)
  }

  private def getErrorFileSize(e: List[ValidationErrors])(implicit messages: Messages): Int = {
    val f = ErrorUtil.errorsToFile(e, "")
    val kb = f.length() * 0.001
    f.delete()
    Math.incrementExact(kb.toInt)
  }

  private def getUploadId()(implicit hc: HeaderCarrier): Future[UploadId] =
    cache.readOption[UploadId] map {
      case Some(uploadId) => uploadId
      case None           => throw new RuntimeException("Cannot find uploadId")
    }

  private def getUploadDetails(uploadSessions: Option[UploadSessionDetails]): UploadedSuccessfully =
    uploadSessions match {
      case Some(uploadDetails) =>
        uploadDetails.status match {
          case uploadDetails: UploadedSuccessfully => uploadDetails
          case _                                   => throw new RuntimeException("File not uploaded successfully")
        }
      case _ => throw new RuntimeException("File not uploaded successfully")
    }

}
