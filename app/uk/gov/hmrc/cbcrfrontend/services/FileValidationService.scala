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
import akka.stream.Materializer
import akka.stream.scaladsl.Sink
import akka.util.ByteString
import cats.data.{EitherT, NonEmptyList}
import cats.instances.all._
import play.api.http.Status
import play.api.i18n.Messages
import play.api.libs.ws.WSClient
import play.api.mvc.{AnyContent, Request}
import play.api.{Configuration, Environment, Logger}
import uk.gov.hmrc.auth.core.AffinityGroup
import uk.gov.hmrc.auth.core.retrieve.Credentials
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.connectors.UpscanConnector
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.model.upscan.{UploadId, UploadSessionDetails, UploadedSuccessfully}
import uk.gov.hmrc.cbcrfrontend.sha256Hash
import uk.gov.hmrc.cbcrfrontend.util.ErrorUtil
import uk.gov.hmrc.cbcrfrontend.util.ModifySize.calculateFileSize
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.cache.client.CacheMap

import java.io.File
import java.time.{Duration, LocalDateTime}
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

class FileValidationService @Inject()(
  val schemaValidator: CBCRXMLValidator,
  val businessRuleValidator: CBCBusinessRuleValidator,
  val xmlExtractor: XmlInfoExtract,
  val env: Environment,
  val upscanConnector: UpscanConnector,
  ws: WSClient,
  val auditService: AuditService)(
  implicit ec: ExecutionContext,
  cache: CBCSessionCache,
  val config: Configuration,
  feConfig: FrontendAppConfig,
  materializer: Materializer) {

  case class FileValidationSuccess(
    userType: Option[AffinityGroup],
    fileName: Option[String],
    fileSize: Option[BigDecimal],
    schemaErrors: Option[Int],
    busErrors: Option[Int],
    reportingRole: Option[ReportingRole])

  lazy val logger: Logger = Logger(this.getClass)

  def fileValidate(creds: Credentials, affinity: Option[AffinityGroup], enrolment: Option[CBCEnrolment])(
    implicit hc: HeaderCarrier,
    messages: Messages,
    request: Request[AnyContent]): EitherT[Future, CBCErrors, FileValidationSuccess] = {

    val fileDtls: Future[(String, String, UploadId)] = for {
      uploadId       <- getUploadId()
      uploadSessions <- upscanConnector.getUploadDetails(uploadId)
      (fileName, upScanUrl) = getDownloadUrl(uploadSessions)
    } yield (fileName, upScanUrl, uploadId)

    for {
      file_meta <- EitherT.right[Future, CBCErrors, (String, String, UploadId)](fileDtls)
      file      <- getFile(file_meta._3, file_meta._2)
      schemaErrors: XmlErrorHandler = schemaValidator.validateSchema(file)
      xmlErrors = XMLErrors.errorHandlerToXmlErrors(schemaErrors)
      schemaSize = if (xmlErrors.errors.nonEmpty) Some(getErrorFileSize(List(xmlErrors))) else None
      _      <- EitherT.right[Future, CBCErrors, CacheMap](cache.save(XMLErrors.errorHandlerToXmlErrors(schemaErrors)))
      result <- validateBusinessRules(file, file_meta._1, enrolment, affinity)
      businessSize = result.fold(e => Some(getErrorFileSize(e.toList)), _ => None)
      length = calculateFileSize(10l) //TODO
      _ <- if (schemaErrors.hasErrors)
            auditService.auditFailedSubmission(creds, affinity, enrolment, "schema validation errors")
          else if (result.isLeft)
            auditService.auditFailedSubmission(creds, affinity, enrolment, "business rules errors")
          else EitherT.pure[Future, CBCErrors, Unit](())
      _ = java.nio.file.Files.deleteIfExists(file.toPath)
    } yield
      FileValidationSuccess(
        affinity,
        Some(file_meta._1),
        Some(BigDecimal.valueOf(0l)),
        schemaSize,
        businessSize,
        result.map(_.reportingEntity.reportingRole).toOption)
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
          info => cache.save(info).flatMap(_ => cache.save(Hash(sha256Hash(xmlFile))).map(_ => Right(info)))
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

  private def getDownloadUrl(uploadSessions: Option[UploadSessionDetails]) =
    uploadSessions match {
      case Some(uploadDetails) =>
        uploadDetails.status match {
          case UploadedSuccessfully(name, downloadUrl) => (name, downloadUrl)
          case _                                       => throw new RuntimeException("File not uploaded successfully")
        }
      case _ => throw new RuntimeException("File not uploaded successfully")
    }

  def getFile(uploadId: UploadId, url: String)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): ServiceResponse[File] =
    EitherT(
      ws.url(s"$url")
        .withMethod("GET")
        .stream()
        .flatMap { res =>
          res.status match {
            case Status.OK =>
              val file = java.nio.file.Files.createTempFile(uploadId.value, "xml")
              val outputStream = java.nio.file.Files.newOutputStream(file)

              val sink = Sink.foreach[ByteString] { bytes =>
                outputStream.write(bytes.toArray)
              }

              res.bodyAsSource
                .runWith(sink)
                .andThen {
                  case result =>
                    outputStream.close()
                    result.get
                }
                .map(_ => Right(file.toFile))
            case otherStatus =>
              Future.successful(
                Left(
                  UnexpectedState(
                    s"Failed to retrieve a file with uploadId: ${uploadId.value} from upscan - received $otherStatus response")
                ))
          }
        }
    )
}
