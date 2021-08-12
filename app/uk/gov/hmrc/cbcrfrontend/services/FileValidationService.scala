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
import cats.syntax.all._
import play.api.i18n.Messages
import play.api.{Configuration, Environment, Logger}
import uk.gov.hmrc.auth.core.AffinityGroup
import uk.gov.hmrc.auth.core.retrieve.Credentials
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.connectors.UpscanConnector
import uk.gov.hmrc.cbcrfrontend.controllers.actions.IdentifierAction
import uk.gov.hmrc.cbcrfrontend.controllers.routes
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.model.upscan.{UploadId, UploadSessionDetails, UploadedSuccessfully}
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.util.ModifySize.calculateFileSize
import uk.gov.hmrc.cbcrfrontend.views.Views
import uk.gov.hmrc.cbcrfrontend.{CBCRErrorHandler, sha256Hash}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.cache.client.CacheMap
import uk.gov.hmrc.play.audit.http.connector.AuditConnector

import java.io.File
import java.time.{Duration, LocalDateTime}
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

class FileValidationService @Inject()(
  val schemaValidator: CBCRXMLValidator,
  val businessRuleValidator: CBCBusinessRuleValidator,
  val xmlExtractor: XmlInfoExtract,
  val audit: AuditConnector,
  val env: Environment,
  val upscanConnector: UpscanConnector,
  identify: IdentifierAction,
  errorHandler: CBCRErrorHandler,
  fileUploadService: FileUploadService,
  views: Views)(
  implicit ec: ExecutionContext,
  cache: CBCSessionCache,
  val config: Configuration,
  feConfig: FrontendAppConfig) {


  case class FileValidationSuccess(userType: Option[AffinityGroup], fileName: Option[String], fileSize: Option[BigDecimal], schemaErrors: Option[Int], busErrors: Option[Int], reportingRole: Option[ReportingRole])

  lazy val logger: Logger = Logger(this.getClass)

  def fileValidate(creds: Credentials, affinity: Option[AffinityGroup], enrolment: Option[CBCEnrolment]):
  EitherT[Future, CBCErrors, FileValidationSuccess] = { implicit request =>

    val fileDtls: Future[(String, String, UploadId)] = for {
      uploadId <- getUploadId()
      uploadSessions <- upscanConnector.getUploadDetails(uploadId)
      (fileName, upScanUrl) = getDownloadUrl(uploadSessions)
    } yield (fileName, upScanUrl, uploadId)


    val result = for {
      (fileName, upScanUrl, uploadId) <-  EitherT.right[Future, CBCErrors, (String, String, UploadId)](fileDtls)
      file <- fileUploadService.getFileUrl( uploadId, upScanUrl)
      schemaErrors = schemaValidator.validateSchema(file)
      xmlErrors = XMLErrors.errorHandlerToXmlErrors(schemaErrors)
      schemaSize = if (xmlErrors.errors.nonEmpty) Some(getErrorFileSize(List(xmlErrors))) else None
      _ <- EitherT.right[Future, CBCErrors, CacheMap](cache.save(XMLErrors.errorHandlerToXmlErrors(schemaErrors)))
      result <- validateBusinessRules(file, fileName, enrolment, affinity)
      businessSize = result.fold(e => Some(getErrorFileSize(e.toList)), _ => None)
//      length = calculateFileSize(file_metadata._2)
//      _ <- if (schemaErrors.hasErrors) auditFailedSubmission(creds, affinity, enrolment, "schema validation errors")
//          else if (result.isLeft) auditFailedSubmission(creds, affinity, enrolment, "business rules errors")
//          else EitherT.pure[Future, CBCErrors, Unit](())
//      _ = java.nio.file.Files.deleteIfExists(file_metadata._1.toPath)
    } yield
      FileValidationSuccess(
          affinity,
          Some(fileName),
          Some(BigDecimal.valueOf(0l)),
          schemaSize,
          businessSize,
          result.map(_.reportingEntity.reportingRole).toOption)

    result
      .leftMap {
        case FatalSchemaErrors(size) =>
          Ok(views.fileUploadResult(None, None, None, size, None, None))
        case InvalidFileType(_) =>
          Redirect(routes.FileUploadController.fileInvalid)
        case e: CBCErrors =>
          logger.error(e.toString)
          Redirect(routes.SharedController.technicalDifficulties)
      }
      .merge
      .recover {
        case NonFatal(e) =>
          logger.error(e.getMessage, e)
          Redirect(routes.SharedController.technicalDifficulties)
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
          s"File validation failed for file ${file_metadata._2.id} (${calculateFileSize(file_metadata._2)}kb) in ${Duration.between(startValidation, endValidation).toMillis} milliseconds ")
      case Success(_) =>
        val endValidation = LocalDateTime.now()
        logger.info(
          s"File validation succeeded for file ${file_metadata._2.id} (${calculateFileSize(file_metadata._2)}kb) in ${Duration.between(startValidation, endValidation).toMillis} milliseconds ")

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
    val f = fileUploadService.errorsToFile(e, "")
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
}
