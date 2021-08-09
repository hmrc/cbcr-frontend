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

package uk.gov.hmrc.cbcrfrontend.controllers.upscan

import cats.data.EitherT
import cats.data.{EitherT, _}
import cats.instances.all._
import cats.syntax.all._

import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import play.api.{Configuration, Environment}
import uk.gov.hmrc.cbcrfrontend.CBCRErrorHandler
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.connectors.UpscanConnector
import uk.gov.hmrc.cbcrfrontend.controllers.actions.IdentifierAction
import uk.gov.hmrc.cbcrfrontend.model.{CBCErrors, FatalSchemaErrors, InvalidFileType, XMLErrors}
import uk.gov.hmrc.cbcrfrontend.model.upscan.{UploadId, UploadSessionDetails, UploadedSuccessfully}
import uk.gov.hmrc.cbcrfrontend.services.{CBCBusinessRuleValidator, CBCRXMLValidator, CBCSessionCache, XmlInfoExtract}
import uk.gov.hmrc.cbcrfrontend.views.Views
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.cache.client.CacheMap
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import java.net.URL
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

class FileValidationController @Inject()(
  override val messagesApi: MessagesApi,
  val schemaValidator: CBCRXMLValidator,
  val businessRuleValidator: CBCBusinessRuleValidator,
  val xmlExtractor: XmlInfoExtract,
  val audit: AuditConnector,
  val env: Environment,
  identify: IdentifierAction,
  messagesControllerComponents: MessagesControllerComponents,
  upscanConnector: UpscanConnector,
  errorHandler: CBCRErrorHandler,
  views: Views)(
  implicit ec: ExecutionContext,
  cache: CBCSessionCache,
  val config: Configuration,
  feConfig: FrontendAppConfig)
    extends FrontendController(messagesControllerComponents) with I18nSupport {

  /*def fileValidate() = identify.async { implicit request =>
    val fileDtls: Future[(String, String)] = for {
      uploadId <- getUploadId()
      uploadSessions <- upscanConnector.getUploadDetails(uploadId)
      (fileName, upScanUrl) = getDownloadUrl(uploadSessions)
    } yield (fileName, upScanUrl)

    val result = for {
      file <- fileDtls
      schemaErrors = schemaValidator.validateSchema(new URL(file._2))
      xmlErrors = XMLErrors.errorHandlerToXmlErrors(schemaErrors)
      schemaSize = if (xmlErrors.errors.nonEmpty) Some(getErrorFileSize(List(xmlErrors))) else None
      _ <- cache.save(XMLErrors.errorHandlerToXmlErrors(schemaErrors))
      _ <- if (!schemaErrors.hasFatalErrors) EitherT.pure[Future, CBCErrors, Unit](())
      else
        auditFailedSubmission(creds, affinity, enrolment, "schema validation errors").flatMap(_ =>
          EitherT.left[Future, CBCErrors, Unit](Future.successful(FatalSchemaErrors(schemaSize))))
      result <- validateBusinessRules(file_metadata, enrolment, affinity)
      businessSize = result.fold(e => Some(getErrorFileSize(e.toList)), _ => None)
      length = calculateFileSize(file_metadata._2)
      _ <- if (schemaErrors.hasErrors) auditFailedSubmission(creds, affinity, enrolment, "schema validation errors")
      else if (result.isLeft) auditFailedSubmission(creds, affinity, enrolment, "business rules errors")
      else EitherT.pure[Future, CBCErrors, Unit](())
      _ = java.nio.file.Files.deleteIfExists(file_metadata._1.toPath)
    } yield
      Ok(
        views.fileUploadResult(
          request.affinityGroup,
          Some(file._1),
          Some(length),
          schemaSize,
          businessSize,
          result.map(_.reportingEntity.reportingRole).toOption))

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
  }*/

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
