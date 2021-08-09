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

import cats.instances.all._
import cats.syntax.all._
import org.slf4j.LoggerFactory
import play.api.data.Form
import play.api.data.Forms.{single, text}
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc._
import play.api.{Configuration, Environment}
import uk.gov.hmrc.cbcrfrontend._

import uk.gov.hmrc.cbcrfrontend.controllers.{routes => fileRoutes}
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.connectors.UpscanConnector
import uk.gov.hmrc.cbcrfrontend.controllers.actions.IdentifierAction
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.model.requests.IdentifierRequest
import uk.gov.hmrc.cbcrfrontend.model.upscan._
import uk.gov.hmrc.cbcrfrontend.services._
import uk.gov.hmrc.cbcrfrontend.views.Views
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class UploadFormController @Inject()(
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

  implicit val credentialsFormat = uk.gov.hmrc.cbcrfrontend.controllers.credentialsFormat
  val assetsLocation = (config.get[String](s"assets.url") |+| config.get[String](s"assets.version"))
  lazy val hostName = config.get[String]("cbcr-frontend.host")
  lazy val fileUploadErrorRedirectUrl = s"$hostName${routes.UploadFormController.handleError().url}"

  private val logger = LoggerFactory.getLogger(getClass)

  private[controllers] def form: Form[String] = Form(
    single("file" -> text())
  )

  def onPageLoad: Action[AnyContent] = identify.async { implicit request =>
    toResponse(form)
  }

  private[controllers] def toResponse(
    form: Form[String])(implicit request: IdentifierRequest[AnyContent], hc: HeaderCarrier): Future[Result] =
    (for {
      upscanInitiateResponse <- upscanConnector.getUpscanFormData
      uploadId               <- upscanConnector.requestUpload(upscanInitiateResponse.fileReference)
      _                      <- cache.save(uploadId)
      html                   <- Future.successful(views.uploadForm(upscanInitiateResponse, request.affinityGroup))
    } yield html).map(Ok(_))

  def fileUploadProgress(uploadId: UploadId, fileId: String) = identify.async { implicit request =>
    cache
      .read[UploadId]
      .subflatMap { e =>
        if (e != uploadId) {
          logger.error("BAD_ENVELOPE_ID")
          cache.remove()
          Left(
            UnexpectedState(
              s"The envelopeId in the cache was: ${e.value} while the progress request was for $uploadId"))
        } else {
          Right(Ok(views.uploadProgress(uploadId, fileId, hostName, assetsLocation)))
        }
      }
      .leftMap((error: CBCErrors) => errorRedirect(error, views.notAuthorisedIndividual, views.errorTemplate))
      .merge
  }

  def fileUploadResponse(uploadId: UploadId): Action[AnyContent] = identify.async { implicit request =>
    logger.debug("Show status called")
    upscanConnector.getUploadStatus(uploadId) flatMap {
      case Some(_: UploadedSuccessfully) =>
        Accepted
      case Some(r: UploadRejected) =>
        val errorMessage = if (r.details.message.contains("octet-stream")) {
          "upload_form.error.file.empty"
        } else {
          "upload_form.error.file.invalid"
        }
        val errorForm: Form[String] = form.withError("file", errorMessage)
        logger.debug(s"Show errorForm on rejection $errorForm")
        toResponse(errorForm)
      case Some(Quarantined) =>
        Conflict
      case Some(Failed) =>
        errorHandler.onServerError(request, new Throwable("Upload to upscan failed"))
      case _ =>
        NoContent
    }
  }

  def handleError(errorCode: String, errorMessage: String) = identify.async { implicit request =>
    logger.error(s"Error response received from FileUpload callback - ErrorCode: $errorCode - Reason $errorMessage")
    errorCode match {
      case "EntityTooLarge"  => Redirect(fileRoutes.FileUploadController.fileTooLarge)
      case "InvalidArgument" => Redirect(fileRoutes.FileUploadController.fileInvalid)
      case _                 => Redirect(fileRoutes.SharedController.technicalDifficulties)
    }
  }

  def unregisteredGGAccount: Action[AnyContent] = Action.async { implicit request =>
    Ok(views.unregisteredGGAccount())
  }

}
