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

import cats.data.OptionT
import cats.instances.all._
import org.slf4j.LoggerFactory
import play.api.i18n.{I18nSupport, Messages, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import play.api.{Configuration, Environment}
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.controllers.actions.IdentifierAction
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.services.{AuditService, CBCSessionCache, FileValidationService}
import uk.gov.hmrc.cbcrfrontend.util.ErrorUtil
import uk.gov.hmrc.cbcrfrontend.views.Views
import uk.gov.hmrc.cbcrfrontend.controllers.{routes => mainRoutes}
import uk.gov.hmrc.cbcrfrontend.{CBCRErrorHandler, errorRedirect}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import java.io.File
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

class FileValidationController @Inject()(
  override val messagesApi: MessagesApi,
  val env: Environment,
  identify: IdentifierAction,
  messagesControllerComponents: MessagesControllerComponents,
  errorHandler: CBCRErrorHandler,
  auditService: AuditService,
  fileValidationService: FileValidationService,
  views: Views)(
  implicit ec: ExecutionContext,
  cache: CBCSessionCache,
  val config: Configuration,
  feConfig: FrontendAppConfig)
    extends FrontendController(messagesControllerComponents) with I18nSupport {

  private val logger = LoggerFactory.getLogger(getClass)

  def fileValidate(): Action[AnyContent] = identify.async { implicit request =>
    fileValidationService
      .fileValidate()
      .fold(
        fa = {
          case FatalSchemaErrors(size) =>
            Ok(views.uploadResult(None, None, None, size, None, None))
          case InvalidFileType(_) =>
            Redirect(routes.FileValidationController.fileInvalid)
          case e: CBCErrors =>
            logger.error(e.toString)
            Redirect(mainRoutes.SharedController.technicalDifficulties)
        },
        fb = result => {
          Ok(
            views.uploadResult(
              request.affinityGroup,
              result.fileName,
              result.fileSize,
              result.schemaErrorSize,
              result.businessErrorSize,
              result.reportingRole))
        }
      )
      .recover {
        case NonFatal(e) =>
          logger.error(e.getMessage, e)
          Redirect(mainRoutes.SharedController.technicalDifficulties)
      }
  }

  def getBusinessRuleErrors: Action[AnyContent] = identify.async { implicit request =>
    OptionT(cache.readOption[AllBusinessRuleErrors])
      .map(x => ErrorUtil.errorsToFile(x.errors, fileUploadName("fileUpload.BusinessRuleErrors")))
      .fold(
        NoContent
      )((file: File) => Ok.sendFile(content = file, inline = false, onClose = () => file.delete()))
  }

  def getXmlSchemaErrors: Action[AnyContent] = Action.async { implicit request =>
    OptionT(cache.readOption[XMLErrors])
      .map(x => ErrorUtil.errorsToFile(List(x), fileUploadName("fileUpload.XMLSchemaErrors")))
      .fold(
        NoContent
      )((file: File) => Ok.sendFile(content = file, inline = false, onClose = () => file.delete()))
  }

  def fileInvalid: Action[AnyContent] = fileUploadError(FileNotXml)
  def fileTooLarge: Action[AnyContent] = fileUploadError(FileTooLarge)
  def fileContainsVirus: Action[AnyContent] = fileUploadError(FileContainsVirus)

  private def fileUploadError(errorType: FileUploadErrorType): Action[AnyContent] = identify.async { implicit request =>
    auditService
      .auditFailedSubmission(errorType.toString)
      .map(_ => Ok(views.uploadError(errorType)))
      .leftMap((error: CBCErrors) => errorRedirect(error, views.notAuthorisedIndividual, views.errorTemplate))
      .merge
  }

  private def fileUploadName(fname: String)(implicit messages: Messages): String =
    messages(fname)
}
