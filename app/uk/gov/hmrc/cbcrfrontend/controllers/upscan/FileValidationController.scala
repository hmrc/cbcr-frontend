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

import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.MessagesControllerComponents
import play.api.{Configuration, Environment}
import uk.gov.hmrc.cbcrfrontend.CBCRErrorHandler
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.controllers.actions.IdentifierAction
import uk.gov.hmrc.cbcrfrontend.services.{CBCSessionCache, FileValidationService}
import uk.gov.hmrc.cbcrfrontend.views.Views
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class FileValidationController @Inject()(
  override val messagesApi: MessagesApi,
  val env: Environment,
  identify: IdentifierAction,
  messagesControllerComponents: MessagesControllerComponents,
  errorHandler: CBCRErrorHandler,
  fileValidationService: FileValidationService,
  views: Views)(
  implicit ec: ExecutionContext,
  cache: CBCSessionCache,
  val config: Configuration,
  feConfig: FrontendAppConfig)
    extends FrontendController(messagesControllerComponents) with I18nSupport {

  def fileValidate() = identify.async { implicit request =>
    fileValidationService.fileValidate
    Future.successful(Ok(views.fileUploadResult(None, None, None, None, None, None)))
  }
}
