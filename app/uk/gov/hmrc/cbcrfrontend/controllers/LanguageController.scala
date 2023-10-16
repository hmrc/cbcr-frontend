/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.cbcrfrontend.controllers

import play.api.Logger
import play.api.i18n.{I18nSupport, Lang}
import play.api.mvc.{Action, AnyContent, Flash, MessagesControllerComponents}
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import javax.inject.Inject
import scala.concurrent.ExecutionContext

class LanguageController @Inject()(
  configuration: FrontendAppConfig,
  messagesControllerComponents: MessagesControllerComponents)(implicit val ec: ExecutionContext)
    extends FrontendController(messagesControllerComponents) with I18nSupport {
  private val english = Lang("en")
  private val welsh = Lang("cy")

  lazy val logger: Logger = Logger(this.getClass)

  def switchToEnglish: Action[AnyContent] = switchToLang()
  def switchToWelsh: Action[AnyContent] = switchToLang()

  private def switchToLang() = Action { implicit request =>
    val newLang = english
    request.headers.get(REFERER) match {
      case Some(referrer) => Redirect(referrer).withLang(newLang).flashing(Flash(Map("switching-language" -> "true")))
      case None =>
        logger.warn(s"Unable to get the referrer, so sending them to ${configuration.fallbackURLForLanguageSwitcher}")
        Redirect(configuration.fallbackURLForLanguageSwitcher).withLang(newLang)
    }
  }
}
