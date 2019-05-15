/*
 * Copyright 2019 HM Revenue & Customs
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

import javax.inject.Inject
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import play.api.{Application, Logger}
import play.api.mvc.{Action, AnyContent, Flash, LegacyI18nSupport}
import play.api.i18n.Lang
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import play.api.i18n.Messages.Implicits._
import uk.gov.hmrc.cbcrfrontend.util.CbcrSwitches
//import play.api.Play.current

import scala.concurrent.ExecutionContext

class LanguageController @Inject()(configuration: FrontendAppConfig)(implicit val ec: ExecutionContext, application: Application) extends FrontendController with LegacyI18nSupport{
  val english = Lang("en")
  val welsh = Lang("cy")

  def switchToEnglish: Action[AnyContent] = switchToLang(english)
  def switchToWelsh: Action[AnyContent] = switchToLang(welsh)

  private def switchToLang(lang: Lang) = Action { implicit request =>
    val newLang = if (CbcrSwitches.enableLanguageSwitching.enabled) lang else english
    request.headers.get(REFERER) match {
      case Some(referrer) => Redirect(referrer).withLang(newLang).flashing(Flash(Map("switching-language" -> "true")))
      case None =>
        Logger.warn(s"Unable to get the referrer, so sending them to ${configuration.fallbackURLForLanguageSwitcher}")
        Redirect(configuration.fallbackURLForLanguageSwitcher).withLang(newLang)
    }
  }
}
