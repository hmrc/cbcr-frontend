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

package uk.gov.hmrc.cbcrfrontend

import javax.inject.Inject
import play.api.i18n.{I18nSupport, Lang, MessagesApi}
import play.api.mvc._
import play.api.{Configuration, Environment, Logger}
import uk.gov.hmrc.auth.core.{NoActiveSession, UnsupportedAffinityGroup, UnsupportedCredentialRole, _}
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.controllers.routes
import uk.gov.hmrc.play.bootstrap.config.AuthRedirects
import uk.gov.hmrc.play.bootstrap.http.FrontendErrorHandler

import scala.concurrent.Future


class CBCRErrorHandler @Inject()(override val messagesApi: MessagesApi,
                                 val env:Environment,
                                 val config:Configuration,
                                 val authConnector:AuthConnector)(implicit val feConfig:FrontendAppConfig)
  extends FrontendErrorHandler with Results with AuthorisedFunctions with I18nSupport with AuthRedirects {

  implicit val lang = Lang.defaultLang

  override def resolveError(rh: RequestHeader, ex: Throwable) = ex match {
    case _:NoActiveSession            =>
      toGGLogin(rh.uri)
    case _:UnsupportedCredentialRole  =>
      Redirect(routes.SubmissionController.noAssistants())
    case _:UnsupportedAffinityGroup   =>
      Redirect(routes.SharedController.unsupportedAffinityGroup())
    case _                            =>
      Logger.error(s"Unresolved error: ${ex.getMessage}", ex)
      super.resolveError(rh,ex)
  }

  override def onServerError(request: RequestHeader, exception: Throwable): Future[Result] =
    resolveError(request,exception)

  override def standardErrorTemplate (pageTitle: String, heading: String, message: String) (implicit request: Request[_] ) =
  uk.gov.hmrc.cbcrfrontend.views.html.error_template (pageTitle, heading, message)
}