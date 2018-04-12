/*
 * Copyright 2018 HM Revenue & Customs
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

import uk.gov.hmrc.cbcrfrontend.controllers.routes
import uk.gov.hmrc.auth.core.{NoActiveSession, UnsupportedAffinityGroup, UnsupportedCredentialRole}
import uk.gov.hmrc.play.bootstrap.config.AuthRedirects
import uk.gov.hmrc.play.bootstrap.http.FrontendErrorHandler
import play.api.{Configuration, Environment, Logger}
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import javax.inject.{Inject, Singleton}
import play.api.i18n.{I18nSupport, Messages, MessagesApi}
import play.api.mvc._
import uk.gov.hmrc.auth.core.AffinityGroup.{Agent, Individual}
import uk.gov.hmrc.auth.core.retrieve.Retrievals
import uk.gov.hmrc.cbcrfrontend.model.UnexpectedState
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.Future


class CBCRErrorHandler @Inject()(val messagesApi: MessagesApi, val env:Environment, val config:Configuration,
                                 val authConnector:AuthConnector)(implicit val feConfig:FrontendAppConfig)
  extends FrontendErrorHandler with AuthorisedFunctions with I18nSupport with AuthRedirects with FrontendController{

  override def resolveError(rh: RequestHeader, ex: Throwable) = ex match {
    case _:NoActiveSession            => toGGLogin(rh.uri)
    case _:UnsupportedCredentialRole  => {
      Logger.info("++++++++++++++++++++++++++++++++++++++++++++++++++ CBCError individual ++++++++++++++++++++++++++++++++++++++++++++++++++")
      Redirect(routes.SubmissionController.noAssistants())
    }
    case _:UnsupportedAffinityGroup   => {
      Logger.info("++++++++++++++++++++++++++++++++++++++++++++++++++ CBCError Agent ++++++++++++++++++++++++++++++++++++++++++++++++++")
      Redirect(routes.SharedController.unsupportedAffinityGroup())
    }
    case _                            => super.resolveError(rh,ex)
  }

  override def standardErrorTemplate (pageTitle: String, heading: String, message: String) (implicit request: Request[_] ) =
  uk.gov.hmrc.cbcrfrontend.views.html.error_template (pageTitle, heading, message)


}
