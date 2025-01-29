/*
 * Copyright 2024 HM Revenue & Customs
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

import play.api.i18n.MessagesApi
import play.api.mvc._
import play.api.{Configuration, Environment}
import play.twirl.api.{Html, HtmlFormat}
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.cbcrfrontend.auth.AuthRedirectsExternal
import uk.gov.hmrc.cbcrfrontend.controllers.routes
import uk.gov.hmrc.cbcrfrontend.views.Views
import uk.gov.hmrc.play.bootstrap.frontend.http.FrontendErrorHandler

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class CBCRErrorHandler @Inject() (
  val messagesApi: MessagesApi,
  val env: Environment,
  val config: Configuration,
  views: Views
) extends FrontendErrorHandler with Results with AuthRedirectsExternal {

  override def resolveError(rh: RequestHeader, ex: Throwable): Future[Result] = ex match {
    case _: NoActiveSession =>
      toGGLogin(rh.uri)
    case _: UnsupportedCredentialRole =>
      Redirect(routes.SubmissionController.noAssistants)
    case _: UnsupportedAffinityGroup =>
      Redirect(routes.SharedController.unsupportedAffinityGroup)
    case _ =>
      logger.error(s"Unresolved error: ${ex.getMessage}", ex)
      super.resolveError(rh, ex)
  }

  override def onServerError(request: RequestHeader, exception: Throwable): Future[Result] =
    resolveError(request, exception)

  override def standardErrorTemplate(pageTitle: String, heading: String, message: String)(implicit
    request: RequestHeader
  ): Future[Html] =
    convertToFutureHtml(views.errorTemplate(pageTitle, heading, message))

  override protected implicit val ec: ExecutionContext = ExecutionContext.global

  private def convertToFutureHtml(appendable: HtmlFormat.Appendable): Future[Html] =
    Future {
      Html(appendable.toString())
    }
}
