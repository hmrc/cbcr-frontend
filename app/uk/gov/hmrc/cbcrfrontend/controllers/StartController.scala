/*
 * Copyright 2020 HM Revenue & Customs
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

import javax.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.data.Forms._
import play.api.data._
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.MessagesControllerComponents
import uk.gov.hmrc.auth.core.AffinityGroup.{Agent, Individual, Organisation}
import uk.gov.hmrc.auth.core.retrieve.{Retrievals, _}
import uk.gov.hmrc.auth.core.{AuthConnector, AuthorisedFunctions}
import uk.gov.hmrc.cbcrfrontend._
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.services.CBCSessionCache
import uk.gov.hmrc.cbcrfrontend.views.Views
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class StartController @Inject()(
  override val messagesApi: MessagesApi,
  val authConnector: AuthConnector,
  messagesControllerComponents: MessagesControllerComponents,
  views: Views)(
  implicit val cache: CBCSessionCache,
  val config: Configuration,
  feConfig: FrontendAppConfig,
  val ec: ExecutionContext)
    extends FrontendController(messagesControllerComponents) with AuthorisedFunctions with I18nSupport {

  val startForm: Form[String] = Form(
    single("choice" -> nonEmptyText)
  )

  def start = Action.async { implicit request =>
    authorised().retrieve(Retrievals.affinityGroup and cbcEnrolment) {
      case Some(Agent) ~ _                      => Future.successful(Redirect(routes.FileUploadController.chooseXMLFile()))
      case Some(Organisation) ~ Some(enrolment) => Ok(views.start(startForm))
      case Some(Organisation) ~ None            => Redirect(routes.SharedController.verifyKnownFactsOrganisation())
      case Some(Individual) ~ _                 => errorRedirect(UnexpectedState("Individuals are not permitted to use this service"), views.notAuthorisedIndividual, views.errorTemplate)
    }
  }

  def submit = Action.async { implicit request =>
    authorised() {
      startForm
        .bindFromRequest()
        .fold(
          errors => BadRequest(views.start(errors)),
          (str: String) =>
            str match {
              case "upload"             => Redirect(routes.FileUploadController.chooseXMLFile())
              case "editSubscriberInfo" => Redirect(routes.SubscriptionController.updateInfoSubscriber())
              case _                    => BadRequest(views.start(startForm))
          }
        )
    }
  }

}
