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

package uk.gov.hmrc.cbcrfrontend.controllers

import javax.inject.{Inject, Singleton}

import uk.gov.hmrc.cbcrfrontend.auth.SecuredActions
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.services.CBCSessionCache
import uk.gov.hmrc.play.frontend.auth.connectors.AuthConnector
import uk.gov.hmrc.play.frontend.controller.FrontendController
import uk.gov.hmrc.cbcrfrontend._
import uk.gov.hmrc.cbcrfrontend.connectors.EnrolmentsConnector
import cats.instances.future._
import play.api.data._
import play.api.data.Forms._
import play.api.Play.current
import play.api.i18n.Messages.Implicits._
import uk.gov.hmrc.cbcrfrontend.views.html._

import scala.concurrent.Future

@Singleton
class StartController @Inject()(val sec: SecuredActions,val enrolmentsConnector: EnrolmentsConnector)(implicit val cache:CBCSessionCache, val auth:AuthConnector) extends FrontendController {


  val startForm: Form[String] = Form(
    single("choice" -> nonEmptyText)
  )

  def start =  sec.AsyncAuthenticatedAction(){ authContext => implicit request =>
    getUserType(authContext).semiflatMap{
      case Agent()         => Future.successful(Redirect(routes.FileUploadController.chooseXMLFile()))
      case Organisation(_) => enrolmentsConnector.getCbcId.cata(
        Redirect(routes.SharedController.verifyKnownFactsOrganisation()),
        (_: CBCId) => Ok(views.html.start(includes.asideCbc(), includes.phaseBannerBeta(), startForm))
      )
      case Individual()    => Future.successful(errorRedirect(UnexpectedState("Individuals are not permitted to use this service")))
    }.leftMap(errorRedirect).merge

  }

  def submit = sec.AsyncAuthenticatedAction(Some(Organisation(true))){ _ => implicit request =>
    startForm.bindFromRequest().fold(
      errors        => BadRequest(views.html.start(includes.asideCbc(), includes.phaseBannerBeta(), errors)),
      (str: String) => str match {
        case "upload"             => Redirect(routes.FileUploadController.chooseXMLFile())
        case "editSubscriberInfo" => Redirect(routes.SubscriptionController.updateInfoSubscriber())
        case _                    => BadRequest(views.html.start(includes.asideCbc(), includes.phaseBannerBeta(), startForm))
      }
    )
  }

}
