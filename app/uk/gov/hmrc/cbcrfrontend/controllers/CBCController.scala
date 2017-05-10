/*
 * Copyright 2017 HM Revenue & Customs
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
import play.api.mvc.Action
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.frontend.controller.FrontendController
import uk.gov.hmrc.cbcrfrontend.views.html._
import play.api.data.Form
import play.api.data.Forms._
import cats.instances.future._

import scala.concurrent.Future
import play.api.Play.current
import play.api.i18n.Messages.Implicits._
import play.api.mvc._
import javax.inject.{Inject, Singleton}

import play.api.data.Form
import uk.gov.hmrc.cbcrfrontend.auth.SecuredActions
import uk.gov.hmrc.cbcrfrontend.model.{CBCId, SubscriptionDetails}
import uk.gov.hmrc.cbcrfrontend.services.SubscriptionDataService


@Singleton
class CBCController @Inject()(val sec: SecuredActions, val subDataService: SubscriptionDataService)  extends FrontendController with ServicesConfig {


  val form : Form[String] = Form(
    single(
      "cbcId" -> nonEmptyText.verifying("Please enter a valid CBCId",{s => CBCId(s).isDefined})
    )
  )

  val enterCBCId = sec.AsyncAuthenticatedAction { authContext => implicit request =>
    Logger.debug("Country by Country: Enter CBCID: "+request.secure)

    Future.successful(Ok(forms.enterCBCId(includes.asideCbc(), includes.phaseBannerBeta(), form)))
  }

  val submitCBCId = Action.async { implicit request =>

    form.bindFromRequest().fold(
      errors =>{
        Logger.error(s"ERRORS IN FORM: $errors")
        Future.successful(BadRequest(forms.enterCBCId(includes.asideCbc(), includes.phaseBannerBeta(),errors)))
      },
      id => CBCId(id) match {
        case Some(cbcId) => subDataService.retrieveSubscriptionData(cbcId).fold(
          error   => {
            Logger.info("subDataService errored: $error")
            InternalServerError(error.errorMsg)
          },
          details => details.fold {
            Logger.info("subDataService returned NONE")
            BadRequest(forms.enterCBCId(includes.asideCbc(), includes.phaseBannerBeta(), form, true))
            }(_ => Redirect(uk.gov.hmrc.cbcrfrontend.controllers.routes.FileUpload.chooseXMLFile()))
        )
        case None => Future.successful(InternalServerError)
      }
    )

  }


}

