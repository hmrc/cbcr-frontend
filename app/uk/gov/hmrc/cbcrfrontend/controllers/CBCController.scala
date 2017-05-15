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

import javax.inject.{Inject, Singleton}

import play.api.Logger
import play.api.Play.current
import play.api.i18n.Messages.Implicits._
import play.api.mvc.{Action, Result}
import uk.gov.hmrc.cbcrfrontend.FrontendAuthConnector
import uk.gov.hmrc.cbcrfrontend.auth.SecuredActions
import uk.gov.hmrc.cbcrfrontend.model.{AffinityGroup, Agent, Organisation, UserType}
import uk.gov.hmrc.cbcrfrontend.services.CBCSessionCache
import uk.gov.hmrc.cbcrfrontend.views.html._
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

import cats.data.EitherT
import play.api.data.Form
import uk.gov.hmrc.cbcrfrontend.auth.SecuredActions
import uk.gov.hmrc.cbcrfrontend.model.{CBCId, SubscriptionDetails}
import uk.gov.hmrc.cbcrfrontend.services.SubscriptionDataService
import uk.gov.hmrc.cbcrfrontend._
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.exceptions.UnexpectedState


@Singleton
class CBCController @Inject()(val sec: SecuredActions, val subDataService: SubscriptionDataService)(implicit val auth:FrontendAuthConnector, val cache:CBCSessionCache)  extends FrontendController with ServicesConfig {


  val cbcIdForm : Form[String] = Form(
    single(
      "cbcId" -> nonEmptyText.verifying("Please enter a valid CBCId",{s => CBCId(s).isDefined})
    )
  )

  val enterCBCId = sec.AsyncAuthenticatedAction { authContext => implicit request =>
    getUserType(authContext).fold[Result](
      error => {
        Logger.error(error.errorMsg)
        InternalServerError
      },
      userType =>
        Ok(forms.enterCBCId(includes.asideCbc(), includes.phaseBannerBeta(), userType, cbcIdForm))

    )

  }

  val submitCBCId = sec.AsyncAuthenticatedAction { authContext =>
    implicit request =>
      getUserType(authContext).leftMap(errors => InternalServerError(errors.errorMsg)).flatMap(userType =>
        cbcIdForm.bindFromRequest().fold[EitherT[Future,Result,Result]](
          errors => EitherT.left(Future.successful(BadRequest(forms.enterCBCId(includes.asideCbc(), includes.phaseBannerBeta(), userType, errors)))),
          id => {
            val result: EitherT[Future, UnexpectedState, Option[SubscriptionDetails]] = for {
              id <- EitherT.fromOption[Future](CBCId(id), UnexpectedState(s"CBCId $id is Invalid"))
              sd <- subDataService.retrieveSubscriptionData(id)
            } yield sd

            result.bimap(
              error => {
                InternalServerError(error.errorMsg)
              },
              details => details.fold {
                BadRequest(forms.enterCBCId(includes.asideCbc(), includes.phaseBannerBeta(), userType, cbcIdForm, true))
              }(_ => {
                userType match {
                  case Agent => Redirect(uk.gov.hmrc.cbcrfrontend.controllers.routes.Subscription.enterKnownFacts())
                  case Organisation => Redirect(uk.gov.hmrc.cbcrfrontend.controllers.routes.FileUpload.chooseXMLFile())
                }
              })
            )

          }
        )
      ).merge
  }
}


