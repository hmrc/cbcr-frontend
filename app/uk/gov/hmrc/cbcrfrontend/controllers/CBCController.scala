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

import cats.data.EitherT
import cats.instances.future._
import play.api.Logger
import play.api.Play.current
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.Messages.Implicits._
import play.api.mvc.{Action, Result}
import uk.gov.hmrc.cbcrfrontend._
import uk.gov.hmrc.cbcrfrontend.auth.SecuredActions
import uk.gov.hmrc.cbcrfrontend.exceptions.UnexpectedState
import uk.gov.hmrc.cbcrfrontend.model.{Agent, CBCId, Organisation, SubscriptionDetails}
import uk.gov.hmrc.cbcrfrontend.services.{CBCSessionCache, SubscriptionDataService}
import uk.gov.hmrc.cbcrfrontend.views.html._
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.frontend.auth.connectors.AuthConnector
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.Future


@Singleton
class CBCController @Inject()(val sec: SecuredActions, val subDataService: SubscriptionDataService)(implicit val auth:AuthConnector, val cache:CBCSessionCache)  extends FrontendController with ServicesConfig {


  val cbcIdForm : Form[String] = Form(
    single(
      "cbcId" -> nonEmptyText.verifying("Please enter a valid CBCId",{s => CBCId(s).isDefined})
    )
  )

  val technicalDifficulties = Action{ implicit request =>
    InternalServerError(FrontendGlobal.internalServerErrorTemplate)
  }

  val enterCBCId = sec.AsyncAuthenticatedAction(){ authContext => implicit request =>
    getUserType(authContext).fold[Result](
      error => {
        Logger.error(error.errorMsg)
        InternalServerError(FrontendGlobal.internalServerErrorTemplate)
      },
      userType =>
        Ok(forms.enterCBCId(includes.asideCbc(), includes.phaseBannerBeta(), userType, cbcIdForm))

    )

  }

  val submitCBCId = sec.AsyncAuthenticatedAction() { authContext =>
    implicit request =>
      getUserType(authContext).leftMap{errors =>
        Logger.error(errors.errorMsg)
        InternalServerError(FrontendGlobal.internalServerErrorTemplate)
      }.semiflatMap(userType =>
        cbcIdForm.bindFromRequest().fold[Future[Result]](
          errors => Future.successful(BadRequest(forms.enterCBCId(includes.asideCbc(), includes.phaseBannerBeta(), userType, errors))),
          id => {
            val result: EitherT[Future, UnexpectedState, Option[SubscriptionDetails]] = for {
              id <- EitherT.fromOption[Future](CBCId(id), UnexpectedState(s"CBCId $id is Invalid"))
              sd <- subDataService.retrieveSubscriptionData(id)
            } yield sd

            result.value.flatMap(_.fold(
              (error: UnexpectedState)               => {
                Logger.error(error.errorMsg)
                Future.successful(InternalServerError(FrontendGlobal.internalServerErrorTemplate))
              },
              (details: Option[SubscriptionDetails]) => details.fold {
                Future.successful(BadRequest(forms.enterCBCId(includes.asideCbc(), includes.phaseBannerBeta(), userType, cbcIdForm, true)))
              }(submissionDetails => {
                for {
                  _ <- cache.save(submissionDetails.utr)
                  _ <- cache.save(submissionDetails.businessPartnerRecord)
                  _ <- cache.save(submissionDetails.cbcId)
                } yield userType match {
                  case Agent => Redirect(uk.gov.hmrc.cbcrfrontend.controllers.routes.Subscription.enterKnownFacts())
                  case Organisation => Redirect(uk.gov.hmrc.cbcrfrontend.controllers.routes.FileUpload.chooseXMLFile())
                }
              })
            ))
          }
        )
      ).merge
  }
}


