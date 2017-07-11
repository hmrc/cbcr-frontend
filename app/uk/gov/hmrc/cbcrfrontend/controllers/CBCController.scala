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

import cats.data.{EitherT, OptionT}
import cats.syntax.all._
import cats.instances.all._
import play.api.Logger
import play.api.Play.current
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.Messages.Implicits._
import play.api.mvc.{Action, AnyContent, Result}
import uk.gov.hmrc.cbcrfrontend._
import uk.gov.hmrc.cbcrfrontend.auth.SecuredActions
import uk.gov.hmrc.cbcrfrontend.connectors.EnrolmentsConnector
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.services.{CBCSessionCache, SubscriptionDataService}
import uk.gov.hmrc.cbcrfrontend.views.html._
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.frontend.auth.connectors.AuthConnector
import uk.gov.hmrc.play.frontend.controller.FrontendController
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.Future


@Singleton
class CBCController @Inject()(val sec: SecuredActions, val subDataService: SubscriptionDataService, val enrolments:EnrolmentsConnector)(implicit val auth:AuthConnector, val cache:CBCSessionCache)  extends FrontendController with ServicesConfig {

  val cbcIdForm : Form[CBCId] = Form(
    single( "cbcId" -> of[CBCId] )
  )

  val technicalDifficulties = Action{ implicit request =>
    InternalServerError(FrontendGlobal.internalServerErrorTemplate)
  }

  val enterCBCId = sec.AsyncAuthenticatedAction(){ authContext => implicit request =>
    getUserType(authContext).fold[Result](
      error    => errorRedirect(error),
      userType => Ok(forms.enterCBCId(includes.asideCbc(), includes.phaseBannerBeta(), userType, cbcIdForm))
    )
  }

  private def getCBCEnrolment(implicit hc:HeaderCarrier) : EitherT[Future,UnexpectedState,CBCEnrolment] = for {
    enrolment <- OptionT(enrolments.getEnrolments.map(_.find(_.key == "HMRC-CBC-ORG")))
      .toRight(UnexpectedState("Enrolment not found"))
    cbcString <- OptionT.fromOption[Future](enrolment.identifiers.find(_.key.equalsIgnoreCase("cbcid")).map(_.value))
      .toRight(UnexpectedState("Enrolment did not contain a cbcid"))
    cbcId     <- OptionT.fromOption[Future](CBCId(cbcString))
      .toRight(UnexpectedState(s"Enrolment contains an invalid cbcid: $cbcString"))
    utrString <- OptionT.fromOption[Future](enrolment.identifiers.find(_.key.equalsIgnoreCase("utr")).map(_.value))
      .toRight(UnexpectedState(s"Enrolment does not contain a utr"))
    utr       <- OptionT.fromOption[Future](if(Utr(utrString).isValid){ Some(Utr(utrString)) } else { None })
      .toRight(UnexpectedState(s"Enrolment contains an invalid utr $utrString"))
  } yield CBCEnrolment(cbcId,utr)


  private def saveSubscriptionDetails(s:SubscriptionDetails)(implicit hc:HeaderCarrier): Future[Unit] = for {
    _ <- cache.save(s.utr)
    _ <- cache.save(s.businessPartnerRecord)
    _ <- cache.save(s.cbcId)
  } yield ()

  val submitCBCId = sec.AsyncAuthenticatedAction() { authContext => implicit request =>
    getUserType(authContext).leftMap(errorRedirect).semiflatMap(userType =>
      cbcIdForm.bindFromRequest().fold[Future[Result]](
        errors => BadRequest(forms.enterCBCId(includes.asideCbc(), includes.phaseBannerBeta(), userType, errors)),
        id     => {
          subDataService.retrieveSubscriptionData(id).value.flatMap(_.fold(
            error    => errorRedirect(error),
            details  => details.fold[Future[Result]] {
              BadRequest(forms.enterCBCId(includes.asideCbc(), includes.phaseBannerBeta(), userType, cbcIdForm, true))
            }(subscriptionDetails => userType match {
              case Agent =>
                saveSubscriptionDetails(subscriptionDetails).map(_ =>
                  Redirect(uk.gov.hmrc.cbcrfrontend.controllers.routes.Subscription.enterKnownFacts())
                )
              case Organisation =>
                getCBCEnrolment.ensure(InvalidSession)(
                  _.cbcId === subscriptionDetails.cbcId
                ).fold[Future[Result]](
                  {
                    case InvalidSession => BadRequest(forms.enterCBCId(includes.asideCbc(), includes.phaseBannerBeta(), userType, cbcIdForm, false, true))
                    case error          => errorRedirect(error)
                  },
                  _     => {
                    saveSubscriptionDetails(subscriptionDetails).map(_ =>
                      Redirect(uk.gov.hmrc.cbcrfrontend.controllers.routes.FileUpload.chooseXMLFile())
                    )
                  }
                ).flatten
            })))
        })).merge
  }


  val signOut = sec.AsyncAuthenticatedAction() { authContext => implicit request => {
    val continue = s"?continue=${FrontendAppConfig.cbcrFrontendHost}${uk.gov.hmrc.cbcrfrontend.controllers.routes.CBCController.guidance.url}"
    Future.successful(Redirect(s"${FrontendAppConfig.cbcrFrontendHost}/gg/sign-out$continue"))
  }}


  def guidance =  Action.async { implicit request =>
    Future.successful(Ok(uk.gov.hmrc.cbcrfrontend.views.html.guidance.guidanceOverviewQa()))
  }

}


