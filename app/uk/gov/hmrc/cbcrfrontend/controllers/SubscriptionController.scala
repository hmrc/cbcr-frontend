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
import cats.instances.all._
import cats.syntax.all._
import play.api.Logger
import play.api.Play._
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.Messages.Implicits._
import play.api.libs.json.Json
import play.api.mvc.{Action, AnyContent, Request, Result}
import play.mvc.Http
import uk.gov.hmrc.cbcrfrontend._
import uk.gov.hmrc.cbcrfrontend.auth.SecuredActions
import uk.gov.hmrc.cbcrfrontend.connectors.{BPRKnownFactsConnector, EnrolmentsConnector}
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.model.Implicits._
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.services._
import uk.gov.hmrc.cbcrfrontend.util.CbcrSwitches
import uk.gov.hmrc.cbcrfrontend.views.html._
import uk.gov.hmrc.emailaddress.EmailAddress
import uk.gov.hmrc.http.cache.client.CacheMap
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}
import uk.gov.hmrc.play.audit.model.ExtendedDataEvent
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.frontend.auth.AuthContext
import uk.gov.hmrc.play.frontend.auth.connectors.{AuthConnector => PlayAuthConnector}
import uk.gov.hmrc.play.frontend.controller.FrontendController
import uk.gov.hmrc.play.http.HeaderCarrier
import uk.gov.hmrc.play.audit.AuditExtensions._

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class SubscriptionController @Inject()(val sec: SecuredActions,
                                       val subscriptionDataService: SubscriptionDataService,
                                       val connector:BPRKnownFactsConnector,
                                       val cbcIdService:CBCIdService,
                                       val kfService:CBCKnownFactsService,
                                       val enrolConnector:EnrolmentsConnector,
                                       val knownFactsService:BPRKnownFactsService)
                                      (implicit ec: ExecutionContext,
                             val playAuth:PlayAuthConnector,
                             val session:CBCSessionCache) extends FrontendController with ServicesConfig {

  lazy val audit: AuditConnector = FrontendAuditConnector

  val subscriptionDataForm: Form[SubscriberContact] = Form(
    mapping(
      "name"        -> nonEmptyText,
      "phoneNumber" -> nonEmptyText,
      "email"       -> email.verifying(EmailAddress.isValid(_))
    )((name: String, phoneNumber:String, email: String) =>
      SubscriberContact(name, phoneNumber, EmailAddress(email))
    )(sc => Some((sc.name,sc.phoneNumber, sc.email.value)))
  )

  val reconfirmEmailForm : Form[EmailAddress] = Form(
    mapping(
      "reconfirmEmail" -> email.verifying(EmailAddress.isValid(_))
    )(EmailAddress.apply)(EmailAddress.unapply)
  )

  val submitSubscriptionData: Action[AnyContent] = sec.AsyncAuthenticatedAction(Some(Organisation)) { authContext =>
    implicit request =>
      Logger.debug("Country by Country: Generate CBCId and Store Data")

      subscriptionDataForm.bindFromRequest.fold(
        errors => BadRequest(subscription.contactInfoSubscriber(includes.asideCbc(), includes.phaseBannerBeta(), errors)),
        data => {
          cbcIdService.getCbcId.flatMap {
            case Some(id) =>

              val result = for {
                bpr <- EitherT[Future, CBCErrors, BusinessPartnerRecord](
                  session.read[BusinessPartnerRecord].map(_.toRight(UnexpectedState("BPR record not found")))
                )
                utr <- EitherT[Future, CBCErrors, Utr](
                  session.read[Utr].map(_.toRight(UnexpectedState("UTR record not found")))
                )
                _ <- subscriptionDataService.saveSubscriptionData(SubscriptionDetails(bpr, data, id, utr))
                _ <- kfService.addKnownFactsToGG(CBCKnownFacts(utr, id))
                _ <- EitherT.right[Future, CBCErrors, (CacheMap,CacheMap,CacheMap)](
                  (session.save(id) |@| session.save(data) |@| session.save(SubscriptionDetails(bpr, data, id, utr))).tupled
                )
              } yield id


              result.fold[Future[Result]](
                error => {
                  Logger.error(error.show)
                  subscriptionDataService.clearSubscriptionData(id).fold(
                    errorRedirect,
                    _ => InternalServerError(FrontendGlobal.internalServerErrorTemplate)
                  )
                },
                _ => Redirect(routes.SubscriptionController.reconfirmEmail())
              ).flatten

            case None => InternalServerError(FrontendGlobal.internalServerErrorTemplate)
          }
        }
      )
  }


  val reconfirmEmail = sec.AsyncAuthenticatedAction() { authContext => implicit request =>

    OptionT(session.read[SubscriberContact]).cata(
      InternalServerError(FrontendGlobal.internalServerErrorTemplate),
      subscriberContactInfo => Ok(views.html.submission.reconfirmEmail(includes.asideCbc(), includes.phaseBannerBeta(), reconfirmEmailForm.fill(subscriberContactInfo.email)))
    )
  }

  val alreadySubscribed = sec.AsyncAuthenticatedAction(Some(Organisation)) { authContext => implicit request =>
    Future.successful(Ok(subscription.alreadySubscribed(includes.asideCbc(), includes.phaseBannerBeta())))
  }


  val reconfirmEmailSubmit = sec.AsyncAuthenticatedAction() { authContext => implicit request =>

    reconfirmEmailForm.bindFromRequest.fold(

      formWithErrors => BadRequest(views.html.submission.reconfirmEmail(
        includes.asideBusiness(), includes.phaseBannerBeta(), formWithErrors
      )),
      success => (for {
        subscribed        <- EitherT.right[Future,CBCErrors,Boolean](session.read[Subscribed.type].map(_.isDefined))
        _                 <- EitherT.cond[Future](!subscribed,(),UnexpectedState("Already subscribed"))
        subscriberContact <- OptionT(session.read[SubscriberContact]).toRight(UnexpectedState("SubscriberContact not found in the cache"))
        cbcId             <- OptionT(session.read[CBCId]).toRight(UnexpectedState("CBCId not found in the cache"))
        newSubscriberContact = subscriberContact.copy(email = success)
        _                 <- if(subscriberContact.email != success) EitherT.right[Future, CBCErrors, CacheMap](session.save[SubscriberContact](newSubscriberContact))
                             else EitherT.pure[Future,CBCErrors,Unit](())
        data              <- OptionT(session.read[SubscriptionDetails]).map(_.copy(subscriberContact = newSubscriberContact)).toRight(
          UnexpectedState("SubscriptionDetails not found in cache")
        )
        _                 <- EitherT.right[Future,CBCErrors,CacheMap](session.save(Subscribed))
        _                 <- createSuccessfulSubscriptionAuditEvent(authContext,data)
      } yield cbcId).fold(
        error => errorRedirect(error),
        cbcId => Redirect(routes.SubscriptionController.subscribeSuccessCbcId(cbcId.value))
      )
    )
  }


  val contactInfoSubscriber = sec.AsyncAuthenticatedAction(Some(Organisation)){ authContext => implicit request =>
    Ok(subscription.contactInfoSubscriber(includes.asideCbc(), includes.phaseBannerBeta(), subscriptionDataForm))
  }

  def subscribeSuccessCbcId(id:String) = sec.AsyncAuthenticatedAction(Some(Organisation)){ authContext => implicit request =>
    CBCId(id).fold[Future[Result]](
      InternalServerError(FrontendGlobal.internalServerErrorTemplate)
    )((cbcId: CBCId) =>
      Ok(subscription.subscribeSuccessCbcId(includes.asideBusiness(), includes.phaseBannerBeta(),cbcId,request.session.get("companyName")))
    )
  }

  def clearSubscriptionData(u:Utr) = sec.AsyncAuthenticatedAction(Some(Organisation)) { authContext => implicit request =>
    if(CbcrSwitches.clearSubscriptionDataRoute.enabled) {
      subscriptionDataService.clearSubscriptionData(u).fold(
        error => errorRedirect(error),
        {
          case Some(_) => Ok
          case None => NoContent
        }
      )
    } else {
      NotImplemented
    }
  }

  def createSuccessfulSubscriptionAuditEvent(authContext: AuthContext, subscriptionData:SubscriptionDetails)
                                           (implicit hc:HeaderCarrier, request:Request[_]): ServiceResponse[AuditResult.Success.type] =
    EitherT(audit.sendEvent(ExtendedDataEvent("Country-By-Country-Frontend", "CBCRSubscription",
      tags = hc.toAuditTags("CBCRSubscription", "N/A") + ("path" -> request.uri),
      detail = Json.toJson(subscriptionData)
    )).map{
      case AuditResult.Success         => Right(AuditResult.Success)
      case AuditResult.Failure(msg,_)  => Left(UnexpectedState(s"Unable to audit a successful submission: $msg"))
      case AuditResult.Disabled        => Right(AuditResult.Success)
    })


}
