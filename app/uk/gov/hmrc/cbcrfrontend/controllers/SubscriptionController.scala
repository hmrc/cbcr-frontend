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

import cats.data
import cats.data.{EitherT, OptionT}
import cats.instances.all._
import cats.syntax.all._
import play.api.Logger
import play.api.Play._
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.Messages.Implicits._
import play.api.libs.json.{JsString, Json}
import play.api.mvc.{Action, AnyContent, Request, Result}
import play.mvc.Http
import uk.gov.hmrc.cbcrfrontend._
import uk.gov.hmrc.cbcrfrontend.auth.SecuredActions
import uk.gov.hmrc.cbcrfrontend.connectors.{BPRKnownFactsConnector, EnrolmentsConnector, TaxEnrolmentsConnector}
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
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
import uk.gov.hmrc.cbcrfrontend.model.SubscriptionEmailSent
import uk.gov.hmrc.cbcrfrontend.form.SubscriptionDataForm._

import scala.concurrent.{ExecutionContext, Future}
import uk.gov.hmrc.cbcrfrontend.model.Implicits.format

@Singleton
class SubscriptionController @Inject()(val sec: SecuredActions,
                                       val subscriptionDataService: SubscriptionDataService,
                                       val connector: BPRKnownFactsConnector,
                                       val cbcIdService: CBCIdService,
                                       val emailService: EmailService,
                                       val enrolService: EnrolmentsService,
                                       val enrollments: EnrolmentsConnector,
                                       val knownFactsService: BPRKnownFactsService)
                                      (implicit ec: ExecutionContext,
                                       val playAuth: PlayAuthConnector,
                                       val cache: CBCSessionCache) extends FrontendController with ServicesConfig {

  lazy val audit: AuditConnector = FrontendAuditConnector

  val alreadySubscribed = sec.AsyncAuthenticatedAction(Some(Organisation(true))) { authContext =>
    implicit request =>
      Future.successful(Ok(subscription.alreadySubscribed(includes.asideCbc(), includes.phaseBannerBeta())))
  }


  val submitSubscriptionData: Action[AnyContent] = sec.AsyncAuthenticatedAction(Some(Organisation(true))) { authContext =>
    implicit request =>
      Logger.debug("Country by Country: Generate CBCId and Store Data")
      subscriptionDataForm.bindFromRequest.fold(
        errors => {
          BadRequest(subscription.contactInfoSubscriber(includes.asideCbc(), includes.phaseBannerBeta(), errors))
        },
        data => {
          val id_bpr_utr: ServiceResponse[(CBCId, BusinessPartnerRecord, Utr)] = for {
            subscribed <- right[Boolean](cache.readOption[Subscribed.type].map(_.isDefined))
            _          <- EitherT.cond[Future](!subscribed, (), UnexpectedState("Already subscribed"))
            bpr_utr    <- ( cache.read[BusinessPartnerRecord] |@| cache.read[Utr] ).tupled
            subDetails = SubscriptionDetails(bpr_utr._1, data, None, bpr_utr._2)
            id         <- cbcIdService.subscribe(subDetails).toRight[CBCErrors](UnexpectedState("Unable to get CBCId"))
          } yield Tuple3(id, bpr_utr._1, bpr_utr._2)

          id_bpr_utr.semiflatMap {
            case (id, bpr, utr) =>

              val result = for {
                _                     <- subscriptionDataService.saveSubscriptionData(SubscriptionDetails(bpr, data, Some(id), utr))
                _                     <- enrolService.enrol(CBCKnownFacts(utr, id))
                _                     <- right(
                  (cache.save(id)                                            |@|
                   cache.save(data)                                          |@|
                   cache.save(SubscriptionDetails(bpr, data, Some(id), utr)) |@|
                   cache.save(Subscribed)).tupled
                )
                subscriptionEmailSent <- right(cache.readOption[SubscriptionEmailSent].map(_.isDefined))
                emailSent             <- if (!subscriptionEmailSent) right(emailService.sendEmail(makeSubEmail(data, id)).value)
                                         else pure[Option[Boolean]](None)
                _                     <- if (emailSent.getOrElse(false)) right(cache.save(SubscriptionEmailSent()))
                                         else pure(())
                _                     <- createSuccessfulSubscriptionAuditEvent(authContext, SubscriptionDetails(bpr, data, Some(id), utr))
              } yield id

              result.fold[Future[Result]](
                error => {
                  Logger.error(error.show)
                  (createFailedSubscriptionAuditEvent(authContext,id,bpr,utr) *>
                   subscriptionDataService.clearSubscriptionData(id)).fold(
                    errorRedirect,
                    _ => InternalServerError(FrontendGlobal.internalServerErrorTemplate)
                  )
                },
                _ => Redirect(routes.SubscriptionController.subscribeSuccessCbcId(id.value))
              ).flatten

          }.leftMap(errors => errorRedirect(errors)).merge
        }
      )
  }

  private def makeSubEmail(subscriberContact: SubscriberContact, cbcId: CBCId): Email = {
    Email(List(subscriberContact.email.value),
      "cbcr_subscription",
      Map("f_name" → subscriberContact.firstName,
        "s_name" → subscriberContact.lastName,
        "cbcrId" → cbcId.value))
  }


  val contactInfoSubscriber = sec.AsyncAuthenticatedAction(Some(Organisation(true))) { authContext =>
    implicit request =>
      Ok(subscription.contactInfoSubscriber(includes.asideCbc(), includes.phaseBannerBeta(), subscriptionDataForm))
  }


  val updateInfoSubscriber = sec.AsyncAuthenticatedAction(Some(Organisation(true))) { authContext =>
    implicit request =>

      val subscriptionData: EitherT[Future, CBCErrors, (String,String,String,String, String)] = for {
        cbcId           <- enrollments.getCbcId.toRight(UnexpectedState("Couldn't get CBCId"))
        optionalDetails <- subscriptionDataService.retrieveSubscriptionData(Right(cbcId))
        details         <- EitherT.fromOption[Future](optionalDetails, UnexpectedState("No SubscriptionDetails"))
        bpr             =  details.businessPartnerRecord
        _               <- right(cache.save(bpr) *> cache.save(cbcId))
        subData         <- cbcIdService.getETMPSubscriptionData(bpr.safeId).toRight(UnexpectedState("No ETMP Subscription Data"): CBCErrors)
      } yield Tuple5(subData.names.name1,subData.names.name2,subData.contact.email.value,subData.contact.phoneNumber,cbcId.value)

      subscriptionData.fold[Result](
        (error: CBCErrors) => BadRequest(error.toString),
        tuple5 => {
          val prepopulatedForm = subscriptionDataForm.bind(Map(
            "firstName" -> tuple5._1,
            "lastName" -> tuple5._2,
            "email" -> tuple5._3,
            "phoneNumber" -> tuple5._4
          ))
          Ok(update.updateContactInfoSubscriber(includes.asideCbc(), includes.phaseBannerBeta(), prepopulatedForm, tuple5._5))
        }
      )

  }

  val saveUpdatedInfoSubscriber = sec.AsyncAuthenticatedAction(Some(Organisation(true))) { authContext =>
    implicit requests =>

      val ci: EitherT[Future, CBCErrors, (String)] = for {
        cbcId           <- enrollments.getCbcId.toRight(UnexpectedState("Couldn't get CBCId"): CBCErrors)
      } yield cbcId.value

      subscriptionDataForm.bindFromRequest.fold(
        errors => {
          ci.fold((error: CBCErrors) => BadRequest(error.toString), cbcId => {
            BadRequest(update.updateContactInfoSubscriber(includes.asideCbc(), includes.phaseBannerBeta(), errors, cbcId))
          })
        },
        data => {
          (for {
            bpr     <- cache.read[BusinessPartnerRecord]
            cbcId   <- cache.read[CBCId]
            details = CorrespondenceDetails(bpr.address, ContactDetails(data.email, data.phoneNumber), ContactName(data.firstName, data.lastName))
            _       <- cbcIdService.updateETMPSubscriptionData(bpr.safeId, details)
            _       <- subscriptionDataService.updateSubscriptionData(cbcId, SubscriberContact(data.firstName, data.lastName, data.phoneNumber, data.email))
          } yield Redirect(routes.SubscriptionController.savedUpdatedInfoSubscriber())
            ).fold(
            errors => errorRedirect(errors),
            result => result
          )
        }
      )
  }

  val savedUpdatedInfoSubscriber = sec.AsyncAuthenticatedAction(){ _ => implicit request =>
    Ok(views.html.update.contactDetailsUpdated(includes.asideCbc(), includes.phaseBannerBeta()))
  }

  def subscribeSuccessCbcId(id: String) = sec.AsyncAuthenticatedAction(Some(Organisation(true))) { authContext =>
    implicit request =>
      CBCId(id).fold[Future[Result]](
        InternalServerError(FrontendGlobal.internalServerErrorTemplate)
      )((cbcId: CBCId) =>
        Ok(subscription.subscribeSuccessCbcId(includes.asideBusiness(), includes.phaseBannerBeta(), cbcId, request.session.get("companyName")))
      )
  }

  def clearSubscriptionData(u: Utr) = sec.AsyncAuthenticatedAction(Some(Organisation(true))) { authContext =>
    implicit request =>
      if (CbcrSwitches.clearSubscriptionDataRoute.enabled) {
        subscriptionDataService.clearSubscriptionData(u).fold(
          error => errorRedirect(error), {
            case Some(_) => Ok
            case None    => NoContent
          }
        )
      } else {
        NotImplemented
      }
  }

  def createFailedSubscriptionAuditEvent(authContext: AuthContext, cbcId:CBCId, bpr:BusinessPartnerRecord,utr:Utr)
                                            (implicit hc: HeaderCarrier, request: Request[_]): ServiceResponse[AuditResult.Success.type] =
    for {
      ggId   <- right(getUserGGId(authContext))
      result <- eitherT[AuditResult.Success.type](audit.sendEvent(ExtendedDataEvent("Country-By-Country-Frontend", "CBCRFailedSubscription",
        tags = hc.toAuditTags("CBCRFailedSubscription", "N/A") + ("ggId" -> ggId.authProviderId),
        detail = Json.obj(
          "cbcId"                 -> JsString(cbcId.value),
          "businessPartnerRecord" -> Json.toJson(bpr),
          "utr"                   -> JsString(utr.value))
      )).map {
        case AuditResult.Disabled        => Right(AuditResult.Success)
        case AuditResult.Success         => Right(AuditResult.Success)
        case AuditResult.Failure(msg, _) => Left(UnexpectedState(s"Unable to audit a failed subscription: $msg"))
      })
    } yield result



  def createSuccessfulSubscriptionAuditEvent(authContext: AuthContext, subscriptionData: SubscriptionDetails)
                                            (implicit hc: HeaderCarrier, request: Request[_]): ServiceResponse[AuditResult.Success.type] =
    for {
      ggId   <- right(getUserGGId(authContext))
      result <- eitherT[AuditResult.Success.type](audit.sendEvent(ExtendedDataEvent("Country-By-Country-Frontend", "CBCRSubscription",
        tags = hc.toAuditTags("CBCRSubscription", "N/A") + ("path" -> request.uri, "ggId" -> ggId.authProviderId),
        detail = Json.toJson(subscriptionData)
      )).map {
        case AuditResult.Disabled        => Right(AuditResult.Success)
        case AuditResult.Success         => Right(AuditResult.Success)
        case AuditResult.Failure(msg, _) => Left(UnexpectedState(s"Unable to audit a successful subscription: $msg"))
      })
    } yield result


}
