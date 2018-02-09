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

import cats.data.EitherT
import cats.instances.all._
import cats.syntax.all._
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.libs.json.{JsString, Json}
import play.api.mvc.{Action, AnyContent, Request, Result}
import play.api.{Configuration, Environment, Logger}
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.retrieve.{Credentials, Retrievals}
import uk.gov.hmrc.cbcrfrontend._
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.connectors.BPRKnownFactsConnector
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.form.SubscriptionDataForm._
import uk.gov.hmrc.cbcrfrontend.model.Implicits.format
import uk.gov.hmrc.cbcrfrontend.model.{SubscriptionEmailSent, _}
import uk.gov.hmrc.cbcrfrontend.services._
import uk.gov.hmrc.cbcrfrontend.util.CbcrSwitches
import uk.gov.hmrc.cbcrfrontend.views.html._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.AuditExtensions._
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}
import uk.gov.hmrc.play.audit.model.ExtendedDataEvent
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class SubscriptionController @Inject()(val messagesApi:MessagesApi,
                                       val subscriptionDataService: SubscriptionDataService,
                                       val connector: BPRKnownFactsConnector,
                                       val cbcIdService: CBCIdService,
                                       val emailService: EmailService,
                                       val enrolService: EnrolmentsService,
                                       val knownFactsService: BPRKnownFactsService,
                                       val env:Environment,
                                       val audit:AuditConnector,
                                       val authConnector:AuthConnector)
                                      (implicit ec: ExecutionContext,
                                       val cache: CBCSessionCache,
                                       val config:Configuration,
                                       feConfig:FrontendAppConfig) extends FrontendController with AuthorisedFunctions with I18nSupport{



  val alreadySubscribed = Action.async{ implicit request =>
    authorised(AffinityGroup.Organisation and (User or Admin) ) {
      Future.successful(Ok(subscription.alreadySubscribed()))
    }
  }


  val submitSubscriptionData: Action[AnyContent] = Action.async { implicit request =>
    authorised(AffinityGroup.Organisation and (User or Admin)).retrieve(Retrievals.credentials) { creds =>
      Logger.debug("Country by Country: Generate CBCId and Store Data")
      subscriptionDataForm.bindFromRequest.fold(
        errors => BadRequest(subscription.contactInfoSubscriber( errors)),
        data   => {
          val id_bpr_utr: ServiceResponse[(CBCId, BusinessPartnerRecord, Utr)] = for {
            subscribed <- right[Boolean](cache.readOption[Subscribed.type].map(_.isDefined))
            _          <- EitherT.cond[Future](!subscribed, (), UnexpectedState("Already subscribed"))
            bpr_utr    <- (cache.read[BusinessPartnerRecord] |@| cache.read[Utr]).tupled
            subDetails = SubscriptionDetails(bpr_utr._1, data, None, bpr_utr._2)
            id         <- cbcIdService.subscribe(subDetails).toRight[CBCErrors](UnexpectedState("Unable to get CBCId"))
          } yield Tuple3(id, bpr_utr._1, bpr_utr._2)

          id_bpr_utr.semiflatMap {
            case (id, bpr, utr) =>

              val result = for {
                _ <- subscriptionDataService.saveSubscriptionData(SubscriptionDetails(bpr, data, Some(id), utr))
                _ <- enrolService.enrol(CBCKnownFacts(utr, id))
                _ <- right(
                  (cache.save(id) |@|
                    cache.save(data) |@|
                    cache.save(SubscriptionDetails(bpr, data, Some(id), utr)) |@|
                    cache.save(Subscribed)).tupled
                )
                subscriptionEmailSent <- right(cache.readOption[SubscriptionEmailSent].map(_.isDefined))
                emailSent <- if (!subscriptionEmailSent) right(emailService.sendEmail(makeSubEmail(data, id)).value)
                else pure[Option[Boolean]](None)
                _ <- if (emailSent.getOrElse(false)) right(cache.save(SubscriptionEmailSent()))
                else pure(())
                _ <- createSuccessfulSubscriptionAuditEvent(creds, SubscriptionDetails(bpr, data, Some(id), utr))
              } yield id

              result.fold[Future[Result]](
                error => {
                  Logger.error(error.show)
                  (createFailedSubscriptionAuditEvent(creds, id, bpr, utr) *>
                    subscriptionDataService.clearSubscriptionData(id)).fold(
                    errorRedirect,
                    _ => errorRedirect(UnexpectedState("Something went wrong so cleared SubscriptionData"))
                  )
                },
                _ => Redirect(routes.SubscriptionController.subscribeSuccessCbcId(id.value))
              ).flatten

          }.leftMap(errorRedirect).merge
        }
      )
    }
  }

  private def makeSubEmail(subscriberContact: SubscriberContact, cbcId: CBCId): Email = {
    Email(List(subscriberContact.email.value),
      "cbcr_subscription",
      Map("f_name" → subscriberContact.firstName,
        "s_name" → subscriberContact.lastName,
        "cbcrId" → cbcId.value))
  }


  val contactInfoSubscriber = Action.async{ implicit  request =>
    authorised(AffinityGroup.Organisation and (User or Admin)) {
        Ok(subscription.contactInfoSubscriber( subscriptionDataForm))
    }
  }


  val updateInfoSubscriber = Action.async { implicit request =>
    authorised(AffinityGroup.Organisation and (User or Admin)).retrieve(cbcEnrolment) { cbcEnrolment =>

      val subscriptionData: ServiceResponse[ETMPSubscription] = for {
        cbcId           <- fromEither(cbcEnrolment.map(_.cbcId).toRight[CBCErrors](UnexpectedState("Couldn't get CBCId")))
        optionalDetails <- subscriptionDataService.retrieveSubscriptionData(Right(cbcId))
        details         <- EitherT.fromOption[Future](optionalDetails, UnexpectedState("No SubscriptionDetails"))
        bpr              = details.businessPartnerRecord
        _               <- right(cache.save(bpr) *> cache.save(cbcId))
        subData         <- cbcIdService.getETMPSubscriptionData(bpr.safeId).toRight(UnexpectedState("No ETMP Subscription Data"): CBCErrors)
      } yield subData

      subscriptionData.fold(
        error => BadRequest(error.toString),
        data  => {
          val prepopulatedForm = subscriptionDataForm.bind(Map(
            "firstName"   -> data.names.name1,
            "lastName"    -> data.names.name2,
            "email"       -> data.contact.email.value,
            "phoneNumber" -> data.contact.phoneNumber
          ))
          Ok(update.updateContactInfoSubscriber(prepopulatedForm))
        }
      )
    }
  }

  val saveUpdatedInfoSubscriber = Action.async { implicit request =>
    authorised(AffinityGroup.Organisation and (User or Admin)) {

      subscriptionDataForm.bindFromRequest.fold(
        errors => BadRequest(update.updateContactInfoSubscriber( errors)),
        data => {
          (for {
            bpr     <- cache.read[BusinessPartnerRecord]
            cbcId   <- cache.read[CBCId]
            details = CorrespondenceDetails(bpr.address, ContactDetails(data.email, data.phoneNumber), ContactName(data.firstName, data.lastName))
            _       <- cbcIdService.updateETMPSubscriptionData(bpr.safeId, details)
            _       <- subscriptionDataService.updateSubscriptionData(cbcId, SubscriberContact(data.firstName, data.lastName, data.phoneNumber, data.email))
          } yield Ok("Saved")).leftMap(errorRedirect).merge
        }
      )
    }
  }

  def subscribeSuccessCbcId(id: String) = Action.async { implicit request =>
    authorised(AffinityGroup.Organisation and (User or Admin)) {
      CBCId(id).fold[Future[Result]](
        errorRedirect(UnexpectedState(s"CBCId: $id is not valid"))
      )((cbcId: CBCId) =>
        Ok(subscription.subscribeSuccessCbcId( cbcId, request.session.get("companyName")))
      )
    }
  }

  def clearSubscriptionData(u: Utr) = Action.async { implicit request =>
    authorised(AffinityGroup.Organisation and (User or Admin)) {
      if (CbcrSwitches.clearSubscriptionDataRoute.enabled) {
        subscriptionDataService.clearSubscriptionData(u).fold(
          error => errorRedirect(error), {
            case Some(_) => Ok
            case None => NoContent
          }
        )
      } else NotImplemented
    }
  }

  def createFailedSubscriptionAuditEvent(credentials: Credentials, cbcId:CBCId, bpr:BusinessPartnerRecord,utr:Utr)
                                            (implicit hc: HeaderCarrier, request: Request[_]): ServiceResponse[AuditResult.Success.type] =
    for {
      result <- eitherT[AuditResult.Success.type](audit.sendExtendedEvent(ExtendedDataEvent("Country-By-Country-Frontend", "CBCRFailedSubscription",
        tags = hc.toAuditTags("CBCRFailedSubscription", "N/A") + (credentials.providerType -> credentials.providerId),
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



  def createSuccessfulSubscriptionAuditEvent(credentials: Credentials, subscriptionData: SubscriptionDetails)
                                            (implicit hc: HeaderCarrier, request: Request[_]): ServiceResponse[AuditResult.Success.type] =
    for {
      result <- eitherT[AuditResult.Success.type](audit.sendExtendedEvent(ExtendedDataEvent("Country-By-Country-Frontend", "CBCRSubscription",
        tags = hc.toAuditTags("CBCRSubscription", "N/A") + ("path" -> request.uri, credentials.providerType -> credentials.providerId),
        detail = Json.toJson(subscriptionData)
      )).map {
        case AuditResult.Disabled        => Right(AuditResult.Success)
        case AuditResult.Success         => Right(AuditResult.Success)
        case AuditResult.Failure(msg, _) => Left(UnexpectedState(s"Unable to audit a successful subscription: $msg"))
      })
    } yield result


}
