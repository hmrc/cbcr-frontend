/*
 * Copyright 2023 HM Revenue & Customs
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

import cats.data.EitherT
import cats.implicits.{catsStdInstancesForFuture, catsSyntaxApply, catsSyntaxEitherId, toShow}
import play.api.Logging
import play.api.libs.json.{JsString, Json}
import play.api.mvc._
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.retrieve.Credentials
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.form.SubscriptionDataForm._
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.repositories.CBCSessionCache
import uk.gov.hmrc.cbcrfrontend.services._
import uk.gov.hmrc.cbcrfrontend.views.Views
import uk.gov.hmrc.cbcrfrontend.{errorRedirect, _}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}
import uk.gov.hmrc.play.audit.model.ExtendedDataEvent
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class SubscriptionController @Inject() (
  subscriptionDataService: SubscriptionDataService,
  cbcIdService: CBCIdService,
  emailService: EmailService,
  enrolService: EnrolmentsService,
  audit: AuditConnector,
  val authConnector: AuthConnector,
  messagesControllerComponents: MessagesControllerComponents,
  views: Views,
  cache: CBCSessionCache
)(implicit ec: ExecutionContext, feConfig: FrontendAppConfig)
    extends FrontendController(messagesControllerComponents) with AuthorisedFunctions with Logging {

  def alreadySubscribed: Action[AnyContent] = Action.async { implicit request =>
    authorised(AffinityGroup.Organisation and User) {
      Future.successful(Ok(views.alreadySubscribed()))
    }
  }

  def submitSubscriptionData: Action[AnyContent] = Action.async { implicit request =>
    authorised(AffinityGroup.Organisation and User).retrieve(Retrievals.credentials) { creds =>
      logger.debug("Country by Country: Generate CBCId and Store Data")
      subscriptionDataForm
        .bindFromRequest()
        .fold(
          errors => BadRequest(views.contactInfoSubscriber(errors)),
          data => {
            val id_bpr_utr: ServiceResponse[(CBCId, BusinessPartnerRecord, Utr)] = for {
              subscribed <- EitherT.right[CBCErrors](cache.readOption[Subscribed.type].map(_.isDefined))
              _          <- EitherT.cond[Future](!subscribed, (), UnexpectedState("Already subscribed"))
              bpr        <- cache.read[BusinessPartnerRecord]
              utr        <- cache.read[Utr]
              subDetails = SubscriptionDetails(bpr, data, None, utr)
              id <- cbcIdService.subscribe(subDetails).toRight[CBCErrors](UnexpectedState("Unable to get CBCId"))
            } yield Tuple3(id, bpr, utr)

            id_bpr_utr
              .semiflatMap { case (id, bpr, utr) =>
                val result = for {
                  _ <- subscriptionDataService.saveSubscriptionData(SubscriptionDetails(bpr, data, Some(id), utr))
                  _ <- enrolService.enrol(CBCKnownFacts(utr, id))
                  _ <- EitherT.liftF(cache.save[CBCId](id))
                  _ <- EitherT.liftF(cache.save[SubscriberContact](data))
                  _ <- EitherT.liftF(cache.save[SubscriptionDetails](SubscriptionDetails(bpr, data, Some(id), utr)))
                  _ <- EitherT.liftF(cache.save[Subscribed.type](Subscribed))
                  subscriptionEmailSent <-
                    EitherT.right[CBCErrors](cache.readOption[SubscriptionEmailSent].map(_.isDefined))
                  emailSent <- if (!subscriptionEmailSent)
                                 EitherT.right[CBCErrors](emailService.sendEmail(makeSubEmail(data, id)).value)
                               else EitherT.fromEither[Future](None.asRight[CBCErrors])
                  _ <- if (emailSent.getOrElse(false)) EitherT.right(cache.save(SubscriptionEmailSent()))
                       else EitherT.fromEither[Future](().asRight[CBCErrors])
                  _ <- createSuccessfulSubscriptionAuditEvent(creds, SubscriptionDetails(bpr, data, Some(id), utr))
                } yield id

                result
                  .fold[Future[Result]](
                    error => {
                      logger.error(error.show)
                      (createFailedSubscriptionAuditEvent(creds, id, bpr, utr) *>
                        subscriptionDataService.clearSubscriptionData(id)).fold(
                        (error: CBCErrors) => errorRedirect(error, views.notAuthorisedIndividual, views.errorTemplate),
                        _ =>
                          errorRedirect(
                            UnexpectedState("Something went wrong so cleared SubscriptionData"),
                            views.notAuthorisedIndividual,
                            views.errorTemplate
                          )
                      )
                    },
                    _ => Redirect(routes.SubscriptionController.subscribeSuccessCbcId(id.value))
                  )
                  .flatten
              }
              .leftMap((error: CBCErrors) => errorRedirect(error, views.notAuthorisedIndividual, views.errorTemplate))
              .merge
          }
        )
    }
  }

  private def makeSubEmail(subscriberContact: SubscriberContact, cbcId: CBCId): Email =
    Email(
      List(subscriberContact.email.value),
      "cbcr_subscription",
      Map("f_name" -> subscriberContact.firstName, "s_name" -> subscriberContact.lastName, "cbcrId" -> cbcId.value)
    )

  def contactInfoSubscriber: Action[AnyContent] = Action.async { implicit request =>
    authorised(AffinityGroup.Organisation and User) {
      Ok(views.contactInfoSubscriber(subscriptionDataForm))
    }
  }

  def updateInfoSubscriber: Action[AnyContent] = Action.async { implicit request =>
    authorised(AffinityGroup.Organisation and User).retrieve(cbcEnrolment) { cbcEnrolment =>
      val subscriptionData: EitherT[Future, CBCErrors, (ETMPSubscription, CBCId)] = for {
        cbcId           <- EitherT.fromOption[Future](cbcEnrolment.map(_.cbcId), UnexpectedState("Couldn't get CBCId"))
        optionalDetails <- subscriptionDataService.retrieveSubscriptionData(Right(cbcId))
        details         <- EitherT.fromOption[Future](optionalDetails, UnexpectedState("No SubscriptionDetails"))
        bpr = details.businessPartnerRecord
        _       <- EitherT.right(cache.save(bpr) *> cache.save(cbcId))
        subData <- cbcIdService.getETMPSubscriptionData(bpr.safeId).toRight(NoETMPSubscriptionData: CBCErrors)
      } yield subData -> cbcId

      subscriptionData.fold[Result](
        {
          case NoETMPSubscriptionData => Redirect(routes.SharedController.contactDetailsError)
          case error                  => errorRedirect(error, views.notAuthorisedIndividual, views.errorTemplate)
        },
        { case subData -> cbcId =>
          val prepopulatedForm = subscriptionDataForm.bind(
            Map(
              "firstName"   -> subData.names.name1,
              "lastName"    -> subData.names.name2,
              "email"       -> subData.contact.email.value,
              "phoneNumber" -> subData.contact.phoneNumber
            )
          )
          Ok(views.updateContactInfoSubscriber(prepopulatedForm, cbcId))
        }
      )
    }
  }

  def saveUpdatedInfoSubscriber: Action[Map[String, Seq[String]]] = Action.async(parse.formUrlEncoded) {
    implicit request =>
      authorised(AffinityGroup.Organisation and User).retrieve(cbcEnrolment) { cbcEnrolment =>
        val ci: ServiceResponse[CBCId] = for {
          cbcId <- EitherT.fromEither[Future](
                     cbcEnrolment.map(_.cbcId).toRight[CBCErrors](UnexpectedState("Couldn't get CBCId"))
                   )
        } yield cbcId

        subscriptionDataForm
          .bindFromRequest()
          .fold(
            errors =>
              ci.fold(
                (error: CBCErrors) => errorRedirect(error, views.notAuthorisedIndividual, views.errorTemplate),
                cbcId => BadRequest(views.updateContactInfoSubscriber(errors, cbcId))
              ),
            data =>
              (for {
                bpr   <- cache.read[BusinessPartnerRecord]
                cbcId <- cache.read[CBCId]
                details = CorrespondenceDetails(
                            bpr.address,
                            ContactDetails(data.email, data.phoneNumber),
                            ContactName(data.firstName, data.lastName)
                          )
                _ <- cbcIdService.updateETMPSubscriptionData(bpr.safeId, details)
                _ <- subscriptionDataService.updateSubscriptionData(
                       cbcId,
                       SubscriberContact(data.firstName, data.lastName, data.phoneNumber, data.email)
                     )
              } yield Redirect(routes.SubscriptionController.savedUpdatedInfoSubscriber)).fold(
                errors => errorRedirect(errors, views.notAuthorisedIndividual, views.errorTemplate),
                result => result
              )
          )
      }
  }

  def savedUpdatedInfoSubscriber: Action[AnyContent] = Action.async { implicit request =>
    authorised(AffinityGroup.Organisation and User) {
      Ok(views.contactDetailsUpdated())
    }
  }

  def subscribeSuccessCbcId(id: String): Action[AnyContent] = Action.async { implicit request =>
    authorised(AffinityGroup.Organisation and User) {
      CBCId(id).fold[Future[Result]](
        errorRedirect(UnexpectedState(s"CBCId: $id is not valid"), views.notAuthorisedIndividual, views.errorTemplate)
      )((cbcId: CBCId) => Ok(views.subscribeSuccessCbcId(cbcId, request.session.get("companyName"))))
    }
  }

  private def createFailedSubscriptionAuditEvent(
    credentials: Option[Credentials],
    cbcId: CBCId,
    bpr: BusinessPartnerRecord,
    utr: Utr
  )(implicit hc: HeaderCarrier): ServiceResponse[AuditResult.Success.type] =
    for {
      result <- EitherT[Future, CBCErrors, AuditResult.Success.type](
                  audit
                    .sendExtendedEvent(
                      ExtendedDataEvent(
                        "Country-By-Country-Frontend",
                        "CBCRFailedSubscription",
                        detail = getDetailsFailedSubscription(cbcId, credentials, bpr, utr)
                      )
                    )
                    .map {
                      case AuditResult.Disabled => Right(AuditResult.Success)
                      case AuditResult.Success  => Right(AuditResult.Success)
                      case AuditResult.Failure(msg, _) =>
                        Left(UnexpectedState(s"Unable to audit a failed subscription: $msg"))
                    }
                )
    } yield result

  private def createSuccessfulSubscriptionAuditEvent(
    credentials: Option[Credentials],
    subscriptionData: SubscriptionDetails
  )(implicit hc: HeaderCarrier, request: Request[_]): ServiceResponse[AuditResult.Success.type] =
    for {
      result <- EitherT[Future, CBCErrors, AuditResult.Success.type](
                  audit
                    .sendExtendedEvent(
                      ExtendedDataEvent(
                        "Country-By-Country-Frontend",
                        "CBCRSubscription",
                        detail = getDetailsSuccessfulSubscription(request, credentials, subscriptionData)
                      )
                    )
                    .map {
                      case AuditResult.Disabled => Right(AuditResult.Success)
                      case AuditResult.Success  => Right(AuditResult.Success)
                      case AuditResult.Failure(msg, _) =>
                        Left(UnexpectedState(s"Unable to audit a successful subscription: $msg"))
                    }
                )
    } yield result

  private def getDetailsSuccessfulSubscription(
    request: Request[_],
    creds: Option[Credentials],
    subscrDetails: SubscriptionDetails
  ) =
    creds match {
      case Some(c) =>
        Json.obj(
          "path"             -> JsString(request.uri),
          c.providerType     -> JsString(c.providerId),
          "subscriptionData" -> Json.toJson(subscrDetails)
        )
      case None => Json.obj("path" -> JsString(request.uri), "subscriptionData" -> Json.toJson(subscrDetails))
    }

  private def getDetailsFailedSubscription(
    cbcId: CBCId,
    creds: Option[Credentials],
    bpr: BusinessPartnerRecord,
    utr: Utr
  ) =
    creds match {
      case Some(c) =>
        Json.obj(
          "path"                  -> JsString(cbcId.value),
          c.providerType          -> JsString(c.providerId),
          "businessPartnerRecord" -> Json.toJson(bpr),
          "utr"                   -> JsString(utr.value)
        )
      case None =>
        Json.obj(
          "path"                  -> JsString(cbcId.value),
          "businessPartnerRecord" -> Json.toJson(bpr),
          "utr"                   -> JsString(utr.value)
        )
    }
}
