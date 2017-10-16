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
import scala.concurrent.{ExecutionContext, Future}
import uk.gov.hmrc.cbcrfrontend.model.Implicits.format
@Singleton
class SubscriptionController @Inject()(val sec: SecuredActions,
                                       val subscriptionDataService: SubscriptionDataService,
                                       val connector: BPRKnownFactsConnector,
                                       val cbcIdService: CBCIdService,
                                       val emailService: EmailService,
                                       val kfService: CBCKnownFactsService,
                                       val enrollments: EnrolmentsConnector,
                                       val knownFactsService: BPRKnownFactsService)
                                      (implicit ec: ExecutionContext,
                                       val playAuth: PlayAuthConnector,
                                       val cache: CBCSessionCache) extends FrontendController with ServicesConfig {

  lazy val audit: AuditConnector = FrontendAuditConnector

  val subscriptionDataForm: Form[SubscriberContact] = Form(
    mapping(
      "firstName" -> nonEmptyText,
      "lastName" -> nonEmptyText,
      "phoneNumber" -> nonEmptyText.verifying(_.matches("""^[A-Z0-9 )/(-*#]{1,24}$""")),
      "email" -> email.verifying(EmailAddress.isValid(_)).transform[EmailAddress](EmailAddress(_), _.value)
    )(SubscriberContact.apply)(SubscriberContact.unapply)
  )

  val alreadySubscribed = sec.AsyncAuthenticatedAction(Some(Organisation)) { authContext =>
    implicit request =>
      Future.successful(Ok(subscription.alreadySubscribed(includes.asideCbc(), includes.phaseBannerBeta())))
  }

  val submitSubscriptionData: Action[AnyContent] = sec.AsyncAuthenticatedAction(Some(Organisation)) { authContext =>
    implicit request =>
      Logger.debug("Country by Country: Generate CBCId and Store Data")

      subscriptionDataForm.bindFromRequest.fold(
        errors => BadRequest(subscription.contactInfoSubscriber(includes.asideCbc(), includes.phaseBannerBeta(), errors)),
        data => {
          val id_bpr_utr:ServiceResponse[(CBCId,BusinessPartnerRecord,Utr)] = for {
            subscribed        <- EitherT.right[Future,CBCErrors,Boolean](cache.read[Subscribed.type].map(_.isDefined))
            _                 <- EitherT.cond[Future](!subscribed,(),UnexpectedState("Already subscribed"))
            bpr_utr           <- (EitherT[Future, CBCErrors, BusinessPartnerRecord](
              cache.read[BusinessPartnerRecord].map(_.toRight(UnexpectedState("BPR record not found")))
            ) |@| EitherT[Future, CBCErrors, Utr](
              cache.read[Utr].map(_.toRight(UnexpectedState("UTR record not found")))
            )).tupled
            subDetails        = SubscriptionDetails(bpr_utr._1, data, None, bpr_utr._2)
            id                <- cbcIdService.subscribe(subDetails).toRight[CBCErrors](UnexpectedState("Unable to get CBCId"))
          } yield Tuple3(id, bpr_utr._1, bpr_utr._2)

          id_bpr_utr.semiflatMap{
            case (id,bpr,utr) =>

              val result = for {
                _ <- subscriptionDataService.saveSubscriptionData(SubscriptionDetails(bpr, data, Some(id), utr))
                _ <- kfService.addKnownFactsToGG(CBCKnownFacts(utr, id))
                _ <- EitherT.right[Future, CBCErrors, (CacheMap,CacheMap,CacheMap, CacheMap)](
                  (cache.save(id) |@| cache.save(data) |@| cache.save(SubscriptionDetails(bpr, data, Some(id), utr)) |@| cache.save(Subscribed)).tupled
                )
                subscriptionEmailSent <- EitherT.right[Future, CBCErrors, Boolean](cache.read[SubscriptionEmailSent].map(_.isDefined))
                emailSent ← if (!subscriptionEmailSent)EitherT.right[Future, CBCErrors, Option[Boolean]](emailService.sendEmail(makeSubEmail(data, id)).value)
                            else EitherT.pure[Future, CBCErrors, Option[Boolean]](None)
                _ <- if (emailSent.getOrElse(false)) EitherT.right[Future, CBCErrors, CacheMap](cache.save(SubscriptionEmailSent()))
                     else EitherT.pure[Future, CBCErrors, Unit](())
                _ <- createSuccessfulSubscriptionAuditEvent(authContext,SubscriptionDetails(bpr, data, Some(id), utr))
              } yield id


              result.fold[Future[Result]](
                error => {
                  Logger.error(error.show)
                  subscriptionDataService.clearSubscriptionData(id).fold(
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


  val contactInfoSubscriber = sec.AsyncAuthenticatedAction(Some(Organisation)){ authContext => implicit request =>
    Ok(subscription.contactInfoSubscriber(includes.asideCbc(), includes.phaseBannerBeta(), subscriptionDataForm))
  }


  val updateInfoSubscriber = sec.AsyncAuthenticatedAction(Some(Organisation)) { authContext =>
    implicit request =>

    val subscriptionData: ServiceResponse[ETMPSubscription] = for {
      cbcId <- enrollments.getCbcId.toRight(UnexpectedState("Couldn't get CBCId"))
      optionalDetails <- subscriptionDataService.retrieveSubscriptionData(Right(cbcId))
      details <- EitherT.fromOption[Future](optionalDetails, UnexpectedState("No SubscriptionDetails"))
      bpr = details.businessPartnerRecord
      _ <- EitherT.right[Future, CBCErrors, CacheMap](cache.save(bpr))
      _ <- EitherT.right[Future, CBCErrors, CacheMap](cache.save(cbcId))
      subData <- cbcIdService.getETMPSubscriptionData(bpr.safeId).toRight(UnexpectedState("No ETMP Subscription Data"): CBCErrors)
    } yield subData

    subscriptionData.fold(
      error => BadRequest(error.toString),
      data => {
        val prepopulatedForm = subscriptionDataForm.bind(Map(
          "firstName" -> data.names.name1,
          "lastName" -> data.names.name2,
          "email" -> data.contact.email.value,
          "phoneNumber" -> data.contact.phoneNumber
        ))
        Ok(update.updateContactInfoSubscriber(includes.asideCbc(), includes.phaseBannerBeta(), prepopulatedForm))
      }
    )

  }

  val saveUpdatedInfoSubscriber = sec.AsyncAuthenticatedAction(Some(Organisation)) { authContext =>
    implicit requests =>
      subscriptionDataForm.bindFromRequest.fold(
        errors => BadRequest(update.updateContactInfoSubscriber(includes.asideCbc(), includes.phaseBannerBeta(), errors)),
        data => {
          (for {
            bpr <- OptionT(cache.read[BusinessPartnerRecord]).toRight(UnexpectedState("No BPR found in cache"))
            cbcId <- OptionT(cache.read[CBCId]).toRight(UnexpectedState("No CBCId found in cache"))
            details = CorrespondenceDetails(bpr.address, ContactDetails(data.email, data.phoneNumber), ContactName(data.firstName, data.lastName))
            _ <- cbcIdService.updateETMPSubscriptionData(bpr.safeId, details)
            _ <- subscriptionDataService.updateSubscriptionData(cbcId, SubscriberContact(data.firstName, data.lastName, data.phoneNumber, data.email))
          } yield Ok("Saved")
            ).fold(
            errors => errorRedirect(errors),
            result => result
          )
        }
      )
  }

  def subscribeSuccessCbcId(id: String) = sec.AsyncAuthenticatedAction(Some(Organisation)) { authContext =>
    implicit request =>
      CBCId(id).fold[Future[Result]](
        InternalServerError(FrontendGlobal.internalServerErrorTemplate)
      )((cbcId: CBCId) =>
        Ok(subscription.subscribeSuccessCbcId(includes.asideBusiness(), includes.phaseBannerBeta(), cbcId, request.session.get("companyName")))
      )
  }

  def clearSubscriptionData(u: Utr) = sec.AsyncAuthenticatedAction(Some(Organisation)) { authContext =>
    implicit request =>
      if (CbcrSwitches.clearSubscriptionDataRoute.enabled) {
        subscriptionDataService.clearSubscriptionData(u).fold(
          error => errorRedirect(error), {
            case Some(_) => Ok
            case None => NoContent
          }
        )
      } else {
        NotImplemented
      }
  }

  def createSuccessfulSubscriptionAuditEvent(authContext: AuthContext, subscriptionData: SubscriptionDetails)
                                            (implicit hc: HeaderCarrier, request: Request[_]): ServiceResponse[AuditResult.Success.type] =
    for {
      ggId   <- right(getUserGGId(authContext))
      result <- EitherT[Future,CBCErrors,AuditResult.Success.type](audit.sendEvent(ExtendedDataEvent("Country-By-Country-Frontend", "CBCRSubscription",
        tags = hc.toAuditTags("CBCRSubscription", "N/A") + ("path" -> request.uri, "ggId" -> ggId.authProviderId),
        detail = Json.toJson(subscriptionData)
      )).map {
        case AuditResult.Success => Right(AuditResult.Success)
        case AuditResult.Failure(msg, _) => Left(UnexpectedState(s"Unable to audit a successful submission: $msg"))
        case AuditResult.Disabled => Right(AuditResult.Success)
      })
    } yield result


}
