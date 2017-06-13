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
import cats.instances.all._
import play.api.Logger
import play.api.Play._
import play.api.data.Form
import play.api.data.Forms._
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationError}
import play.api.i18n.Messages.Implicits._
import play.api.mvc.{Action, AnyContent, Result}
import uk.gov.hmrc.cbcrfrontend._
import uk.gov.hmrc.cbcrfrontend.auth.SecuredActions
import uk.gov.hmrc.cbcrfrontend.connectors.{AuthConnector, BPRKnownFactsConnector}
import uk.gov.hmrc.cbcrfrontend.exceptions.UnexpectedState
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.services._
import uk.gov.hmrc.cbcrfrontend.views.html._
import uk.gov.hmrc.emailaddress.EmailAddress
import uk.gov.hmrc.http.cache.client.CacheMap
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.frontend.controller.FrontendController
import uk.gov.hmrc.play.frontend.auth.connectors.{AuthConnector => PlayAuthConnector}

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class Subscription @Inject()(val sec: SecuredActions,
                             val subscriptionDataService: SubscriptionDataService,
                             val connector:BPRKnownFactsConnector,
                             val cbcIdService:CBCIdService,
                             val kfService:CBCKnownFactsService,
                             val authConnector:AuthConnector)
                            (implicit ec: ExecutionContext,
                             val playAuth:PlayAuthConnector,
                             val session:CBCSessionCache) extends FrontendController with ServicesConfig {


  lazy val knownFactsService:BPRKnownFactsService = new BPRKnownFactsService(connector)

  val subscriptionDataForm: Form[SubscriberContact] = Form(
    mapping(
      "name"        -> nonEmptyText,
      "phoneNumber" -> nonEmptyText,
      "email"       -> email.verifying(EmailAddress.isValid(_))
    )((name: String, phoneNumber:String, email: String) =>
      SubscriberContact(name, phoneNumber, EmailAddress(email))
    )(sc => Some((sc.name,sc.phoneNumber, sc.email.value)))
  )


  val submitSubscriptionData: Action[AnyContent] = sec.AsyncAuthenticatedAction(Some(Organisation)) { authContext =>
    implicit request =>
      Logger.debug("Country by Country: Generate CBCId and Store Data")

      subscriptionDataForm.bindFromRequest.fold(
        errors => Future.successful(BadRequest(subscription.contactInfoSubscriber(includes.asideCbc(), includes.phaseBannerBeta(), errors))),
        data => {
          cbcIdService.getCbcId.flatMap {
            case Some(id) =>

              val result = for {
                bpr <- EitherT[Future, UnexpectedState, BusinessPartnerRecord](
                  session.read[BusinessPartnerRecord].map(_.toRight(UnexpectedState("BPR record not found")))
                )
                utr <- EitherT[Future, UnexpectedState, Utr](
                  session.read[Utr].map(_.toRight(UnexpectedState("UTR record not found")))
                )
                _ <- subscriptionDataService.saveSubscriptionData(SubscriptionDetails(bpr, data, id, utr))
                _ <- kfService.addKnownFactsToGG(CBCKnownFacts(utr, id))
                _ <- EitherT.right[Future, UnexpectedState, CacheMap](session.save(id))
                _ <- EitherT.right[Future, UnexpectedState, CacheMap](session.save(data))
              } yield id

              result.fold(
                error => {
                  subscriptionDataService.clearSubscriptionData(id)
                  Logger.error(error.errorMsg)
                  InternalServerError(FrontendGlobal.internalServerErrorTemplate)
                },
                cbcId => Redirect(routes.Subscription.subscribeSuccessCbcId(cbcId.value))
              )
            case None => Future.successful(InternalServerError(FrontendGlobal.internalServerErrorTemplate))
          }
        }
      )
  }

  val utrConstraint: Constraint[String] = Constraint("constraints.utrcheck"){
    case utr if Utr(utr).isValid => Valid
    case _                       => Invalid(ValidationError("UTR is not valid"))
  }

 val knownFactsForm = Form(
    mapping(
      "utr" -> nonEmptyText.verifying(utrConstraint),
      "postCode" -> nonEmptyText
    )((u,p) => BPRKnownFacts(Utr(u),p))((facts: BPRKnownFacts) => Some(facts.utr.value -> facts.postCode))
 )

  val enterKnownFacts = sec.AsyncAuthenticatedAction(){ authContext => implicit request =>
    authConnector.getEnrolments.flatMap{enrolments =>
      if(enrolments.exists(_.key == "HMRC-CBC-ORG")) {
        Future.successful(NotAcceptable(subscription.alreadySubscribed(includes.asideCbc(),includes.phaseBannerBeta())))
      } else {
        getUserType(authContext).map{
          case Agent => Ok(subscription.enterKnownFacts(includes.asideCbc(), includes.phaseBannerBeta(), knownFactsForm, false, Agent))
          case Organisation => Ok(subscription.enterKnownFacts(includes.asideCbc(), includes.phaseBannerBeta(), knownFactsForm, noMatchingBusiness = false,Organisation))
        }.leftMap { errors =>
          Logger.error(errors.errorMsg)
          InternalServerError(FrontendGlobal.internalServerErrorTemplate)
        }.merge
      }
    }
  }

  val checkKnownFacts = sec.AsyncAuthenticatedAction() { authContext =>
    implicit request =>

      getUserType(authContext).leftMap{ errors =>
        Logger.error(errors.errorMsg)
        InternalServerError(FrontendGlobal.internalServerErrorTemplate)
      }.flatMap { userType =>

        knownFactsForm.bindFromRequest.fold[EitherT[Future,Result,Result]](
          formWithErrors => EitherT.left(Future.successful(
            BadRequest(subscription.enterKnownFacts(includes.asideCbc(), includes.phaseBannerBeta(), formWithErrors, false, userType))
          )),
          knownFacts =>  for {
            bpr <- knownFactsService.checkBPRKnownFacts(knownFacts).toRight(
              NotFound(subscription.enterKnownFacts(includes.asideCbc(), includes.phaseBannerBeta(), knownFactsForm, noMatchingBusiness = true, userType))
            )
            alreadySubscribed <- subscriptionDataService.alreadySubscribed(knownFacts.utr).leftMap { error =>
              Logger.error(error.errorMsg)
              InternalServerError(FrontendGlobal.internalServerErrorTemplate)
            }
            _ <- EitherT.cond[Future]( userType match {
              case Organisation => !alreadySubscribed
              case Agent        => alreadySubscribed
            },(), NotAcceptable(subscription.alreadySubscribed(includes.asideCbc(), includes.phaseBannerBeta())))
            _ <- EitherT.right[Future, Result, CacheMap](session.save(bpr))
            _ <- EitherT.right[Future, Result, CacheMap](session.save(knownFacts.utr))
          } yield Ok(subscription.subscribeMatchFound(includes.asideCbc(), includes.phaseBannerBeta(), bpr.organisation.map(_.organisationName).getOrElse(""), knownFacts.postCode, knownFacts.utr.value,userType))

        )
      }.merge
  }

  val contactInfoSubscriber = sec.AsyncAuthenticatedAction(Some(Organisation)){ authContext => implicit request =>
    Logger.debug("Country by Country: Contact Info Subscriber View")

    Future.successful(Ok(subscription.contactInfoSubscriber(includes.asideCbc(), includes.phaseBannerBeta(), subscriptionDataForm)))
  }

  def subscribeSuccessCbcId(id:String) = sec.AsyncAuthenticatedAction(Some(Organisation)){ authContext => implicit request =>
    Logger.debug("Country by Country: Contact Info Subscribe Success CbcId View")
    CBCId(id).fold[Future[Result]](
      Future.successful(InternalServerError(FrontendGlobal.internalServerErrorTemplate))
    )((cbcId: CBCId) =>
    Future.successful(Ok(subscription.subscribeSuccessCbcId(includes.asideBusiness(), includes.phaseBannerBeta(),cbcId,request.session.get("companyName"))))
    )
  }

  def clearSubscriptionData(u:Utr) = sec.AsyncAuthenticatedAction(Some(Organisation)) { authContext => implicit request =>
    subscriptionDataService.clearSubscriptionData(u).fold(
      error  => InternalServerError(error.errorMsg),
      {
        case Some(_) => Ok
        case None    => NoContent
      }
    )
  }


}
