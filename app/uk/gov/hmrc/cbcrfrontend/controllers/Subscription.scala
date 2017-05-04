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

import cats.data.OptionT
import cats.instances.all._
import cats.syntax.all._
import play.api.Logger
import play.api.Play._
import play.api.data.Form
import play.api.data.Forms._
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationError}
import play.api.i18n.Messages.Implicits._
import play.api.mvc.{Action, AnyContent, Result}
import uk.gov.hmrc.cbcrfrontend.auth.SecuredActions
import uk.gov.hmrc.cbcrfrontend.connectors.{BPRKnownFactsConnector, GGConnector}
import uk.gov.hmrc.cbcrfrontend.exceptions.UnexpectedState
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.services.{BPRKnownFactsService, CBCIdService, CBCKnownFactsService, SubscriptionDataService}
import uk.gov.hmrc.cbcrfrontend.views.html._
import uk.gov.hmrc.emailaddress.EmailAddress
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class Subscription @Inject()(val sec: SecuredActions,
                             val subscriptionDataService: SubscriptionDataService,
                             val connector:BPRKnownFactsConnector,
                             val cbcIdService:CBCIdService,
                             val kfService:CBCKnownFactsService)
                            (implicit ec: ExecutionContext) extends FrontendController with ServicesConfig {

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


  val submitSubscriptionData: Action[AnyContent] = sec.AsyncAuthenticatedAction { authContext =>
    implicit request =>
      Logger.debug("Country by Country: Generate CBCId and Store Data")

      subscriptionDataForm.bindFromRequest.fold(
        errors => Future.successful(BadRequest(subscription.contactInfoSubscriber(includes.asideCbc(), includes.phaseBannerBeta(), errors))),
        data => {
          cbcIdService.getCbcId.flatMap { oId =>
            oId match {
              case Some(id) =>

                val bpr = BusinessPartnerRecord(None, None, EtmpAddress(None, None, None, None, None, None))
                val details = SubscriptionDetails(bpr, data, id)
                val utr = Utr("1234567890")

                val result = for {
                  _ <- subscriptionDataService.saveSubscriptionData(details)
                  _ <- kfService.addKnownFactsToGG(CBCKnownFacts(utr, id))
                } yield id

                result.fold(
                  error => {
                    subscriptionDataService.clearSubscriptionData(id)
                    InternalServerError(error.errorMsg)
                  },
                  cbcId => Redirect(routes.Subscription.subscribeSuccessCbcId(cbcId.value))
                )
              case None => Future.successful(InternalServerError("Could not generate CBCId"))
            }
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

  val subscribeFirst = sec.AsyncAuthenticatedAction{ authContext => implicit request =>
    Logger.debug("Country by Country: Subscribe First")
    Future.successful(Ok(subscription.subscribeFirst(includes.asideCbc(), includes.phaseBannerBeta(), knownFactsForm)))
  }

  val checkKnownFacts = sec.AsyncAuthenticatedAction { authContext =>
    implicit request =>

      knownFactsForm.bindFromRequest.fold(
        formWithErrors => Future.successful(
          BadRequest(subscription.subscribeFirst(includes.asideCbc(), includes.phaseBannerBeta(), formWithErrors))
        ),
        knownFacts => knownFactsService.checkBPRKnownFacts(knownFacts).cata(
          NotFound(subscription.subscribeFirst(includes.asideCbc(), includes.phaseBannerBeta(), knownFactsForm, noMatchingBusiness = true)),
          (s: BusinessPartnerRecord) =>
            Ok(subscription.subscribeMatchFound(includes.asideCbc(), includes.phaseBannerBeta(), s.organisation.map(_.organisationName).getOrElse(""), knownFacts.postCode, knownFacts.utr.value))
        )
      )
  }

  val contactInfoSubscriber = sec.AsyncAuthenticatedAction{ authContext => implicit request =>
    Logger.debug("Country by Country: Contact Info Subscriber View")

    Future.successful(Ok(subscription.contactInfoSubscriber(includes.asideCbc(), includes.phaseBannerBeta(), subscriptionDataForm)))
  }

  def subscribeSuccessCbcId(id:String) = sec.AsyncAuthenticatedAction{ authContext => implicit request =>
    Logger.debug("Country by Country: Contact Info Subscribe Success CbcId View")
    CBCId(id).fold[Future[Result]](
      Future.successful(BadRequest)
    )((cbcId: CBCId) =>
    Future.successful(Ok(subscription.subscribeSuccessCbcId(includes.asideBusiness(), includes.phaseBannerBeta(),cbcId,request.session.get("companyName"))))
    )
  }


}
