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


import java.util.UUID
import javax.inject.{Inject, Singleton}

import cats.instances.future._
import play.api.Logger
import play.api.Play.current
import play.api.data.Form
import play.api.data.Forms._
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationError}
import play.api.i18n.Messages.Implicits._
import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.cbcrfrontend.auth.SecuredActions
import uk.gov.hmrc.cbcrfrontend.connectors.KnownFactsConnector
import uk.gov.hmrc.cbcrfrontend.exceptions.UnexpectedState
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.services.{KnownFactsService, SubscriptionDataService}
import uk.gov.hmrc.cbcrfrontend.views.html._
import uk.gov.hmrc.emailaddress.EmailAddress
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}
import scala.util.matching.Regex

@Singleton
class Subscription @Inject()(val sec: SecuredActions, val subscriptionDataService: SubscriptionDataService, val connector:KnownFactsConnector)(implicit ec: ExecutionContext) extends FrontendController with ServicesConfig {

  //TODO: Find out genuine CBCID specs
  def generateCBCId(): CbcId = CbcId(UUID.randomUUID().toString)

  lazy val knownFactsService:KnownFactsService = new KnownFactsService(connector)

  val subscriptionDataForm: Form[SubscriptionData] = Form(
    mapping(
      "name"        -> nonEmptyText,
      "role"        -> nonEmptyText,
      "phoneNumber" -> nonEmptyText,
      "email"       -> email.verifying(EmailAddress.isValid(_))
    )((name: String, role: String, phoneNumber:String, email: String) =>
      SubscriptionData(name, role, phoneNumber, EmailAddress(email), generateCBCId())
    )(sc => Some((sc.name,sc.role,sc.phoneNumber, sc.email.value)))
  )


  val submitSubscriptionData: Action[AnyContent] = sec.AsyncAuthenticatedAction{ authContext => implicit request =>
    Logger.debug("Country by Country: Generate CBCid and Store Data")

    subscriptionDataForm.bindFromRequest.fold(
      (errors: Form[SubscriptionData]) => Future.successful(BadRequest(subscription.contactInfoSubscriber(includes.asideCbc(),includes.phaseBannerBeta(),errors))),
      (data: SubscriptionData)         => subscriptionDataService.saveSubscriptionData(data).fold(
        (state: UnexpectedState) => InternalServerError(state.errorMsg),
        (id: String)             => Redirect(routes.Subscription.subscribeSuccessCbcId(data.cbcId.id))
      )
    )
  }

  val postCodeRegexp: Regex = """^[A-Za-z]{1,2}[0-9]{1,2}\s*[A-Za-z]?[0-9][A-Za-z]{2}$""".r

  val postCodeConstraint: Constraint[String] = Constraint("constraints.postcodecheck") {
    case postCodeRegexp() => Valid
    case _                => Invalid(ValidationError("Post Code is not valid"))

  }

  val utrConstraint: Constraint[String] = Constraint("constraints.utrcheck"){
    case utr if Utr(utr).isValid => Valid
    case _                       => Invalid(ValidationError("UTR is not valid"))
  }

  val knownFactsForm = Form(
    mapping(
      "utr" -> nonEmptyText.verifying(utrConstraint),
      "postCode" -> nonEmptyText.verifying(postCodeConstraint)
    )((u,p) => KnownFacts(Utr(u),p))((facts: KnownFacts) => Some(facts.utr.value -> facts.postCode))
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
        knownFacts => knownFactsService.checkKnownFacts(knownFacts).cata(
          NotFound(subscription.subscribeFirst(includes.asideCbc(), includes.phaseBannerBeta(), knownFactsForm, noMatchingBusiness = true)),
          (s: FindBusinessDataResponse) =>
            Ok(subscription.subscribeMatchFound(includes.asideCbc(), includes.phaseBannerBeta(), s.organisation.map(_.organisationName).getOrElse(""), knownFacts.postCode, knownFacts.utr.value))
        )
      )
  }

  val contactInfoSubscriber = sec.AsyncAuthenticatedAction{ authContext => implicit request =>
    Logger.debug("Country by Country: Contact Info Subscriber View")

    Future.successful(Ok(subscription.contactInfoSubscriber(includes.asideCbc(), includes.phaseBannerBeta(), subscriptionDataForm)))
  }

  def subscribeSuccessCbcId(cbcId:String) = sec.AsyncAuthenticatedAction{ authContext => implicit request =>
    Logger.debug("Country by Country: Contact Info Subscribe Success CbcId View")

    Future.successful(Ok(subscription.subscribeSuccessCbcId(includes.asideBusiness(), includes.phaseBannerBeta(),cbcId,request.session.get("companyName"))))
  }


}
