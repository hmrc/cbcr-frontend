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

import cats.instances.future._
import play.api.Logger
import play.api.Play.current
import play.api.data.Form
import play.api.data.Forms._
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationError}
import play.api.i18n.Messages.Implicits._
import play.api.mvc.Action
import uk.gov.hmrc.cbcrfrontend.connectors.ETMPConnector
import uk.gov.hmrc.cbcrfrontend.model.{KnownFacts, OrganisationResponse}
import uk.gov.hmrc.cbcrfrontend.services.{ETMPService, KnownFactsCheckService}
import uk.gov.hmrc.cbcrfrontend.views.html._
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.Future
import scala.util.matching.Regex

object Subscription extends Subscription {
  val connector = ETMPConnector
  val businessMatchService = new ETMPService(connector)
  val knownFactsService = new KnownFactsCheckService(businessMatchService)
}

trait Subscription extends FrontendController with ServicesConfig {

  val knownFactsService:KnownFactsCheckService

  val postCodeRegexp: Regex = """^[A-Za-z]{1,2}[0-9]{1,2}\s*[A-Za-z]?[0-9][A-Za-z]{2}$""".r

  val utrRegexp: Regex ="""\d{10}""".r

  val postCodeConstraint: Constraint[String] = Constraint("constraints.postcodecheck") {
    case postCodeRegexp() => Valid
    case _                => Invalid(ValidationError("Post Code is not valid"))

  }
  //TODO: What checks for valid UTR should there be?
  val utrConstraint: Constraint[String] = Constraint("constraints.utrcheck"){
    case utrRegexp() => Valid
    case _           => Invalid(ValidationError("UTR is not valid"))
  }

  val knownFactsForm = Form(
    mapping(
      "utr" -> nonEmptyText.verifying(utrConstraint),
      "postcode" -> nonEmptyText.verifying(postCodeConstraint)
    )(KnownFacts.apply)(KnownFacts.unapply)
  )

  val subscribeFirst = Action.async{ implicit request =>
    Logger.debug("Country by Country: Subscribe First")

    Future.successful(Ok(subscription.subscribeFirst(includes.asideCbc(), includes.phaseBannerBeta(), knownFactsForm)))
  }

  def checkKnownFacts = Action.async { implicit request =>

    knownFactsForm.bindFromRequest.fold(
      formWithErrors => Future.successful(
        BadRequest(subscription.subscribeFirst(includes.asideCbc(), includes.phaseBannerBeta(), formWithErrors))
      ),
      knownFacts => knownFactsService.checkKnownFacts(knownFacts).cata(
        Ok(subscription.subscribeFirst(includes.asideCbc(), includes.phaseBannerBeta(), knownFactsForm, noMatchingBusiness = true)),
        (s: OrganisationResponse) => Ok(subscription.subscribeMatchFound(includes.asideCbc(), includes.phaseBannerBeta()))
      )
    )
  }


  val subscribeMatchFound = Action.async { implicit request =>
    Logger.debug("Country by Country: Subscribe Match Found view")

    Future.successful(Ok(subscription.subscribeMatchFound(includes.asideCbc(), includes.phaseBannerBeta())))
  }


  val contactInfoSubscriber = Action.async { implicit request =>
    Logger.debug("Country by Country: Contact Info Subscriber View")

    Future.successful(Ok(subscription.contactInfoSubscriber(includes.asideCbc(), includes.phaseBannerBeta())))
  }

  val subscribeSuccessCbcId = Action.async { implicit request =>
    Logger.debug("Country by Country: Contact Info Subscribe Success CbcId View")

    Future.successful(Ok(subscription.subscribeSuccessCbcId(includes.asideBusiness(), includes.phaseBannerBeta())))
  }


}
