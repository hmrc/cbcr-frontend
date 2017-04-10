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

import play.api.Logger
import play.api.mvc.Action
import uk.gov.hmrc.cbcrfrontend.views.html._
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.frontend.controller.FrontendController
import play.api.Play.current
import play.api.i18n.Messages.Implicits._

import scala.concurrent.Future

object Subscription extends Subscription

trait Subscription extends FrontendController with ServicesConfig {

  val subscribeFirst = Action.async { implicit request =>
    Logger.debug("Country by Country: Subscribe First")

    Future.successful(Ok(subscription.subscribeFirst(includes.asideCbc(), includes.phaseBannerBeta())))
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
