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

import play.api.mvc.Action
import uk.gov.hmrc.cbcrfrontend.auth.SecuredActions
import uk.gov.hmrc.cbcrfrontend.views.html.includes
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.frontend.controller.FrontendController
import play.api.i18n.Messages.Implicits._
import uk.gov.hmrc.cbcrfrontend.views.html._
import play.api.Play.current
import play.api.data.Form
import play.api.data.Forms._
import uk.gov.hmrc.cbcrfrontend.model.FilingType
import uk.gov.hmrc.cbcrfrontend.services.CBCSessionCache

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.ExecutionContext.Implicits.global

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
@Singleton
class Submission @Inject()(val sec: SecuredActions, val session:CBCSessionCache) (implicit ec: ExecutionContext) extends FrontendController with ServicesConfig{




  val filingType = Action.async { implicit request =>
    Future.successful(Ok(uk.gov.hmrc.cbcrfrontend.views.html.forms.submitInfoFilingType(
      includes.asideBusiness(), includes.phaseBannerBeta()
    )))
  }

  val submitFilingType = Action.async { implicit request =>

    val filingTypeForm: Form[FilingType] = Form(
      mapping("filingType" -> nonEmptyText
      )((filingType: String) => FilingType(filingType)) (ft => Some(ft.filingType))
    )

    filingTypeForm.bindFromRequest.fold(
      errors => Future.successful(Ok(uk.gov.hmrc.cbcrfrontend.views.html.forms.submitInfoFilingType(
        includes.asideBusiness(), includes.phaseBannerBeta()))),
      success => {
        session.save(success)
        Future.successful(Ok(uk.gov.hmrc.cbcrfrontend.views.html.forms.submitInfoUltimateParentEntity(
          includes.asideBusiness(), includes.phaseBannerBeta()
        )))
      }
    )
  }

  val submitInfoUltimateParentEntity = Action.async { implicit request =>
    Future.successful(Ok(uk.gov.hmrc.cbcrfrontend.views.html.forms.submitInfoUltimateParentEntity(
      includes.asideBusiness(), includes.phaseBannerBeta()
    )))
  }

  val submitInfoFilingCapacity = Action.async { implicit request =>
    Future.successful(Ok(uk.gov.hmrc.cbcrfrontend.views.html.forms.submitInfoFilingCapacity(
      includes.asideBusiness(), includes.phaseBannerBeta()
    )))
  }

  val contactInfoSubmitter = Action.async { implicit request =>
    Future.successful(Ok(uk.gov.hmrc.cbcrfrontend.views.html.forms.contactInfoSubmitter(
      includes.asideBusiness(), includes.phaseBannerBeta()
    )))
  }

  val submitSummary = Action.async { implicit request =>
    Future.successful(Ok(uk.gov.hmrc.cbcrfrontend.views.html.forms.submitSummary(
      includes.phaseBannerBeta()
    )))
  }

  val submitSuccessReceipt = Action.async { implicit request =>
    Future.successful(Ok(uk.gov.hmrc.cbcrfrontend.views.html.forms.submitSuccessReceipt(
      includes.asideBusiness(), includes.phaseBannerBeta()
    )))
  }

  val filingHistory = Action.async { implicit request =>
    Future.successful(Ok(uk.gov.hmrc.cbcrfrontend.views.html.forms.filingHistory(
      includes.phaseBannerBeta()
    )))
  }
}
