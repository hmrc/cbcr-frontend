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
import play.api.Play.current
import play.api.data.Form
import play.api.data.Forms._
import uk.gov.hmrc.cbcrfrontend.model.{FilingCapacity, FilingType, SubmitterInfo, UltimateParentEntity}
import uk.gov.hmrc.cbcrfrontend.services.CBCSessionCache
import uk.gov.hmrc.emailaddress.EmailAddress

import scala.concurrent.{ExecutionContext, Future}

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


  val filingTypeForm: Form[FilingType] = Form(
    mapping("filingType" -> nonEmptyText
    )((filingType: String) => FilingType(filingType)) (ft => Some(ft.filingType))
  )

  val ultimateParentEntityForm: Form[UltimateParentEntity] = Form(
    mapping("ultimateParentEntity" -> nonEmptyText
    )((ultimateParentEntity: String) => UltimateParentEntity(ultimateParentEntity)) (upe => Some(upe.ultimateParentEntity))
  )

  val filingCapacityForm: Form[FilingCapacity] = Form(
    mapping("filingCapacity" -> nonEmptyText
    )((filingCapacity: String) => FilingCapacity(filingCapacity)) (ft => Some(ft.filingCapacity))
  )

  val submitterInfoForm: Form[SubmitterInfo] = Form(
    mapping(
      "fullName"        -> nonEmptyText,
      "agencyBusinessName" -> nonEmptyText,
      "jobRole"        -> nonEmptyText,
      "contactPhone" -> nonEmptyText,
      "email"       -> email.verifying(EmailAddress.isValid(_))
    )((fullName: String, agencyBusinessName:String, jobRole:String, contactPhone:String, email: String) =>
      SubmitterInfo(fullName, agencyBusinessName, jobRole, contactPhone, EmailAddress(email))
    )(si => Some((si.fullName,si.agencyBusinessName,si.jobRole, si.contactPhone, si.email.value)))
  )

  val filingType = sec.AsyncAuthenticatedAction { authContext => implicit request =>
    Future.successful(Ok(uk.gov.hmrc.cbcrfrontend.views.html.forms.submitInfoFilingType(
      includes.asideBusiness(), includes.phaseBannerBeta(), filingTypeForm
    )))
  }

  val submitFilingType = sec.AsyncAuthenticatedAction { authContext => implicit request =>

    filingTypeForm.bindFromRequest.fold(
      formWithErrors => Future.successful(BadRequest(uk.gov.hmrc.cbcrfrontend.views.html.forms.submitInfoFilingType(
        includes.asideBusiness(), includes.phaseBannerBeta(), formWithErrors))),
      success => {
        session.save(success)
        Future.successful(Ok(uk.gov.hmrc.cbcrfrontend.views.html.forms.submitInfoUltimateParentEntity(
          includes.asideBusiness(), includes.phaseBannerBeta(), ultimateParentEntityForm
        )))
      }
    )
  }



  val submitUltimateParentEntity = sec.AsyncAuthenticatedAction { authContext => implicit request =>

    ultimateParentEntityForm.bindFromRequest.fold(
      formWithErrors => Future.successful(BadRequest(uk.gov.hmrc.cbcrfrontend.views.html.forms.submitInfoUltimateParentEntity(
        includes.asideBusiness(), includes.phaseBannerBeta(), formWithErrors))),
      success => {
        session.save(success)
        Future.successful(Ok(uk.gov.hmrc.cbcrfrontend.views.html.forms.submitInfoFilingCapacity(
          includes.asideBusiness(), includes.phaseBannerBeta(),filingCapacityForm
        )))
      }
    )

  }



  val submitFilingCapacity = sec.AsyncAuthenticatedAction { authContext => implicit request =>

    filingCapacityForm.bindFromRequest.fold(
      formWithErrors => Future.successful(BadRequest(uk.gov.hmrc.cbcrfrontend.views.html.forms.submitInfoFilingCapacity(
        includes.asideBusiness(), includes.phaseBannerBeta(), formWithErrors))),
      success => {
        session.save(success)
        Future.successful(Ok(uk.gov.hmrc.cbcrfrontend.views.html.forms.submitterInfo(
          includes.asideBusiness(), includes.phaseBannerBeta(),submitterInfoForm
        )))
      }
    )
  }

  val submitSubmitterInfo = sec.AsyncAuthenticatedAction { authContext => implicit request =>

    submitterInfoForm.bindFromRequest.fold(
      formWithErrors => Future.successful(BadRequest(uk.gov.hmrc.cbcrfrontend.views.html.forms.submitterInfo(
        includes.asideBusiness(), includes.phaseBannerBeta(), formWithErrors
      ))),
      success => {
        session.save(success)
        Future.successful(Ok(uk.gov.hmrc.cbcrfrontend.views.html.forms.submitSummary(
          includes.phaseBannerBeta()
        )))

      }

    )
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
