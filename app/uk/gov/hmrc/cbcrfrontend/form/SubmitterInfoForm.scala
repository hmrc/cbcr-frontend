/*
 * Copyright 2024 HM Revenue & Customs
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

package uk.gov.hmrc.cbcrfrontend.form

import play.api.data.Form
import play.api.data.Forms.{mapping, text}
import play.api.data.validation.Constraint
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.emailaddress.{EmailAddress, EmailAddressValidation}
import uk.gov.hmrc.cbcrfrontend.form.SubscriptionDataForm.condTrue
import uk.gov.hmrc.cbcrfrontend.model.SubmitterInfo
import uk.gov.hmrc.cbcrfrontend.util.CBCRMapping.validatePhoneNumber

object SubmitterInfoForm {
  def submitterInfoForm(frontendAppConfig: FrontendAppConfig, constraint: Constraint[String]): Form[SubmitterInfo] =
    Form(
      mapping(
        "fullName" -> text.verifying("submitterInfo.fullName.error", _.trim != ""),
        "contactPhone" -> validatePhoneNumber(
          constraint
        ),
        "email" -> text
          .verifying("submitterInfo.emailAddress.error.empty", _.trim != "")
          .verifying(
            "submitterInfo.emailAddress.error.invalid",
            x => condTrue(x.trim != "", new EmailAddressValidation(frontendAppConfig).isValid(x))
          )
      ) { (fullName: String, contactPhone: String, email: String) =>
        SubmitterInfo(fullName, None, contactPhone, EmailAddress(email), None)
      }(si => Some((si.fullName, si.contactPhone, si.email)))
    )
}
