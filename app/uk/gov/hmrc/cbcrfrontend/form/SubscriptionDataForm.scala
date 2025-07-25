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
import uk.gov.hmrc.cbcrfrontend.model.SubscriberContact
import uk.gov.hmrc.cbcrfrontend.util.CBCRMapping.validatePhoneNumber

object SubscriptionDataForm {
  def condTrue(condition: Boolean, statement: Boolean): Boolean = if (condition) statement else true

  def subscriptionDataForm(
    frontendAppConfig: FrontendAppConfig,
    constraint: Constraint[String]
  ): Form[SubscriberContact] = Form(
    mapping(
      "firstName" -> text.verifying("contactInfoSubscriber.firstName.error", _.trim != ""),
      "lastName"  -> text.verifying("contactInfoSubscriber.lastName.error", _.trim != ""),
      "phoneNumber" -> validatePhoneNumber(
        constraint
      ),
      "email" -> text
        .verifying("contactInfoSubscriber.emailAddress.error.empty", _.trim != "")
        .verifying(
          "contactInfoSubscriber.emailAddress.error.invalid",
          x => condTrue(x.trim != "", new EmailAddressValidation(frontendAppConfig).isValid(x))
        )
        .transform[EmailAddress](EmailAddress(_), _.value)
    )(SubscriberContact.apply)(o => Some(o.firstName, o.lastName, o.phoneNumber, o.email))
  )
}
