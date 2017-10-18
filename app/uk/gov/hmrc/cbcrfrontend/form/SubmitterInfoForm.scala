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

package uk.gov.hmrc.cbcrfrontend.form

import play.api.data.Form
import play.api.data.Forms.{email, mapping, text}
import uk.gov.hmrc.cbcrfrontend.form.SubscriptionDataForm.condTrue
import uk.gov.hmrc.cbcrfrontend.model.SubmitterInfo
import uk.gov.hmrc.emailaddress.EmailAddress


object SubmitterInfoForm {
  val submitterInfoForm: Form[SubmitterInfo] = Form(
    mapping(
      "fullName" -> text.verifying("submitterInfo.fullName.error", _.trim != ""),
      "contactPhone" -> text.verifying("submitterInfo.phoneNumber.error.empty", _.trim != "")
        .verifying("submitterInfo.phoneNumber.error.invalid", x => condTrue(x.trim != "", x.matches("""^[A-Z0-9 )/(-*#]{1,24}$"""))),
      "email" -> text.verifying("submitterInfo.emailAddress.error.empty", _.trim != "")
        .verifying("submitterInfo.emailAddress.error.invalid", x => condTrue(x.trim != "", EmailAddress.isValid(x)))
    )((fullName: String, contactPhone: String, email: String) => {
      SubmitterInfo(fullName, None, contactPhone, EmailAddress(email), None)
    }
    )(si => Some((si.fullName, si.contactPhone, si.email.value)))
  )
}
