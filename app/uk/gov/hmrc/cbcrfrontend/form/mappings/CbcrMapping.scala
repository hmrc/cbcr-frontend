/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.cbcrfrontend.form.mappings

import com.google.i18n.phonenumbers.PhoneNumberUtil
import configs.Result.{Success, Try}
import play.api.data.Forms.text
import play.api.data.Mapping
import play.api.data.validation.{Constraint, Invalid, Valid}

object CbcrMapping {

  def nonEmptyPhoneNumber(errorEmpty: String, errorInvalid: String): Mapping[String] = {

    val phoneNumberUtil = PhoneNumberUtil.getInstance()

    def nonEmptyPhoneNumber: Constraint[String] =
      Constraint {
        case text if text.isBlank => Invalid(errorEmpty)
        case text =>
          Try(phoneNumberUtil.isPossibleNumber(phoneNumberUtil.parse(text, "GB"))) match {
            case Success(true) => Valid
            case _             => Invalid(errorInvalid)
          }
      }

    text.verifying(nonEmptyPhoneNumber)
  }
}
