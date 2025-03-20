/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.cbcrfrontend.util

import play.api.data.Forms.text
import play.api.data.Mapping
import play.api.data.validation.{Constraint, Invalid, Valid}

object CBCRMapping {

  case class PhoneNumberErrors(
    emptyError: String,
    plusSignError: String,
    invalidCharactersError: String,
    forbiddenCharacterError: String
  )

  def validatePhoneNumber(constraint: Constraint[String]): Mapping[String] =
    text.verifying(constraint)

  def ukPhoneNumberConstraint(errors: PhoneNumberErrors): Constraint[String] =
    Constraint {
      case text if text.isBlank       => Invalid(errors.emptyError)
      case text if text.contains("+") => Invalid(errors.plusSignError)
      case text if text.exists(c => "@!$%^&_=~{}[]:".contains(c)) =>
        Invalid(errors.forbiddenCharacterError)
      case text if !text.matches("^[A-Z0-9 )/(-*#]+$") =>
        Invalid(errors.invalidCharactersError)
      case _ => Valid
    }

  def phoneNumberConstraint(emptyError: String, invalidError: String): Constraint[String] =
    Constraint {
      case text if text.isBlank => Invalid(emptyError)
      case text if !text.matches("^[0-9 )/(-*#]{1,24}$") =>
        Invalid(invalidError)
      case _ => Valid
    }

}
