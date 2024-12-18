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

package uk.gov.hmrc.cbcrfrontend.emailaddress

import com.google.inject.ImplementedBy
import play.api.data.validation.{Constraint, Constraints, Valid}
import uk.gov.hmrc.cbcrfrontend

import javax.inject.Singleton

case class EmailAddress(value: String) extends StringValue

@ImplementedBy(classOf[EmailAddressValidation])
trait EmailValidation {
  def isValid(email: String): Boolean
}

@Singleton
class EmailAddressValidation extends EmailValidation {

  def isValid(email: String): Boolean = {
    val emailConstraint: Constraint[String] = Constraints.emailAddress
    emailConstraint(email) match {
      case Valid => true
      case _     => cbcrfrontend.logger.debug(s"Invalid email:$email"); false
    }
  }
}
