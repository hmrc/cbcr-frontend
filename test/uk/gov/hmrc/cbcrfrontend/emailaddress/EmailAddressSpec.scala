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

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class EmailAddressSpec extends AnyWordSpec with Matchers {
  "EmailValidation" should {
    "validate a correct email address" in {
      new EmailAddressValidation().isValid("user@test.com") shouldBe true
      new EmailAddressValidation().isValid("user@test.co.uk") shouldBe true
      new EmailAddressValidation().isValid("a@a") shouldBe true
    }
    "validate invalid email" in {
      new EmailAddressValidation().isValid("user @test.com") shouldBe false
      new EmailAddressValidation().isValid("user@") shouldBe false
    }
  }

}
