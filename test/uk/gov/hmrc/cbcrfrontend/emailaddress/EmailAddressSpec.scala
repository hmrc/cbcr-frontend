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

import com.typesafe.config.{Config, ConfigFactory}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig

import scala.jdk.CollectionConverters.CollectionHasAsScala

class EmailAddressSpec extends AnyWordSpec with GuiceOneAppPerSuite with Matchers {

  private val appConfig = fakeApplication().injector.instanceOf[FrontendAppConfig]

  private val appConfigWithEmptyEmailWhitelist =
    FrontendAppConfig(
      fakeApplication().configuration
        .copy(underlying = ConfigFactory.parseString("email-domains-whitelist = []"))
        .withFallback(appConfig.config)
    )

  "EmailValidation" should {
    val validation = new EmailAddressValidation(appConfig)

    "validate a correct email address" in {
      validation.isValid("user@test.com") shouldBe true
      validation.isValid("user@test.co.uk") shouldBe true
    }
    "validate invalid email" in {
      validation.isValid("a@a") shouldBe false
      validation.isValid("user @test.com") shouldBe false
      validation.isValid("user@") shouldBe false
    }

    "validate an email with unknow domain based on whitelisted domain configuration" in {
      new EmailAddressValidation(appConfig).isValid("test@uk.tiauto.com") shouldBe true
      new EmailAddressValidation(appConfig).isValid("test@ie.tiautu.com") shouldBe false
      new EmailAddressValidation(appConfigWithEmptyEmailWhitelist).isValid("test@uk.tiauto.com") shouldBe false
    }

    "validate emails with all whitelisted domains" in {
      val configTemp: Config = ConfigFactory.defaultApplication()
      val whitelistedDomains: Seq[String] = configTemp.getStringList("email-domains-whitelist").asScala.toSeq

      val emailValidator = new EmailAddressValidation(appConfig)

      whitelistedDomains.foreach { domain =>
        val email = s"testUser@$domain"
        withClue(s"Failed for domain: $domain") {
          emailValidator.isValid(email) shouldBe true
        }
      }
    }

  }

}
