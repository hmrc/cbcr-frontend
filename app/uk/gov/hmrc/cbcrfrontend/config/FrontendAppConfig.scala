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

package uk.gov.hmrc.cbcrfrontend.config

import play.api.Configuration
import uk.gov.hmrc.cbcrfrontend.util.ConfigurationOps.ConfigurationOps

import java.time.LocalDate
import javax.inject.{Inject, Singleton}

@Singleton
class FrontendAppConfig @Inject() (val config: Configuration) {
  val cbcrFrontendHost: String = config.load[String](s"cbcr-frontend.host")
  val fileUploadMaxPolls: Int = config.load[Int]("maximum-js-polls")

  val governmentGatewaySignInUrl: String = config.load[String]("government-gateway-sign-in-url")
  val governmentGatewaySignOutUrl: String = config.load[String]("government-gateway-sign-out-url")

  val cbcrFrontendBaseUrl: String = config.load[String]("cbcr-frontend-base-url")
  val cbcrGuidanceUrl: String = config.load[String]("cbcr-guidance-url")
  val cbcrGuidanceRegisterUrl: String = config.load[String]("cbcr-guidance-register-url")
  val cbcrOecdGuideUrl: String = config.load[String]("cbcr-OECD-guide-url")
  val emailDigitalService: String = config.load[String]("email.digitalservice")
  val cbcrGuidanceUtrUrl: String = config.load[String]("cbcr-guidance-utr-url")
  val oecdSchemaVersion: String = config.load[String]("Prod.oecd-schema-version")

  private val feedbackFeUrl: String = config.load[String]("microservice.services.feedback-frontend.url")

  val exitSurveyUrl: String = s"$feedbackFeUrl/CBCR"

  private val basGatewayUrl: String = config.load[String]("microservice.services.bas-gateway.url")

  private val signOutURi: String = config.load[String]("microservice.services.bas-gateway.signout.uri")

  val oneLoginSignoutUrl: String = s"$basGatewayUrl$signOutURi?continue=$exitSurveyUrl"

  val defaultCreationDate: LocalDate = {
    val creationDay = config.load[Int]("Prod.default-creation-date.day")
    val creationMonth = config.load[Int]("Prod.default-creation-date.month")
    val creationYear = config.load[Int]("Prod.default-creation-date.year")
    LocalDate.of(creationYear, creationMonth, creationDay)
  }
  val envelopeExpiryDays: Int = config.load[Int]("envelope-expire-days")
  val emailDomainsWhitelist: Seq[String] =
    config.getOptional[Seq[String]]("email-domains-whitelist").getOrElse(Seq.empty[String])
}
