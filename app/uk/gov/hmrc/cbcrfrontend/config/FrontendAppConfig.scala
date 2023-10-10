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

package uk.gov.hmrc.cbcrfrontend.config

import play.api.Configuration

import javax.inject.{Inject, Singleton}

@Singleton
class FrontendAppConfig @Inject()(val runModeConfiguration: Configuration) {
  private def loadConfig(key: String) = runModeConfiguration.get[String](key)

  val cbcrFrontendHost: String = loadConfig(s"cbcr-frontend.host")
  val fileUploadMaxPolls: Int = loadConfig("maximum-js-polls").toInt
  val millisecondsBeforePoll: Int = loadConfig("milliseconds-before-poll").toInt

  val governmentGatewaySignInUrl: String = loadConfig("government-gateway-sign-in-url")
  val governmentGatewaySignOutUrl: String = loadConfig("government-gateway-sign-out-url")

  val cbcrFrontendBaseUrl: String = loadConfig("cbcr-frontend-base-url")
  val cbcrGuidanceUrl: String = loadConfig("cbcr-guidance-url")
  val cbcrGuidanceRegisterUrl: String = loadConfig("cbcr-guidance-register-url")
  val cbcrOecdGuideUrl: String = loadConfig("cbcr-OECD-guide-url")
  val emailDigitalService: String = loadConfig("email.digitalservice")
  val cbcrGuidanceUtrUrl: String = loadConfig("cbcr-guidance-utr-url")

  def fallbackURLForLanguageSwitcher: String = loadConfig("languageSwitcher.fallback.url")
}
