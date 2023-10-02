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

import play.api.{Configuration, Environment}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import java.util.Base64
import javax.inject.{Inject, Singleton}

@Singleton
class FrontendAppConfig @Inject()(
  val runModeConfiguration: Configuration,
  val environment: Environment,
  servicesConfig: ServicesConfig) {

  val mode = environment.mode
  private def loadConfig(key: String) =
    runModeConfiguration.getOptional[String](key).getOrElse(throw new Exception(s"Missing configuration key: $key"))

  val contactHost = runModeConfiguration.getOptional[String](s"contact-frontend.host").getOrElse("")
  val contactFormServiceIdentifier = "CountryByCountryReporting"

  val cbcrFrontendHost: String = loadConfig(s"cbcr-frontend.host")
  val fileUploadFrontendHost: String = loadConfig(s"file-upload-public-frontend.host")
  val fileUploadMaxPolls: Int = loadConfig("maximum-js-polls").toInt

  val reportAProblemPartialUrl: String =
    s"$contactHost/contact/problem_reports_ajax?service=$contactFormServiceIdentifier"
  val reportAProblemNonJSUrl: String =
    s"$contactHost/contact/problem_reports_nonjs?service=$contactFormServiceIdentifier"

  val governmentGatewaySignInUrl: String = loadConfig("government-gateway-sign-in-url")
  val governmentGatewaySignOutUrl: String = loadConfig("government-gateway-sign-out-url")

  val cbcrFrontendBaseUrl: String = loadConfig("cbcr-frontend-base-url")
  val betaFeedbackUrlNoAuth: String =
    s"$contactHost/contact/beta-feedback-unauthenticated?service=$contactFormServiceIdentifier"
  val cbcrGuidanceUrl: String = loadConfig("cbcr-guidance-url")
  val cbcrGuidanceRegisterUrl: String = loadConfig("cbcr-guidance-register-url")
  val cbcrOecdGuideUrl: String = loadConfig("cbcr-OECD-guide-url")
  val emailDigitalService: String = loadConfig("email.digitalservice")

  val cbcEnhancementFeature: Boolean = loadConfig("features.cbcEnhancementFeature").toBoolean

  private def allowlistConfig(key: String): Seq[String] =
    Some(
      new String(
        Base64.getDecoder
          .decode(runModeConfiguration.getOptional[String](key).getOrElse("")),
        "UTF-8"))
      .map(_.split(","))
      .getOrElse(Array.empty)
      .toSeq

  val allowlist: Seq[String] = allowlistConfig("allowlist")
  val allowlistExcluded: Seq[String] = allowlistConfig("allowlist-excluded")

  val timeOutSeconds = loadConfig("sessionTimeout.timeOutSeconds")
  val timeOutCountdownSeconds = loadConfig("sessionTimeout.timeOutCountdownSeconds")
  val timeOutShowDialog: Boolean =
    runModeConfiguration.getOptional[Boolean](s"sessionTimeout.timeOutShowDialog").getOrElse(false)
  val keepAliveUrl = loadConfig("sessionTimeout.keepAliveUrl")
  val signOutUrl = loadConfig("sessionTimeout.signOutUrl")

  def fallbackURLForLanguageSwitcher: String = loadConfig("languageSwitcher.fallback.url")

  lazy val username = servicesConfig.getString("credentials.username")
  lazy val password = servicesConfig.getString("credentials.password")
}
