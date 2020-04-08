/*
 * Copyright 2020 HM Revenue & Customs
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

/**
  * Copyright 2018 HM Revenue & Customs
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
import java.util.Base64

import javax.inject.{Inject, Singleton}
import play.api.{Configuration, Environment}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

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

  val analyticsToken: String = loadConfig(s"google-analytics.token")
  val analyticsHost: String = loadConfig(s"google-analytics.host")
  val assetsPrefix: String = loadConfig(s"assets.url") + loadConfig(s"assets.version")
  val cbcrFrontendHost: String = loadConfig(s"cbcr-frontend.host")
  val fileUploadFrontendHost: String = loadConfig(s"file-upload-public-frontend.host")

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

  private def whitelistConfig(key: String): Seq[String] =
    Some(
      new String(
        Base64.getDecoder
          .decode(runModeConfiguration.getOptional[String](key).getOrElse("")),
        "UTF-8"))
      .map(_.split(","))
      .getOrElse(Array.empty)
      .toSeq

  val whitelist: Seq[String] = whitelistConfig("whitelist")
  val whitelistExcluded: Seq[String] = whitelistConfig("whitelist-excluded")

  val timeOutSeconds = loadConfig("sessionTimeout.timeOutSeconds")
  val timeOutCountdownSeconds = loadConfig("sessionTimeout.timeOutCountdownSeconds")
  val timeOutShowDialog: Boolean =
    runModeConfiguration.getOptional[Boolean](s"sessionTimeout.timeOutShowDialog").getOrElse(false)
  val keepAliveUrl = loadConfig("sessionTimeout.keepAliveUrl")
  val signOutUrl = loadConfig("sessionTimeout.signOutUrl")

  val gtmContainerId = loadConfig("googleTagManager.containerId")

  def fallbackURLForLanguageSwitcher: String = loadConfig("languageSwitcher.fallback.url")

  lazy val username = servicesConfig.getString("credentials.username")
  lazy val password = servicesConfig.getString("credentials.password")
}
