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

package uk.gov.hmrc.cbcrfrontend

import play.api.Play.{configuration, current}
import uk.gov.hmrc.play.config.ServicesConfig
import java.util.Base64

trait AppConfig {
  val analyticsToken: String
  val analyticsHost: String
  val reportAProblemPartialUrl: String
  val reportAProblemNonJSUrl: String
  def governmentGatewaySignInUrl: String
  def cbcrFrontendBaseUrl: String
  def assetsPrefix: String
  val cbcrFrontendHost: String
  val fileUploadFrontendHost: String
  val betaFeedbackUrlNoAuth: String
  val governmentGatewaySignOutUrl: String


}

object FrontendAppConfig extends AppConfig with ServicesConfig {

  private def loadConfig(key: String) = configuration.getString(key).getOrElse(throw new Exception(s"Missing configuration key: $key"))

  private val contactHost = configuration.getString(s"contact-frontend.host").getOrElse("")
  private val contactFormServiceIdentifier = "CountryByCountryReporting"

  override lazy val analyticsToken = loadConfig(s"google-analytics.token")
  override lazy val analyticsHost = loadConfig(s"google-analytics.host")
  override lazy val reportAProblemPartialUrl = s"$contactHost/contact/problem_reports_ajax?service=$contactFormServiceIdentifier"
  override lazy val reportAProblemNonJSUrl = s"$contactHost/contact/problem_reports_nonjs?service=$contactFormServiceIdentifier"
  override lazy val assetsPrefix = loadConfig(s"assets.url") + loadConfig(s"assets.version")
  override lazy val cbcrFrontendHost = loadConfig(s"cbcr-frontend.host")
  override lazy val fileUploadFrontendHost = loadConfig(s"file-upload-public-frontend.host")

  override lazy val governmentGatewaySignInUrl = loadConfig("government-gateway-sign-in-url")

  override lazy val governmentGatewaySignOutUrl: String = loadConfig("government-gateway-sign-out-url")


  // this will be empty in non-local environments
  override lazy val cbcrFrontendBaseUrl = loadConfig("cbcr-frontend-base-url")

  override lazy val betaFeedbackUrlNoAuth = s"$contactHost/contact/beta-feedback-unauthenticated?service=$contactFormServiceIdentifier"


  private def whitelistConfig(key: String): Seq[String] = Some(new String(Base64.getDecoder
    .decode(configuration.getString(key).getOrElse("")), "UTF-8"))
    .map(_.split(",")).getOrElse(Array.empty).toSeq

  lazy val whitelist = whitelistConfig("whitelist")
  lazy val whitelistExcluded = whitelistConfig("whitelist-excluded")

}
