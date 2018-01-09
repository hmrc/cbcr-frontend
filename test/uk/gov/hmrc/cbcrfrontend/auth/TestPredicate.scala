/*
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

package uk.gov.hmrc.cbcrfrontend.auth

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mock.MockitoSugar
import org.scalatestplus.play.OneAppPerSuite
import play.api.test.FakeRequest
import uk.gov.hmrc.cbcrfrontend.controllers.{AsyncUserRequest, CSRFTest, FakeAuthConnector}
import uk.gov.hmrc.cbcrfrontend.services.{CBCSessionCache, WithConfigFakeApplication}
import uk.gov.hmrc.play.frontend.auth.connectors.AuthConnector
import uk.gov.hmrc.play.frontend.auth.{AuthContext, LoggedInUser, Principal}
import uk.gov.hmrc.play.frontend.auth.connectors.domain.Accounts
import uk.gov.hmrc.play.frontend.auth.connectors.domain.ConfidenceLevel.L500
import uk.gov.hmrc.play.frontend.auth.connectors.domain.CredentialStrength.Strong
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.ExecutionContext

trait TestPredicate  extends UnitSpec with ScalaFutures with OneAppPerSuite with CSRFTest with FakeAuthConnector with WithConfigFakeApplication with MockitoSugar {

  def configFile = "application.conf"
  private[auth] val cache = mock[CBCSessionCache]
  private[auth] val authConnector = mock[AuthConnector]
  private[auth] val ec = fakeApplication.injector.instanceOf[ExecutionContext]
  private[auth] val req = mock[AsyncUserRequest]
  private[auth] val config = fakeApplication.configuration
  private[auth] val sec = new SecuredActionsImpl(config)(cache, authConnector, ec)
  private[auth] val request = FakeRequest("GET", "/anypath")

  private[auth] def authContext = {
    val user: LoggedInUser = LoggedInUser("userId", None, None, Some("token"), Strong, L500, "userId")
    val principal: Principal = Principal(None, Accounts())
    val userDetailsUri: Option[String] = None
    val enrolmentsUri: Option[String] = None
    val idsUri: Option[String] = None

    new AuthContext(user,
      principal,
      None,
      userDetailsUri,
      enrolmentsUri,
      idsUri)
  }

}
