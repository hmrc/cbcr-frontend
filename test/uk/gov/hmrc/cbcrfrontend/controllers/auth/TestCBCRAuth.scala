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

package uk.gov.hmrc.cbcrfrontend.controllers.auth

import uk.gov.hmrc.play.frontend.auth.connectors.domain.{ Accounts, ConfidenceLevel, CredentialStrength }
import uk.gov.hmrc.play.frontend.auth._

trait TestUsers {
  def businessUser = AuthContext(
    LoggedInUser("uid", None, None, None, CredentialStrength.Strong, ConfidenceLevel.L500, "oid/1234567890"),
    Principal(Some("Bob P"), Accounts()),
    attorney = None,
    Some("/user-details/1234567890"),
    Some("/auth/oid/1234567890/enrolments"),
    Some("/auth/oid/1234567890/ids")
  )

  def cbcrUser = businessUser.copy(
    attorney = Some(Attorney("Dave A", Link("A", "A")))
  )
}

object TestUsers extends TestUsers
