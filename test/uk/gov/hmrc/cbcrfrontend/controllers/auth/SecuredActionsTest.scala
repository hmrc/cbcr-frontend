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

package uk.gov.hmrc.cbcrfrontend.controllers.auth

import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.cbcrfrontend.auth.SecuredActions
import uk.gov.hmrc.cbcrfrontend.controllers.{AsyncUserRequest, UserRequest}
import uk.gov.hmrc.play.frontend.auth.AuthContext
import uk.gov.hmrc.play.frontend.auth.connectors.AuthConnector


class SecuredActionsTest(authContext: AuthContext, val authConnector: AuthConnector) extends SecuredActions {

  def AuthenticatedAction(r: UserRequest): Action[AnyContent] = Action {
    r(authContext)
  }
  def AsyncAuthenticatedAction(r: AsyncUserRequest): Action[AnyContent] = Action.async {
    r(authContext)
  }
}
