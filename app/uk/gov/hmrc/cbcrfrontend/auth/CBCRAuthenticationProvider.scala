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

package uk.gov.hmrc.cbcrfrontend.auth

import java.net.URI
import javax.inject.{Inject, Singleton}

import play.api.Configuration
import play.api.mvc.Action
import play.api.mvc.Results.Redirect
import play.api.mvc.Results.Unauthorized
import play.api.mvc.{AnyContent, Request, Result}

import scala.concurrent.{ExecutionContext, Future}
import uk.gov.hmrc.cbcrfrontend.controllers.{AsyncUserRequest, UserRequest}
import uk.gov.hmrc.cbcrfrontend._
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.exceptions.UnexpectedState
import uk.gov.hmrc.cbcrfrontend.model.{AffinityGroup, Agent, Organisation, UserType}
import uk.gov.hmrc.cbcrfrontend.services.CBCSessionCache
import uk.gov.hmrc.play.frontend.auth._
import uk.gov.hmrc.play.frontend.auth.connectors.AuthConnector
import uk.gov.hmrc.play.frontend.controller.UnauthorisedAction
import uk.gov.hmrc.play.http.HeaderCarrier
import cats.instances.future._


trait SecuredActions extends Actions {
  def AuthenticatedAction(r: UserRequest): Action[AnyContent]
  def AsyncAuthenticatedAction(u:Option[UserType] = None)(r: AsyncUserRequest): Action[AnyContent]
}

@Singleton
class SecuredActionsImpl @Inject()(configuration: Configuration)(implicit cache:CBCSessionCache,val authConnector: AuthConnector, ec:ExecutionContext) extends SecuredActions {

  private val normalAuth = AuthenticatedBy(new CBCRAuthenticationProvider(configuration), CBCRPageVisibilityPredicate)

  private val organisationAuth = AuthenticatedBy(new CBCRAuthenticationProvider(configuration), new AffinityGroupPredicate(Organisation))

  private val agentAuth = AuthenticatedBy(new CBCRAuthenticationProvider(configuration), new AffinityGroupPredicate(Agent))

  override def AuthenticatedAction(r: UserRequest) = normalAuth(r)

  override def AsyncAuthenticatedAction(u:Option[UserType] = None)(r: AsyncUserRequest) = u match {
    case Some(Agent)        => agentAuth.async(r)
    case Some(Organisation) => organisationAuth.async(r)
    case None               => normalAuth.async(r)
  }

}

class CBCRAuthenticationProvider (configuration: Configuration) extends GovernmentGateway {

  val cbcrFrontendBaseUrl = configuration.getString("cbcr-frontend-base-url").getOrElse("")
  val governmentGatewaySignInUrl = configuration.getString("government-gateway-sign-in-url").getOrElse("")

  override def redirectToLogin(implicit request: Request[_]): Future[Result] = {

    val queryStringParams = Map("continue" -> Seq(cbcrFrontendBaseUrl + request.uri))
    Future.successful(Redirect(loginURL, queryStringParams))
  }

  def continueURL: String = "not used since we override redirectToLogin"

  def loginURL: String = governmentGatewaySignInUrl
}


object CBCRPageVisibilityPredicate extends PageVisibilityPredicate {
  def apply(authContext: AuthContext, request: Request[AnyContent]): Future[PageVisibilityResult] =
    Future.successful(PageIsVisible)
}

class AffinityGroupPredicate(restrictToUser: UserType)(implicit auth: AuthConnector, cache:CBCSessionCache, ec:ExecutionContext)
  extends PageVisibilityPredicate {

  private val errorPage = Future.successful(Unauthorized)

  override def apply(authContext: AuthContext, request: Request[AnyContent]): Future[PageVisibilityResult] = {

    implicit def hc(implicit request: Request[_]): HeaderCarrier = HeaderCarrier.fromHeadersAndSession(request.headers, Some(request.session))

    val userType: ServiceResponse[UserType] = getUserType(authContext)(cache,auth,hc(request),ec)

    userType.fold( _ => PageBlocked(errorPage),{ ut => if(ut == restrictToUser){PageIsVisible} else { PageBlocked(errorPage)}})
  }

}
