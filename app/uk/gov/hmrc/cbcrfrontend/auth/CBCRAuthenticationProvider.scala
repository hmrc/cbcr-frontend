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

import javax.inject.{Inject, Singleton}

import cats.instances.future._
import play.api.{Configuration, Logger}
import play.api.mvc.Results.{Redirect, Unauthorized}
import play.api.mvc.{Action, AnyContent, Request, Result}
import uk.gov.hmrc.cbcrfrontend._
import uk.gov.hmrc.cbcrfrontend.controllers.{AsyncUserRequest, UserRequest}
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.model.{Agent, Individual, Organisation, UserType}
import uk.gov.hmrc.cbcrfrontend.services.CBCSessionCache
import uk.gov.hmrc.play.frontend.auth.{PageVisibilityResult, _}
import uk.gov.hmrc.play.frontend.auth.connectors.AuthConnector
import uk.gov.hmrc.play.http.HeaderCarrier
import play.api.i18n.Messages.Implicits._
import play.api.Play.current

import scala.concurrent.{ExecutionContext, Future}
import uk.gov.hmrc.cbcrfrontend.views.html._
import uk.gov.hmrc.cbcrfrontend.views.html.subscription.notAuthorised


trait SecuredActions extends Actions {
  def AuthenticatedAction(r: UserRequest): Action[AnyContent]

  def AsyncAuthenticatedAction(u: Option[UserType] = None)(r: AsyncUserRequest): Action[AnyContent]
}

@Singleton
class SecuredActionsImpl @Inject()(configuration: Configuration)(implicit cache: CBCSessionCache, val authConnector: AuthConnector, ec: ExecutionContext) extends SecuredActions {

  private val normalAuth = AuthenticatedBy(new CBCRAuthenticationProvider(configuration), new CBCRPageVisibilityPredicate())

  private val organisationAuth = AuthenticatedBy(new CBCRAuthenticationProvider(configuration), new AffinityGroupPredicate(Organisation))

  private val agentAuth = AuthenticatedBy(new CBCRAuthenticationProvider(configuration), new AffinityGroupPredicate(Agent))

  private val individualAuth = AuthenticatedBy(new CBCRAuthenticationProvider(configuration), CBCRPageVisibilityIndividual)

  override def AuthenticatedAction(r: UserRequest) = normalAuth(r)

  override def AsyncAuthenticatedAction(u: Option[UserType] = None)(r: AsyncUserRequest) = u match {
    case Some(Agent) => agentAuth.async(r)
    case Some(Organisation) => organisationAuth.async(r)
    case Some(Individual) => individualAuth.async(r)
    case None => normalAuth.async(r)
  }

}

class CBCRAuthenticationProvider(configuration: Configuration) extends GovernmentGateway {

  val cbcrFrontendBaseUrl = configuration.getString("cbcr-frontend-base-url").getOrElse("")
  val governmentGatewaySignInUrl = configuration.getString("government-gateway-sign-in-url").getOrElse("")

  override def redirectToLogin(implicit request: Request[_]): Future[Result] = {

    val queryStringParams = Map("continue" -> Seq(cbcrFrontendBaseUrl + request.uri))
    Future.successful(Redirect(loginURL, queryStringParams))
  }

  def continueURL: String = "not used since we override redirectToLogin"

  def loginURL: String = governmentGatewaySignInUrl
}
trait UserTypeRedirect{
  def errorPage(userType: Option[UserType] = None)(implicit request: Request[_]) = {
    userType match {
      case Some(Agent) => Future.successful(Unauthorized(notAuthorised(includes.asideBusiness(), includes.phaseBannerBeta())))
      case Some(Individual) => Future.successful(Unauthorized(not_authorised_indivitial()))
      case _ => Future.successful(Unauthorized(notAuthorised(includes.asideBusiness(), includes.phaseBannerBeta())))
    }
  }
  def checkUser(userType: ServiceResponse[UserType],allowedUsers:Set[UserType])(implicit request: Request[_],ec: ExecutionContext) = {
    userType.fold(_ => PageBlocked(errorPage()), { ut =>
      if (allowedUsers.contains(ut)) {
        PageIsVisible
      } else {
        PageBlocked(errorPage(Some(ut)))
      }
    })
  }
}

class CBCRPageVisibilityPredicate()(implicit auth: AuthConnector, cache: CBCSessionCache, ec: ExecutionContext) extends PageVisibilityPredicate with UserTypeRedirect{
  def apply(authContext: AuthContext, request: Request[AnyContent]): Future[PageVisibilityResult] = {

    implicit val r = request

    implicit def hc(implicit request: Request[_]): HeaderCarrier = HeaderCarrier.fromHeadersAndSession(request.headers, Some(request.session))

    checkUser(getUserType(authContext)(cache, auth, hc(request), ec), Set(Agent, Organisation))
  }
}

  object CBCRPageVisibilityIndividual extends PageVisibilityPredicate {
    def apply(authContext: AuthContext, request: Request[AnyContent]): Future[PageBlocked] = {
      implicit val r = request
      Future.successful(PageBlocked(Future.successful
      (Unauthorized(uk.gov.hmrc.cbcrfrontend.views.html.not_authorised_indivitial()))))
    }
  }

  class AffinityGroupPredicate(restrictToUser: UserType)(implicit auth: AuthConnector, cache: CBCSessionCache, ec: ExecutionContext) extends PageVisibilityPredicate with UserTypeRedirect{


    override def apply(authContext: AuthContext, request: Request[AnyContent]): Future[PageVisibilityResult] = {

      implicit val r = request

      implicit def hc(implicit request: Request[_]): HeaderCarrier = HeaderCarrier.fromHeadersAndSession(request.headers, Some(request.session))

      checkUser(getUserType(authContext)(cache, auth, hc(request), ec), Set(restrictToUser))
    }
  }


