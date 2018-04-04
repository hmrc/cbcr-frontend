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

import play.api.{Configuration, Logger}
import play.api.mvc._
import uk.gov.hmrc.auth.core.AffinityGroup.Organisation
import uk.gov.hmrc.auth.core.authorise.{EmptyPredicate, Predicate}
import uk.gov.hmrc.auth.core.retrieve.{EmptyRetrieval, Retrieval, Retrievals}
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.model.CBCEnrolment
import uk.gov.hmrc.cbcrfrontend.views.html.subscription.notAuthorised
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.cache.client.NoSessionException
import uk.gov.hmrc.play.HeaderCarrierConverter

import scala.concurrent.{ExecutionContext, Future}

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


import play.api.mvc.Results.Unauthorized
import uk.gov.hmrc.play.bootstrap.config.AuthRedirects


trait CBCRAuthFunctions2 extends AuthorisedFunctions with AuthRedirects {
  //  def CBCRAuth[R](implicit ec:ExecutionContext) : ActionBuilder[Request] = CBCRAuth(None)(None)
  //  def CBCRAuth[R](predicate: Predicate)(implicit ec:ExecutionContext) : ActionBuilder[Request] = CBCRAuth(Some(predicate))(None)
  //  def CBCRAuth[R]()(retrieval: Retrieval[R])(implicit ec:ExecutionContext) : ActionBuilder[Request] = CBCRAuth(None)(Some(retrieval))
  //  def CBCRAuth[R](predicate: Predicate)(retrieval: Retrieval[R]) : ActionBuilder[Request] = CBCRAuth(Some(predicate))(Some(retrieval))
//  def authWithAffinity[R](predicate: Option[Predicate]) = new ActionBuilder[AffinityRequest] with ActionTransformer[Request,AffinityRequest] with Results {
//    override protected def transform[A](request: Request[A]): Future[AffinityRequest[A]] =
//      authorised(predicate.getOrElse(EmptyPredicate)).retrieve(Retrievals.affinityGroup) {
//        case oa@Some(Organisation) => Future.successful(AffinityRequest(oa, request))
//      }
//  }
  //  def CBCRAuth[R,A](predicate: Option[Predicate])(retrieval: Option[Retrieval[R]])
  //                 (implicit ec:ExecutionContext) = new ActionRefiner[Request,RetrievalRequest[R,A]] with Results {
  //    override protected def refine[A](request: Request[A]): Future[Either[Result, R => Request[A]]] = {
  //      implicit val hc = HeaderCarrierConverter.fromHeadersAndSession(request.headers, Some(request.session))
  //      authorised(predicate.getOrElse(EmptyPredicate)).retrieve(retrieval.getOrElse(EmptyRetrieval)){ r =>
  //        Future.successful(Right((r:R) => request))
  //      }.recover{
  //        case nas:NoActiveSession =>
  //          Logger.info(s"No active session - redirecting to GG login page: ${nas.reason}")
  //          Left(toGGLogin(request.uri))
  //      }
  //    }
  //  }
}


case class AffinityRequest[A](affinityGroup: Option[AffinityGroup], request: Request[A])

trait CBCRAuthFunctions extends AuthRedirects{

  def authConnector: AuthConnector


  def auth[A,B](body: B => Future[A],predicate: Predicate,retrieval: Retrieval[B])(implicit request: Request[_],hc: HeaderCarrier, ec: ExecutionContext): Future[A] = {
    authConnector.authorise(predicate, retrieval).flatMap(body).recover {
      case _: NoActiveSession => toGGLogin(request.uri).asInstanceOf[A]
    }
  }

  class CBCRAuthorisedFunction(predicate: Predicate){


    def apply[A](body: => Future[A])(implicit hc: HeaderCarrier, ec: ExecutionContext, request: Request[_]): Future[A] = {
      auth((unit: Unit) => body,predicate,EmptyRetrieval)
    }

    def retrieve[A](retrieval: Retrieval[A]) = new CBCRAuthorisedFunctionWithResult(predicate, retrieval)
  }

  class CBCRAuthorisedFunctionWithResult[A](predicate: Predicate, retrieval: Retrieval[A]){

    def apply[B](body: A => Future[B])(implicit hc: HeaderCarrier, ec: ExecutionContext, request: Request[_]): Future[B] =
      auth(body,predicate,retrieval)

  }


  def authorised(): CBCRAuthorisedFunction = {
    new CBCRAuthorisedFunction(EmptyPredicate)
  }
  def authorised(predicate: Predicate): CBCRAuthorisedFunction = {
    new CBCRAuthorisedFunction(predicate)
  }

}
//
//
//import javax.inject.{Inject, Singleton}
//
//import cats.instances.future._
//import play.api.{Configuration, Logger}
//import play.api.mvc.Results.{Redirect, Unauthorized}
//import play.api.mvc.{Action, AnyContent, Request, Result}
//import uk.gov.hmrc.cbcrfrontend._
//import uk.gov.hmrc.cbcrfrontend.controllers.{AsyncUserRequest, UserRequest}
//import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
//import uk.gov.hmrc.cbcrfrontend.model.{Agent, Individual, Organisation, UserType}
//import uk.gov.hmrc.cbcrfrontend.services.CBCSessionCache
//import uk.gov.hmrc.play.frontend.auth.{PageVisibilityResult, _}
//import uk.gov.hmrc.play.frontend.auth.connectors.AuthConnector
//import play.api.i18n.Messages.Implicits._
//import play.api.Play.current
//
//import scala.concurrent.{ExecutionContext, Future}
//import uk.gov.hmrc.cbcrfrontend.views.html._
//import uk.gov.hmrc.cbcrfrontend.views.html.subscription.notAuthorised
//import uk.gov.hmrc.http.HeaderCarrier
//import uk.gov.hmrc.play.HeaderCarrierConverter
//
//
//trait SecuredActions extends Actions {
//  def AuthenticatedAction(r: UserRequest): Action[AnyContent]
//
//  def AsyncAuthenticatedAction(u: Option[UserType] = None)(r: AsyncUserRequest): Action[AnyContent]
//}
//
//@Singleton
//class SecuredActionsImpl @Inject()(configuration: Configuration)(implicit cache: CBCSessionCache, val authConnector: AuthConnector, ec: ExecutionContext) extends SecuredActions {
//
//  private val normalAuth = AuthenticatedBy(new CBCRAuthenticationProvider(configuration), new CBCRPageVisibilityPredicate())
//
//  private val organisationAuth = AuthenticatedBy(new CBCRAuthenticationProvider(configuration), new AffinityGroupPredicate(Organisation(true)))
//
//  private val agentAuth = AuthenticatedBy(new CBCRAuthenticationProvider(configuration), new AffinityGroupPredicate(Agent()))
//
//  private val individualAuth = AuthenticatedBy(new CBCRAuthenticationProvider(configuration), CBCRPageVisibilityIndividual)
//
//  override def AuthenticatedAction(r: UserRequest) = normalAuth(r)
//
//  override def AsyncAuthenticatedAction(u: Option[UserType] = None)(r: AsyncUserRequest) = u match {
//    case Some(Agent()) => agentAuth.async(r)
//    case Some(Organisation(_)) => organisationAuth.async(r)
//    case Some(Individual()) => individualAuth.async(r)
//    case None => normalAuth.async(r)
//  }
//
//}
//
//class CBCRAuthenticationProvider(configuration: Configuration) extends GovernmentGateway {
//
//  val cbcrFrontendBaseUrl = configuration.getString("cbcr-frontend-base-url").getOrElse("")
//  val governmentGatewaySignInUrl = configuration.getString("government-gateway-sign-in-url").getOrElse("")
//
//  override def redirectToLogin(implicit request: Request[_]): Future[Result] = {
//
//    val queryStringParams = Map("continue" -> Seq(cbcrFrontendBaseUrl + request.uri))
//    Future.successful(Redirect(loginURL, queryStringParams))
//  }
//
//  def continueURL: String = "not used since we override redirectToLogin"
//
//  def loginURL: String = governmentGatewaySignInUrl
//}
//trait UserTypeRedirect{
//  def errorPage(userType: Option[UserType] = None)(implicit request: Request[_]) = {
//    userType match {
//      case Some(Agent()) => Future.successful(Unauthorized(notAuthorised()))
//      case Some(Individual()) => Future.successful(Unauthorized(not_authorised_individual()))
//      case Some(Organisation(false)) => Future.successful(Unauthorized(not_authorised_assistant()))
//      case _ => Future.successful(Unauthorized(notAuthorised()))
//    }
//  }
//  def checkUser(userType: ServiceResponse[UserType],allowedUsers:Set[UserType])(implicit request: Request[_],ec: ExecutionContext) = {
//    userType.fold(_ => PageBlocked(errorPage()), { ut =>
//      if (allowedUsers.contains(ut)) {
//        PageIsVisible
//      } else {
//        PageBlocked(errorPage(Some(ut)))
//      }
//    })
//  }
//}
//
//class CBCRPageVisibilityPredicate()(implicit auth: AuthConnector, cache: CBCSessionCache, ec: ExecutionContext) extends PageVisibilityPredicate with UserTypeRedirect{
//  def apply(authContext: AuthContext, request: Request[AnyContent]): Future[PageVisibilityResult] = {
//
//    implicit val r = request
//
//    implicit def hc(implicit request: Request[_]): HeaderCarrier = HeaderCarrierConverter.fromHeadersAndSession(request.headers, Some(request.session))
//
//    checkUser(getUserType(authContext)(cache, auth, hc(request), ec), Set(Agent(), Organisation(true)))
//  }
//}
//
//object CBCRPageVisibilityIndividual extends PageVisibilityPredicate {
//  def apply(authContext: AuthContext, request: Request[AnyContent]): Future[PageBlocked] = {
//    implicit val r = request
//    Future.successful(PageBlocked(Future.successful
//    (Unauthorized(uk.gov.hmrc.cbcrfrontend.views.html.not_authorised_individual()))))
//  }
//}
//
//class AffinityGroupPredicate(restrictToUser: UserType)(implicit auth: AuthConnector, cache: CBCSessionCache, ec: ExecutionContext) extends PageVisibilityPredicate with UserTypeRedirect{
//
//
//  override def apply(authContext: AuthContext, request: Request[AnyContent]): Future[PageVisibilityResult] = {
//
//    implicit val r = request
//
//    implicit def hc(implicit request: Request[_]): HeaderCarrier = HeaderCarrierConverter.fromHeadersAndSession(request.headers, Some(request.session))
//
//    checkUser(getUserType(authContext)(cache, auth, hc(request), ec), Set(restrictToUser))
//  }
//}
//

