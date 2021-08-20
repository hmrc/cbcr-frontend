/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.cbcrfrontend.controllers.actions

import com.google.inject.Inject
import play.api.Logger
import play.api.mvc.Results._
import play.api.mvc._
import uk.gov.hmrc.auth.core.AffinityGroup.{Individual, Organisation}
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.auth.core.retrieve.~
import uk.gov.hmrc.cbcrfrontend.controllers.routes
import uk.gov.hmrc.cbcrfrontend.controllers.upscan.{routes => upscanRoutes}
import uk.gov.hmrc.cbcrfrontend.model.CBCId
import uk.gov.hmrc.cbcrfrontend.model.requests.IdentifierRequest
import uk.gov.hmrc.cbcrfrontend.services.CBCSessionCache
import uk.gov.hmrc.cbcrfrontend.{CBCRErrorHandler, cbcEnrolment}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.http.HeaderCarrierConverter

import scala.concurrent.duration.{Duration => SDuration}
import scala.concurrent.{Await, ExecutionContext, Future}

trait IdentifierAction
    extends ActionBuilder[IdentifierRequest, AnyContent] with ActionFunction[Request, IdentifierRequest]

class AuthenticatedIdentifierAction @Inject()(
  override val authConnector: AuthConnector,
  val parser: BodyParsers.Default,
  cache: CBCSessionCache,
  errorHandler: CBCRErrorHandler,
)(implicit val executionContext: ExecutionContext)
    extends IdentifierAction with AuthorisedFunctions {

  lazy val logger: Logger = Logger(this.getClass)

  override def invokeBlock[A](request: Request[A], block: IdentifierRequest[A] => Future[Result]): Future[Result] = {

    implicit val hc: HeaderCarrier = HeaderCarrierConverter.fromRequestAndSession(request, request.session)

    authorised(AffinityGroup.Organisation or AffinityGroup.Agent)
      .retrieve(Retrievals.credentials and Retrievals.affinityGroup and cbcEnrolment) {
        case _ ~ (None | Some(Individual)) ~ _ =>
          Future.successful(Redirect(routes.SharedController.unsupportedAffinityGroup))
        case _ ~ Some(Organisation) ~ None
            if Await.result(cache.readOption[CBCId].map(_.isEmpty), SDuration(5, "seconds")) =>
          Future.successful(Redirect(routes.SharedController.unregisteredGGAccount))
        case cred ~ affinityGroup ~ enrolments =>
          block(IdentifierRequest(request, cred, affinityGroup, enrolments))
      } recover {
      case _: InsufficientEnrolments =>
        logger.info(s"User has InsufficientEnrolments")
        Redirect(routes.SharedController.unregisteredGGAccount) //TODO new unauthorised page
      case e: UnsupportedAuthProvider =>
        logger.info(s"UnsupportedAuthProvider:Failed to login: ${e.msg}")
        Redirect(routes.SharedController.unregisteredGGAccount)
      case e: Exception =>
        logger.info(s"Failed to login: ${e.getMessage}")
        errorHandler.resolveError(request, e)
    }
  }

}
