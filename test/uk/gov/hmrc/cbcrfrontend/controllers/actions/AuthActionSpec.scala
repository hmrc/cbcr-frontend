/*
 * Copyright 2022 HM Revenue & Customs
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

import base.SpecBase
import com.google.inject.Inject
import org.scalatest.MustMatchers.convertToAnyMustWrapper
import play.api.mvc.{Action, AnyContent, BodyParsers, Results}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.authorise.Predicate
import uk.gov.hmrc.auth.core.retrieve.Retrieval
import uk.gov.hmrc.cbcrfrontend.CBCRErrorHandler
import uk.gov.hmrc.cbcrfrontend.services.CBCSessionCache
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}

class AuthActionSpec extends SpecBase {

  class Harness(authAction: IdentifierAction) {
    def onPageLoad(): Action[AnyContent] = authAction { _ =>
      Results.Ok
    }
  }

  val bodyParsers: BodyParsers.Default = app.injector.instanceOf[BodyParsers.Default]
  val errorHandler: CBCRErrorHandler = app.injector.instanceOf[CBCRErrorHandler]
  val sessionCache: CBCSessionCache = app.injector.instanceOf[CBCSessionCache]

  "Auth Action" - {

    "when the user hasn't logged in" - {

      "must redirect the user to log in " in {

        val authAction = new AuthenticatedIdentifierAction(
          new FakeFailingAuthConnector(new MissingBearerToken),
          bodyParsers,
          sessionCache,
          errorHandler
        )
        val controller = new Harness(authAction)
        val result = controller.onPageLoad()(FakeRequest())

        status(result) shouldBe SEE_OTHER
        redirectLocation(result).value shouldBe "/bas-gateway/sign-in?continue_url=%2F&origin=cbcr-frontend"
      }
    }

    "the user's session has expired" - {

      "must redirect the user to log in " in {

        val authAction = new AuthenticatedIdentifierAction(
          new FakeFailingAuthConnector(new BearerTokenExpired),
          bodyParsers,
          sessionCache,
          errorHandler
        )
        val controller = new Harness(authAction)
        val result = controller.onPageLoad()(FakeRequest())

        status(result) shouldBe SEE_OTHER
        redirectLocation(result).value shouldBe "/bas-gateway/sign-in?continue_url=%2F&origin=cbcr-frontend"
      }
    }

    "the user doesn't have sufficient enrolments" - {

      "must redirect the user to the unauthorised page" in {

        val authAction = new AuthenticatedIdentifierAction(
          new FakeFailingAuthConnector(new InsufficientEnrolments),
          bodyParsers,
          sessionCache,
          errorHandler
        )
        val controller = new Harness(authAction)
        val result = controller.onPageLoad()(FakeRequest())

        status(result) shouldBe SEE_OTHER
        redirectLocation(result).value shouldBe "/country-by-country-reporting/unregistered-gg-account-view"
      }
    }

    "the user used an unaccepted auth provider" - {

      "must redirect the user to the unauthorised page" in {

        val authAction = new AuthenticatedIdentifierAction(
          new FakeFailingAuthConnector(new UnsupportedAuthProvider),
          bodyParsers,
          sessionCache,
          errorHandler
        )
        val controller = new Harness(authAction)
        val result = controller.onPageLoad()(FakeRequest())

        status(result) shouldBe SEE_OTHER
        redirectLocation(result).value shouldBe "/country-by-country-reporting/unregistered-gg-account-view"
      }
    }

    "the user has an unsupported affinity group" - {

      "must redirect the user to the unauthorised page" in {

        val authAction = new AuthenticatedIdentifierAction(
          new FakeFailingAuthConnector(new UnsupportedAffinityGroup),
          bodyParsers,
          sessionCache,
          errorHandler
        )
        val controller = new Harness(authAction)
        val result = controller.onPageLoad()(FakeRequest())

        status(result) shouldBe SEE_OTHER
        redirectLocation(result) shouldBe Some("/country-by-country-reporting/unsupportedAffinityGroup")
      }
    }

    "the user has an unsupported credential role" - {

      "must redirect the user to the unauthorised page" in {

        val authAction = new AuthenticatedIdentifierAction(
          new FakeFailingAuthConnector(new UnsupportedCredentialRole),
          bodyParsers,
          sessionCache,
          errorHandler
        )
        val controller = new Harness(authAction)
        val result = controller.onPageLoad()(FakeRequest())

        status(result) shouldBe SEE_OTHER
        redirectLocation(result) shouldBe Some("/country-by-country-reporting/no-assistants")
      }
    }
  }
}

class FakeFailingAuthConnector @Inject()(exceptionToReturn: Throwable) extends AuthConnector {
  val serviceUrl: String = ""

  override def authorise[A](predicate: Predicate, retrieval: Retrieval[A])(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[A] =
    Future.failed(exceptionToReturn)
}
