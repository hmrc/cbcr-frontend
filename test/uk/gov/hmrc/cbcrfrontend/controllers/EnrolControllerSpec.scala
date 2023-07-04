/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.cbcrfrontend.controllers

import com.typesafe.config.ConfigFactory
import org.mockito.ArgumentMatchersSugar.*
import org.mockito.IdiomaticMockito
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.mvc.MessagesControllerComponents
import play.api.test.FakeRequest
import play.api.test.Helpers.{defaultAwaitTimeout, status}
import play.api.{Configuration, Environment}
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.cbcrfrontend.connectors.TaxEnrolmentsConnector
import uk.gov.hmrc.cbcrfrontend.model.CBCId
import uk.gov.hmrc.http.HttpResponse

import scala.concurrent.{ExecutionContext, Future}

class EnrolControllerSpec
    extends AnyWordSpec with Matchers with GuiceOneAppPerSuite with CSRFTest with IdiomaticMockito {

  private implicit val ec: ExecutionContext = app.injector.instanceOf[ExecutionContext]

  private val config = new Configuration(ConfigFactory.load("application.conf"))

  private val authConnector = mock[AuthConnector]
  private val enrolConnector = mock[TaxEnrolmentsConnector]
  private val env = mock[Environment]
  private val mcc = app.injector.instanceOf[MessagesControllerComponents]
  private val controller = new EnrolController(config, enrolConnector, authConnector, env, mcc)

  private val id = CBCId.create(5678).getOrElse(fail("bad cbcid"))

  "Calling enrol controller" should {
    "return 200 when calling deEnrol if authorised Organisation and User or Admin" in {
      val fakeRequest = addToken(FakeRequest("GET", "/de-enrol"))
      val response = mock[HttpResponse]
      authConnector.authorise[Any](*, *)(*, *) returns Future.successful(())
      enrolConnector.deEnrol(*) returns Future.successful(response)
      response.body returns "deEnrol"
      status(controller.deEnrol()(fakeRequest)) shouldBe Status.OK
    }

    "return 200 when calling getEnrolments if authorised" in {
      val fakeRequest = addToken(FakeRequest("GET", "/enrolments"))
      val enrolments = Enrolments(Set(Enrolment("CBC", Seq(EnrolmentIdentifier("cbcId", id.toString)), "something")))
      authConnector.authorise[Enrolments](*, *)(*, *) returns Future.successful(enrolments)
      status(controller.getEnrolments(fakeRequest)) shouldBe Status.OK
    }
  }
}
