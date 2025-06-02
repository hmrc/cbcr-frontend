/*
 * Copyright 2024 HM Revenue & Customs
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

import org.mockito.ArgumentMatchers.{any, eq => eqTo}
import org.mockito.Mockito.when
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.mockito.MockitoSugar
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.mvc.MessagesControllerComponents
import play.api.test.FakeRequest
import play.api.test.Helpers.{defaultAwaitTimeout, status}
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.cbcrfrontend.connectors.TaxEnrolmentsConnector
import uk.gov.hmrc.cbcrfrontend.model.CBCId
import uk.gov.hmrc.http.HttpResponse

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class EnrolControllerSpec extends AnyWordSpec with Matchers with GuiceOneAppPerSuite with CSRFTest with MockitoSugar {

  private val authConnector = mock[AuthConnector]
  private val enrolConnector = mock[TaxEnrolmentsConnector]
  private val mcc = app.injector.instanceOf[MessagesControllerComponents]
  private val controller = new EnrolController(enrolConnector, authConnector, mcc)

  private val id = CBCId.create(5678).getOrElse(fail("bad cbcid"))

  "Calling enrol controller" should {
    "return 200 when calling deEnrol if authorised Organisation and User or Admin" in {
      val fakeRequest = addToken(FakeRequest("GET", "/de-enrol"))
      val response = mock[HttpResponse]
      when(authConnector.authorise[Any](any, any)(any, any)).thenReturn(Future.successful(()))
      when(enrolConnector.deEnrol(any)).thenReturn(Future.successful(response))
      when(response.body).thenReturn("deEnrol")
      status(controller.deEnrol()(fakeRequest)) shouldBe Status.OK
    }

    "return 200 when calling getEnrolments if authorised" in {
      val fakeRequest = addToken(FakeRequest("GET", "/enrolments"))
      val enrolments = Enrolments(Set(Enrolment("CBC", Seq(EnrolmentIdentifier("cbcId", id.toString)), "something")))
      when(authConnector.authorise[Enrolments](any, any)(any, any)).thenReturn(Future.successful(enrolments))
      status(controller.getEnrolments(fakeRequest)) shouldBe Status.OK
    }
  }
}
