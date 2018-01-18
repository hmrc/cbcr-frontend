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

package uk.gov.hmrc.cbcrfrontend.controllers

import cats.data.OptionT
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mock.MockitoSugar
import org.scalatestplus.play.OneAppPerSuite
import play.api.Configuration
import uk.gov.hmrc.cbcrfrontend.connectors.EnrolmentsConnector
import uk.gov.hmrc.cbcrfrontend.controllers.auth.{TestSecuredActions, TestUsers}
import uk.gov.hmrc.cbcrfrontend.services.CBCSessionCache
import uk.gov.hmrc.play.frontend.auth.connectors.AuthConnector
import uk.gov.hmrc.play.http.HeaderCarrier
import uk.gov.hmrc.play.test.UnitSpec
import cats.instances.future._

import scala.concurrent.{ExecutionContext, Future}
import org.mockito.Matchers.{eq => EQ, _}
import org.mockito.Mockito._
import play.api.http.Status
import play.api.test.FakeRequest
import uk.gov.hmrc.cbcrfrontend.model.{AffinityGroup, CBCEnrolment, CBCId, Utr}

class StartControllerSpec  extends UnitSpec with ScalaFutures with OneAppPerSuite with CSRFTest with FakeAuthConnector with MockitoSugar with BeforeAndAfterEach{


  implicit val ec                     = app.injector.instanceOf[ExecutionContext]
  val configuration                   = mock[Configuration]
  implicit val enrol                  = mock[EnrolmentsConnector]
  implicit val cache: CBCSessionCache = mock[CBCSessionCache]
  implicit val sec: AuthConnector     = mock[AuthConnector]
  implicit val authCon                = authConnector(TestUsers.cbcrUser)
  val securedActions                  = new TestSecuredActions(TestUsers.cbcrUser, authCon)

  implicit val hc: HeaderCarrier      = new HeaderCarrier()

  val controller = new StartController(securedActions,enrol)(cache,authCon)

  val fakeRequest = addToken(FakeRequest("GET", "/"))
  val fakePost    = addToken(FakeRequest("POST", "/"))

  override protected def afterEach(): Unit = {
    reset(cache,sec,enrol,configuration)
    super.afterEach()
  }


  val cbcId = CBCId.create(99).getOrElse(fail("Unable to create CBCId"))
  val utr = Utr("9000000001")


  "The StartControllers default route redirects correctly" when  {
    "the user is an Agent" in { // #5
      when(cache.readOption[AffinityGroup](any(),any(),any())) thenReturn Future.successful(Some(AffinityGroup("Agent",Some("admin"))))
      val result = controller.start(fakeRequest)
      status(result) shouldBe Status.SEE_OTHER
      result.header.headers.getOrElse("location", "") shouldBe routes.FileUploadController.chooseXMLFile().url
    }
    "the user is an Organisation and has not registered" in { // #1
      when(cache.readOption[AffinityGroup](any(),any(),any())) thenReturn Future.successful(Some(AffinityGroup("Organisation",Some("admin"))))
      when(enrol.getCbcId(any())) thenReturn OptionT.none[Future,CBCId]
      val result = controller.start(fakeRequest)
      status(result) shouldBe Status.SEE_OTHER
      result.header.headers.getOrElse("location", "") shouldBe routes.SharedController.verifyKnownFactsOrganisation().url
    }

  }
  "The StartController returns the start page" when {
    "the user is an Organisation and is registered" in { // #2
      when(cache.readOption[AffinityGroup](any(),any(),any())) thenReturn Future.successful(Some(AffinityGroup("Organisation",Some("admin"))))
      when(enrol.getCBCEnrolment(any())) thenReturn OptionT.pure[Future,CBCEnrolment](CBCEnrolment(cbcId,utr))
      when(enrol.getCbcId(any())) thenReturn OptionT.pure[Future,CBCId](cbcId)
      val result = controller.start(fakeRequest)
      status(result) shouldBe Status.OK
    }
  }

  "The start page form" when {
    "submitted with 'check and send a country-by-country-report' selected, redirects to the submission journey" in {  // #3
      val result = controller.submit(fakePost.withFormUrlEncodedBody("choice" -> "upload"))
      status(result) shouldBe Status.SEE_OTHER
      result.header.headers.getOrElse("location", "") shouldBe routes.FileUploadController.chooseXMLFile().url
    }
    "submitted with 'Change registration contact details' selected, redirects to the change registration details journey" in { // #4
      val result = controller.submit(fakePost.withFormUrlEncodedBody("choice" -> "editSubscriberInfo"))
      status(result) shouldBe Status.SEE_OTHER
      result.header.headers.getOrElse("location", "") shouldBe routes.SubscriptionController.updateInfoSubscriber().url
    }
    "submitted with nothing selected, returns a 400" in { // #6
      val result = controller.submit(fakePost.withFormUrlEncodedBody("choice" -> ""))
      status(result) shouldBe Status.BAD_REQUEST
    }
  }

}
