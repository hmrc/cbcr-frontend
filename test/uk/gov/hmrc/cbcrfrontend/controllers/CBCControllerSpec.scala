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

package uk.gov.hmrc.cbcrfrontend.controllers

import cats.data.EitherT
import cats.instances.future._
import org.scalatest.concurrent.ScalaFutures
import org.scalatestplus.play.OneAppPerSuite
import play.api.http.Status
import play.api.i18n.MessagesApi
import play.api.test.FakeRequest
import uk.gov.hmrc.cbcrfrontend.controllers.auth.{SecuredActionsTest, TestUsers}
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.{Await, ExecutionContext, Future}
import org.mockito.Mockito._
import org.mockito.Matchers._
import org.scalatest.mock.MockitoSugar
import play.api.libs.json.{JsValue, Json}
import uk.gov.hmrc.cbcrfrontend.connectors.EnrolmentsConnector
import uk.gov.hmrc.cbcrfrontend.exceptions.{CBCErrors, UnexpectedState}
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.services.{CBCSessionCache, SubscriptionDataService}
import uk.gov.hmrc.emailaddress.EmailAddress
import uk.gov.hmrc.http.cache.client.CacheMap
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.duration._


class CBCControllerSpec extends UnitSpec with ScalaFutures with OneAppPerSuite with CSRFTest with FakeAuthConnector with MockitoSugar{

  implicit val ec = app.injector.instanceOf[ExecutionContext]
  implicit val messagesApi = app.injector.instanceOf[MessagesApi]

  implicit val authCon = authConnector(TestUsers.cbcrUser)
  val securedActions = new SecuredActionsTest(TestUsers.cbcrUser, authCon)
  implicit val cache = mock[CBCSessionCache]
  implicit val enrol = mock[EnrolmentsConnector]
  val subDataS = mock[SubscriptionDataService]
  when(cache.read[AffinityGroup](any(),any(),any())) thenReturn Future.successful(Some(AffinityGroup("Organisation")))
  when(cache.save[Utr](any())(any(),any(),any())) thenReturn Future.successful(CacheMap("id",Map.empty[String,JsValue]))
  when(enrol.getEnrolments(any())) thenReturn Future.successful(List.empty)

  val controller = new CBCController(securedActions, subDataS, enrol)

  val id: CBCId = CBCId("XGCBC0000000001").getOrElse(fail("unable to create cbcid"))
  val subDetails = SubscriptionDetails(
    BusinessPartnerRecord("safeid",None,EtmpAddress(None,None,None,None,None,None)),
    SubscriberContact("lkajsdf","lkasjdf",EmailAddress("max@max.com")),
    id,
    Utr("utr")
  )

  implicit val hc = HeaderCarrier()

  val fakeRequestEnterCBCId = addToken(FakeRequest("GET", "/enter-CBCId"))

  "GET /enter-CBCId" should {
    "return 200" in {
      val result = Await.result(controller.enterCBCId(fakeRequestEnterCBCId), 5.second)
      status(result) shouldBe Status.OK
    }
  }

  val fakeRequestSubmitCBCId = addToken(FakeRequest("POST", "/enter-CBCId"))

  "GET /submitCBCId" should {
    "return 403 if it was not a valid CBCId" in {
      val result = controller.submitCBCId(fakeRequestEnterCBCId.withJsonBody(Json.obj("cbcId" -> "NOTAVALIDID")))
      status(result) shouldBe Status.BAD_REQUEST
    }
    "return 403 if the CBCId has not been registered" in {
      when(subDataS.retrieveSubscriptionData(any())(any(),any())) thenReturn EitherT.right[Future,CBCErrors, Option[SubscriptionDetails]](None)
      val result = controller.submitCBCId(fakeRequestEnterCBCId.withJsonBody(Json.obj("cbcId" -> id.toString)))
      status(result) shouldBe Status.BAD_REQUEST
    }
    "return 403 if the CBCId has been registered but doesnt match the CBCId in the bearer token" in {
      when(subDataS.retrieveSubscriptionData(any())(any(),any())) thenReturn EitherT.right[Future,CBCErrors, Option[SubscriptionDetails]](Some(subDetails))
      when(enrol.getEnrolments(any())) thenReturn Future.successful(List(Enrolment("HMRC-CBC-ORG",List(Identifier("cbcId","XLCBC0000000006"),Identifier("utr","7000000002")))))
      val result = controller.submitCBCId(fakeRequestEnterCBCId.withJsonBody(Json.obj("cbcId" -> id.toString)))
      status(result) shouldBe Status.BAD_REQUEST
    }
    "return a redirect if successful" in {
      when(subDataS.retrieveSubscriptionData(any())(any(),any())) thenReturn EitherT.right[Future,CBCErrors, Option[SubscriptionDetails]](Some(subDetails))
      when(enrol.getEnrolments(any())) thenReturn Future.successful(List(Enrolment("HMRC-CBC-ORG",List(Identifier("cbcId",id.value),Identifier("utr","7000000002")))))
      val result = controller.submitCBCId(fakeRequestEnterCBCId.withJsonBody(Json.obj("cbcId" -> id.toString)))
      status(result) shouldBe Status.SEE_OTHER
    }
  }

  val fakeRequestSignOut = addToken(FakeRequest("GET", "/signOut"))

  "GET /signOut" should {
    "return 303" in {
      val result = Await.result(controller.signOut(fakeRequestSignOut), 5.second)
      status(result) shouldBe Status.SEE_OTHER
    }
  }


}