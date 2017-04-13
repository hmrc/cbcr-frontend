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

import cats.data.OptionT
import org.mockito.Mockito._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mock.MockitoSugar
import org.scalatestplus.play.OneAppPerSuite
import play.api.http.Status
import play.api.i18n.MessagesApi
import play.api.libs.json.Json
import play.api.test.FakeRequest
import uk.gov.hmrc.cbcrfrontend.connectors.DESConnector
import uk.gov.hmrc.cbcrfrontend.controllers.auth.{SecuredActionsTest, TestUsers}
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.services.KnownFactsCheckService
import uk.gov.hmrc.play.http.HttpResponse
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.{ExecutionContext, Future}

class SubscriptionSpec extends UnitSpec with ScalaFutures with OneAppPerSuite with CSRFTest with MockitoSugar with FakeAuthConnector{

  val dc = mock[DESConnector]

  implicit val ec = app.injector.instanceOf[ExecutionContext]
  implicit val messagesApi = app.injector.instanceOf[MessagesApi]


  val controller = subscriptionController

  "GET /subscribeFirst" should {
    "return 200" in {
      val fakeRequestSubscribe = addToken(FakeRequest("GET", "/subscribeFirst"))
      status(controller.subscribeFirst(fakeRequestSubscribe)) shouldBe Status.OK
    }
  }
  "GET /contactInfoSubscriber" should {
    "return 200" in {
      val fakeRequestSubscribe = addToken(FakeRequest("GET", "/contactInfoSubscriber"))
      status(controller.contactInfoSubscriber(fakeRequestSubscribe)) shouldBe Status.OK
    }
  }
  "GET /subscribeSuccessCbcId" should {
    "return 200" in {
      val fakeRequestSubscribe = addToken(FakeRequest("GET", "/subscribeSuccessCbcId"))
      status(controller.subscribeSuccessCbcId(fakeRequestSubscribe)) shouldBe Status.OK
    }
  }
  "POST /checkKnownFacts" should {
    "return 400 when KnownFacts are missing" in {
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/checkKnownFacts"))
      status(controller.checkKnownFacts(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }
    "return 400 when the postcode is invalid" in {
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/checkKnownFacts").withJsonBody(Json.toJson(KnownFacts(Utr("1234567890"),"NOTAPOSTCODE"))))
      status(controller.checkKnownFacts(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }
    "return 400 when the utr is invalid" in {
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/checkKnownFacts").withJsonBody(Json.toJson(KnownFacts(Utr("IAMNOTAUTR"),"SW4 6NR"))))
      status(controller.checkKnownFacts(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }
    "return 404 when the utr and postcode are valid but the postcode doesn't match" in {
      val kf = KnownFacts(Utr("7000000002"),"SW46NR")
      val response = FindBusinessDataResponse(false,None,None,"safeid",EtmpAddress(None,None,None,None,Some("SW46NS"),None))
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/checkKnownFacts").withJsonBody(Json.toJson(kf)))
      when(dc.lookup(kf.utr.value)) thenReturn Future.successful(HttpResponse(Status.OK,Some(Json.toJson(response))))
      status(controller.checkKnownFacts(fakeRequestSubscribe)) shouldBe Status.NOT_FOUND
    }
    "return 200 when the utr and postcode are valid" in {
      val kf = KnownFacts(Utr("7000000002"),"SW46NR")
      val response = FindBusinessDataResponse(false,None,None,"safeid",EtmpAddress(None,None,None,None,Some("SW46NR"),None), Some(OrganisationResponse("FooCorp", None, None)))
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/checkKnownFacts").withJsonBody(Json.toJson(kf)))
      when(dc.lookup(kf.utr.value)) thenReturn Future.successful(HttpResponse(Status.OK,Some(Json.toJson(response))))
      status(controller.checkKnownFacts(fakeRequestSubscribe)) shouldBe Status.OK
    }
  }


  def subscriptionController(implicit messagesApi: MessagesApi) = {

    val authCon = authConnector(TestUsers.cbcrUser)
    val securedActions = new SecuredActionsTest(TestUsers.cbcrUser, authCon)

    new Subscription(securedActions, dc) {}
  }

}
