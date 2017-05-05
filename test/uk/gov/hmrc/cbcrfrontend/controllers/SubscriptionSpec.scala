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

import cats.data.{EitherT, OptionT}
import org.mockito.Mockito._
import org.mockito.Matchers._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mock.MockitoSugar
import org.scalatestplus.play.OneAppPerSuite
import play.api.http.Status
import play.api.i18n.MessagesApi
import play.api.libs.json.Json
import play.api.test.FakeRequest
import uk.gov.hmrc.cbcrfrontend.controllers.auth.{SecuredActionsTest, TestUsers}
import uk.gov.hmrc.cbcrfrontend.exceptions.UnexpectedState
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.services.{CBCIdService, CBCKnownFactsService, SubscriptionDataService}
import uk.gov.hmrc.cbcrfrontend.typesclasses.{CbcrsUrl, ServiceUrl}
import uk.gov.hmrc.emailaddress.EmailAddress
import uk.gov.hmrc.play.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.play.test.{UnitSpec, WithFakeApplication}
import cats.instances.future._
import org.mockito.Matchers
import uk.gov.hmrc.cbcrfrontend.connectors.BPRKnownFactsConnector

import scala.concurrent.{ExecutionContext, Future}

class SubscriptionSpec extends UnitSpec with ScalaFutures with OneAppPerSuite with CSRFTest with MockitoSugar with FakeAuthConnector {

  implicit val ec = app.injector.instanceOf[ExecutionContext]
  implicit val messagesApi = app.injector.instanceOf[MessagesApi]
  val authCon = authConnector(TestUsers.cbcrUser)
  val securedActions = new SecuredActionsTest(TestUsers.cbcrUser, authCon)
  val subService = mock[SubscriptionDataService]

  val dc = mock[BPRKnownFactsConnector]
  val cbcId = mock[CBCIdService]
  val cbcKF = mock[CBCKnownFactsService]

  val controller = new Subscription(securedActions, subService,dc,cbcId,cbcKF)

  implicit val hc = HeaderCarrier()
  implicit val cbcrsUrl = new ServiceUrl[CbcrsUrl] { val url = "cbcr"}

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

  "POST /checkKnownFacts" should {
    "return 400 when KnownFacts are missing" in {
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/checkKnownFacts"))
      status(controller.checkKnownFacts(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }
    "return 400 when the postcode is invalid" in {
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/checkKnownFacts").withJsonBody(Json.toJson(BPRKnownFacts(Utr("1234567890"), "NOTAPOSTCODE"))))
      status(controller.checkKnownFacts(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }
    "return 400 when the utr is invalid" in {
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/checkKnownFacts").withJsonBody(Json.toJson(BPRKnownFacts(Utr("IAMNOTAUTR"), "SW4 6NR"))))
      status(controller.checkKnownFacts(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }
    "return 404 when the utr and postcode are valid but the postcode doesn't match" in {
      val kf = BPRKnownFacts(Utr("7000000002"), "SW46NR")
      val response = BusinessPartnerRecord(Some("safeid"), Some(OrganisationResponse("My Corp")), EtmpAddress(None, None, None, None, Some("SW46NS"), None))
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/checkKnownFacts").withJsonBody(Json.toJson(kf)))
      when(dc.lookup(anyObject[String])(anyObject[HeaderCarrier])) thenReturn Future.successful(HttpResponse(Status.OK, Some(Json.toJson(response))))
      status(controller.checkKnownFacts(fakeRequestSubscribe)) shouldBe Status.NOT_FOUND
    }
    "return 200 when the utr and postcode are valid" in {
      val kf = BPRKnownFacts(Utr("7000000002"), "SW46NR")
      val response = BusinessPartnerRecord(Some("safeid"), Some(OrganisationResponse("My Corp")), EtmpAddress(None, None, None, None, Some("SW46NR"), None))
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/checkKnownFacts").withJsonBody(Json.toJson(kf)))
      when(dc.lookup(anyObject[String])(anyObject[HeaderCarrier])) thenReturn Future.successful(HttpResponse(Status.OK, Some(Json.toJson(response))))
      status(controller.checkKnownFacts(fakeRequestSubscribe)) shouldBe Status.OK
    }
  }

  "POST /submitSubscriptionData" should {
    "return 400 when the there is no data" in {
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/submitSubscriptionData"))
      status(controller.submitSubscriptionData(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }
    "return 400 when the name is missing" in {
      val data = Json.obj(
        "phoneNumber" -> "12345678",
        "email" -> "blagh@blagh.com"
      )
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/submitSubscriptionData").withJsonBody(data))
      status(controller.submitSubscriptionData(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }
    "return 400 when the email is missing" in {
      val data = Json.obj(
        "phoneNumber" -> "12345678",
        "name" -> "Dave"
      )
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/submitSubscriptionData").withJsonBody(data))
      status(controller.submitSubscriptionData(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }
    "return 400 when the email is invalid" in {
      val data = Json.obj(
        "phoneNumber" -> "12345678",
        "name" -> "Dave",
        "email" -> "THISISNOTANEMAIL"
      )
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/submitSubscriptionData").withJsonBody(data))
      status(controller.submitSubscriptionData(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }
    "return 400 when the phone number is missing" in {
      val data = Json.obj(
        "name" -> "Dave",
        "email" -> "blagh@blagh.com"
      )
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/submitSubscriptionData").withJsonBody(data))
      status(controller.submitSubscriptionData(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }

    "return 500 when the SubscriptionDataService errors" in {
      val sData = SubscriberContact("Dave","0207456789",EmailAddress("Bob@bob.com"))
      val fakeRequest = addToken(FakeRequest("POST", "/submitSubscriptionData").withJsonBody(Json.toJson(sData)))
      when(subService.saveSubscriptionData(any(classOf[SubscriptionDetails]))(anyObject(),anyObject())) thenReturn EitherT.left[Future,UnexpectedState,String](Future.successful(UnexpectedState("oops")))
      when(cbcId.getCbcId(anyObject())) thenReturn Future.successful(None)
      status(controller.submitSubscriptionData(fakeRequest)) shouldBe Status.INTERNAL_SERVER_ERROR
    }
    "return 500 when the getCbcId call errors out" in {
      val sData = SubscriberContact("Dave","0207456789",EmailAddress("Bob@bob.com"))
      val fakeRequest = addToken(FakeRequest("POST", "/submitSubscriptionData").withJsonBody(Json.toJson(sData)))
      when(subService.saveSubscriptionData(any(classOf[SubscriptionDetails]))(anyObject(),anyObject())) thenReturn EitherT.left[Future,UnexpectedState,String](Future.successful(UnexpectedState("oops")))
      when(cbcId.getCbcId(anyObject())) thenReturn Future.successful(None)
      status(controller.submitSubscriptionData(fakeRequest)) shouldBe Status.INTERNAL_SERVER_ERROR
    }
    "return 500 when the addKnownFactsToGG call errors" in {
      val sData = SubscriberContact("Dave","0207456789",EmailAddress("Bob@bob.com"))
      val fakeRequest = addToken(FakeRequest("POST", "/submitSubscriptionData").withJsonBody(Json.toJson(sData)))
      when(subService.saveSubscriptionData(any(classOf[SubscriptionDetails]))(anyObject(),anyObject())) thenReturn EitherT.left[Future,UnexpectedState,String](Future.successful(UnexpectedState("oops")))
      when(cbcId.getCbcId(anyObject())) thenReturn Future.successful(CBCId("XGCBC0000000001"))
      when(cbcKF.addKnownFactsToGG(anyObject())(anyObject())) thenReturn EitherT.left[Future,UnexpectedState,Unit](UnexpectedState("oops"))
      status(controller.submitSubscriptionData(fakeRequest)) shouldBe Status.INTERNAL_SERVER_ERROR
    }
    "return 303 (see_other) when all params are present and valid and the SubscriptionDataService returns Ok" in {
      val sData = SubscriberContact("Dave","0207456789",EmailAddress("Bob@bob.com"))
      val fakeRequest = addToken(FakeRequest("POST", "/submitSubscriptionData").withJsonBody(Json.toJson(sData)))
      when(subService.saveSubscriptionData(any(classOf[SubscriptionDetails]))(anyObject(),anyObject())) thenReturn EitherT.pure[Future,UnexpectedState,String]("done")
      when(cbcId.getCbcId(anyObject())) thenReturn Future.successful(CBCId("XGCBC0000000001"))
      when(cbcKF.addKnownFactsToGG(anyObject())(anyObject())) thenReturn EitherT.pure[Future,UnexpectedState,Unit](())
      status(controller.submitSubscriptionData(fakeRequest)) shouldBe Status.SEE_OTHER
    }
  }
}
