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
import org.mockito.Matchers.{eq => EQ, _}
import org.mockito.Mockito._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mock.MockitoSugar
import org.scalatestplus.play.OneAppPerSuite
import play.api.http.Status
import play.api.i18n.MessagesApi
import play.api.libs.json.{JsValue, Json}
import play.api.test.FakeRequest
import uk.gov.hmrc.cbcrfrontend.connectors.{BPRKnownFactsConnector, EnrolmentsConnector}
import uk.gov.hmrc.cbcrfrontend.controllers.auth.{SecuredActionsTest, TestUsers}
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.services._
import uk.gov.hmrc.cbcrfrontend.typesclasses.{CbcrsUrl, ServiceUrl}
import uk.gov.hmrc.cbcrfrontend.util.CbcrSwitches
import uk.gov.hmrc.emailaddress.EmailAddress
import uk.gov.hmrc.http.cache.client.CacheMap
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}
import uk.gov.hmrc.play.http.HeaderCarrier
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.runtime.universe._

class SubscriptionControllerSpec extends UnitSpec with ScalaFutures with OneAppPerSuite with CSRFTest with MockitoSugar with FakeAuthConnector {

  implicit val ec = app.injector.instanceOf[ExecutionContext]
  implicit val messagesApi = app.injector.instanceOf[MessagesApi]
  implicit val authCon = authConnector(TestUsers.cbcrUser)
  val securedActions = new SecuredActionsTest(TestUsers.cbcrUser, authCon)
  val subService = mock[SubscriptionDataService]
  val auditMock = mock[AuditConnector]

  val dc = mock[BPRKnownFactsConnector]
  val cbcId = mock[CBCIdService]
  val cbcKF = mock[CBCKnownFactsService]
  val bprKF = mock[BPRKnownFactsService]
  val auth  = mock[EnrolmentsConnector]
  implicit val cache = mock[CBCSessionCache]
  when(cache.read[AffinityGroup](EQ(AffinityGroup.format),any(),any())) thenReturn Future.successful(Some(AffinityGroup("Organisation")))

  val controller = new SubscriptionController(securedActions, subService,dc,cbcId,cbcKF,auth,bprKF){
    override lazy val audit = auditMock
  }

  implicit val hc = HeaderCarrier()
  implicit val cbcrsUrl = new ServiceUrl[CbcrsUrl] { val url = "cbcr"}

  implicit val bprTag = implicitly[TypeTag[BusinessPartnerRecord]]
  implicit val utrTag = implicitly[TypeTag[Utr]]

  when(auditMock.sendEvent(any())(any(),any())) thenReturn Future.successful(AuditResult.Success)


  val cbcid = CBCId.create(1).getOrElse(fail("Could not generate cbcid"))

  val subscriptionDetails = SubscriptionDetails(
    BusinessPartnerRecord("SAFEID",Some(OrganisationResponse("blagh")),EtmpAddress(None,None,None,None,Some("TF3 XFE"),None)),
    SubscriberContact("name","phonenum",EmailAddress("test@test.com")),cbcid,Utr("7000000002")
  )

  "GET /contactInfoSubscriber" should {
    "return 200" in {
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(securedActions, subService,dc,cbcId,cbcKF,auth,bprKF){
        override lazy val audit = auditMock
      }
      val fakeRequestSubscribe = addToken(FakeRequest("GET", "/contactInfoSubscriber"))
      status(controller.contactInfoSubscriber(fakeRequestSubscribe)) shouldBe Status.OK
    }
  }

  "POST /submitSubscriptionData" should {
    "return 400 when the there is no data" in {
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(securedActions, subService,dc,cbcId,cbcKF,auth,bprKF){
        override lazy val audit = auditMock
      }
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/submitSubscriptionData"))
      status(controller.submitSubscriptionData(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }
    "return 400 when the name is missing" in {
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(securedActions, subService,dc,cbcId,cbcKF,auth,bprKF){
        override lazy val audit = auditMock
      }
      val data = Json.obj(
        "phoneNumber" -> "12345678",
        "email" -> "blagh@blagh.com"
      )
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/submitSubscriptionData").withJsonBody(data))
      status(controller.submitSubscriptionData(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }
    "return 400 when the email is missing" in {
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(securedActions, subService,dc,cbcId,cbcKF,auth,bprKF){
        override lazy val audit = auditMock
      }
      val data = Json.obj(
        "phoneNumber" -> "12345678",
        "name" -> "Dave"
      )
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/submitSubscriptionData").withJsonBody(data))
      status(controller.submitSubscriptionData(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }
    "return 400 when the email is invalid" in {
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(securedActions, subService,dc,cbcId,cbcKF,auth,bprKF){
        override lazy val audit = auditMock
      }
      val data = Json.obj(
        "phoneNumber" -> "12345678",
        "name" -> "Dave",
        "email" -> "THISISNOTANEMAIL"
      )
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/submitSubscriptionData").withJsonBody(data))
      status(controller.submitSubscriptionData(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }
    "return 400 when the phone number is missing" in {
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(securedActions, subService,dc,cbcId,cbcKF,auth,bprKF){
        override lazy val audit = auditMock
      }
      val data = Json.obj(
        "name" -> "Dave",
        "email" -> "blagh@blagh.com"
      )
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/submitSubscriptionData").withJsonBody(data))
      status(controller.submitSubscriptionData(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }

    "return 500 when the SubscriptionDataService errors" in {
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(securedActions, subService,dc,cbcId,cbcKF,auth,bprKF){
        override lazy val audit = auditMock
      }
      val sData = SubscriberContact("Dave","0207456789",EmailAddress("Bob@bob.com"))
      val fakeRequest = addToken(FakeRequest("POST", "/submitSubscriptionData").withJsonBody(Json.toJson(sData)))
      when(cbcId.getCbcId(anyObject())) thenReturn Future.successful(Some(cbcid))
      when(subService.saveSubscriptionData(any(classOf[SubscriptionDetails]))(anyObject(),anyObject())) thenReturn EitherT.left[Future,CBCErrors, String](Future.successful(UnexpectedState("return 500 when the SubscriptionDataService errors")))
      when(subService.clearSubscriptionData(any())(any(),any())) thenReturn EitherT.right[Future,CBCErrors, Option[String]](None)
      when(cache.read[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format),EQ(bprTag),any())) thenReturn Future.successful(Some(BusinessPartnerRecord("safeid",None,EtmpAddress(None,None,None,None,None,None))))
      when(cache.read[Utr](EQ(Utr.utrRead),EQ(utrTag),any())) thenReturn Future.successful(Some(Utr("700000002")))
      status(controller.submitSubscriptionData(fakeRequest)) shouldBe Status.INTERNAL_SERVER_ERROR
      verify(subService).clearSubscriptionData(any())(any(),any())
    }
    "return 500 when the getCbcId call errors out" in {
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(securedActions, subService,dc,cbcId,cbcKF,auth,bprKF){
        override lazy val audit = auditMock
      }
      val sData = SubscriberContact("Dave","0207456789",EmailAddress("Bob@bob.com"))
      val fakeRequest = addToken(FakeRequest("POST", "/submitSubscriptionData").withJsonBody(Json.toJson(sData)))
      when(cache.read[SubscriptionDetails](EQ(SubscriptionDetails.subscriptionDetailsFormat),any(),any())) thenReturn Future.successful(Some(subscriptionDetails))
      when(cbcId.getCbcId(anyObject())) thenReturn Future.successful(None)
      status(controller.submitSubscriptionData(fakeRequest)) shouldBe Status.INTERNAL_SERVER_ERROR
      verify(subService, times(0)).clearSubscriptionData(any())(any(),any())
    }
    "return 500 when the addKnownFactsToGG call errors" in {
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(securedActions, subService,dc,cbcId,cbcKF,auth,bprKF){
        override lazy val audit = auditMock
      }
      val sData = SubscriberContact("Dave","0207456789",EmailAddress("Bob@bob.com"))
      val fakeRequest = addToken(FakeRequest("POST", "/submitSubscriptionData").withJsonBody(Json.toJson(sData)))
      when(subService.saveSubscriptionData(any(classOf[SubscriptionDetails]))(anyObject(),anyObject())) thenReturn EitherT.left[Future,CBCErrors, String](Future.successful(UnexpectedState("oops")))
      when(cbcId.getCbcId(anyObject())) thenReturn Future.successful(CBCId("XGCBC0000000001"))
      when(cbcKF.addKnownFactsToGG(anyObject())(anyObject())) thenReturn EitherT.left[Future,CBCErrors, Unit](UnexpectedState("oops"))
      when(cache.read[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format),EQ(bprTag),any())) thenReturn Future.successful(Some(BusinessPartnerRecord("safeid",None,EtmpAddress(None,None,None,None,None,None))))
      when(cache.read[Utr](EQ(Utr.utrRead),EQ(utrTag),any())) thenReturn Future.successful(Some(Utr("123456789")))
      when(subService.clearSubscriptionData(any())(any(),any())) thenReturn EitherT.right[Future,CBCErrors, Option[String]](None)
      status(controller.submitSubscriptionData(fakeRequest)) shouldBe Status.INTERNAL_SERVER_ERROR
      verify(subService).clearSubscriptionData(any())(any(),any())
    }
    "return 303 (see_other) when all params are present and valid and the SubscriptionDataService returns Ok" in {
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(securedActions, subService,dc,cbcId,cbcKF,auth,bprKF){
        override lazy val audit = auditMock
      }
      val sData = SubscriberContact("Dave","0207456789",EmailAddress("Bob@bob.com"))
      val fakeRequest = addToken(FakeRequest("POST", "/submitSubscriptionData").withJsonBody(Json.toJson(sData)))
      when(subService.saveSubscriptionData(any(classOf[SubscriptionDetails]))(anyObject(),anyObject())) thenReturn EitherT.pure[Future,CBCErrors, String]("done")
      when(cbcId.getCbcId(anyObject())) thenReturn Future.successful(CBCId("XGCBC0000000001"))
      when(cbcKF.addKnownFactsToGG(anyObject())(anyObject())) thenReturn EitherT.pure[Future,CBCErrors, Unit](())
      when(cache.save[SubscriberContact](any())(any(),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      status(controller.submitSubscriptionData(fakeRequest)) shouldBe Status.SEE_OTHER
      verify(subService, times(0)).clearSubscriptionData(any())(any(),any())
    }
  }

  "GET /subscriberReconfirmEmail" should {
    "return 200" in {

      val fakeRequestReconfirmEmail = addToken(FakeRequest("GET", "/subscriberReconfirmEmail"))
      when(cache.read[SubscriberContact] (EQ(SubscriberContact.subscriptionFormat),any(),any())) thenReturn Future.successful(Some(SubscriberContact("name", "0123123123", EmailAddress("max@max.com"))))
      status(controller.reconfirmEmail(fakeRequestReconfirmEmail)) shouldBe Status.OK
    }
  }


  "POST /subscriberReconfirmEmailSubmit" should {
    "return 400 when the Email Address is empty" in {

      val reconfirmEmail = Json.obj("reconfirmEmail" -> "")
      val fakeRequestSubmit = addToken(FakeRequest("POST", "/subscriberReconfirmEmailSubmit").withJsonBody(Json.toJson(reconfirmEmail)))
      status(controller.reconfirmEmailSubmit(fakeRequestSubmit)) shouldBe Status.BAD_REQUEST
    }
    "return 400 when Email Address is in Invalid format" in {
      val reconfirmEmail = Json.obj("reconfirmEmail" -> "abc.xyz")

      val fakeRequestSubmit = addToken(FakeRequest("POST", "/subscriberReconfirmEmailSubmit").withJsonBody(Json.toJson(reconfirmEmail)))
      status(controller.reconfirmEmailSubmit(fakeRequestSubmit)) shouldBe Status.BAD_REQUEST
    }
    "return 303 when Email Address is valid" in {

      val reconfirmEmail = Json.obj("reconfirmEmail" -> "abc@xyz.com")
      val fakeRequestSubmit = addToken(FakeRequest("POST", "/subscriberReconfirmEmailSubmit").withJsonBody(Json.toJson(reconfirmEmail)))
      when(cache.read[Subscribed.type] (EQ(Implicits.format),any(),any())) thenReturn Future.successful(None)
      when(cache.read[SubscriberContact] (EQ(SubscriberContact.subscriptionFormat),any(),any())) thenReturn Future.successful(Some(SubscriberContact("name", "0123123123", EmailAddress("max@max.com"))))
      when(cache.read[CBCId] (EQ(CBCId.cbcIdFormat),any(),any())) thenReturn Future.successful(CBCId("XGCBC0000000001"))
      when(cache.save[SubscriberContact](any())(any(),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      when(cache.save[CBCId](any())(any(),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))


      status(controller.reconfirmEmailSubmit(fakeRequestSubmit)) shouldBe Status.SEE_OTHER
    }
    "return 500 when trying to re-submit" in {

      val reconfirmEmail = Json.obj("reconfirmEmail" -> "abc@xyz.com")
      val fakeRequestSubmit = addToken(FakeRequest("POST", "/subscriberReconfirmEmailSubmit").withJsonBody(Json.toJson(reconfirmEmail)))
      when(cache.read[Subscribed.type] (EQ(Implicits.format),any(),any())) thenReturn Future.successful(Some(Subscribed))
      status(controller.reconfirmEmailSubmit(fakeRequestSubmit)) shouldBe Status.INTERNAL_SERVER_ERROR
    }
  }


  "DELETE to clear-subscription-data/utr" should {
    "work correctly when enabled and" when {
      System.setProperty(CbcrSwitches.clearSubscriptionDataRoute.name, "true")
      "return a 200 if data was successfully cleared" in {
        val fakeRequestSubscribe = addToken(FakeRequest("DELETE", "/clear-subscription-data"))
        val u: Utr = Utr("7000000002")
        when(subService.clearSubscriptionData(any())(any(), any())) thenReturn EitherT.pure[Future, CBCErrors,Option[String]](Some("done"))
        status(controller.clearSubscriptionData(u)(fakeRequestSubscribe)) shouldBe Status.OK
      }
      "return a 204 if data was no data to clear" in {
        val fakeRequestSubscribe = addToken(FakeRequest("DELETE", "/clear-subscription-data"))
        val u: Utr = Utr("7000000002")
        when(subService.clearSubscriptionData(any())(any(), any())) thenReturn EitherT.pure[Future, CBCErrors,Option[String]](None)
        status(controller.clearSubscriptionData(u)(fakeRequestSubscribe)) shouldBe Status.NO_CONTENT
      }
      "return a 500 if something goes wrong" in {
        val fakeRequestSubscribe = addToken(FakeRequest("DELETE", "/clear-subscription-data"))
        val u: Utr = Utr("7000000002")
        when(subService.clearSubscriptionData(any())(any(), any())) thenReturn EitherT.left[Future, CBCErrors,Option[String]](UnexpectedState("oops"))
        status(controller.clearSubscriptionData(u)(fakeRequestSubscribe)) shouldBe Status.INTERNAL_SERVER_ERROR
      }
    }

    "return 501 when feature-disabled" in {
      System.setProperty(CbcrSwitches.clearSubscriptionDataRoute.name, "false")
      val fakeRequestSubscribe = addToken(FakeRequest("DELETE", "/clear-subscription-data"))
      val u: Utr = Utr("7000000002")
      status(controller.clearSubscriptionData(u)(fakeRequestSubscribe)) shouldBe Status.NOT_IMPLEMENTED

    }

  }

}
