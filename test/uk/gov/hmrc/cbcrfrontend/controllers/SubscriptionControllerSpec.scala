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

import java.time.LocalDateTime

import cats.data.{EitherT, OptionT}
import cats.instances.future._
import org.mockito.Matchers.{eq => EQ, _}
import org.mockito.Mockito._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mock.MockitoSugar
import org.scalatestplus.play.OneAppPerSuite
import play.api.http.Status
import play.api.i18n.MessagesApi
import play.api.libs.json.{JsValue, Json}
import play.api.mvc.{AnyContent, Request}
import play.api.test.FakeRequest
import uk.gov.hmrc.cbcrfrontend.connectors.{BPRKnownFactsConnector, EnrolmentsConnector}
import uk.gov.hmrc.cbcrfrontend.controllers.auth.{SecuredActionsTest, TestUsers}
import uk.gov.hmrc.cbcrfrontend.model.{SubscriptionEmailSent, _}
import uk.gov.hmrc.cbcrfrontend.services._
import uk.gov.hmrc.cbcrfrontend.typesclasses.{CbcrsUrl, ServiceUrl}
import uk.gov.hmrc.cbcrfrontend.util.CbcrSwitches
import uk.gov.hmrc.emailaddress.EmailAddress
import uk.gov.hmrc.http.cache.client.CacheMap
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}
import uk.gov.hmrc.play.http.HeaderCarrier
import uk.gov.hmrc.play.test.UnitSpec
import uk.gov.hmrc.cbcrfrontend.model.SubscriptionEmailSent.SubscriptionEmailSentFormat
import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.runtime.universe._

class SubscriptionControllerSpec extends UnitSpec with ScalaFutures with OneAppPerSuite with CSRFTest with MockitoSugar with FakeAuthConnector with BeforeAndAfterEach {

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
  val enrollments  = mock[EnrolmentsConnector]
  implicit val cache = mock[CBCSessionCache]

  override protected def afterEach(): Unit = {
    reset(cache,subService,auditMock,dc,cbcId,bprKF,enrollments,cache)
    super.afterEach()
  }
  when(cache.read[AffinityGroup](EQ(AffinityGroup.format),any(),any())) thenReturn Future.successful(Some(AffinityGroup("Organisation")))

  val controller = new SubscriptionController(securedActions, subService,dc,cbcId,cbcKF,enrollments,bprKF){
    override lazy val audit = auditMock
  }

  implicit val hc = HeaderCarrier()
  implicit val cbcrsUrl = new ServiceUrl[CbcrsUrl] { val url = "cbcr"}

  implicit val bprTag = implicitly[TypeTag[BusinessPartnerRecord]]
  implicit val utrTag = implicitly[TypeTag[Utr]]



  val cbcid = CBCId.create(1).toOption

  val subscriptionDetails = SubscriptionDetails(
    BusinessPartnerRecord("SAFEID",Some(OrganisationResponse("blagh")),EtmpAddress("Line1",None,None,None,Some("TF3 XFE"),"GB")),
    SubscriberContact("Jimbo","Jones","phonenum",EmailAddress("test@test.com")),cbcid,Utr("7000000002")
  )

  val etmpSubscription = ETMPSubscription(
    "safeid",
    ContactName("Firstname","SecondName"),
    ContactDetails(EmailAddress("test@test.com"),"123456789"),
    EtmpAddress("Line1",None,None,None,None,"GB")
  )

  "GET /contactInfoSubscriber" should {
    "return 200" in {
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(securedActions, subService,dc,cbcId,cbcKF,enrollments,bprKF){
        override lazy val audit = auditMock
      }
      val fakeRequestSubscribe = addToken(FakeRequest("GET", "/contactInfoSubscriber"))
      status(controller.contactInfoSubscriber(fakeRequestSubscribe)) shouldBe Status.OK
    }
  }

  "POST /submitSubscriptionData" should {
    "return 400 when the there is no data" in {
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(securedActions, subService,dc,cbcId,cbcKF,enrollments,bprKF){
        override lazy val audit = auditMock
      }
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/submitSubscriptionData"))
      status(controller.submitSubscriptionData(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }
    "return 400 when either first or last name or both are missing" in {
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(securedActions, subService,dc,cbcId,cbcKF,enrollments,bprKF){
        override lazy val audit = auditMock
      }
      val data = Json.obj(
        "phoneNumber" -> "12345678",
        "email" -> "blagh@blagh.com"
      )
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/submitSubscriptionData").withJsonBody(data))
      status(controller.submitSubscriptionData(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST

      val data2 = Json.obj(
        "phoneNumber" -> "12345678",
        "email" -> "blagh@blagh.com",
        "firstName" -> "Bob"
      )
      val fakeRequestSubscribe2 = addToken(FakeRequest("POST", "/submitSubscriptionData").withJsonBody(data2))
      status(controller.submitSubscriptionData(fakeRequestSubscribe2)) shouldBe Status.BAD_REQUEST

      val data3 = Json.obj(
        "phoneNumber" -> "12345678",
        "email" -> "blagh@blagh.com",
        "lastName" -> "Jones"
      )
      val fakeRequestSubscribe3 = addToken(FakeRequest("POST", "/submitSubscriptionData").withJsonBody(data3))
      status(controller.submitSubscriptionData(fakeRequestSubscribe3)) shouldBe Status.BAD_REQUEST
    }
    "return 400 when the email is missing" in {
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(securedActions, subService,dc,cbcId,cbcKF,enrollments,bprKF){
        override lazy val audit = auditMock
      }
      val data = Json.obj(
        "phoneNumber" -> "12345678",
        "firstName" -> "Dave",
        "lastName" -> "Jones"
      )
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/submitSubscriptionData").withJsonBody(data))
      status(controller.submitSubscriptionData(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }
    "return 400 when the email is invalid" in {
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(securedActions, subService,dc,cbcId,cbcKF,enrollments,bprKF){
        override lazy val audit = auditMock
      }
      val data = Json.obj(
        "phoneNumber" -> "12345678",
        "firstName" -> "Dave",
        "lastName" -> "Jones",
        "email" -> "THISISNOTANEMAIL"
      )
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/submitSubscriptionData").withJsonBody(data))
      status(controller.submitSubscriptionData(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }
    "return 400 when the phone number is missing" in {
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(securedActions, subService,dc,cbcId,cbcKF,enrollments,bprKF){
        override lazy val audit = auditMock
      }
      val data = Json.obj(
        "firstName" -> "Dave",
        "lastName" -> "Jones",
        "email" -> "blagh@blagh.com"
      )
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/submitSubscriptionData").withJsonBody(data))
      status(controller.submitSubscriptionData(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }

    "return 400 when the phone number is invalid" in {
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(securedActions, subService,dc,cbcId,cbcKF,enrollments,bprKF){
        override lazy val audit = auditMock
      }
      val data = Json.obj(
        "phoneNumber" -> "I'm not a phone number",
        "firstName" -> "Dave",
        "lastName" -> "Jones",
        "email" -> "blagh@blagh.com"
      )
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/submitSubscriptionData").withJsonBody(data))
      status(controller.submitSubscriptionData(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }

    "return 500 when the SubscriptionDataService errors" in {
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(securedActions, subService,dc,cbcId,cbcKF,enrollments,bprKF){
        override lazy val audit = auditMock
      }
      val sData = SubscriberContact("Dave","Smith", "0207456789",EmailAddress("Bob@bob.com"))
      val fakeRequest = addToken(FakeRequest("POST", "/submitSubscriptionData").withJsonBody(Json.toJson(sData)))
      when(cbcId.subscribe(anyObject())(any())) thenReturn OptionT[Future,CBCId](Future.successful(cbcid))
      when(subService.saveSubscriptionData(any(classOf[SubscriptionDetails]))(anyObject(),anyObject())) thenReturn EitherT.left[Future,CBCErrors, String](Future.successful(UnexpectedState("return 500 when the SubscriptionDataService errors")))
      when(subService.clearSubscriptionData(any())(any(),any())) thenReturn EitherT.right[Future,CBCErrors, Option[String]](None)
      when(cache.read[Subscribed.type](EQ(Implicits.format),any(),any())) thenReturn Future.successful(None)
      when(cache.read[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format),EQ(bprTag),any())) thenReturn Future.successful(Some(BusinessPartnerRecord("safeid",None,EtmpAddress("Line1",None,None,None,None,"GB"))))
      when(cache.read[Utr](EQ(Utr.utrRead),EQ(utrTag),any())) thenReturn Future.successful(Some(Utr("700000002")))
      status(controller.submitSubscriptionData(fakeRequest)) shouldBe Status.INTERNAL_SERVER_ERROR
      verify(subService).clearSubscriptionData(any())(any(),any())
    }
    "return 500 when the getCbcId call errors out" in {
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(securedActions, subService,dc,cbcId,cbcKF,enrollments,bprKF){
        override lazy val audit = auditMock
      }
      val sData = SubscriberContact("Dave","Smith","0207456789",EmailAddress("Bob@bob.com"))
      val fakeRequest = addToken(FakeRequest("POST", "/submitSubscriptionData").withJsonBody(Json.toJson(sData)))
      when(cache.read[SubscriptionDetails](EQ(SubscriptionDetails.subscriptionDetailsFormat),any(),any())) thenReturn Future.successful(Some(subscriptionDetails))
      when(cbcId.subscribe(anyObject())(any())) thenReturn OptionT[Future,CBCId](Future.successful(None))
      when(cache.read[Subscribed.type](EQ(Implicits.format),any(),any())) thenReturn Future.successful(None)
      when(cache.read[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format),EQ(bprTag),any())) thenReturn Future.successful(Some(BusinessPartnerRecord("safeid",None,EtmpAddress("Line1",None,None,None,None,"GB"))))
      when(cache.read[Utr](EQ(Utr.utrRead),EQ(utrTag),any())) thenReturn Future.successful(Some(Utr("700000002")))
      status(controller.submitSubscriptionData(fakeRequest)) shouldBe Status.INTERNAL_SERVER_ERROR
      verify(subService, times(0)).clearSubscriptionData(any())(any(),any())
    }
    "return 500 when the addKnownFactsToGG call errors" in {
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(securedActions, subService,dc,cbcId,cbcKF,enrollments,bprKF){
        override lazy val audit = auditMock
      }
      val sData = SubscriberContact("Dave","Smith","0207456789",EmailAddress("Bob@bob.com"))
      val fakeRequest = addToken(FakeRequest("POST", "/submitSubscriptionData").withJsonBody(Json.toJson(sData)))
      when(subService.saveSubscriptionData(any(classOf[SubscriptionDetails]))(anyObject(),anyObject())) thenReturn EitherT.left[Future,CBCErrors, String](Future.successful(UnexpectedState("oops")))
      when(cbcId.subscribe(anyObject())(any())) thenReturn OptionT(Future.successful(CBCId("XGCBC0000000001")))
      when(cbcKF.addKnownFactsToGG(anyObject())(anyObject())) thenReturn EitherT.left[Future,CBCErrors, Unit](UnexpectedState("oops"))
      when(cache.read[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format),EQ(bprTag),any())) thenReturn Future.successful(Some(BusinessPartnerRecord("safeid",None,EtmpAddress("Line1",None,None,None,None,"GB"))))
      when(cache.read[Subscribed.type](EQ(Implicits.format),any(),any())) thenReturn Future.successful(None)
      when(cache.read[Utr](EQ(Utr.utrRead),EQ(utrTag),any())) thenReturn Future.successful(Some(Utr("123456789")))
      when(subService.clearSubscriptionData(any())(any(),any())) thenReturn EitherT.right[Future,CBCErrors, Option[String]](None)
      status(controller.submitSubscriptionData(fakeRequest)) shouldBe Status.INTERNAL_SERVER_ERROR
      verify(subService).clearSubscriptionData(any())(any(),any())
    }
    "return 303 (see_other) when all params are present and valid and the SubscriptionDataService returns Ok" in {
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(securedActions, subService,dc,cbcId,cbcKF,enrollments,bprKF){
        override lazy val audit = auditMock
      }
      val sData = SubscriberContact("Dave","Smith","0207456789",EmailAddress("Bob@bob.com"))
      val fakeRequest = addToken(FakeRequest("POST", "/submitSubscriptionData").withJsonBody(Json.toJson(sData)))
      when(subService.saveSubscriptionData(any(classOf[SubscriptionDetails]))(anyObject(),anyObject())) thenReturn EitherT.pure[Future,CBCErrors, String]("done")
      when(cbcId.subscribe(anyObject())(any())) thenReturn OptionT(Future.successful(CBCId("XGCBC0000000001")))
      when(cbcKF.addKnownFactsToGG(anyObject())(anyObject())) thenReturn EitherT.pure[Future,CBCErrors, Unit](())
      when(cache.read[Subscribed.type](EQ(Implicits.format),any(),any())) thenReturn Future.successful(None)
      when(cache.save[SubscriberContact](any())(any(),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      when(cache.read[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format),EQ(bprTag),any())) thenReturn Future.successful(Some(BusinessPartnerRecord("safeid",None,EtmpAddress("Line1",None,None,None,None,"GB"))))
      when(cache.read[Utr](EQ(Utr.utrRead),EQ(utrTag),any())) thenReturn Future.successful(Some(Utr("700000002")))
      status(controller.submitSubscriptionData(fakeRequest)) shouldBe Status.SEE_OTHER
      verify(subService, times(0)).clearSubscriptionData(any())(any(),any())
    }
  }

  "GET /subscriberReconfirmEmail" should {
    "return 200" in {

      val fakeRequestReconfirmEmail = addToken(FakeRequest("GET", "/subscriberReconfirmEmail"))
      when(cache.read[SubscriberContact] (EQ(SubscriberContact.subscriptionFormat),any(),any())) thenReturn Future.successful(Some(SubscriberContact("firstName", "lastname","0123123123", EmailAddress("max@max.com"))))
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
      when(cache.read[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format),EQ(bprTag),any())) thenReturn Future.successful(Some(BusinessPartnerRecord("safeid",None,EtmpAddress("Line1",None,None,None,None,"GB"))))
      when(cache.read[Utr](EQ(Utr.utrRead),EQ(utrTag),any())) thenReturn Future.successful(Some(Utr("700000002")))
      when(cache.read[SubscriberContact] (EQ(SubscriberContact.subscriptionFormat),any(),any())) thenReturn Future.successful(Some(SubscriberContact("firstName", "lastname", "0123123123", EmailAddress("max@max.com"))))
      when(cache.read[CBCId] (EQ(CBCId.cbcIdFormat),any(),any())) thenReturn Future.successful(CBCId("XGCBC0000000001"))
      when(cache.save[SubscriberContact](any())(any(),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      when(cache.save[CBCId](any())(any(),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      when(cbcId.email(any())(any())) thenReturn  OptionT.pure[Future,Boolean](true)
      when(cache.read[SubscriptionDetails](EQ(SubscriptionDetails.subscriptionDetailsFormat),any(),any())) thenReturn Future.successful(Some(subscriptionDetails))
      when(cache.read[SubscriptionEmailSent.type] (EQ(SubscriptionEmailSent.SubscriptionEmailSentFormat),any(),any())) thenReturn Future.successful(None)
      when(auditMock.sendEvent(any())(any(),any())) thenReturn Future.successful(AuditResult.Success)
      status(controller.reconfirmEmailSubmit(fakeRequestSubmit)) shouldBe Status.SEE_OTHER
    }
    "send an email in and write SubscriptionEmailSent to the cache in" in {

      val reconfirmEmail = Json.obj("reconfirmEmail" -> "abc@xyz.com")
      val fakeRequestSubmit = addToken(FakeRequest("POST", "/subscriberReconfirmEmailSubmit").withJsonBody(Json.toJson(reconfirmEmail)))
      when(cache.read[Subscribed.type] (EQ(Implicits.format),any(),any())) thenReturn Future.successful(None)
      when(cache.read[SubscriberContact] (EQ(SubscriberContact.subscriptionFormat),any(),any())) thenReturn Future.successful(Some(SubscriberContact("firstName", "lastname", "0123123123", EmailAddress("max@max.com"))))
      when(cache.read[CBCId] (EQ(CBCId.cbcIdFormat),any(),any())) thenReturn Future.successful(CBCId("XGCBC0000000001"))
      when(cache.save[SubscriberContact](any())(any(),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      when(cache.save[CBCId](any())(any(),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      when(cbcId.email(any())(any())) thenReturn  OptionT.pure[Future,Boolean](true)
      when(cache.read[SubscriptionDetails](EQ(SubscriptionDetails.subscriptionDetailsFormat),any(),any())) thenReturn Future.successful(Some(subscriptionDetails))
      when(cache.read[SubscriptionEmailSent.type] (EQ(SubscriptionEmailSent.SubscriptionEmailSentFormat),any(),any())) thenReturn Future.successful(None)
      when(auditMock.sendEvent(any())(any(),any())) thenReturn Future.successful(AuditResult.Success)
      status(controller.reconfirmEmailSubmit(fakeRequestSubmit)) shouldBe Status.SEE_OTHER
      verify(cbcId,times(1)).email(any())(any())
      verify(cache,times(1)).save(any())(EQ(SubscriptionEmailSent.SubscriptionEmailSentFormat),any(),any())
    }
    "Not send the email again if the email has been send" in {

      val reconfirmEmail = Json.obj("reconfirmEmail" -> "abc@xyz.com")
      val fakeRequestSubmit = addToken(FakeRequest("POST", "/subscriberReconfirmEmailSubmit").withJsonBody(Json.toJson(reconfirmEmail)))
      val fakeRequestSubmit2 = addToken(FakeRequest("POST", "/subscriberReconfirmEmailSubmit").withJsonBody(Json.toJson(reconfirmEmail)))

      when(cache.read[Subscribed.type] (EQ(Implicits.format),any(),any())) thenReturn Future.successful(None)
      when(cache.read[SubscriberContact] (EQ(SubscriberContact.subscriptionFormat),any(),any())) thenReturn Future.successful(Some(SubscriberContact("firstName", "lastname", "0123123123", EmailAddress("max@max.com"))))
      when(cache.read[CBCId] (EQ(CBCId.cbcIdFormat),any(),any())) thenReturn Future.successful(CBCId("XGCBC0000000001"))
      when(cache.save[SubscriberContact](any())(any(),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      when(cache.save[CBCId](any())(any(),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      when(cbcId.email(any())(any())) thenReturn  OptionT.pure[Future,Boolean](true)
      when(cache.read[SubscriptionDetails](EQ(SubscriptionDetails.subscriptionDetailsFormat),any(),any())) thenReturn Future.successful(Some(subscriptionDetails))
      when(auditMock.sendEvent(any())(any(),any())) thenReturn Future.successful(AuditResult.Success)
      when(cache.read[SubscriptionEmailSent.type] (EQ(SubscriptionEmailSent.SubscriptionEmailSentFormat),any(),any())) thenReturn Future.successful(None)

      status(controller.reconfirmEmailSubmit(fakeRequestSubmit)) shouldBe Status.SEE_OTHER
      when(cache.read[SubscriptionEmailSent.type] (EQ(SubscriptionEmailSent.SubscriptionEmailSentFormat),any(),any())) thenReturn Future.successful(Some(SubscriptionEmailSent))
      status(controller.reconfirmEmailSubmit(fakeRequestSubmit2)) shouldBe Status.SEE_OTHER
      verify(cbcId,times(1)).email(any())(any())
      verify(cache,times(1)).save(any())(EQ(SubscriptionEmailSent.SubscriptionEmailSentFormat),any(),any())
    }
    "return 303 when Email Address is valid even if the email fails" in {

      val reconfirmEmail = Json.obj("reconfirmEmail" -> "abc@xyz.com")
      val fakeRequestSubmit = addToken(FakeRequest("POST", "/subscriberReconfirmEmailSubmit").withJsonBody(Json.toJson(reconfirmEmail)))
      when(cache.read[Subscribed.type] (EQ(Implicits.format),any(),any())) thenReturn Future.successful(None)
      when(cache.read[SubscriberContact] (EQ(SubscriberContact.subscriptionFormat),any(),any())) thenReturn Future.successful(Some(SubscriberContact("firstName", "lastname", "0123123123", EmailAddress("max@max.com"))))
      when(cache.read[CBCId] (EQ(CBCId.cbcIdFormat),any(),any())) thenReturn Future.successful(CBCId("XGCBC0000000001"))
      when(cache.read[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format),EQ(bprTag),any())) thenReturn Future.successful(Some(BusinessPartnerRecord("safeid",None,EtmpAddress("Line1",None,None,None,None,"GB"))))
      when(cache.read[SubscriptionDetails](EQ(SubscriptionDetails.subscriptionDetailsFormat),any(),any())) thenReturn Future.successful(Some(subscriptionDetails))
      when(cache.save[SubscriberContact](any())(any(),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      when(cache.save[CBCId](any())(any(),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      when(cbcId.email(any())(any())) thenReturn  OptionT.none[Future,Boolean]
      when(auditMock.sendEvent(any())(any(),any())) thenReturn Future.successful(AuditResult.Success)
      when(cache.read[SubscriptionEmailSent.type] (EQ(SubscriptionEmailSent.SubscriptionEmailSentFormat),any(),any())) thenReturn Future.successful(None)
      status(controller.reconfirmEmailSubmit(fakeRequestSubmit)) shouldBe Status.SEE_OTHER
      verify(cache,times(0)).save(any())(EQ(SubscriptionEmailSent.SubscriptionEmailSentFormat),any(),any())
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

  "GET contact-info-subscriber" should {
    "return an error if the user is not subscribed" in {
      val fakeRequest = addToken(FakeRequest("GET","contact-info-subscriber"))
      when(enrollments.getCbcId(any())) thenReturn OptionT.none[Future,CBCId]

      val result = controller.updateInfoSubscriber()(fakeRequest)

      status(result) shouldEqual Status.BAD_REQUEST


    }
    "return an error if there is no etmp data"in {
      val fakeRequest = addToken(FakeRequest("GET","contact-info-subscriber"))
      when(enrollments.getCbcId(any())) thenReturn OptionT.pure[Future,CBCId](CBCId.create(1).getOrElse(fail("oops")))
      when(subService.retrieveSubscriptionData(any())(any(),any())) thenReturn EitherT.pure[Future,CBCErrors,Option[SubscriptionDetails]](Some(subscriptionDetails))
      when(cache.save(any())(any(),any(),any())) thenReturn Future.successful(CacheMap("",Map.empty[String,JsValue]))
      when(cbcId.getETMPSubscriptionData(any())(any())) thenReturn OptionT.none[Future,ETMPSubscription]

      val result = controller.updateInfoSubscriber()(fakeRequest)

      status(result) shouldEqual Status.BAD_REQUEST

    }
    "return OK otherwise" in {
      val fakeRequest = addToken(FakeRequest("GET","contact-info-subscriber"))
      when(enrollments.getCbcId(any())) thenReturn OptionT.pure[Future,CBCId](CBCId.create(1).getOrElse(fail("oops")))
      when(subService.retrieveSubscriptionData(any())(any(),any())) thenReturn EitherT.pure[Future,CBCErrors,Option[SubscriptionDetails]](Some(subscriptionDetails))
      when(cache.save(any())(any(),any(),any())) thenReturn Future.successful(CacheMap("",Map.empty[String,JsValue]))
      when(cbcId.getETMPSubscriptionData(any())(any())) thenReturn OptionT.pure[Future,ETMPSubscription](etmpSubscription)

      val result = controller.updateInfoSubscriber()(fakeRequest)

      status(result) shouldEqual Status.OK

    }
  }
  "POST contact-info-subscriber" should {
    "validate the form fields and return BadRequest" when {
      "the first name is not provided" in {
        val data = Json.obj(
          "phoneNumber" -> "0891505050",
          "email" -> "blagh@blagh.com",
//          "firstName" -> "Dave",
          "lastName" -> "Jones"
        )
        val fakeRequest = addToken(FakeRequest("POST","contact-info-subscriber").withJsonBody(data))
        val result = controller.saveUpdatedInfoSubscriber(fakeRequest)
        status(result) shouldEqual Status.BAD_REQUEST
      }
      "the surname is not provided" in {
        val data = Json.obj(
          "phoneNumber" -> "0891505050",
          "email" -> "blagh@blagh.com",
          "firstName" -> "Dave"
//          "lastName" -> "Jones"
        )
        val fakeRequest = addToken(FakeRequest("POST","contact-info-subscriber").withJsonBody(data))
        val result = controller.saveUpdatedInfoSubscriber()(fakeRequest)
        status(result) shouldEqual Status.BAD_REQUEST

      }
      "the phone number is not provided" in {
        val data = Json.obj(
//          "phoneNumber" -> "I'm not a phone number",
          "email" -> "blagh@blagh.com",
          "firstName" -> "Dave",
          "lastName" -> "Jones"
        )
        val fakeRequest = addToken(FakeRequest("POST","contact-info-subscriber").withJsonBody(data))
        val result = controller.saveUpdatedInfoSubscriber()(fakeRequest)
        status(result) shouldEqual Status.BAD_REQUEST

      }
      "the phone number is invalid" in {
        val data = Json.obj(
          "phoneNumber" -> "I'm not a phone number",
          "email" -> "blagh@blagh.com",
          "firstName" -> "Dave",
          "lastName" -> "Jones"
        )
        val fakeRequest = addToken(FakeRequest("POST","contact-info-subscriber").withJsonBody(data))
        val result = controller.saveUpdatedInfoSubscriber()(fakeRequest)
        status(result) shouldEqual Status.BAD_REQUEST
      }
      "the email is not provided" in {
        val data = Json.obj(
          "phoneNumber" -> "0891505050",
//          "email" -> "blagh.com",
          "firstName" -> "Dave",
          "lastName" -> "Jones"
        )
        val fakeRequest = addToken(FakeRequest("POST","contact-info-subscriber").withJsonBody(data))
        val result = controller.saveUpdatedInfoSubscriber()(fakeRequest)
        status(result) shouldEqual Status.BAD_REQUEST

      }
      "the email is invalid" in {
        val data = Json.obj(
          "phoneNumber" -> "0891505050",
          "email" -> "blagh.com",
          "firstName" -> "Dave",
          "lastName" -> "Jones"
        )
        val fakeRequest = addToken(FakeRequest("POST","contact-info-subscriber").withJsonBody(data))
        val result = controller.saveUpdatedInfoSubscriber()(fakeRequest)
        status(result) shouldEqual Status.BAD_REQUEST
      }
    }
    "call update on the ETMPSubscription data api and the internal subscription data api on the backend" in {
      val data = Json.obj(
        "phoneNumber" -> "0891505050",
        "email" -> "blagh@blagh.com",
        "firstName" -> "Dave",
        "lastName" -> "Jones"
      )

      val fakeRequest = addToken(FakeRequest("POST","contact-info-subscriber").withJsonBody(data))
      when(cache.read[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format),EQ(bprTag),any())) thenReturn Future.successful(Some(BusinessPartnerRecord("safeid",None,EtmpAddress("Line1",None,None,None,None,"GB"))))
      when(cache.read[CBCId] (EQ(CBCId.cbcIdFormat),any(),any())) thenReturn Future.successful(CBCId("XGCBC0000000001"))
      when(cbcId.updateETMPSubscriptionData(any(),any())(any())) thenReturn EitherT.right[Future,CBCErrors,UpdateResponse](UpdateResponse(LocalDateTime.now()))
      when(subService.updateSubscriptionData(any(),any())(any(),any())) thenReturn EitherT.right[Future,CBCErrors,String]("Ok")
      val result = controller.saveUpdatedInfoSubscriber()(fakeRequest)
      status(result) shouldEqual Status.OK

    }

  }

}
