/*
 * Copyright 2019 HM Revenue & Customs
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

import org.mockito.ArgumentMatchers.any
import akka.util.Timeout
import cats.data.{EitherT, OptionT}
import cats.instances.future._
import org.mockito.ArgumentMatchers.{eq => EQ, _}
import org.mockito.Mockito._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mockito.MockitoSugar
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.{Configuration, Environment}
import play.api.http.Status
import play.api.i18n.{Messages, MessagesApi}
import play.api.libs.json.{JsValue, Json}
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.contentAsString
import uk.gov.hmrc.auth.core.retrieve.Credentials
import uk.gov.hmrc.auth.core.{AffinityGroup, AuthConnector}
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.connectors.BPRKnownFactsConnector
import uk.gov.hmrc.cbcrfrontend.model.{SubscriptionEmailSent, _}
import uk.gov.hmrc.cbcrfrontend.services._
import uk.gov.hmrc.cbcrfrontend.typesclasses.{CbcrsUrl, ServiceUrl}
import uk.gov.hmrc.cbcrfrontend.util.CbcrSwitches
import uk.gov.hmrc.emailaddress.EmailAddress
import uk.gov.hmrc.http.cache.client.CacheMap
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.runtime.universe._
import uk.gov.hmrc.http.HeaderCarrier
class SubscriptionControllerSpec  extends UnitSpec with ScalaFutures with GuiceOneAppPerSuite  with CSRFTest with MockitoSugar with BeforeAndAfterEach {
  val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
  def getMessages(r: FakeRequest[_]): Messages = messagesApi.preferred(r)

  implicit val ec = app.injector.instanceOf[ExecutionContext]
  implicit val env = app.injector.instanceOf[Environment]
  val subService = mock[SubscriptionDataService]
  val auditMock = mock[AuditConnector]
  implicit val config = app.injector.instanceOf[Configuration]
  implicit val feConfig = app.injector.instanceOf[FrontendAppConfig]

  val dc = mock[BPRKnownFactsConnector]
  val cbcId = mock[CBCIdService]
  val cbcKF = mock[EnrolmentsService]
  val bprKF = mock[BPRKnownFactsService]
  val emailMock = mock[EmailService]
  implicit val cache = mock[CBCSessionCache]
  val auth = mock[AuthConnector]


  val id = CBCId.create(5678).getOrElse(fail("bad cbcid"))
  val utr = Utr("9000000001")
  implicit val timeout = Timeout(5 seconds)
  override protected def afterEach(): Unit = {
    reset(cache,subService,auditMock,dc,cbcId,bprKF,cache,emailMock,auth)
    super.afterEach()
  }

  when(cache.read[AffinityGroup](EQ(AffinityGroup.jsonFormat),any(),any())).thenReturn(rightE[AffinityGroup](AffinityGroup.Organisation))

  val controller = new SubscriptionController(messagesApi,subService,dc,cbcId,emailMock,cbcKF,bprKF,env,auditMock,auth)

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
      when(auth.authorise[Any](any(),any())(any(),any())) thenReturn Future.successful(())
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(messagesApi,subService,dc,cbcId,emailMock,cbcKF,bprKF,env,auditMock,auth)
      val fakeRequestSubscribe = addToken(FakeRequest("GET", "/contactInfoSubscriber"))
      status(controller.contactInfoSubscriber(fakeRequestSubscribe)) shouldBe Status.OK
    }
  }

  "POST /submitSubscriptionData" should {
    "return 400 when the there is no data" in {
      when(auth.authorise[Credentials](any(),any())(any(),any())) thenReturn Future.successful(Credentials("asdf","gateway"))
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(messagesApi,subService,dc,cbcId,emailMock,cbcKF,bprKF,env,auditMock,auth)
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/submitSubscriptionData"))
      status(controller.submitSubscriptionData(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }
    "return 400 when either first or last name or both are missing" in {
      when(auth.authorise[Credentials](any(),any())(any(),any())) thenReturn Future.successful(Credentials("asdf","gateway"))
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(messagesApi,subService,dc,cbcId,emailMock,cbcKF,bprKF,env,auditMock,auth)
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
      when(auth.authorise[Credentials](any(),any())(any(),any())) thenReturn Future.successful(Credentials("asdf","gateway"))
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(messagesApi,subService,dc,cbcId,emailMock,cbcKF,bprKF,env,auditMock,auth)
      val data = Json.obj(
        "phoneNumber" -> "12345678",
        "firstName" -> "Dave",
        "lastName" -> "Jones"
      )
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/submitSubscriptionData").withJsonBody(data))
      status(controller.submitSubscriptionData(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }
    "return 400 when the email is invalid" in {
      when(auth.authorise[Credentials](any(),any())(any(),any())) thenReturn Future.successful(Credentials("asdf","gateway"))
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(messagesApi,subService,dc,cbcId,emailMock,cbcKF,bprKF,env,auditMock,auth)
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
      when(auth.authorise[Credentials](any(),any())(any(),any())) thenReturn Future.successful(Credentials("asdf","gateway"))
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(messagesApi,subService,dc,cbcId,emailMock,cbcKF,bprKF,env,auditMock,auth)
      val data = Json.obj(
        "firstName" -> "Dave",
        "lastName" -> "Jones",
        "email" -> "blagh@blagh.com"
      )
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/submitSubscriptionData").withJsonBody(data))
      status(controller.submitSubscriptionData(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }

    "return 400 when the phone number is invalid" in {
      when(auth.authorise[Credentials](any(),any())(any(),any())) thenReturn Future.successful(Credentials("asdf","gateway"))
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(messagesApi,subService,dc,cbcId,emailMock,cbcKF,bprKF,env,auditMock,auth)
      val data = Json.obj(
        "phoneNumber" -> "I'm not a phone number",
        "firstName" -> "Dave",
        "lastName" -> "Jones",
        "email" -> "blagh@blagh.com"
      )
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/submitSubscriptionData").withJsonBody(data))
      status(controller.submitSubscriptionData(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }
    "return a custom error message when the phone number is invalid" in {
      when(auth.authorise[Credentials](any(),any())(any(),any())) thenReturn Future.successful(Credentials("asdf","gateway"))
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(messagesApi,subService,dc,cbcId,emailMock,cbcKF,bprKF,env,auditMock,auth)
      val data = Json.obj(
        "phoneNumber" -> "I'm not a phone number",
        "firstName" -> "Dave",
        "lastName" -> "Jones",
        "email" -> "blagh@blagh.com"
      )
      val fakeRequest = FakeRequest("POST", "/submitSubscriptionData").withJsonBody(data)
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/submitSubscriptionData").withJsonBody(data))
      val result: Future[Result] = controller.submitSubscriptionData(fakeRequestSubscribe)
      implicit val messages = getMessages(fakeRequest)
      status(result) shouldBe Status.BAD_REQUEST
      val webPageAsString =   contentAsString(result)
      webPageAsString should include(getMessages(fakeRequest)("contactInfoSubscriber.phoneNumber.error.invalid"))
      webPageAsString should include("recognise the phone number")
      webPageAsString should not include("found some errors")
      webPageAsString should not include(getMessages(fakeRequest)("contactInfoSubscriber.phoneNumber.error.empty"))
    }
    "return a custom error message when the phone number is empty" in {
      when(auth.authorise[Credentials](any(),any())(any(),any())) thenReturn Future.successful(Credentials("asdf","gateway"))
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(messagesApi,subService,dc,cbcId,emailMock,cbcKF,bprKF,env,auditMock,auth)
      val data = Json.obj(
        "phoneNumber" -> "",
        "firstName" -> "Dave",
        "lastName" -> "Jones",
        "email" -> "blagh@blagh.com"
      )
      val fakeRequest = FakeRequest("POST", "/submitSubscriptionData").withJsonBody(data)
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/submitSubscriptionData").withJsonBody(data))
      val result: Future[Result] = controller.submitSubscriptionData(fakeRequestSubscribe)
      implicit val messages = getMessages(fakeRequest)
      status(result) shouldBe Status.BAD_REQUEST
      val webPageAsString =   contentAsString(result)
      webPageAsString should include(getMessages(fakeRequest)("contactInfoSubscriber.phoneNumber.error.empty"))
      webPageAsString should include("entered your phone number")
      webPageAsString should not include("found some errors")
      webPageAsString should not include(getMessages(fakeRequest)("contactInfoSubscriber.phoneNumber.error.invalid"))
    }
    "return a custom error message when the email  is invalid" in {
      when(auth.authorise[Credentials](any(),any())(any(),any())) thenReturn Future.successful(Credentials("asdf","gateway"))
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(messagesApi,subService,dc,cbcId,emailMock,cbcKF,bprKF,env,auditMock,auth)
      val data = Json.obj(
        "phoneNumber" -> "07706641666",
        "firstName" -> "Dave",
        "lastName" -> "Jones",
        "email" -> "I am not a email"
      )
      val fakeRequest = FakeRequest("POST", "/submitSubscriptionData").withJsonBody(data)
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/submitSubscriptionData").withJsonBody(data))
      val result: Future[Result] = controller.submitSubscriptionData(fakeRequestSubscribe)
      implicit val messages = getMessages(fakeRequest)
      status(result) shouldBe Status.BAD_REQUEST
      val webPageAsString =   contentAsString(result)
      webPageAsString should include(getMessages(fakeRequest)("contactInfoSubscriber.emailAddress.error.invalid"))
      webPageAsString should include("recognise the email address")
      webPageAsString should not include("found some errors")
      webPageAsString should not include("entered your email address")
    }
    "return a custom error message when the frist name is empty" in {
      when(auth.authorise[Credentials](any(),any())(any(),any())) thenReturn Future.successful(Credentials("asdf","gateway"))
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(messagesApi,subService,dc,cbcId,emailMock,cbcKF,bprKF,env,auditMock,auth)
      val data = Json.obj(
        "phoneNumber" -> "07706641666",
        "firstName" -> "",
        "lastName" -> "Jones",
        "email" -> "colm.m.cavanagh@gmail.com"
      )
      val fakeRequest = FakeRequest("POST", "/submitSubscriptionData").withJsonBody(data)
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/submitSubscriptionData").withJsonBody(data))
      val result: Future[Result] = controller.submitSubscriptionData(fakeRequestSubscribe)
      implicit val messages = getMessages(fakeRequest)
      status(result) shouldBe Status.BAD_REQUEST
      val webPageAsString =   contentAsString(result)
      webPageAsString should include(getMessages(fakeRequest)("contactInfoSubscriber.firstName.error"))
      webPageAsString should not include("found some errors")
    }
    "return a custom error message when the last name is empty" in {
      when(auth.authorise[Credentials](any(),any())(any(),any())) thenReturn Future.successful(Credentials("asdf","gateway"))
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(messagesApi,subService,dc,cbcId,emailMock,cbcKF,bprKF,env,auditMock,auth)
      val data = Json.obj(
        "phoneNumber" -> "07706641666",
        "firstName" -> "Dave",
        "lastName" -> "",
        "email" -> "colm.m.cavanagh@gmail.com"
      )
      val fakeRequest = FakeRequest("POST", "/submitSubscriptionData").withJsonBody(data)
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/submitSubscriptionData").withJsonBody(data))
      val result: Future[Result] = controller.submitSubscriptionData(fakeRequestSubscribe)
      implicit val messages = getMessages(fakeRequest)
      status(result) shouldBe Status.BAD_REQUEST
      val webPageAsString =   contentAsString(result)
      webPageAsString should include(getMessages(fakeRequest)("contactInfoSubscriber.lastName.error"))
      webPageAsString should not include("found some errors")
    }
    "return a custom error message when the email is empty" in {
      when(auth.authorise[Credentials](any(),any())(any(),any())) thenReturn Future.successful(Credentials("asdf","gateway"))
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(messagesApi,subService,dc,cbcId,emailMock,cbcKF,bprKF,env,auditMock,auth)
      val data = Json.obj(
        "phoneNumber" -> "07706641666",
        "firstName" -> "Dave",
        "lastName" -> "Jones",
        "email" -> ""
      )
      val fakeRequest = FakeRequest("POST", "/submitSubscriptionData").withJsonBody(data)
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/submitSubscriptionData").withJsonBody(data))
      val result: Future[Result] = controller.submitSubscriptionData(fakeRequestSubscribe)
      implicit val messages = getMessages(fakeRequest)
      status(result) shouldBe Status.BAD_REQUEST
      val webPageAsString =   contentAsString(result)
      webPageAsString should include("entered your email address")
      webPageAsString should not include("found some errors")
      webPageAsString should not include(getMessages(fakeRequest)("contactInfoSubscriber.emailAddress.error.invalid"))
    }
    "return 500 when the SubscriptionDataService errors" in {
      when(auth.authorise[Credentials](any(),any())(any(),any())) thenReturn Future.successful(Credentials("asdf","gateway"))
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(messagesApi,subService,dc,cbcId,emailMock,cbcKF,bprKF,env,auditMock,auth)
      val sData = SubscriberContact("Dave","Smith", "0207456789",EmailAddress("Bob@bob.com"))
      val fakeRequest = addToken(FakeRequest("POST", "/submitSubscriptionData").withJsonBody(Json.toJson(sData)))
      when(cbcId.subscribe(anyObject())(any())) thenReturn OptionT[Future,CBCId](Future.successful(cbcid))
      when(subService.saveSubscriptionData(any(classOf[SubscriptionDetails]))(anyObject(),anyObject())) thenReturn EitherT.left[Future,CBCErrors, String](Future.successful(UnexpectedState("return 500 when the SubscriptionDataService errors")))
      when(subService.clearSubscriptionData(any())(any(),any())) thenReturn EitherT.right[Future,CBCErrors, Option[String]](None)
      when(cache.readOption[Subscribed.type](EQ(Implicits.format),any(),any())) thenReturn Future.successful(None)
      when(cache.read[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format),EQ(bprTag),any())) thenReturn rightE(BusinessPartnerRecord("safeid",None,EtmpAddress("Line1",None,None,None,None,"GB")))
      when(cache.read[Utr](EQ(Utr.utrRead),EQ(utrTag),any())) thenReturn rightE(Utr("700000002"))
      when(cache.readOption[GGId](EQ(GGId.format),any(),any())) thenReturn Future.successful(Some(GGId("ggid","type")))
      when(auditMock.sendExtendedEvent(any())(any(),any())) thenReturn Future.successful(AuditResult.Success)
      status(controller.submitSubscriptionData(fakeRequest)) shouldBe Status.INTERNAL_SERVER_ERROR
      verify(subService).clearSubscriptionData(any())(any(),any())
      verify(auditMock).sendExtendedEvent(any())(any(),any())
    }
    "return 500 when the getCbcId call errors out" in {
      when(auth.authorise[Credentials](any(),any())(any(),any())) thenReturn Future.successful(Credentials("asdf","gateway"))
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(messagesApi,subService,dc,cbcId,emailMock,cbcKF,bprKF,env,auditMock,auth)
      val sData = SubscriberContact("Dave","Smith","0207456789",EmailAddress("Bob@bob.com"))
      val fakeRequest = addToken(FakeRequest("POST", "/submitSubscriptionData").withJsonBody(Json.toJson(sData)))
      when(cache.read[SubscriptionDetails](EQ(SubscriptionDetails.subscriptionDetailsFormat),any(),any())) thenReturn rightE(subscriptionDetails)
      when(cbcId.subscribe(anyObject())(any())) thenReturn OptionT[Future,CBCId](Future.successful(None))
      when(cache.readOption[Subscribed.type](EQ(Implicits.format),any(),any())) thenReturn Future.successful(None)
      when(cache.read[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format),EQ(bprTag),any())) thenReturn rightE(BusinessPartnerRecord("safeid",None,EtmpAddress("Line1",None,None,None,None,"GB")))
      when(cache.read[Utr](EQ(Utr.utrRead),EQ(utrTag),any())) thenReturn rightE(Utr("700000002"))
      status(controller.submitSubscriptionData(fakeRequest)) shouldBe Status.INTERNAL_SERVER_ERROR
      verify(subService, times(0)).clearSubscriptionData(any())(any(),any())
    }
    "return 500 when the addKnownFactsToGG call errors" in {
      when(auth.authorise[Credentials](any(),any())(any(),any())) thenReturn Future.successful(Credentials("asdf","gateway"))
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(messagesApi,subService,dc,cbcId,emailMock,cbcKF,bprKF,env,auditMock,auth)
      val sData = SubscriberContact("Dave","Smith","0207456789",EmailAddress("Bob@bob.com"))
      val fakeRequest = addToken(FakeRequest("POST", "/submitSubscriptionData").withJsonBody(Json.toJson(sData)))
      when(subService.saveSubscriptionData(any(classOf[SubscriptionDetails]))(anyObject(),anyObject())) thenReturn EitherT.left[Future,CBCErrors, String](Future.successful(UnexpectedState("oops")))
      when(cbcId.subscribe(anyObject())(any())) thenReturn OptionT(Future.successful(CBCId("XGCBC0000000001")))
      when(cbcKF.enrol(anyObject())(anyObject())) thenReturn EitherT.left[Future,CBCErrors, Unit](UnexpectedState("oops"))
      when(cache.read[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format),EQ(bprTag),any())) thenReturn rightE(BusinessPartnerRecord("safeid",None,EtmpAddress("Line1",None,None,None,None,"GB")))
      when(cache.readOption[Subscribed.type](EQ(Implicits.format),any(),any())) thenReturn Future.successful(None)
      when(cache.read[Utr](EQ(Utr.utrRead),EQ(utrTag),any())) thenReturn rightE(Utr("123456789"))
      when(cache.readOption[GGId](EQ(GGId.format),any(),any())) thenReturn Future.successful(Some(GGId("ggid","type")))
      when(auditMock.sendExtendedEvent(any())(any(),any())) thenReturn Future.successful(AuditResult.Success)
      when(subService.clearSubscriptionData(any())(any(),any())) thenReturn EitherT.right[Future,CBCErrors, Option[String]](None)
      status(controller.submitSubscriptionData(fakeRequest)) shouldBe Status.INTERNAL_SERVER_ERROR
      verify(subService).clearSubscriptionData(any())(any(),any())
      verify(auditMock).sendExtendedEvent(any())(any(),any())
    }
    "return 303 (see_other) when all params are present and valid and the SubscriptionDataService returns Ok and send an email " in {
      when(auth.authorise[Credentials](any(),any())(any(),any())) thenReturn Future.successful(Credentials("asdf","gateway"))
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(messagesApi,subService,dc,cbcId,emailMock,cbcKF,bprKF,env,auditMock,auth)
      val sData = SubscriberContact("Dave","Smith","0207456789",EmailAddress("Bob@bob.com"))
      val fakeRequest = addToken(FakeRequest("POST", "/submitSubscriptionData").withJsonBody(Json.toJson(sData)))
      when(subService.saveSubscriptionData(any(classOf[SubscriptionDetails]))(anyObject(),anyObject())) thenReturn EitherT.pure[Future,CBCErrors, String]("done")
      when(cache.readOption[GGId](EQ(GGId.format),any(),any())) thenReturn Future.successful(Some(GGId("ggid","type")))
      when(cbcId.subscribe(anyObject())(any())) thenReturn OptionT(Future.successful(CBCId("XGCBC0000000001")))
      when(cbcKF.enrol(anyObject())(anyObject())) thenReturn EitherT.pure[Future,CBCErrors, Unit](())
      when(cache.read[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format),EQ(bprTag),any())) thenReturn rightE(BusinessPartnerRecord("safeid",None,EtmpAddress("Line1",None,None,None,None,"GB")))
      when(cache.read[Utr](EQ(Utr.utrRead),EQ(utrTag),any())) thenReturn rightE(Utr("123456789"))
      when(cache.readOption[Subscribed.type](EQ(Implicits.format),any(),any())) thenReturn Future.successful(None)
      when(cache.read[CBCId](EQ(CBCId.cbcIdFormat),any(),any())) thenReturn rightE(cbcid.getOrElse(fail("aslkjfd")))
      when(cache.readOption[SubscriptionEmailSent](EQ(SubscriptionEmailSent.SubscriptionEmailSentFormat),any(),any())) thenReturn Future.successful(None)
      when(cache.save[SubscriberContact](any())(any(),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      when(cache.save[SubscriptionEmailSent](any())(any(),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      when(auditMock.sendExtendedEvent(any())(any(),any())) thenReturn Future.successful(AuditResult.Success)
      when(emailMock.sendEmail(any())(any())) thenReturn  OptionT.pure[Future,Boolean](true)
      status(controller.submitSubscriptionData(fakeRequest)) shouldBe Status.SEE_OTHER
      verify(subService, times(0)).clearSubscriptionData(any())(any(),any())
      verify(emailMock,times(1)).sendEmail(any())(any())
    }
    "not send an email if one has already been send " in {
      when(auth.authorise[Credentials](any(),any())(any(),any())) thenReturn Future.successful(Credentials("asdf","gateway"))
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(messagesApi,subService,dc,cbcId,emailMock,cbcKF,bprKF,env,auditMock,auth)
      val sData = SubscriberContact("Dave","Smith","0207456789",EmailAddress("Bob@bob.com"))
      val fakeRequest = addToken(FakeRequest("POST", "/submitSubscriptionData").withJsonBody(Json.toJson(sData)))
      when(cache.readOption[GGId](EQ(GGId.format),any(),any())) thenReturn Future.successful(Some(GGId("ggid","type")))
      when(subService.saveSubscriptionData(any(classOf[SubscriptionDetails]))(anyObject(),anyObject())) thenReturn right("done")
      when(cbcId.subscribe(anyObject())(any())) thenReturn OptionT(Future.successful(CBCId("XGCBC0000000001")))
      when(cbcKF.enrol(anyObject())(anyObject())) thenReturn EitherT.pure[Future,CBCErrors, Unit](())
      when(cache.read[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format),EQ(bprTag),any())) thenReturn rightE(BusinessPartnerRecord("safeid",None,EtmpAddress("Line1",None,None,None,None,"GB")))
      when(cache.read[Utr](EQ(Utr.utrRead),EQ(utrTag),any())) thenReturn rightE(Utr("123456789"))
      when(cache.readOption[Subscribed.type](EQ(Implicits.format),any(),any())) thenReturn Future.successful(None)
      when(cache.read[CBCId](EQ(CBCId.cbcIdFormat),any(),any())) thenReturn rightE(cbcid.getOrElse(fail("kajsjdf")))
      when(cache.readOption[SubscriptionEmailSent](EQ(SubscriptionEmailSent.SubscriptionEmailSentFormat),any(),any())) thenReturn Future.successful(Some(SubscriptionEmailSent()))
      when(cache.save[SubscriberContact](any())(any(),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      when(cache.save[SubscriptionEmailSent](any())(any(),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      when(auditMock.sendExtendedEvent(any())(any(),any())) thenReturn Future.successful(AuditResult.Success)
      when(emailMock.sendEmail(any())(any())) thenReturn  OptionT.pure[Future,Boolean](true)
      status(controller.submitSubscriptionData(fakeRequest)) shouldBe Status.SEE_OTHER
      verify(subService, times(0)).clearSubscriptionData(any())(any(),any())
      verify(emailMock,times(0)).sendEmail(any())(any())
    }
    "return 500 when trying to resubmit subscription details" in {
      when(auth.authorise[Credentials](any(),any())(any(),any())) thenReturn Future.successful(Credentials("asdf","gateway"))
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(messagesApi,subService,dc,cbcId,emailMock,cbcKF,bprKF,env,auditMock,auth)
      val sData = SubscriberContact("Dave","Smith","0207456789",EmailAddress("Bob@bob.com"))
      val fakeRequest = addToken(FakeRequest("POST", "/submitSubscriptionData").withJsonBody(Json.toJson(sData)))
      when(cache.readOption[Subscribed.type] (EQ(Implicits.format),any(),any())) thenReturn Future.successful(Some(Subscribed))
      status(controller.submitSubscriptionData(fakeRequest)) shouldBe Status.INTERNAL_SERVER_ERROR

    }
  }


  "DELETE to clear-subscription-data/utr" should {
    "work correctly when enabled and" when {
      System.setProperty(CbcrSwitches.clearSubscriptionDataRoute.name, "true")
      "return a 200 if data was successfully cleared" in {
        when(auth.authorise[Any](any(),any())(any(),any())) thenReturn Future.successful(())
        val fakeRequestSubscribe = addToken(FakeRequest("DELETE", "/clear-subscription-data"))
        val u: Utr = Utr("7000000002")
        when(subService.clearSubscriptionData(any())(any(), any())) thenReturn EitherT.pure[Future, CBCErrors,Option[String]](Some("done"))
        status(controller.clearSubscriptionData(u)(fakeRequestSubscribe)) shouldBe Status.OK
      }
      "return a 204 if data was no data to clear" in {
        when(auth.authorise[Any](any(),any())(any(),any())) thenReturn Future.successful(())
        val fakeRequestSubscribe = addToken(FakeRequest("DELETE", "/clear-subscription-data"))
        val u: Utr = Utr("7000000002")
        when(subService.clearSubscriptionData(any())(any(), any())) thenReturn EitherT.pure[Future, CBCErrors,Option[String]](None)
        status(controller.clearSubscriptionData(u)(fakeRequestSubscribe)) shouldBe Status.NO_CONTENT
      }
      "return a 500 if something goes wrong" in {
        when(auth.authorise[Any](any(),any())(any(),any())) thenReturn Future.successful(())
        val fakeRequestSubscribe = addToken(FakeRequest("DELETE", "/clear-subscription-data"))
        val u: Utr = Utr("7000000002")
        when(subService.clearSubscriptionData(any())(any(), any())) thenReturn EitherT.left[Future, CBCErrors,Option[String]](UnexpectedState("oops"))
        status(controller.clearSubscriptionData(u)(fakeRequestSubscribe)) shouldBe Status.INTERNAL_SERVER_ERROR
      }
    }

    "return 501 when feature-disabled" in {
      when(auth.authorise[Any](any(),any())(any(),any())) thenReturn Future.successful(())
      System.setProperty(CbcrSwitches.clearSubscriptionDataRoute.name, "false")
      val fakeRequestSubscribe = addToken(FakeRequest("DELETE", "/clear-subscription-data"))
      val u: Utr = Utr("7000000002")
      status(controller.clearSubscriptionData(u)(fakeRequestSubscribe)) shouldBe Status.NOT_IMPLEMENTED

    }
  }

  "GET contact-info-subscriber" should {
    "return an error if the user is not subscribed" in {
      when(auth.authorise[Option[CBCEnrolment]](any(),any())(any(),any())) thenReturn Future.successful(Some(CBCEnrolment(id,utr)))
      val fakeRequest = addToken(FakeRequest("GET","contact-info-subscriber"))
      when(subService.retrieveSubscriptionData(any())(any(),any())) thenReturn EitherT.pure[Future,CBCErrors,Option[SubscriptionDetails]](None)
      val result = controller.updateInfoSubscriber()(fakeRequest)

      status(result) shouldEqual Status.BAD_REQUEST


    }
    "return an error if there is no etmp data"in {
      when(auth.authorise[Option[CBCEnrolment]](any(),any())(any(),any())) thenReturn Future.successful(Some(CBCEnrolment(id,utr)))
      val fakeRequest = addToken(FakeRequest("GET","contact-info-subscriber"))
      when(subService.retrieveSubscriptionData(any())(any(),any())) thenReturn EitherT.pure[Future,CBCErrors,Option[SubscriptionDetails]](Some(subscriptionDetails))
      when(cache.save(any())(any(),any(),any())) thenReturn Future.successful(CacheMap("",Map.empty[String,JsValue]))
      when(cbcId.getETMPSubscriptionData(any())(any())) thenReturn OptionT.none[Future,ETMPSubscription]

      val result = controller.updateInfoSubscriber()(fakeRequest)

      status(result) shouldEqual Status.BAD_REQUEST

    }
    "return OK otherwise" in {
      when(auth.authorise[Option[CBCEnrolment]](any(),any())(any(),any())) thenReturn Future.successful(Some(CBCEnrolment(id,utr)))
      val fakeRequest = addToken(FakeRequest("GET","contact-info-subscriber"))
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
          "lastName" -> "Jones"
        )
        when(auth.authorise[Option[CBCEnrolment]](any(),any())(any(),any())) thenReturn Future.successful(Some(CBCEnrolment(id,utr)))
        val fakeRequest = addToken(FakeRequest("POST","contact-info-subscriber").withJsonBody(data))
        val result = controller.saveUpdatedInfoSubscriber(fakeRequest)
        status(result) shouldEqual Status.BAD_REQUEST
      }
      "the surname is not provided" in {
        val data = Json.obj(
          "phoneNumber" -> "0891505050",
          "email" -> "blagh@blagh.com",
          "firstName" -> "Dave"
        )
        when(auth.authorise[Option[CBCEnrolment]](any(),any())(any(),any())) thenReturn Future.successful(Some(CBCEnrolment(id,utr)))
        val fakeRequest = addToken(FakeRequest("POST","contact-info-subscriber").withJsonBody(data))
        val result = controller.saveUpdatedInfoSubscriber()(fakeRequest)
        status(result) shouldEqual Status.BAD_REQUEST

      }
      "the phone number is not provided" in {
        val data = Json.obj(
          "email" -> "blagh@blagh.com",
          "firstName" -> "Dave",
          "lastName" -> "Jones"
        )
        when(auth.authorise[Option[CBCEnrolment]](any(),any())(any(),any())) thenReturn Future.successful(Some(CBCEnrolment(id,utr)))
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
        when(auth.authorise[Option[CBCEnrolment]](any(),any())(any(),any())) thenReturn Future.successful(Some(CBCEnrolment(id,utr)))
        val fakeRequest = addToken(FakeRequest("POST","contact-info-subscriber").withJsonBody(data))
        val result = controller.saveUpdatedInfoSubscriber()(fakeRequest)
        status(result) shouldEqual Status.BAD_REQUEST
      }
      "the email is not provided" in {
        val data = Json.obj(
          "phoneNumber" -> "0891505050",
          "firstName" -> "Dave",
          "lastName" -> "Jones"
        )
        when(auth.authorise[Option[CBCEnrolment]](any(),any())(any(),any())) thenReturn Future.successful(Some(CBCEnrolment(id,utr)))
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
        when(auth.authorise[Option[CBCEnrolment]](any(),any())(any(),any())) thenReturn Future.successful(Some(CBCEnrolment(id,utr)))
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
      when(auth.authorise[Option[CBCEnrolment]](any(),any())(any(),any())) thenReturn Future.successful(Some(CBCEnrolment(id,utr)))
      when(cache.read[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format),EQ(bprTag),any())) thenReturn rightE(BusinessPartnerRecord("safeid",None,EtmpAddress("Line1",None,None,None,None,"GB")))
      when(cache.read[CBCId] (EQ(CBCId.cbcIdFormat),any(),any())) thenReturn rightE(CBCId("XGCBC0000000001").getOrElse(fail("lsadkjf")))
      when(cbcId.updateETMPSubscriptionData(any(),any())(any())) thenReturn EitherT.right[Future,CBCErrors,UpdateResponse](UpdateResponse(LocalDateTime.now()))
      when(subService.updateSubscriptionData(any(),any())(any(),any())) thenReturn EitherT.right[Future,CBCErrors,String]("Ok")
      val result = controller.saveUpdatedInfoSubscriber()(fakeRequest)
      status(result) shouldEqual Status.SEE_OTHER

    }

  }

}
