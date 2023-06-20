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

import akka.util.Timeout
import cats.data.OptionT
import cats.data.OptionT.catsDataMonadForOptionT
import cats.implicits.catsStdInstancesForFuture
import org.mockito.ArgumentMatchers.{any, eq => EQ}
import org.mockito.MockitoSugar
import org.mockito.cats.MockitoCats
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Play.materializer
import play.api.http.Status
import play.api.i18n.{Messages, MessagesApi}
import play.api.libs.json.JsValue
import play.api.mvc.{MessagesControllerComponents, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers.{call, contentAsString, writeableOf_AnyContentAsFormUrlEncoded}
import play.api.{Configuration, Environment}
import uk.gov.hmrc.auth.core.retrieve.Credentials
import uk.gov.hmrc.auth.core.{AffinityGroup, AuthConnector}
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.connectors.BPRKnownFactsConnector
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.services._
import uk.gov.hmrc.cbcrfrontend.typesclasses.{CbcrsUrl, ServiceUrl}
import uk.gov.hmrc.cbcrfrontend.util.{CbcrSwitches, UnitSpec}
import uk.gov.hmrc.cbcrfrontend.views.Views
import uk.gov.hmrc.emailaddress.EmailAddress
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.cache.client.CacheMap
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}

import java.time.LocalDateTime
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps
import scala.reflect.runtime.universe._
class SubscriptionControllerSpec
    extends UnitSpec with GuiceOneAppPerSuite with CSRFTest with BeforeAndAfterEach
    with MockitoSugar with MockitoCats {
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
  val mcc = app.injector.instanceOf[MessagesControllerComponents]
  val views = app.injector.instanceOf[Views]

  val id = CBCId.create(5678).getOrElse(fail("bad cbcid"))
  val utr = Utr("9000000001")
  implicit val timeout = Timeout(5 seconds)

  override protected def afterEach(): Unit = {
    reset(cache, subService, auditMock, dc, cbcId, bprKF, cache, emailMock, auth)
    super.afterEach()
  }

  whenF(cache.read[AffinityGroup](EQ(AffinityGroup.jsonFormat), any(), any())) thenReturn AffinityGroup.Organisation

  val controller =
    new SubscriptionController(
      messagesApi,
      subService,
      dc,
      cbcId,
      emailMock,
      cbcKF,
      bprKF,
      env,
      auditMock,
      auth,
      mcc,
      views)

  implicit val hc = HeaderCarrier()
  implicit val cbcrsUrl = new ServiceUrl[CbcrsUrl] { val url = "cbcr" }

  implicit val bprTag = implicitly[TypeTag[BusinessPartnerRecord]]
  implicit val utrTag = implicitly[TypeTag[Utr]]

  val cbcid = CBCId.create(1).toOption

  val subscriptionDetails = SubscriptionDetails(
    BusinessPartnerRecord(
      "SAFEID",
      Some(OrganisationResponse("blagh")),
      EtmpAddress("Line1", None, None, None, Some("TF3 XFE"), "GB")),
    SubscriberContact("Jimbo", "Jones", "phonenum", EmailAddress("test@test.com")),
    cbcid,
    Utr("7000000002")
  )

  val etmpSubscription = ETMPSubscription(
    "safeid",
    ContactName("Firstname", "SecondName"),
    ContactDetails(EmailAddress("test@test.com"), "123456789"),
    EtmpAddress("Line1", None, None, None, None, "GB")
  )

  "GET /contactInfoSubscriber" should {
    "return 200" in {
      when(auth.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful(())
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(
        messagesApi,
        subService,
        dc,
        cbcId,
        emailMock,
        cbcKF,
        bprKF,
        env,
        auditMock,
        auth,
        mcc,
        views
      )
      val fakeRequestSubscribe = addToken(FakeRequest("GET", "/contactInfoSubscriber"))
      status(controller.contactInfoSubscriber(fakeRequestSubscribe)) shouldBe Status.OK
    }
  }

  "POST /submitSubscriptionData" should {
    "return 400 when the there is no data" in {
      when(auth.authorise[Option[Credentials]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(Credentials("asdf", "gateway")))
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(
        messagesApi,
        subService,
        dc,
        cbcId,
        emailMock,
        cbcKF,
        bprKF,
        env,
        auditMock,
        auth,
        mcc,
        views)
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/submitSubscriptionData"))
      status(controller.submitSubscriptionData(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }

    "return 400 when either first or last name or both are missing" in {
      when(auth.authorise[Option[Credentials]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(Credentials("asdf", "gateway")))
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(
        messagesApi,
        subService,
        dc,
        cbcId,
        emailMock,
        cbcKF,
        bprKF,
        env,
        auditMock,
        auth,
        mcc,
        views)
      val data = Seq(
        "phoneNumber" -> "12345678",
        "email"       -> "blagh@blagh.com"
      )
      val fakeRequestSubscribe =
        addToken(FakeRequest("POST", "/submitSubscriptionData").withFormUrlEncodedBody(data: _*))
      status(controller.submitSubscriptionData(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST

      val data2 = Seq(
        "phoneNumber" -> "12345678",
        "email"       -> "blagh@blagh.com",
        "firstName"   -> "Bob"
      )
      val fakeRequestSubscribe2 =
        addToken(FakeRequest("POST", "/submitSubscriptionData").withFormUrlEncodedBody(data2: _*))
      status(controller.submitSubscriptionData(fakeRequestSubscribe2)) shouldBe Status.BAD_REQUEST

      val data3 = Seq(
        "phoneNumber" -> "12345678",
        "email"       -> "blagh@blagh.com",
        "lastName"    -> "Jones"
      )
      val fakeRequestSubscribe3 =
        addToken(FakeRequest("POST", "/submitSubscriptionData").withFormUrlEncodedBody(data3: _*))
      status(controller.submitSubscriptionData(fakeRequestSubscribe3)) shouldBe Status.BAD_REQUEST
    }

    "return 400 when the email is missing" in {
      when(auth.authorise[Option[Credentials]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(Credentials("asdf", "gateway")))
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(
        messagesApi,
        subService,
        dc,
        cbcId,
        emailMock,
        cbcKF,
        bprKF,
        env,
        auditMock,
        auth,
        mcc,
        views)
      val data = Seq(
        "phoneNumber" -> "12345678",
        "firstName"   -> "Dave",
        "lastName"    -> "Jones"
      )
      val fakeRequestSubscribe =
        addToken(FakeRequest("POST", "/submitSubscriptionData").withFormUrlEncodedBody(data: _*))
      status(controller.submitSubscriptionData(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }

    "return 400 when the email is invalid" in {
      when(auth.authorise[Option[Credentials]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(Credentials("asdf", "gateway")))
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(
        messagesApi,
        subService,
        dc,
        cbcId,
        emailMock,
        cbcKF,
        bprKF,
        env,
        auditMock,
        auth,
        mcc,
        views)
      val data = Seq(
        "phoneNumber" -> "12345678",
        "firstName"   -> "Dave",
        "lastName"    -> "Jones",
        "email"       -> "THISISNOTANEMAIL"
      )
      val fakeRequestSubscribe =
        addToken(FakeRequest("POST", "/submitSubscriptionData").withFormUrlEncodedBody(data: _*))
      status(controller.submitSubscriptionData(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }

    "return 400 when the phone number is missing" in {
      when(auth.authorise[Option[Credentials]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(Credentials("asdf", "gateway")))
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(
        messagesApi,
        subService,
        dc,
        cbcId,
        emailMock,
        cbcKF,
        bprKF,
        env,
        auditMock,
        auth,
        mcc,
        views)
      val data = Seq(
        "firstName" -> "Dave",
        "lastName"  -> "Jones",
        "email"     -> "blagh@blagh.com"
      )
      val fakeRequestSubscribe =
        addToken(FakeRequest("POST", "/submitSubscriptionData").withFormUrlEncodedBody(data: _*))
      status(controller.submitSubscriptionData(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }

    "return 400 when the phone number is invalid" in {
      when(auth.authorise[Option[Credentials]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(Credentials("asdf", "gateway")))
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(
        messagesApi,
        subService,
        dc,
        cbcId,
        emailMock,
        cbcKF,
        bprKF,
        env,
        auditMock,
        auth,
        mcc,
        views)
      val data = Seq(
        "phoneNumber" -> "I'm not a phone number",
        "firstName"   -> "Dave",
        "lastName"    -> "Jones",
        "email"       -> "blagh@blagh.com"
      )
      val fakeRequestSubscribe =
        addToken(FakeRequest("POST", "/submitSubscriptionData").withFormUrlEncodedBody(data: _*))
      status(controller.submitSubscriptionData(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }

    "return a custom error message when the phone number is invalid" in {
      when(auth.authorise[Option[Credentials]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(Credentials("asdf", "gateway")))
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(
        messagesApi,
        subService,
        dc,
        cbcId,
        emailMock,
        cbcKF,
        bprKF,
        env,
        auditMock,
        auth,
        mcc,
        views)
      val data = Seq(
        "phoneNumber" -> "I'm not a phone number",
        "firstName"   -> "Dave",
        "lastName"    -> "Jones",
        "email"       -> "blagh@blagh.com"
      )
      val fakeRequest = FakeRequest("POST", "/submitSubscriptionData").withFormUrlEncodedBody(data: _*)
      val fakeRequestSubscribe =
        addToken(FakeRequest("POST", "/submitSubscriptionData").withFormUrlEncodedBody(data: _*))
      val result: Future[Result] = controller.submitSubscriptionData(fakeRequestSubscribe)
      status(result) shouldBe Status.BAD_REQUEST
      val webPageAsString = contentAsString(result)
      webPageAsString should include(getMessages(fakeRequest)("contactInfoSubscriber.phoneNumber.error.invalid"))
      webPageAsString should include("There is a problem")
      webPageAsString should not include ("found some errors")
      webPageAsString should not include (getMessages(fakeRequest)("contactInfoSubscriber.phoneNumber.error.empty"))
    }

    "return a custom error message when the phone number is empty" in {
      when(auth.authorise[Option[Credentials]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(Credentials("asdf", "gateway")))
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(
        messagesApi,
        subService,
        dc,
        cbcId,
        emailMock,
        cbcKF,
        bprKF,
        env,
        auditMock,
        auth,
        mcc,
        views)
      val data = Seq(
        "phoneNumber" -> "",
        "firstName"   -> "Dave",
        "lastName"    -> "Jones",
        "email"       -> "blagh@blagh.com"
      )
      val fakeRequest = FakeRequest("POST", "/submitSubscriptionData").withFormUrlEncodedBody(data: _*)
      val fakeRequestSubscribe =
        addToken(FakeRequest("POST", "/submitSubscriptionData").withFormUrlEncodedBody(data: _*))
      val result: Future[Result] = controller.submitSubscriptionData(fakeRequestSubscribe)
      status(result) shouldBe Status.BAD_REQUEST
      val webPageAsString = contentAsString(result)
      webPageAsString should include(getMessages(fakeRequest)("contactInfoSubscriber.phoneNumber.error.empty"))
      webPageAsString should include("Enter the phone number")
      webPageAsString should not include ("found some errors")
      webPageAsString should not include (getMessages(fakeRequest)("contactInfoSubscriber.phoneNumber.error.invalid"))
    }

    "return a custom error message when the email  is invalid" in {
      when(auth.authorise[Option[Credentials]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(Credentials("asdf", "gateway")))
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(
        messagesApi,
        subService,
        dc,
        cbcId,
        emailMock,
        cbcKF,
        bprKF,
        env,
        auditMock,
        auth,
        mcc,
        views)
      val data = Seq(
        "phoneNumber" -> "07706641666",
        "firstName"   -> "Dave",
        "lastName"    -> "Jones",
        "email"       -> "I am not a email"
      )
      val fakeRequest = FakeRequest("POST", "/submitSubscriptionData").withFormUrlEncodedBody(data: _*)
      val fakeRequestSubscribe =
        addToken(FakeRequest("POST", "/submitSubscriptionData").withFormUrlEncodedBody(data: _*))
      val result: Future[Result] = controller.submitSubscriptionData(fakeRequestSubscribe)
      status(result) shouldBe Status.BAD_REQUEST
      val webPageAsString = contentAsString(result)
      webPageAsString should include(getMessages(fakeRequest)("contactInfoSubscriber.emailAddress.error.invalid"))
      webPageAsString should include("There is a problem")
      webPageAsString should not include ("found some errors")
      webPageAsString should not include ("entered your email address")
    }

    "return a custom error message when the first name is empty" in {
      when(auth.authorise[Option[Credentials]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(Credentials("asdf", "gateway")))
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(
        messagesApi,
        subService,
        dc,
        cbcId,
        emailMock,
        cbcKF,
        bprKF,
        env,
        auditMock,
        auth,
        mcc,
        views)
      val data = Seq(
        "phoneNumber" -> "07706641666",
        "firstName"   -> "",
        "lastName"    -> "Jones",
        "email"       -> "colm.m.cavanagh@gmail.com"
      )
      val fakeRequest = FakeRequest("POST", "/submitSubscriptionData").withFormUrlEncodedBody(data: _*)
      val fakeRequestSubscribe =
        addToken(FakeRequest("POST", "/submitSubscriptionData").withFormUrlEncodedBody(data: _*))
      val result: Future[Result] = controller.submitSubscriptionData(fakeRequestSubscribe)
      status(result) shouldBe Status.BAD_REQUEST
      val webPageAsString = contentAsString(result)
      webPageAsString should include(getMessages(fakeRequest)("contactInfoSubscriber.firstName.error"))
      webPageAsString should not include ("found some errors")
    }

    "return a custom error message when the last name is empty" in {
      when(auth.authorise[Option[Credentials]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(Credentials("asdf", "gateway")))
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(
        messagesApi,
        subService,
        dc,
        cbcId,
        emailMock,
        cbcKF,
        bprKF,
        env,
        auditMock,
        auth,
        mcc,
        views)
      val data = Seq(
        "phoneNumber" -> "07706641666",
        "firstName"   -> "Dave",
        "lastName"    -> "",
        "email"       -> "colm.m.cavanagh@gmail.com"
      )
      val fakeRequest = FakeRequest("POST", "/submitSubscriptionData").withFormUrlEncodedBody(data: _*)
      val fakeRequestSubscribe =
        addToken(FakeRequest("POST", "/submitSubscriptionData").withFormUrlEncodedBody(data: _*))
      val result: Future[Result] = controller.submitSubscriptionData(fakeRequestSubscribe)
      status(result) shouldBe Status.BAD_REQUEST
      val webPageAsString = contentAsString(result)
      webPageAsString should include(getMessages(fakeRequest)("contactInfoSubscriber.lastName.error"))
      webPageAsString should not include ("found some errors")
    }

    "return a custom error message when the email is empty" in {
      when(auth.authorise[Option[Credentials]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(Credentials("asdf", "gateway")))
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(
        messagesApi,
        subService,
        dc,
        cbcId,
        emailMock,
        cbcKF,
        bprKF,
        env,
        auditMock,
        auth,
        mcc,
        views)
      val data = Seq(
        "phoneNumber" -> "07706641666",
        "firstName"   -> "Dave",
        "lastName"    -> "Jones",
        "email"       -> ""
      )
      val fakeRequest = FakeRequest("POST", "/submitSubscriptionData").withFormUrlEncodedBody(data: _*)
      val fakeRequestSubscribe =
        addToken(FakeRequest("POST", "/submitSubscriptionData").withFormUrlEncodedBody(data: _*))
      val result: Future[Result] = controller.submitSubscriptionData(fakeRequestSubscribe)
      status(result) shouldBe Status.BAD_REQUEST
      val webPageAsString = contentAsString(result)
      webPageAsString should include("Enter the email address")
      webPageAsString should not include ("found some errors")
      webPageAsString should not include (getMessages(fakeRequest)("contactInfoSubscriber.emailAddress.error.invalid"))
    }

    "return 500 when the SubscriptionDataService errors" in {
      when(auth.authorise[Option[Credentials]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(Credentials("asdf", "gateway")))
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(
        messagesApi,
        subService,
        dc,
        cbcId,
        emailMock,
        cbcKF,
        bprKF,
        env,
        auditMock,
        auth,
        mcc,
        views)
      val sData = SubscriberContact(
        firstName = "Dave",
        lastName = "Smith",
        phoneNumber = "0207456789",
        email = EmailAddress("Bob@bob.com"))
      val dataSeq = Seq(
        "firstName"   -> sData.firstName,
        "lastName"    -> sData.lastName,
        "phoneNumber" -> sData.phoneNumber,
        "email"       -> sData.email.toString,
      )
      val fakeRequest = addToken(FakeRequest("POST", "/submitSubscriptionData").withFormUrlEncodedBody(dataSeq: _*))
      whenF(cbcId.subscribe(any())(any())) thenReturn cbcid.get
      whenF(subService.saveSubscriptionData(any(classOf[SubscriptionDetails]))(any(), any())) thenFailWith UnexpectedState("return 500 when the SubscriptionDataService errors")
      whenF(subService.clearSubscriptionData(any())(any(), any())) thenReturn None
      when(cache.readOption[Subscribed.type](EQ(Implicits.format), any(), any())) thenReturn Future.successful(None)
      whenF(cache.read[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format), EQ(bprTag), any())) thenReturn BusinessPartnerRecord("safeid", None, EtmpAddress("Line1", None, None, None, None, "GB"))
      whenF(cache.read[Utr](EQ(Utr.utrRead), EQ(utrTag), any())) thenReturn Utr("700000002")
      when(cache.readOption[GGId](EQ(GGId.format), any(), any())) thenReturn Future.successful(
        Some(GGId("ggid", "type")))
      when(auditMock.sendExtendedEvent(any())(any(), any())) thenReturn Future.successful(AuditResult.Success)
      status(controller.submitSubscriptionData(fakeRequest)) shouldBe Status.INTERNAL_SERVER_ERROR
      verify(subService).clearSubscriptionData(any())(any(), any())
      verify(auditMock).sendExtendedEvent(any())(any(), any())
    }

    "return 500 when the getCbcId call errors out" in {
      when(auth.authorise[Option[Credentials]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(Credentials("asdf", "gateway")))
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(
        messagesApi,
        subService,
        dc,
        cbcId,
        emailMock,
        cbcKF,
        bprKF,
        env,
        auditMock,
        auth,
        mcc,
        views)
      val sData = SubscriberContact("Dave", "Smith", "0207456789", EmailAddress("Bob@bob.com"))
      val dataSeq = Seq(
        "firstName"   -> sData.firstName,
        "lastName"    -> sData.lastName,
        "phoneNumber" -> sData.phoneNumber,
        "email"       -> sData.email.toString,
      )
      val fakeRequest = addToken(FakeRequest("POST", "/submitSubscriptionData").withFormUrlEncodedBody(dataSeq: _*))
      whenF(cache.read[SubscriptionDetails](EQ(SubscriptionDetails.subscriptionDetailsFormat), any(), any())) thenReturn subscriptionDetails
      when(cbcId.subscribe(any())(any())) thenReturn OptionT.none
      when(cache.readOption[Subscribed.type](EQ(Implicits.format), any(), any())) thenReturn Future.successful(None)
      whenF(cache.read[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format), EQ(bprTag), any())) thenReturn BusinessPartnerRecord("safeid", None, EtmpAddress("Line1", None, None, None, None, "GB"))
      whenF(cache.read[Utr](EQ(Utr.utrRead), EQ(utrTag), any())) thenReturn Utr("700000002")
      status(controller.submitSubscriptionData(fakeRequest)) shouldBe Status.INTERNAL_SERVER_ERROR
      verify(subService, times(0)).clearSubscriptionData(any())(any(), any())
    }

    "return 500 when the addKnownFactsToGG call errors" in {
      when(auth.authorise[Option[Credentials]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(Credentials("asdf", "gateway")))
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(
        messagesApi,
        subService,
        dc,
        cbcId,
        emailMock,
        cbcKF,
        bprKF,
        env,
        auditMock,
        auth,
        mcc,
        views)
      val sData = SubscriberContact("Dave", "Smith", "0207456789", EmailAddress("Bob@bob.com"))
      val dataSeq = Seq(
        "firstName"   -> sData.firstName,
        "lastName"    -> sData.lastName,
        "phoneNumber" -> sData.phoneNumber,
        "email"       -> sData.email.toString,
      )
      val fakeRequest = addToken(FakeRequest("POST", "/submitSubscriptionData").withFormUrlEncodedBody(dataSeq: _*))
      whenF(subService.saveSubscriptionData(any(classOf[SubscriptionDetails]))(any(), any())) thenFailWith UnexpectedState("oops")
      whenF(cbcId.subscribe(any())(any())) thenReturn CBCId("XGCBC0000000001").get
      whenF(cbcKF.enrol(any())(any())) thenFailWith UnexpectedState("oops")
      whenF(cache.read[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format), EQ(bprTag), any())) thenReturn BusinessPartnerRecord("safeid", None, EtmpAddress("Line1", None, None, None, None, "GB"))
      when(cache.readOption[Subscribed.type](EQ(Implicits.format), any(), any())) thenReturn Future.successful(None)
      whenF(cache.read[Utr](EQ(Utr.utrRead), EQ(utrTag), any())) thenReturn Utr("123456789")
      when(cache.readOption[GGId](EQ(GGId.format), any(), any())) thenReturn Future.successful(
        Some(GGId("ggid", "type")))
      when(auditMock.sendExtendedEvent(any())(any(), any())) thenReturn Future.successful(AuditResult.Success)
      whenF(subService.clearSubscriptionData(any())(any(), any())) thenReturn None
      status(controller.submitSubscriptionData(fakeRequest)) shouldBe Status.INTERNAL_SERVER_ERROR
      verify(subService).clearSubscriptionData(any())(any(), any())
      verify(auditMock).sendExtendedEvent(any())(any(), any())
    }

    "return 303 (see_other) when all params are present and valid and the SubscriptionDataService returns Ok and send an email " in {
      when(auth.authorise[Option[Credentials]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(Credentials("asdf", "gateway")))
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(
        messagesApi,
        subService,
        dc,
        cbcId,
        emailMock,
        cbcKF,
        bprKF,
        env,
        auditMock,
        auth,
        mcc,
        views)
      val sData = SubscriberContact("Dave", "Smith", "0207456789", EmailAddress("Bob@bob.com"))
      val dataSeq = Seq(
        "firstName"   -> sData.firstName,
        "lastName"    -> sData.lastName,
        "phoneNumber" -> sData.phoneNumber,
        "email"       -> sData.email.toString,
      )
      val fakeRequest = addToken(FakeRequest("POST", "/submitSubscriptionData").withFormUrlEncodedBody(dataSeq: _*))
      whenF(subService.saveSubscriptionData(any(classOf[SubscriptionDetails]))(any(), any())) thenReturn "done"
      when(cache.readOption[GGId](EQ(GGId.format), any(), any())) thenReturn Future.successful(
        Some(GGId("ggid", "type")))
      whenF(cbcId.subscribe(any())(any())) thenReturn CBCId("XGCBC0000000001").get
      whenF(cbcKF.enrol(any())(any())) thenReturn ()
      whenF(cache.read[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format), EQ(bprTag), any())) thenReturn BusinessPartnerRecord("safeid", None, EtmpAddress("Line1", None, None, None, None, "GB"))
      whenF(cache.read[Utr](EQ(Utr.utrRead), EQ(utrTag), any())) thenReturn Utr("123456789")
      when(cache.readOption[Subscribed.type](EQ(Implicits.format), any(), any())) thenReturn Future.successful(None)
      whenF(cache.read[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn cbcid.getOrElse(fail("aslkjfd"))
      when(cache.readOption[SubscriptionEmailSent](EQ(SubscriptionEmailSent.SubscriptionEmailSentFormat), any(), any())) thenReturn Future
        .successful(None)
      when(cache.save[SubscriberContact](any())(any(), any(), any())) thenReturn Future.successful(
        CacheMap("cache", Map.empty[String, JsValue]))
      when(cache.save[SubscriptionEmailSent](any())(any(), any(), any())) thenReturn Future.successful(
        CacheMap("cache", Map.empty[String, JsValue]))
      when(auditMock.sendExtendedEvent(any())(any(), any())) thenReturn Future.successful(AuditResult.Success)
      whenF(emailMock.sendEmail(any())(any())) thenReturn true
      status(controller.submitSubscriptionData(fakeRequest)) shouldBe Status.SEE_OTHER
      verify(subService, times(0)).clearSubscriptionData(any())(any(), any())
      verify(emailMock, times(1)).sendEmail(any())(any())
    }

    "not send an email if one has already been send " in {
      when(auth.authorise[Option[Credentials]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(Credentials("asdf", "gateway")))
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(
        messagesApi,
        subService,
        dc,
        cbcId,
        emailMock,
        cbcKF,
        bprKF,
        env,
        auditMock,
        auth,
        mcc,
        views)
      val sData = SubscriberContact("Dave", "Smith", "0207456789", EmailAddress("Bob@bob.com"))
      val dataSeq = Seq(
        "firstName"   -> sData.firstName,
        "lastName"    -> sData.lastName,
        "phoneNumber" -> sData.phoneNumber,
        "email"       -> sData.email.toString,
      )
      val fakeRequest = addToken(FakeRequest("POST", "/submitSubscriptionData").withFormUrlEncodedBody(dataSeq: _*))
      when(cache.readOption[GGId](EQ(GGId.format), any(), any())) thenReturn Future.successful(
        Some(GGId("ggid", "type")))
      whenF(subService.saveSubscriptionData(any(classOf[SubscriptionDetails]))(any(), any())) thenReturn "done"
      whenF(cbcId.subscribe(any())(any())) thenReturn CBCId("XGCBC0000000001").get
      whenF(cbcKF.enrol(any())(any())) thenReturn ()
      whenF(cache.read[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format), EQ(bprTag), any())) thenReturn BusinessPartnerRecord("safeid", None, EtmpAddress("Line1", None, None, None, None, "GB"))
      whenF(cache.read[Utr](EQ(Utr.utrRead), EQ(utrTag), any())) thenReturn Utr("123456789")
      when(cache.readOption[Subscribed.type](EQ(Implicits.format), any(), any())) thenReturn Future.successful(None)
      whenF(cache.read[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn cbcid.getOrElse(fail("kajsjdf"))
      when(cache.readOption[SubscriptionEmailSent](EQ(SubscriptionEmailSent.SubscriptionEmailSentFormat), any(), any())) thenReturn Future
        .successful(Some(SubscriptionEmailSent()))
      when(cache.save[SubscriberContact](any())(any(), any(), any())) thenReturn Future.successful(
        CacheMap("cache", Map.empty[String, JsValue]))
      when(cache.save[SubscriptionEmailSent](any())(any(), any(), any())) thenReturn Future.successful(
        CacheMap("cache", Map.empty[String, JsValue]))
      when(auditMock.sendExtendedEvent(any())(any(), any())) thenReturn Future.successful(AuditResult.Success)
      whenF(emailMock.sendEmail(any())(any())) thenReturn true
      status(controller.submitSubscriptionData(fakeRequest)) shouldBe Status.SEE_OTHER
      verify(subService, times(0)).clearSubscriptionData(any())(any(), any())
      verify(emailMock, times(0)).sendEmail(any())(any())
    }

    "return 500 when trying to resubmit subscription details" in {
      when(auth.authorise[Option[Credentials]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(Credentials("asdf", "gateway")))
      val subService = mock[SubscriptionDataService]
      val controller = new SubscriptionController(
        messagesApi,
        subService,
        dc,
        cbcId,
        emailMock,
        cbcKF,
        bprKF,
        env,
        auditMock,
        auth,
        mcc,
        views)
      val sData = SubscriberContact("Dave", "Smith", "0207456789", EmailAddress("Bob@bob.com"))
      val dataSeq = Seq(
        "firstName"   -> sData.firstName,
        "lastName"    -> sData.lastName,
        "phoneNumber" -> sData.phoneNumber,
        "email"       -> sData.email.toString,
      )
      val fakeRequest = addToken(FakeRequest("POST", "/submitSubscriptionData").withFormUrlEncodedBody(dataSeq: _*))
      when(cache.readOption[Subscribed.type](EQ(Implicits.format), any(), any())) thenReturn Future.successful(
        Some(Subscribed))
      status(controller.submitSubscriptionData(fakeRequest)) shouldBe Status.INTERNAL_SERVER_ERROR
    }
  }

  "DELETE to clear-subscription-data/utr" should {
    "work correctly when enabled and" when {
      System.setProperty(CbcrSwitches.clearSubscriptionDataRoute.name, "true")
      "return a 200 if data was successfully cleared" in {
        when(auth.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful(())
        val fakeRequestSubscribe = addToken(FakeRequest("DELETE", "/clear-subscription-data"))
        val u: Utr = Utr("7000000002")
        whenF(subService.clearSubscriptionData(any())(any(), any())) thenReturn Some("done")
        status(controller.clearSubscriptionData(u)(fakeRequestSubscribe)) shouldBe Status.OK
      }

      "return a 204 if data was no data to clear" in {
        when(auth.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful(())
        val fakeRequestSubscribe = addToken(FakeRequest("DELETE", "/clear-subscription-data"))
        val u: Utr = Utr("7000000002")
        whenF(subService.clearSubscriptionData(any())(any(), any())) thenReturn None
        status(controller.clearSubscriptionData(u)(fakeRequestSubscribe)) shouldBe Status.NO_CONTENT
      }

      "return a 500 if something goes wrong" in {
        when(auth.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful(())
        val fakeRequestSubscribe = addToken(FakeRequest("DELETE", "/clear-subscription-data"))
        val u: Utr = Utr("7000000002")
        whenF(subService.clearSubscriptionData(any())(any(), any())) thenFailWith UnexpectedState("oops")
        status(controller.clearSubscriptionData(u)(fakeRequestSubscribe)) shouldBe Status.INTERNAL_SERVER_ERROR
      }
    }

    "return 501 when feature-disabled" in {
      when(auth.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful(())
      System.setProperty(CbcrSwitches.clearSubscriptionDataRoute.name, "false")
      val fakeRequestSubscribe = addToken(FakeRequest("DELETE", "/clear-subscription-data"))
      val u: Utr = Utr("7000000002")
      status(controller.clearSubscriptionData(u)(fakeRequestSubscribe)) shouldBe Status.NOT_IMPLEMENTED
    }
  }

  "GET contact-info-subscriber" should {
    "return an error if the user is not subscribed" in {
      when(auth.authorise[Option[CBCEnrolment]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(CBCEnrolment(id, utr)))
      val fakeRequest = addToken(FakeRequest("GET", "contact-info-subscriber"))
      whenF(subService.retrieveSubscriptionData(any())(any(), any())) thenReturn None
      val result = controller.updateInfoSubscriber()(fakeRequest)

      status(result) shouldEqual Status.BAD_REQUEST
    }

    "return an error if there is no etmp data" in {
      when(auth.authorise[Option[CBCEnrolment]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(CBCEnrolment(id, utr)))
      val fakeRequest = addToken(FakeRequest("GET", "contact-info-subscriber"))
      whenF(subService.retrieveSubscriptionData(any())(any(), any())) thenReturn Some(subscriptionDetails)
      when(cache.save(any())(any(), any(), any())) thenReturn Future.successful(
        CacheMap("", Map.empty[String, JsValue]))
      when(cbcId.getETMPSubscriptionData(any())(any())) thenReturn OptionT.none

      val result = controller.updateInfoSubscriber()(fakeRequest)

      status(result) shouldEqual Status.BAD_REQUEST
    }

    "return OK otherwise" in {
      when(auth.authorise[Option[CBCEnrolment]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(CBCEnrolment(id, utr)))
      val fakeRequest = addToken(FakeRequest("GET", "contact-info-subscriber"))
      whenF(subService.retrieveSubscriptionData(any())(any(), any())) thenReturn Some(subscriptionDetails)
      when(cache.save(any())(any(), any(), any())) thenReturn Future.successful(
        CacheMap("", Map.empty[String, JsValue]))
      whenF(cbcId.getETMPSubscriptionData(any())(any())) thenReturn etmpSubscription

      val result = controller.updateInfoSubscriber()(fakeRequest)

      status(result) shouldEqual Status.OK
    }
  }

  "POST contact-info-subscriber" should {
    "validate the form fields and return BadRequest" when {
      "the first name is not provided" in {
        val data = Seq(
          "phoneNumber" -> "0891505050",
          "email"       -> "blagh@blagh.com",
          "lastName"    -> "Jones"
        )
        when(auth.authorise[Option[CBCEnrolment]](any(), any())(any(), any())) thenReturn Future.successful(
          Some(CBCEnrolment(id, utr)))
        val fakeRequest = addToken(FakeRequest("POST", "contact-info-subscriber").withFormUrlEncodedBody(data: _*))
        val result = call(controller.saveUpdatedInfoSubscriber, fakeRequest)
        status(result) shouldEqual Status.BAD_REQUEST
      }

      "the surname is not provided" in {
        val data = Seq(
          "phoneNumber" -> "0891505050",
          "email"       -> "blagh@blagh.com",
          "firstName"   -> "Dave"
        )
        when(auth.authorise[Option[CBCEnrolment]](any(), any())(any(), any())) thenReturn Future.successful(
          Some(CBCEnrolment(id, utr)))
        val fakeRequest = addToken(FakeRequest("POST", "contact-info-subscriber").withFormUrlEncodedBody(data: _*))
        val result = call(controller.saveUpdatedInfoSubscriber, fakeRequest)
        status(result) shouldEqual Status.BAD_REQUEST
      }

      "the phone number is not provided" in {
        val data = Seq(
          "email"     -> "blagh@blagh.com",
          "firstName" -> "Dave",
          "lastName"  -> "Jones"
        )
        when(auth.authorise[Option[CBCEnrolment]](any(), any())(any(), any())) thenReturn Future.successful(
          Some(CBCEnrolment(id, utr)))
        val fakeRequest = addToken(FakeRequest("POST", "contact-info-subscriber").withFormUrlEncodedBody(data: _*))
        val result = call(controller.saveUpdatedInfoSubscriber, fakeRequest)
        status(result) shouldEqual Status.BAD_REQUEST
      }

      "the phone number is invalid" in {
        val data = Seq(
          "phoneNumber" -> "I'm not a phone number",
          "email"       -> "blagh@blagh.com",
          "firstName"   -> "Dave",
          "lastName"    -> "Jones"
        )
        when(auth.authorise[Option[CBCEnrolment]](any(), any())(any(), any())) thenReturn Future.successful(
          Some(CBCEnrolment(id, utr)))
        val fakeRequest = addToken(FakeRequest("POST", "contact-info-subscriber").withFormUrlEncodedBody(data: _*))
        val result = call(controller.saveUpdatedInfoSubscriber, fakeRequest)
        status(result) shouldEqual Status.BAD_REQUEST
      }

      "the email is not provided" in {
        val data = Seq(
          "phoneNumber" -> "0891505050",
          "firstName"   -> "Dave",
          "lastName"    -> "Jones"
        )
        when(auth.authorise[Option[CBCEnrolment]](any(), any())(any(), any())) thenReturn Future.successful(
          Some(CBCEnrolment(id, utr)))
        val fakeRequest = addToken(FakeRequest("POST", "contact-info-subscriber").withFormUrlEncodedBody(data: _*))
        val result = call(controller.saveUpdatedInfoSubscriber, fakeRequest)
        status(result) shouldEqual Status.BAD_REQUEST
      }

      "the email is invalid" in {
        val data = Seq(
          "phoneNumber" -> "0891505050",
          "email"       -> "blagh.com",
          "firstName"   -> "Dave",
          "lastName"    -> "Jones"
        )
        when(auth.authorise[Option[CBCEnrolment]](any(), any())(any(), any())) thenReturn Future.successful(
          Some(CBCEnrolment(id, utr)))
        val fakeRequest = addToken(FakeRequest("POST", "contact-info-subscriber").withFormUrlEncodedBody(data: _*))
        val result = call(controller.saveUpdatedInfoSubscriber, fakeRequest)
        status(result) shouldEqual Status.BAD_REQUEST
      }
    }

    "call update on the ETMPSubscription data api and the internal subscription data api on the backend" in {
      val data = Seq(
        "phoneNumber" -> "0891505050",
        "email"       -> "blagh@blagh.com",
        "firstName"   -> "Dave",
        "lastName"    -> "Jones"
      )

      val fakeRequest = addToken(FakeRequest("POST", "contact-info-subscriber").withFormUrlEncodedBody(data: _*))
      when(auth.authorise[Option[CBCEnrolment]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(CBCEnrolment(id, utr)))
      whenF(cache.read[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format), EQ(bprTag), any())) thenReturn BusinessPartnerRecord("safeid", None, EtmpAddress("Line1", None, None, None, None, "GB"))
      whenF(cache.read[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn CBCId("XGCBC0000000001").getOrElse(fail("lsadkjf"))
      whenF(cbcId.updateETMPSubscriptionData(any(), any())(any())) thenReturn UpdateResponse(LocalDateTime.now())
      whenF(subService.updateSubscriptionData(any(), any())(any(), any())) thenReturn "Ok"
      val result = call(controller.saveUpdatedInfoSubscriber, fakeRequest)
      status(result) shouldEqual Status.SEE_OTHER
    }
  }
}
