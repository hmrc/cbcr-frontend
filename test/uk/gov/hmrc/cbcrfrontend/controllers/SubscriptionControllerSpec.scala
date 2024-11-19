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

import cats.data.OptionT
import cats.implicits.catsStdInstancesForFuture
import org.mockito.ArgumentMatchersSugar.{*, any}
import org.mockito.IdiomaticMockito
import org.mockito.cats.IdiomaticMockitoCats.StubbingOpsCats
import org.mockito.cats.MockitoCats
import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Play.materializer
import play.api.http.Status
import play.api.i18n.{Messages, MessagesApi}
import play.api.libs.json.JsObject
import play.api.mvc.MessagesControllerComponents
import play.api.test.FakeRequest
import play.api.test.Helpers.{await, call, contentAsString, defaultAwaitTimeout, status, writeableOf_AnyContentAsFormUrlEncoded}
import uk.gov.hmrc.auth.core.retrieve.Credentials
import uk.gov.hmrc.auth.core.{AffinityGroup, AuthConnector}
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.repositories.CBCSessionCache
import uk.gov.hmrc.cbcrfrontend.services._
import uk.gov.hmrc.cbcrfrontend.views.Views
import uk.gov.hmrc.emailaddress.EmailAddress
import uk.gov.hmrc.mongo.cache.CacheItem
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}

import java.time.{Instant, LocalDateTime}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class SubscriptionControllerSpec
    extends AnyWordSpec with Matchers with GuiceOneAppPerSuite with CSRFTest with BeforeAndAfterEach
    with IdiomaticMockito with MockitoCats {
  private val messagesApi = app.injector.instanceOf[MessagesApi]

  private def getMessages(r: FakeRequest[_]): Messages = messagesApi.preferred(r)

  private val subService = mock[SubscriptionDataService]
  private val auditMock = mock[AuditConnector]
  private implicit val feConfig: FrontendAppConfig = app.injector.instanceOf[FrontendAppConfig]

  private val cbcIdService = mock[CBCIdService]
  private val cbcKF = mock[EnrolmentsService]
  private val emailMock = mock[EmailService]
  private val cache = mock[CBCSessionCache]
  private val auth = mock[AuthConnector]
  private val mcc = app.injector.instanceOf[MessagesControllerComponents]
  private val views = app.injector.instanceOf[Views]

  private val id = CBCId.create(5678).getOrElse(fail("bad cbcId"))
  private val utr = Utr("9000000001")

  override protected def afterEach(): Unit = {
    reset(cache, subService, auditMock, cbcIdService, cache, emailMock, auth)
    super.afterEach()
  }

  cache.read[AffinityGroup](AffinityGroup.jsonFormat, *, *) returnsF AffinityGroup.Organisation

  private val controller =
    new SubscriptionController(subService, cbcIdService, emailMock, cbcKF, auditMock, auth, mcc, views, cache)

  private val cbcId = CBCId.create(1).toOption

  private val subscriptionDetails = SubscriptionDetails(
    BusinessPartnerRecord(
      "SAFEID",
      Some(OrganisationResponse("blagh")),
      EtmpAddress("Line1", None, None, None, Some("TF3 XFE"), "GB")
    ),
    SubscriberContact("Jimbo", "Jones", "phonenum", EmailAddress("test@test.com")),
    cbcId,
    Utr("7000000002")
  )

  private val etmpSubscription = ETMPSubscription(
    "safeid",
    ContactName("Firstname", "SecondName"),
    ContactDetails(EmailAddress("test@test.com"), "123456789"),
    EtmpAddress("Line1", None, None, None, None, "GB")
  )

  "GET /contactInfoSubscriber" should {
    "return 200" in {
      auth.authorise[Any](*, *)(*, *) returns Future.successful(())
      val fakeRequestSubscribe = addToken(FakeRequest("GET", "/contactInfoSubscriber"))
      status(controller.contactInfoSubscriber(fakeRequestSubscribe)) shouldBe Status.OK
    }
  }

  "POST /submitSubscriptionData" should {
    "return 400 when the there is no data" in {
      auth.authorise[Option[Credentials]](*, *)(*, *) returns Future.successful(Some(Credentials("asdf", "gateway")))
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/submitSubscriptionData"))
      status(controller.submitSubscriptionData(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }

    "return 400 when either first or last name or both are missing" in {
      auth.authorise[Option[Credentials]](*, *)(*, *) returns Future.successful(Some(Credentials("asdf", "gateway")))
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
      auth.authorise[Option[Credentials]](*, *)(*, *) returns Future.successful(Some(Credentials("asdf", "gateway")))
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
      auth.authorise[Option[Credentials]](*, *)(*, *) returns Future.successful(Some(Credentials("asdf", "gateway")))
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
      auth.authorise[Option[Credentials]](*, *)(*, *) returns Future.successful(Some(Credentials("asdf", "gateway")))
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
      auth.authorise[Option[Credentials]](*, *)(*, *) returns Future.successful(Some(Credentials("asdf", "gateway")))
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
      auth.authorise[Option[Credentials]](*, *)(*, *) returns Future.successful(Some(Credentials("asdf", "gateway")))
      val data = Seq(
        "phoneNumber" -> "I'm not a phone number",
        "firstName"   -> "Dave",
        "lastName"    -> "Jones",
        "email"       -> "blagh@blagh.com"
      )
      val fakeRequest = FakeRequest("POST", "/submitSubscriptionData").withFormUrlEncodedBody(data: _*)
      val fakeRequestSubscribe =
        addToken(FakeRequest("POST", "/submitSubscriptionData").withFormUrlEncodedBody(data: _*))
      val result = controller.submitSubscriptionData(fakeRequestSubscribe)
      status(result) shouldBe Status.BAD_REQUEST
      val webPageAsString = contentAsString(result)
      webPageAsString should include(getMessages(fakeRequest)("contactInfoSubscriber.phoneNumber.error.invalid"))
      webPageAsString should include("There is a problem")
      webPageAsString should not include "found some errors"
      webPageAsString should not include getMessages(fakeRequest)("contactInfoSubscriber.phoneNumber.error.empty")
    }

    "return a custom error message when the phone number is empty" in {
      auth.authorise[Option[Credentials]](*, *)(*, *) returns Future.successful(Some(Credentials("asdf", "gateway")))
      val data = Seq(
        "phoneNumber" -> "",
        "firstName"   -> "Dave",
        "lastName"    -> "Jones",
        "email"       -> "blagh@blagh.com"
      )
      val fakeRequest = FakeRequest("POST", "/submitSubscriptionData").withFormUrlEncodedBody(data: _*)
      val fakeRequestSubscribe =
        addToken(FakeRequest("POST", "/submitSubscriptionData").withFormUrlEncodedBody(data: _*))
      val result = controller.submitSubscriptionData(fakeRequestSubscribe)
      status(result) shouldBe Status.BAD_REQUEST
      val webPageAsString = contentAsString(result)
      webPageAsString should include(getMessages(fakeRequest)("contactInfoSubscriber.phoneNumber.error.empty"))
      webPageAsString should include("Enter the phone number")
      webPageAsString should not include "found some errors"
      webPageAsString should not include getMessages(fakeRequest)("contactInfoSubscriber.phoneNumber.error.invalid")
    }

    "return a custom error message when the email  is invalid" in {
      auth.authorise[Option[Credentials]](*, *)(*, *) returns Future.successful(Some(Credentials("asdf", "gateway")))
      val data = Seq(
        "phoneNumber" -> "07706641666",
        "firstName"   -> "Dave",
        "lastName"    -> "Jones",
        "email"       -> "I am not a email"
      )
      val fakeRequest = FakeRequest("POST", "/submitSubscriptionData").withFormUrlEncodedBody(data: _*)
      val fakeRequestSubscribe =
        addToken(FakeRequest("POST", "/submitSubscriptionData").withFormUrlEncodedBody(data: _*))
      val result = controller.submitSubscriptionData(fakeRequestSubscribe)
      status(result) shouldBe Status.BAD_REQUEST
      val webPageAsString = contentAsString(result)
      webPageAsString should include(getMessages(fakeRequest)("contactInfoSubscriber.emailAddress.error.invalid"))
      webPageAsString should include("There is a problem")
      webPageAsString should not include "found some errors"
      webPageAsString should not include "entered your email address"
    }

    "return a custom error message when the first name is empty" in {
      auth.authorise[Option[Credentials]](*, *)(*, *) returns Future.successful(Some(Credentials("asdf", "gateway")))
      val data = Seq(
        "phoneNumber" -> "07706641666",
        "firstName"   -> "",
        "lastName"    -> "Jones",
        "email"       -> "colm.m.cavanagh@gmail.com"
      )
      val fakeRequest = FakeRequest("POST", "/submitSubscriptionData").withFormUrlEncodedBody(data: _*)
      val fakeRequestSubscribe =
        addToken(FakeRequest("POST", "/submitSubscriptionData").withFormUrlEncodedBody(data: _*))
      val result = controller.submitSubscriptionData(fakeRequestSubscribe)
      status(result) shouldBe Status.BAD_REQUEST
      val webPageAsString = contentAsString(result)
      webPageAsString should include(getMessages(fakeRequest)("contactInfoSubscriber.firstName.error"))
      webPageAsString should not include "found some errors"
    }

    "return a custom error message when the last name is empty" in {
      auth.authorise[Option[Credentials]](*, *)(*, *) returns Future.successful(Some(Credentials("asdf", "gateway")))
      val data = Seq(
        "phoneNumber" -> "07706641666",
        "firstName"   -> "Dave",
        "lastName"    -> "",
        "email"       -> "colm.m.cavanagh@gmail.com"
      )
      val fakeRequest = FakeRequest("POST", "/submitSubscriptionData").withFormUrlEncodedBody(data: _*)
      val fakeRequestSubscribe =
        addToken(FakeRequest("POST", "/submitSubscriptionData").withFormUrlEncodedBody(data: _*))
      val result = controller.submitSubscriptionData(fakeRequestSubscribe)
      status(result) shouldBe Status.BAD_REQUEST
      val webPageAsString = contentAsString(result)
      webPageAsString should include(getMessages(fakeRequest)("contactInfoSubscriber.lastName.error"))
      webPageAsString should not include "found some errors"
    }

    "return a custom error message when the email is empty" in {
      auth.authorise[Option[Credentials]](*, *)(*, *) returns Future.successful(Some(Credentials("asdf", "gateway")))
      val data = Seq(
        "phoneNumber" -> "07706641666",
        "firstName"   -> "Dave",
        "lastName"    -> "Jones",
        "email"       -> ""
      )
      val fakeRequest = FakeRequest("POST", "/submitSubscriptionData").withFormUrlEncodedBody(data: _*)
      val fakeRequestSubscribe =
        addToken(FakeRequest("POST", "/submitSubscriptionData").withFormUrlEncodedBody(data: _*))
      val result = controller.submitSubscriptionData(fakeRequestSubscribe)
      status(result) shouldBe Status.BAD_REQUEST
      val webPageAsString = contentAsString(result)
      webPageAsString should include("Enter the email address")
      webPageAsString should not include "found some errors"
      webPageAsString should not include getMessages(fakeRequest)("contactInfoSubscriber.emailAddress.error.invalid")
    }

    "return 500 when the SubscriptionDataService errors" in {
      auth.authorise[Option[Credentials]](*, *)(*, *) returns Future.successful(Some(Credentials("asdf", "gateway")))
      val sData = SubscriberContact(
        firstName = "Dave",
        lastName = "Smith",
        phoneNumber = "0207456789",
        email = EmailAddress("Bob@bob.com")
      )
      val dataSeq = Seq(
        "firstName"   -> sData.firstName,
        "lastName"    -> sData.lastName,
        "phoneNumber" -> sData.phoneNumber,
        "email"       -> sData.email.toString
      )
      val fakeRequest = addToken(FakeRequest("POST", "/submitSubscriptionData").withFormUrlEncodedBody(dataSeq: _*))
      cbcIdService.subscribe(*)(*) returnsF cbcId.get
      subService.saveSubscriptionData(any[SubscriptionDetails])(*) raises UnexpectedState(
        "return 500 when the SubscriptionDataService errors"
      )
      subService.clearSubscriptionData(*)(*) returnsF None
      cache.readOption[Subscribed.type](Subscribed.format, *, *) returns Future.successful(None)
      cache.read[BusinessPartnerRecord](BusinessPartnerRecord.format, *, *) returnsF BusinessPartnerRecord(
        "safeid",
        None,
        EtmpAddress("Line1", None, None, None, None, "GB")
      )
      cache.read[Utr](Utr.format, *, *) returnsF Utr("700000002")
      cache.readOption[GGId](GGId.format, *, *) returns Future.successful(Some(GGId("ggid", "type")))
      auditMock.sendExtendedEvent(*)(*, *) returns Future.successful(AuditResult.Success)
      status(controller.submitSubscriptionData(fakeRequest)) shouldBe Status.INTERNAL_SERVER_ERROR
      subService.clearSubscriptionData(*)(*) was called
      auditMock.sendExtendedEvent(*)(*, *) was called
    }

    "return 500 when the getCbcId call errors out" in {
      auth.authorise[Option[Credentials]](*, *)(*, *) returns Future.successful(Some(Credentials("asdf", "gateway")))
      val sData = SubscriberContact("Dave", "Smith", "0207456789", EmailAddress("Bob@bob.com"))
      val dataSeq = Seq(
        "firstName"   -> sData.firstName,
        "lastName"    -> sData.lastName,
        "phoneNumber" -> sData.phoneNumber,
        "email"       -> sData.email.toString
      )
      val fakeRequest = addToken(FakeRequest("POST", "/submitSubscriptionData").withFormUrlEncodedBody(dataSeq: _*))
      cache.read[SubscriptionDetails](SubscriptionDetails.subscriptionDetailsFormat, *, *) returnsF subscriptionDetails
      cbcIdService.subscribe(*)(*) returns OptionT.none
      cache.readOption[Subscribed.type](Subscribed.format, *, *) returns Future.successful(None)
      cache.read[BusinessPartnerRecord](BusinessPartnerRecord.format, *, *) returnsF BusinessPartnerRecord(
        "safeid",
        None,
        EtmpAddress("Line1", None, None, None, None, "GB")
      )
      cache.read[Utr](Utr.format, *, *) returnsF Utr("700000002")
      status(controller.submitSubscriptionData(fakeRequest)) shouldBe Status.INTERNAL_SERVER_ERROR
      subService.clearSubscriptionData(*)(*) wasNever called
    }

    "return 500 when the addKnownFactsToGG call errors" in {
      auth.authorise[Option[Credentials]](*, *)(*, *) returns Future.successful(Some(Credentials("asdf", "gateway")))
      val sData = SubscriberContact("Dave", "Smith", "0207456789", EmailAddress("Bob@bob.com"))
      val dataSeq = Seq(
        "firstName"   -> sData.firstName,
        "lastName"    -> sData.lastName,
        "phoneNumber" -> sData.phoneNumber,
        "email"       -> sData.email.toString
      )
      val fakeRequest = addToken(FakeRequest("POST", "/submitSubscriptionData").withFormUrlEncodedBody(dataSeq: _*))
      subService.saveSubscriptionData(any[SubscriptionDetails])(*) raises UnexpectedState("oops")
      cbcIdService.subscribe(*)(*) returnsF CBCId("XGCBC0000000001").get
      cbcKF.enrol(*)(*) raises UnexpectedState("oops")
      cache.read[BusinessPartnerRecord](BusinessPartnerRecord.format, *, *) returnsF BusinessPartnerRecord(
        "safeid",
        None,
        EtmpAddress("Line1", None, None, None, None, "GB")
      )
      cache.readOption[Subscribed.type](Subscribed.format, *, *) returns Future.successful(None)
      cache.read[Utr](Utr.format, *, *) returnsF Utr("123456789")
      cache.readOption[GGId](GGId.format, *, *) returns Future.successful(Some(GGId("ggid", "type")))
      auditMock.sendExtendedEvent(*)(*, *) returns Future.successful(AuditResult.Success)
      subService.clearSubscriptionData(*)(*) returnsF None
      status(controller.submitSubscriptionData(fakeRequest)) shouldBe Status.INTERNAL_SERVER_ERROR
      subService.clearSubscriptionData(*)(*) was called
      auditMock.sendExtendedEvent(*)(*, *) was called
    }

    "return 303 (see_other) when all params are present and valid and the SubscriptionDataService returns Ok and send an email " in {
      auth.authorise[Option[Credentials]](*, *)(*, *) returns Future.successful(Some(Credentials("asdf", "gateway")))
      val sData = SubscriberContact("Dave", "Smith", "0207456789", EmailAddress("Bob@bob.com"))
      val dataSeq = Seq(
        "firstName"   -> sData.firstName,
        "lastName"    -> sData.lastName,
        "phoneNumber" -> sData.phoneNumber,
        "email"       -> sData.email.toString
      )
      val fakeRequest = addToken(FakeRequest("POST", "/submitSubscriptionData").withFormUrlEncodedBody(dataSeq: _*))
      subService.saveSubscriptionData(any[SubscriptionDetails])(*) returnsF "done"
      cache.readOption[GGId](GGId.format, *, *) returns Future.successful(Some(GGId("ggid", "type")))
      cbcIdService.subscribe(*)(*) returnsF CBCId("XGCBC0000000001").get
      cbcKF.enrol(*)(*) returnsF ()
      cache.read[BusinessPartnerRecord](BusinessPartnerRecord.format, *, *) returnsF BusinessPartnerRecord(
        "safeid",
        None,
        EtmpAddress("Line1", None, None, None, None, "GB")
      )
      cache.read[Utr](Utr.format, *, *) returnsF Utr("123456789")
      cache.readOption[Subscribed.type](Subscribed.format, *, *) returns Future.successful(None)
      cache.read[CBCId](CBCId.cbcIdFormat, *, *) returnsF cbcId.getOrElse(fail("aslkjfd"))
      cache.readOption[SubscriptionEmailSent](SubscriptionEmailSent.SubscriptionEmailSentFormat, *, *) returns Future
        .successful(None)
      cache.save(*)(*, *, *) returns Future.successful(CacheItem("", JsObject.empty, Instant.now, Instant.now))
      cache.save[SubscriptionEmailSent](*)(SubscriptionEmailSent.SubscriptionEmailSentFormat, *, *) returns Future
        .successful(CacheItem("", JsObject.empty, Instant.now, Instant.now))
      auditMock.sendExtendedEvent(*)(*, *) returns Future.successful(AuditResult.Success)
      emailMock.sendEmail(*)(*) returnsF true
      status(controller.submitSubscriptionData(fakeRequest)) shouldBe Status.SEE_OTHER
      subService.clearSubscriptionData(*)(*) wasNever called
      emailMock.sendEmail(*)(*) was called
    }

    "not send an email if one has already been send " in {
      auth.authorise[Option[Credentials]](*, *)(*, *) returns Future.successful(Some(Credentials("asdf", "gateway")))
      val sData = SubscriberContact("Dave", "Smith", "0207456789", EmailAddress("Bob@bob.com"))
      val dataSeq = Seq(
        "firstName"   -> sData.firstName,
        "lastName"    -> sData.lastName,
        "phoneNumber" -> sData.phoneNumber,
        "email"       -> sData.email.toString
      )
      val fakeRequest = addToken(FakeRequest("POST", "/submitSubscriptionData").withFormUrlEncodedBody(dataSeq: _*))
      cache.readOption[GGId](GGId.format, *, *) returns Future.successful(Some(GGId("ggid", "type")))
      subService.saveSubscriptionData(any[SubscriptionDetails])(*) returnsF "done"
      cbcIdService.subscribe(*)(*) returnsF CBCId("XGCBC0000000001").get
      cbcKF.enrol(*)(*) returnsF ()
      cache.read[BusinessPartnerRecord](BusinessPartnerRecord.format, *, *) returnsF BusinessPartnerRecord(
        "safeid",
        None,
        EtmpAddress("Line1", None, None, None, None, "GB")
      )
      cache.read[Utr](Utr.format, *, *) returnsF Utr("123456789")
      cache.readOption[Subscribed.type](Subscribed.format, *, *) returns Future.successful(None)
      cache.read[CBCId](CBCId.cbcIdFormat, *, *) returnsF cbcId.getOrElse(fail("kajsjdf"))
      cache.readOption[SubscriptionEmailSent](SubscriptionEmailSent.SubscriptionEmailSentFormat, *, *) returns Future
        .successful(Some(SubscriptionEmailSent()))
      cache.save(*)(*, *, *) returns Future.successful(CacheItem("", JsObject.empty, Instant.now, Instant.now))
      cache.save[SubscriptionEmailSent](*)(SubscriptionEmailSent.SubscriptionEmailSentFormat, *, *) returns Future
        .successful(CacheItem("", JsObject.empty, Instant.now, Instant.now))
      auditMock.sendExtendedEvent(*)(*, *) returns Future.successful(AuditResult.Success)
      emailMock.sendEmail(*)(*) returnsF true
      status(controller.submitSubscriptionData(fakeRequest)) shouldBe Status.SEE_OTHER
      subService.clearSubscriptionData(*)(*) wasNever called
      emailMock.sendEmail(*)(*) wasNever called
    }

    "return 500 when trying to resubmit subscription details" in {
      auth.authorise[Option[Credentials]](*, *)(*, *) returns Future.successful(Some(Credentials("asdf", "gateway")))
      val sData = SubscriberContact("Dave", "Smith", "0207456789", EmailAddress("Bob@bob.com"))
      val dataSeq = Seq(
        "firstName"   -> sData.firstName,
        "lastName"    -> sData.lastName,
        "phoneNumber" -> sData.phoneNumber,
        "email"       -> sData.email.toString
      )
      val fakeRequest = addToken(FakeRequest("POST", "/submitSubscriptionData").withFormUrlEncodedBody(dataSeq: _*))
      cache.readOption[Subscribed.type](Subscribed.format, *, *) returns Future.successful(Some(Subscribed))
      status(controller.submitSubscriptionData(fakeRequest)) shouldBe Status.INTERNAL_SERVER_ERROR
    }
  }

  "GET contact-info-subscriber" should {
    "return an error if the user is not subscribed" in {
      auth.authorise[Option[CBCEnrolment]](*, *)(*, *) returns Future.successful(Some(CBCEnrolment(id, utr)))
      val fakeRequest = addToken(FakeRequest("GET", "contact-info-subscriber"))
      subService.retrieveSubscriptionData(*)(*) returnsF None
      val result = controller.getUpdateInfoSubscriber()(fakeRequest)

      status(result) shouldEqual Status.INTERNAL_SERVER_ERROR
    }

    "Redirect to the support page if etmp details are empty" in {
      auth.authorise[Option[CBCEnrolment]](*, *)(*, *) returns Future.successful(Some(CBCEnrolment(id, utr)))
      val fakeRequest = addToken(FakeRequest("GET", "contact-info-subscriber"))
      subService.retrieveSubscriptionData(*)(*) returnsF Some(subscriptionDetails)
      cache.save(*)(*, *, *) returns Future.successful(CacheItem("", JsObject.empty, Instant.now, Instant.now))
      cbcIdService.getETMPSubscriptionData(*)(*) returns OptionT.none[Future, ETMPSubscription]

      val result = await(controller.getUpdateInfoSubscriber()(fakeRequest))

      result.header.status shouldEqual Status.SEE_OTHER
      result.header.headers shouldEqual Map("Location" -> routes.SharedController.contactDetailsError.url)
    }

    "return OK otherwise" in {
      auth.authorise[Option[CBCEnrolment]](*, *)(*, *) returns Future.successful(Some(CBCEnrolment(id, utr)))
      val fakeRequest = addToken(FakeRequest("GET", "contact-info-subscriber"))
      subService.retrieveSubscriptionData(*)(*) returnsF Some(subscriptionDetails)
      cache.save(*)(*, *, *) returns Future.successful(CacheItem("", JsObject.empty, Instant.now, Instant.now))
      cbcIdService.getETMPSubscriptionData(*)(*) returnsF etmpSubscription

      val result = controller.getUpdateInfoSubscriber()(fakeRequest)

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
        auth.authorise[Option[CBCEnrolment]](*, *)(*, *) returns Future.successful(Some(CBCEnrolment(id, utr)))
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
        auth.authorise[Option[CBCEnrolment]](*, *)(*, *) returns Future.successful(Some(CBCEnrolment(id, utr)))
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
        auth.authorise[Option[CBCEnrolment]](*, *)(*, *) returns Future.successful(Some(CBCEnrolment(id, utr)))
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
        auth.authorise[Option[CBCEnrolment]](*, *)(*, *) returns Future.successful(Some(CBCEnrolment(id, utr)))
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
        auth.authorise[Option[CBCEnrolment]](*, *)(*, *) returns Future.successful(Some(CBCEnrolment(id, utr)))
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
        auth.authorise[Option[CBCEnrolment]](*, *)(*, *) returns Future.successful(Some(CBCEnrolment(id, utr)))
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
      auth.authorise[Option[CBCEnrolment]](*, *)(*, *) returns Future.successful(Some(CBCEnrolment(id, utr)))
      cache.read[BusinessPartnerRecord](BusinessPartnerRecord.format, *, *) returnsF BusinessPartnerRecord(
        "safeid",
        None,
        EtmpAddress("Line1", None, None, None, None, "GB")
      )
      cache.read[CBCId](CBCId.cbcIdFormat, *, *) returnsF CBCId("XGCBC0000000001").getOrElse(fail("lsadkjf"))
      cbcIdService.updateETMPSubscriptionData(*, *)(*) returnsF UpdateResponse(LocalDateTime.now())
      subService.updateSubscriptionData(*, *)(*) returnsF "Ok"
      val result = call(controller.saveUpdatedInfoSubscriber, fakeRequest)
      status(result) shouldEqual Status.SEE_OTHER
    }
  }
}
