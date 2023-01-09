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
import cats.data.{EitherT, OptionT}
import cats.instances.future._
import org.mockito.ArgumentMatchers.{any, eq => EQ, _}
import org.mockito.Mockito._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mockito.MockitoSugar
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.i18n.{Messages, MessagesApi}
import play.api.libs.json.JsValue
import play.api.mvc.{MessagesControllerComponents, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers.contentAsString
import play.api.{Configuration, Environment, Logger}
import uk.gov.hmrc.auth.core.{AffinityGroup, AuthConnector}
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.services._
import uk.gov.hmrc.cbcrfrontend.util.UnitSpec
import uk.gov.hmrc.cbcrfrontend.views.Views
import uk.gov.hmrc.emailaddress.EmailAddress
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.cache.client.CacheMap
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}

import scala.concurrent.duration.{Duration, _}
import scala.concurrent.{Await, ExecutionContext, Future}

class SharedControllerSpec
    extends UnitSpec with ScalaFutures with GuiceOneAppPerSuite with CSRFTest with MockitoSugar
    with BeforeAndAfterEach {

  implicit val ec = app.injector.instanceOf[ExecutionContext]
  implicit val messagesApi = app.injector.instanceOf[MessagesApi]
  implicit val cache = mock[CBCSessionCache]
  implicit val config = app.injector.instanceOf[Configuration]
  implicit val feConfig = mock[FrontendAppConfig]
  val subService = mock[SubscriptionDataService]
  val bprKF = mock[BPRKnownFactsService]
  val configuration = mock[Configuration]
  val auditC: AuditConnector = mock[AuditConnector]
  val runMode = mock[RunMode]
  val env = mock[Environment]
  val authC = mock[AuthConnector]
  val mcc = app.injector.instanceOf[MessagesControllerComponents]
  val views: Views = app.injector.instanceOf[Views]

  val id: CBCId = CBCId.create(42).getOrElse(fail("unable to create cbcid"))
  val id2: CBCId = CBCId.create(99).getOrElse(fail("unable to create cbcid"))

  val docRefId = "GB2016RGXVCBC0000000056CBC40120170311T090000X_7000000002OECD1ENTZ"

  val logger: Logger = Logger(this.getClass)

  def getMessages(r: FakeRequest[_]): Messages = messagesApi.preferred(r)

  when(cache.save[Utr](any())(any(), any(), any())) thenReturn Future.successful(
    CacheMap("id", Map.empty[String, JsValue]))
  when(runMode.env) thenReturn "Dev"

  val schemaVer: String = "2.0"

  val controller =
    new SharedController(messagesApi, subService, bprKF, auditC, env, authC, mcc, views)(cache, config, feConfig, ec)

  val utr = Utr("7000000001")
  val bpr = BusinessPartnerRecord("safeid", None, EtmpAddress("Line1", None, None, None, None, "GB"))
  val subDetails = SubscriptionDetails(
    bpr,
    SubscriberContact("firstName", "lastName", "lkasjdf", EmailAddress("max@max.com")),
    Some(id),
    utr
  )

  implicit val hc = HeaderCarrier()

  implicit val timeout: Timeout = Duration.apply(20, "s")

  val fakeRequestEnterCBCId = addToken(FakeRequest("GET", "/enter-CBCId"))

  "GET /enter-CBCId" should {
    "return 200" in {
      when(authC.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful(())
      when(cache.read(EQ(CBCId.cbcIdFormat), any(), any())) thenReturn rightE(id)
      val result = Await.result(controller.enterCBCId(fakeRequestEnterCBCId), 5.second)
      status(result) shouldBe Status.OK
    }
  }

  val fakeRequestSubmitCBCId = addToken(FakeRequest("POST", "/enter-CBCId"))

  "GET /submitCBCId" should {
    "return 400 if it was not a valid CBCId" in {
      when(authC.authorise[Option[CBCEnrolment]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(CBCEnrolment(id, utr)))
      val result = controller.submitCBCId(fakeRequestSubmitCBCId.withFormUrlEncodedBody("cbcId" -> "NOTAVALIDID"))
      status(result) shouldBe Status.BAD_REQUEST
    }
    "return 400 if the CBCId has not been registered" in {
      when(authC.authorise[Option[CBCEnrolment]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(CBCEnrolment(id, utr)))
      when(subService.retrieveSubscriptionData(any())(any(), any())) thenReturn EitherT
        .right[Future, CBCErrors, Option[SubscriptionDetails]](None)
      val result = controller.submitCBCId(fakeRequestSubmitCBCId.withFormUrlEncodedBody("cbcId" -> id.toString))
      status(result) shouldBe Status.BAD_REQUEST
    }
    "return 400 if the CBCId has been registered but doesnt match the CBCId in the bearer token" in {
      when(authC.authorise[Option[CBCEnrolment]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(CBCEnrolment(CBCId.create(99).getOrElse(fail("bad cbcid")), utr)))
      when(subService.retrieveSubscriptionData(any())(any(), any())) thenReturn EitherT
        .right[Future, CBCErrors, Option[SubscriptionDetails]](Some(subDetails))
      val result = controller.submitCBCId(fakeRequestSubmitCBCId.withFormUrlEncodedBody("cbcId" -> id.toString))
      status(result) shouldBe Status.BAD_REQUEST
    }
    "return a redirect if successful" in {
      when(authC.authorise[Option[CBCEnrolment]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(CBCEnrolment(id, utr)))
      when(subService.retrieveSubscriptionData(any())(any(), any())) thenReturn EitherT
        .right[Future, CBCErrors, Option[SubscriptionDetails]](Some(subDetails))
      when(cache.save(any())(any(), any(), any())) thenReturn Future.successful(
        CacheMap("cache", Map.empty[String, JsValue]))
      val result = controller.submitCBCId(fakeRequestSubmitCBCId.withFormUrlEncodedBody("cbcId" -> id.toString))
      status(result) shouldBe Status.SEE_OTHER
    }
  }

  val fakeRequestSignOut = addToken(FakeRequest("GET", "/signOut"))

  "GET /signOut" should {
    "return 303 to Company Auth" in {
      val guidanceUrl = "http://localhost:9696/"
      when(feConfig.cbcrFrontendHost) thenReturn "http://localhost:9696"
      when(feConfig.governmentGatewaySignOutUrl) thenReturn "http://localhost:9553"
      when(feConfig.cbcrGuidanceUrl) thenReturn guidanceUrl
      when(authC.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful(())
      val result: Result = Await.result(controller.signOut(fakeRequestSignOut), 5.second)
      status(result) shouldBe Status.SEE_OTHER
      val maybeUri = result.header.headers.getOrElse("location", "")
      logger.debug(s"location: $maybeUri")
      maybeUri shouldBe s"http://localhost:9553/bas-gateway/sign-out-without-state?continue=$guidanceUrl"

    }
  }
  "GET /known-facts-check" should {
    "return 406 if we have already subscribed" in {
      when(authC.authorise[Option[CBCEnrolment]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(CBCEnrolment(id, utr)))
      when(cache.readOption[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format), any(), any())) thenReturn Future
        .successful(Some(bpr))
      when(cache.readOption[Utr](EQ(Utr.utrRead), any(), any())) thenReturn Future.successful(Some(utr))
      val fakeRequestSubscribe = addToken(FakeRequest("GET", "/known-facts-check"))
      status(controller.verifyKnownFactsOrganisation(fakeRequestSubscribe)) shouldBe Status.NOT_ACCEPTABLE

    }
    "return 200 if we haven't already subscribed" in {
      when(authC.authorise[Option[CBCEnrolment]](any(), any())(any(), any())) thenReturn Future.successful(None)
      when(cache.readOption[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format), any(), any())) thenReturn Future
        .successful(Some(bpr))
      when(cache.readOption[Utr](EQ(Utr.utrRead), any(), any())) thenReturn Future.successful(Some(utr))
      when(auditC.sendEvent(any())(any(), any())) thenReturn Future.successful(AuditResult.Success)
      val fakeRequestSubscribe = addToken(FakeRequest("GET", "/known-facts-check"))
      status(controller.verifyKnownFactsOrganisation(fakeRequestSubscribe)) shouldBe Status.OK
    }
    "return 200 if an Agent" in {
      when(authC.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful((): Unit)
      when(cache.readOption[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format), any(), any())) thenReturn Future
        .successful(Some(bpr))
      when(cache.readOption[Utr](EQ(Utr.utrRead), any(), any())) thenReturn Future.successful(Some(utr))
      when(auditC.sendEvent(any())(any(), any())) thenReturn Future.successful(AuditResult.Success)
      val fakeRequestSubscribe = addToken(FakeRequest("GET", "/known-facts-check"))
      status(controller.verifyKnownFactsAgent(fakeRequestSubscribe)) shouldBe Status.OK
    }
  }

  "POST /checkKnownFacts" should {
    "return 400 when KnownFacts are missing" in {
      when(authC.authorise[Option[AffinityGroup]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(AffinityGroup.Organisation))
      when(cache.readOption[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format), any(), any())) thenReturn Future
        .successful(None)
      when(cache.readOption[Utr](EQ(Utr.utrRead), any(), any())) thenReturn Future.successful(None)
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/checkKnownFacts"))
      status(controller.checkKnownFacts(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }
    "return 400 when the postcode is invalid" in {
      when(authC.authorise[Option[AffinityGroup]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(AffinityGroup.Organisation))
      when(cache.readOption[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format), any(), any())) thenReturn Future
        .successful(None)
      when(cache.readOption[Utr](EQ(Utr.utrRead), any(), any())) thenReturn Future.successful(None)
      val fakeRequestSubscribe = addToken(
        FakeRequest("POST", "/checkKnownFacts")
          .withFormUrlEncodedBody("utr" -> "1234567890", "postCode" -> "NOTAPOSTCODE"))
      status(controller.checkKnownFacts(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }
    "return 400 when the utr is invalid" in {
      when(authC.authorise[Option[AffinityGroup]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(AffinityGroup.Organisation))
      when(cache.readOption[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format), any(), any())) thenReturn Future
        .successful(None)
      when(cache.readOption[Utr](EQ(Utr.utrRead), any(), any())) thenReturn Future.successful(None)
      val fakeRequestSubscribe = addToken(
        FakeRequest("POST", "/checkKnownFacts").withFormUrlEncodedBody("utr" -> "IAMNOTAUTR", "postCode" -> "SW4 6NR"))
      status(controller.checkKnownFacts(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }
    "return 404 when the utr and postcode are valid but the postcode doesn't match" in {
      when(authC.authorise[Option[AffinityGroup]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(AffinityGroup.Organisation))
      val fakeRequestSubscribe = addToken(
        FakeRequest("POST", "/checkKnownFacts").withFormUrlEncodedBody("utr" -> "7000000002", "postCode" -> "SW46NR"))
      when(cache.readOption[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format), any(), any())) thenReturn Future
        .successful(None)
      when(cache.readOption[Utr](EQ(Utr.utrRead), any(), any())) thenReturn Future.successful(None)
      when(bprKF.checkBPRKnownFacts(any())(any())) thenReturn OptionT.none[Future, BusinessPartnerRecord]
      when(subService.retrieveSubscriptionData(any())(any(), any())) thenReturn EitherT
        .right[Future, CBCErrors, Option[SubscriptionDetails]](None)
      status(controller.checkKnownFacts(fakeRequestSubscribe)) shouldBe Status.NOT_FOUND
    }
    "return 303 when we have already used that utr and we are an Organisation" in {
      val fakeRequestSubscribe = addToken(
        FakeRequest("POST", "/checkKnownFacts").withFormUrlEncodedBody("utr" -> "7000000002", "postCode" -> "SW46NR"))
      when(authC.authorise[Option[AffinityGroup]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(AffinityGroup.Organisation))
      when(cache.readOption[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format), any(), any())) thenReturn Future
        .successful(None)
      when(cache.readOption[Utr](EQ(Utr.utrRead), any(), any())) thenReturn Future.successful(None)
      when(cache.readOption[CompleteXMLInfo](EQ(CompleteXMLInfo.format), any(), any())) thenReturn Future.successful(
        None)
      val response = BusinessPartnerRecord(
        "safeid",
        Some(OrganisationResponse("My Corp")),
        EtmpAddress("line1", None, None, None, Some("SW46NR"), "GB"))
      when(bprKF.checkBPRKnownFacts(any())(any())) thenReturn OptionT.some[Future, BusinessPartnerRecord](response)
      when(cache.save[BusinessPartnerRecord](any())(any(), any(), any())) thenReturn Future.successful(
        CacheMap("cache", Map.empty[String, JsValue]))
      when(cache.save[Utr](any())(any(), any(), any())) thenReturn Future.successful(
        CacheMap("cache", Map.empty[String, JsValue]))
      when(subService.retrieveSubscriptionData(any())(any(), any())) thenReturn EitherT
        .right[Future, CBCErrors, Option[SubscriptionDetails]](Some(subDetails))
      val result = controller.checkKnownFacts(fakeRequestSubscribe)
      status(result) shouldBe Status.SEE_OTHER
      result.header.headers("Location") should endWith("/already-subscribed")

    }
    "return 303 when that utr has not been registered and we are an Agent" in {
      val response = BusinessPartnerRecord(
        "safeid",
        Some(OrganisationResponse("My Corp")),
        EtmpAddress("Line1", None, None, None, Some("SW46NR"), "GB"))
      val fakeRequestSubscribe = addToken(
        FakeRequest("POST", "/checkKnownFacts").withFormUrlEncodedBody("utr" -> "7000000002", "postCode" -> "SW46NR"))
      when(authC.authorise[Option[AffinityGroup]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(AffinityGroup.Agent))
      when(subService.retrieveSubscriptionData(any())(any(), any())) thenReturn EitherT
        .right[Future, CBCErrors, Option[SubscriptionDetails]](Some(subDetails))
      when(bprKF.checkBPRKnownFacts(any())(any())) thenReturn OptionT.some[Future, BusinessPartnerRecord](response)
      when(cache.readOption[CompleteXMLInfo](EQ(CompleteXMLInfo.format), any(), any())) thenReturn Future.successful(
        None)
      when(cache.readOption[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format), any(), any())) thenReturn Future
        .successful(None)
      when(cache.save[BusinessPartnerRecord](any())(any(), any(), any())) thenReturn Future.successful(
        CacheMap("cache", Map.empty[String, JsValue]))
      when(cache.save[Utr](any())(any(), any(), any())) thenReturn Future.successful(
        CacheMap("cache", Map.empty[String, JsValue]))
      val result = controller.checkKnownFacts(fakeRequestSubscribe)
      status(result) shouldBe Status.NOT_FOUND
    }

    "return 303 when the utr and postcode are valid" in {
      val response = BusinessPartnerRecord(
        "safeid",
        Some(OrganisationResponse("My Corp")),
        EtmpAddress("Line1", None, None, None, Some("SW46NR"), "GB"))
      val fakeRequestSubscribe = addToken(
        FakeRequest("POST", "/checkKnownFacts").withFormUrlEncodedBody("utr" -> "7000000002", "postCode" -> "SW46NR"))
      when(authC.authorise[Option[AffinityGroup]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(AffinityGroup.Organisation))
      when(bprKF.checkBPRKnownFacts(any())(any())) thenReturn OptionT.some[Future, BusinessPartnerRecord](response)
      when(cache.readOption[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format), any(), any())) thenReturn Future
        .successful(None)
      when(cache.readOption[CompleteXMLInfo](EQ(CompleteXMLInfo.format), any(), any())) thenReturn Future.successful(
        None)
      when(cache.save[Utr](any())(any(), any(), any())) thenReturn Future.successful(
        CacheMap("cache", Map.empty[String, JsValue]))
      when(subService.retrieveSubscriptionData(any())(any(), any())) thenReturn EitherT
        .right[Future, CBCErrors, Option[SubscriptionDetails]](None)
      val result = controller.checkKnownFacts(fakeRequestSubscribe)
      status(result) shouldBe Status.SEE_OTHER
      result.header.headers("Location") should endWith("/known-facts/match")
    }

    "return 303 when the utr and postcode are valid and the postcode is blank" in {
      val response = BusinessPartnerRecord(
        "safeid",
        Some(OrganisationResponse("I live far away")),
        EtmpAddress("Line1", None, None, None, None, "NL"))
      val fakeRequestSubscribe = addToken(
        FakeRequest("POST", "/checkKnownFacts").withFormUrlEncodedBody("utr" -> "7000000002", "postCode" -> ""))
      when(authC.authorise[Option[AffinityGroup]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(AffinityGroup.Organisation))
      when(bprKF.checkBPRKnownFacts(any())(any())) thenReturn OptionT.some[Future, BusinessPartnerRecord](response)
      when(cache.readOption[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format), any(), any())) thenReturn Future
        .successful(None)
      when(cache.readOption[CompleteXMLInfo](EQ(CompleteXMLInfo.format), any(), any())) thenReturn Future.successful(
        None)
      when(cache.save[Utr](any())(any(), any(), any())) thenReturn Future.successful(
        CacheMap("cache", Map.empty[String, JsValue]))
      when(subService.retrieveSubscriptionData(any())(any(), any())) thenReturn EitherT
        .right[Future, CBCErrors, Option[SubscriptionDetails]](None)
      val result = controller.checkKnownFacts(fakeRequestSubscribe)
      status(result) shouldBe Status.SEE_OTHER
      result.header.headers("Location") should endWith("/known-facts/match")
    }

    "return 404 when CBCId in KF does not match CBCId in submitted XML" in {
      val response = BusinessPartnerRecord(
        "safeid",
        Some(OrganisationResponse("My Corp")),
        EtmpAddress("Line1", None, None, None, Some("SW46NR"), "GB"))
      val fakeRequestSubscribe = addToken(
        FakeRequest("POST", "/checkKnownFacts").withFormUrlEncodedBody("utr" -> "7000000002", "postCode" -> "SW46NR"))
      when(authC.authorise[Option[AffinityGroup]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(AffinityGroup.Agent))
      when(bprKF.checkBPRKnownFacts(any())(any())) thenReturn OptionT.some[Future, BusinessPartnerRecord](response)
      when(subService.retrieveSubscriptionData(any())(any(), any())) thenReturn EitherT
        .right[Future, CBCErrors, Option[SubscriptionDetails]](Some(subDetails))
      when(cache.readOption[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format), any(), any())) thenReturn Future
        .successful(None)
      when(cache.readOption[Utr](EQ(Utr.utrRead), any(), any())) thenReturn Future.successful(None)
      when(cache.readOption[CompleteXMLInfo](EQ(CompleteXMLInfo.format), any(), any())) thenReturn Future.successful(
        None)
      when(cache.save[BusinessPartnerRecord](any())(any(), any(), any())) thenReturn Future.successful(
        CacheMap("cache", Map.empty[String, JsValue]))
      when(cache.save[Utr](any())(any(), any(), any())) thenReturn Future.successful(
        CacheMap("cache", Map.empty[String, JsValue]))
      val result = controller.checkKnownFacts(fakeRequestSubscribe)
      status(result) shouldBe Status.NOT_FOUND
    }

  }

  "Redirect calls to information pages" should {
    "redirect to error page and return 500" in {
      val request = addToken(FakeRequest())
      val result = controller.technicalDifficulties(request)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
      val webPageAsString = contentAsString(result)
      webPageAsString should include("Internal Server Error")
    }

    "redirect to sessionExpired page and return 200" in {
      val request = addToken(FakeRequest())
      val result = controller.sessionExpired(request)
      status(result) shouldBe Status.OK
      val webPageAsString = contentAsString(result)
      webPageAsString should include(getMessages(request)("sessionExpired.mainHeading"))
    }

    "redirect to GG page and return 303" in {
      val request = addToken(FakeRequest())
      val result = controller.signOutGG(request)
      status(result) shouldBe Status.SEE_OTHER
    }

    "redirect to signOutSurvey page and return 200" in {
      val request = addToken(FakeRequest())
      when(authC.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful((): Unit)
      when(feConfig.cbcrFrontendHost) thenReturn "http://localhost:9696"
      when(feConfig.governmentGatewaySignOutUrl) thenReturn "http://localhost:9553"
      val result = controller.signOutSurvey(request)
      status(result) shouldBe Status.SEE_OTHER
    }

    "keepSessionAlive returns 200" in {
      val request = addToken(FakeRequest())
      when(authC.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful((): Unit)
      val result = controller.keepSessionAlive(request)
      status(result) shouldBe Status.OK
    }

  }

  "unsupportedAffinityGroup" should {
    "return 401 for individuals" in {
      val request = addToken(FakeRequest())
      when(authC.authorise[Option[AffinityGroup]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(AffinityGroup.Individual))
      val result = controller.unsupportedAffinityGroup(request)
      status(result) shouldBe Status.UNAUTHORIZED
    }

    "return 500 when no affinity group found" in {
      val request = addToken(FakeRequest())
      when(authC.authorise[Option[AffinityGroup]](any(), any())(any(), any())) thenReturn Future.successful(None)
      val result = controller.unsupportedAffinityGroup(request)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }

    "return 401 when unexpected affinity group found" in {
      val request = addToken(FakeRequest())
      when(authC.authorise[Option[AffinityGroup]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(AffinityGroup.Organisation))
      val result = controller.unsupportedAffinityGroup(request)
      status(result) shouldBe Status.UNAUTHORIZED
    }
  }

  "knownFactsMatch" should {
    "return 500 when no affinity group found" in {
      val request = addToken(FakeRequest())
      when(authC.authorise[Option[AffinityGroup]](any(), any())(any(), any())) thenReturn Future.successful(None)
      val result = controller.knownFactsMatch(request)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }

    "return 200 when affinity group found" in {
      val request = addToken(FakeRequest())
      when(authC.authorise[Option[AffinityGroup]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(AffinityGroup.Organisation))
      when(cache.read[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format), any(), any())) thenReturn rightE(bpr)
      when(cache.read[Utr](EQ(Utr.utrRead), any(), any())) thenReturn rightE(Utr("700000002"))
      val result = controller.knownFactsMatch(request)
      status(result) shouldBe Status.OK
    }
  }

}
