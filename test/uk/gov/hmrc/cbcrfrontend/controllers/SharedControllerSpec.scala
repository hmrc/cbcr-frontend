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
import cats.implicits._
import org.mockito.ArgumentMatchersSugar.*
import org.mockito.IdiomaticMockito
import org.mockito.cats.IdiomaticMockitoCats.StubbingOpsCats
import org.mockito.cats.MockitoCats
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.i18n.{Messages, MessagesApi}
import play.api.libs.json.JsValue
import play.api.mvc.MessagesControllerComponents
import play.api.test.FakeRequest
import play.api.test.Helpers.{contentAsString, header, status}
import play.api.{Configuration, Environment}
import uk.gov.hmrc.auth.core.{AffinityGroup, AuthConnector}
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.services._
import uk.gov.hmrc.cbcrfrontend.views.Views
import uk.gov.hmrc.emailaddress.EmailAddress
import uk.gov.hmrc.http.cache.client.CacheMap
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

class SharedControllerSpec
    extends AnyWordSpec with Matchers with GuiceOneAppPerSuite with CSRFTest with IdiomaticMockito with MockitoCats {

  private implicit val ec: ExecutionContext = app.injector.instanceOf[ExecutionContext]
  private implicit val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
  private implicit val cache: CBCSessionCache = mock[CBCSessionCache]
  private implicit val config: Configuration = app.injector.instanceOf[Configuration]
  private implicit val feConfig: FrontendAppConfig = mock[FrontendAppConfig]
  private val subService = mock[SubscriptionDataService]
  private val bprKF = mock[BPRKnownFactsService]
  private val auditC: AuditConnector = mock[AuditConnector]
  private val env = mock[Environment]
  private val authC = mock[AuthConnector]
  private val mcc = app.injector.instanceOf[MessagesControllerComponents]
  private val views: Views = app.injector.instanceOf[Views]

  private val id = CBCId.create(42).getOrElse(fail("unable to create cbcid"))

  private def getMessages(r: FakeRequest[_]): Messages = messagesApi.preferred(r)

  cache.save[Utr](*)(*, *, *) returns Future.successful(
    CacheMap("id", Map.empty[String, JsValue]))

  private val controller =
    new SharedController(messagesApi, subService, bprKF, auditC, env, authC, mcc, views)(cache, config, feConfig, ec)

  private val utr = Utr("7000000001")
  private val bpr = BusinessPartnerRecord("safeid", None, EtmpAddress("Line1", None, None, None, None, "GB"))
  private val subDetails = SubscriptionDetails(
    bpr,
    SubscriberContact("firstName", "lastName", "lkasjdf", EmailAddress("max@max.com")),
    Some(id),
    utr
  )

  private implicit val timeout: Timeout = Duration.apply(20, "s")

  private val fakeRequestEnterCBCId = addToken(FakeRequest("GET", "/enter-CBCId"))

  "GET /enter-CBCId" should {
    "return 200" in {
      authC.authorise[Any](*, *)(*, *) returns Future.successful(())
      cache.read[CBCId](CBCId.cbcIdFormat, *, *) returnsF id
      val result = controller.enterCBCId(fakeRequestEnterCBCId)
      status(result) shouldBe Status.OK
    }
  }

  private val fakeRequestSubmitCBCId = addToken(FakeRequest("POST", "/enter-CBCId"))

  "GET /submitCBCId" should {
    "return 400 if it was not a valid CBCId" in {
      authC.authorise[Option[CBCEnrolment]](*, *)(*, *) returns Future.successful(
        Some(CBCEnrolment(id, utr)))
      val result = controller.submitCBCId(fakeRequestSubmitCBCId.withFormUrlEncodedBody("cbcId" -> "NOTAVALIDID"))
      status(result) shouldBe Status.BAD_REQUEST
    }

    "return 400 if the CBCId has not been registered" in {
      authC.authorise[Option[CBCEnrolment]](*, *)(*, *) returns Future.successful(
        Some(CBCEnrolment(id, utr)))
      subService.retrieveSubscriptionData(*)(*, *) returnsF None
      val result = controller.submitCBCId(fakeRequestSubmitCBCId.withFormUrlEncodedBody("cbcId" -> id.toString))
      status(result) shouldBe Status.BAD_REQUEST
    }

    "return 400 if the CBCId has been registered but doesnt match the CBCId in the bearer token" in {
      authC.authorise[Option[CBCEnrolment]](*, *)(*, *) returns Future.successful(
        Some(CBCEnrolment(CBCId.create(99).getOrElse(fail("bad cbcid")), utr)))
      subService.retrieveSubscriptionData(*)(*, *) returnsF Some(subDetails)
      val result = controller.submitCBCId(fakeRequestSubmitCBCId.withFormUrlEncodedBody("cbcId" -> id.toString))
      status(result) shouldBe Status.BAD_REQUEST
    }

    "return a redirect if successful" in {
      authC.authorise[Option[CBCEnrolment]](*, *)(*, *) returns Future.successful(
        Some(CBCEnrolment(id, utr)))
      subService.retrieveSubscriptionData(*)(*, *) returnsF Some(subDetails)
      cache.save(*)(*, *, *) returns Future.successful(
        CacheMap("cache", Map.empty[String, JsValue]))
      val result = controller.submitCBCId(fakeRequestSubmitCBCId.withFormUrlEncodedBody("cbcId" -> id.toString))
      status(result) shouldBe Status.SEE_OTHER
    }
  }

  private val fakeRequestSignOut = addToken(FakeRequest("GET", "/signOut"))

  "GET /signOut" should {
    "return 303 to Company Auth" in {
      val guidanceUrl = "http://localhost:9696/"
      feConfig.cbcrFrontendHost returns "http://localhost:9696"
      feConfig.governmentGatewaySignOutUrl returns "http://localhost:9553"
      feConfig.cbcrGuidanceUrl returns guidanceUrl
      authC.authorise[Any](*, *)(*, *) returns Future.successful(())
      val result = controller.signOut(fakeRequestSignOut)
      status(result) shouldBe Status.SEE_OTHER
      header("Location", result) shouldBe Some(s"http://localhost:9553/bas-gateway/sign-out-without-state?continue=$guidanceUrl")
    }
  }

  "GET /known-facts-check" should {
    "return 406 if we have already subscribed" in {
      authC.authorise[Option[CBCEnrolment]](*, *)(*, *) returns Future.successful(
        Some(CBCEnrolment(id, utr)))
      cache.readOption[BusinessPartnerRecord](BusinessPartnerRecord.format, *, *) returns Future
        .successful(Some(bpr))
      cache.readOption[Utr](Utr.utrRead, *, *) returns Future.successful(Some(utr))
      val fakeRequestSubscribe = addToken(FakeRequest("GET", "/known-facts-check"))
      status(controller.verifyKnownFactsOrganisation(fakeRequestSubscribe)) shouldBe Status.NOT_ACCEPTABLE
    }

    "return 200 if we haven't already subscribed" in {
      authC.authorise[Option[CBCEnrolment]](*, *)(*, *) returns Future.successful(None)
      cache.readOption[BusinessPartnerRecord](BusinessPartnerRecord.format, *, *) returns Future
        .successful(Some(bpr))
      cache.readOption[Utr](Utr.utrRead, *, *) returns Future.successful(Some(utr))
      auditC.sendEvent(*)(*, *) returns Future.successful(AuditResult.Success)
      val fakeRequestSubscribe = addToken(FakeRequest("GET", "/known-facts-check"))
      status(controller.verifyKnownFactsOrganisation(fakeRequestSubscribe)) shouldBe Status.OK
    }

    "return 200 if an Agent" in {
      authC.authorise[Any](*, *)(*, *) returns Future.successful((): Unit)
      cache.readOption[BusinessPartnerRecord](BusinessPartnerRecord.format, *, *) returns Future
        .successful(Some(bpr))
      cache.readOption[Utr](Utr.utrRead, *, *) returns Future.successful(Some(utr))
      auditC.sendEvent(*)(*, *) returns Future.successful(AuditResult.Success)
      val fakeRequestSubscribe = addToken(FakeRequest("GET", "/known-facts-check"))
      status(controller.verifyKnownFactsAgent(fakeRequestSubscribe)) shouldBe Status.OK
    }
  }

  "POST /checkKnownFacts" should {
    "return 400 when KnownFacts are missing" in {
      authC.authorise[Option[AffinityGroup]](*, *)(*, *) returns Future.successful(
        Some(AffinityGroup.Organisation))
      cache.readOption[BusinessPartnerRecord](BusinessPartnerRecord.format, *, *) returns Future
        .successful(None)
      cache.readOption[Utr](Utr.utrRead, *, *) returns Future.successful(None)
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/checkKnownFacts"))
      status(controller.checkKnownFacts(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }

    "return 400 when the postcode is invalid" in {
      authC.authorise[Option[AffinityGroup]](*, *)(*, *) returns Future.successful(
        Some(AffinityGroup.Organisation))
      cache.readOption[BusinessPartnerRecord](BusinessPartnerRecord.format, *, *) returns Future
        .successful(None)
      cache.readOption[Utr](Utr.utrRead, *, *) returns Future.successful(None)
      val fakeRequestSubscribe = addToken(
        FakeRequest("POST", "/checkKnownFacts")
          .withFormUrlEncodedBody("utr" -> "1234567890", "postCode" -> "NOTAPOSTCODE"))
      status(controller.checkKnownFacts(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }

    "return 400 when the utr is invalid" in {
      authC.authorise[Option[AffinityGroup]](*, *)(*, *) returns Future.successful(
        Some(AffinityGroup.Organisation))
      cache.readOption[BusinessPartnerRecord](BusinessPartnerRecord.format, *, *) returns Future
        .successful(None)
      cache.readOption[Utr](Utr.utrRead, *, *) returns Future.successful(None)
      val fakeRequestSubscribe = addToken(
        FakeRequest("POST", "/checkKnownFacts").withFormUrlEncodedBody("utr" -> "IAMNOTAUTR", "postCode" -> "SW4 6NR"))
      status(controller.checkKnownFacts(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }

    "return 404 when the utr and postcode are valid but the postcode doesn't match" in {
      authC.authorise[Option[AffinityGroup]](*, *)(*, *) returns Future.successful(
        Some(AffinityGroup.Organisation))
      val fakeRequestSubscribe = addToken(
        FakeRequest("POST", "/checkKnownFacts").withFormUrlEncodedBody("utr" -> "7000000002", "postCode" -> "SW46NR"))
      cache.readOption[BusinessPartnerRecord](BusinessPartnerRecord.format, *, *) returns Future
        .successful(None)
      cache.readOption[Utr](Utr.utrRead, *, *) returns Future.successful(None)
      bprKF.checkBPRKnownFacts(*)(*) returns OptionT.none[Future, BusinessPartnerRecord]
      subService.retrieveSubscriptionData(*)(*, *) returnsF None
      status(controller.checkKnownFacts(fakeRequestSubscribe)) shouldBe Status.NOT_FOUND
    }

    "return 303 when we have already used that utr and we are an Organisation" in {
      val fakeRequestSubscribe = addToken(
        FakeRequest("POST", "/checkKnownFacts").withFormUrlEncodedBody("utr" -> "7000000002", "postCode" -> "SW46NR"))
      authC.authorise[Option[AffinityGroup]](*, *)(*, *) returns Future.successful(
        Some(AffinityGroup.Organisation))
      cache.readOption[BusinessPartnerRecord](BusinessPartnerRecord.format, *, *) returns Future
        .successful(None)
      cache.readOption[Utr](Utr.utrRead, *, *) returns Future.successful(None)
      cache.readOption[CompleteXMLInfo](CompleteXMLInfo.format, *, *) returns Future.successful(
        None)
      val response = BusinessPartnerRecord(
        "safeid",
        Some(OrganisationResponse("My Corp")),
        EtmpAddress("line1", None, None, None, Some("SW46NR"), "GB"))
      bprKF.checkBPRKnownFacts(*)(*) returnsF response
      cache.save[BusinessPartnerRecord](*)(*, *, *) returns Future.successful(
        CacheMap("cache", Map.empty[String, JsValue]))
      cache.save[Utr](*)(*, *, *) returns Future.successful(
        CacheMap("cache", Map.empty[String, JsValue]))
      subService.retrieveSubscriptionData(*)(*, *) returnsF Some(subDetails)
      val result = controller.checkKnownFacts(fakeRequestSubscribe)
      status(result) shouldBe Status.SEE_OTHER
      header("Location", result).get should endWith("/already-subscribed")
    }

    "return 303 when that utr has not been registered and we are an Agent" in {
      val response = BusinessPartnerRecord(
        "safeid",
        Some(OrganisationResponse("My Corp")),
        EtmpAddress("Line1", None, None, None, Some("SW46NR"), "GB"))
      val fakeRequestSubscribe = addToken(
        FakeRequest("POST", "/checkKnownFacts").withFormUrlEncodedBody("utr" -> "7000000002", "postCode" -> "SW46NR"))
      authC.authorise[Option[AffinityGroup]](*, *)(*, *) returns Future.successful(
        Some(AffinityGroup.Agent))
      subService.retrieveSubscriptionData(*)(*, *) returnsF Some(subDetails)
      bprKF.checkBPRKnownFacts(*)(*) returnsF response
      cache.readOption[CompleteXMLInfo](CompleteXMLInfo.format, *, *) returns Future.successful(
        None)
      cache.readOption[BusinessPartnerRecord](BusinessPartnerRecord.format, *, *) returns Future
        .successful(None)
      cache.save[BusinessPartnerRecord](*)(*, *, *) returns Future.successful(
        CacheMap("cache", Map.empty[String, JsValue]))
      cache.save[Utr](*)(*, *, *) returns Future.successful(
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
      authC.authorise[Option[AffinityGroup]](*, *)(*, *) returns Future.successful(
        Some(AffinityGroup.Organisation))
      bprKF.checkBPRKnownFacts(*)(*) returnsF response
      cache.readOption[BusinessPartnerRecord](BusinessPartnerRecord.format, *, *) returns Future
        .successful(None)
      cache.readOption[CompleteXMLInfo](CompleteXMLInfo.format, *, *) returns Future.successful(
        None)
      cache.save[Utr](*)(*, *, *) returns Future.successful(
        CacheMap("cache", Map.empty[String, JsValue]))
      subService.retrieveSubscriptionData(*)(*, *) returnsF None
      val result = controller.checkKnownFacts(fakeRequestSubscribe)
      status(result) shouldBe Status.SEE_OTHER
      header("Location", result).get should endWith("/known-facts/match")
    }

    "return 303 when the utr and postcode are valid and the postcode is blank" in {
      val response = BusinessPartnerRecord(
        "safeid",
        Some(OrganisationResponse("I live far away")),
        EtmpAddress("Line1", None, None, None, None, "NL"))
      val fakeRequestSubscribe = addToken(
        FakeRequest("POST", "/checkKnownFacts").withFormUrlEncodedBody("utr" -> "7000000002", "postCode" -> ""))
      authC.authorise[Option[AffinityGroup]](*, *)(*, *) returns Future.successful(
        Some(AffinityGroup.Organisation))
      bprKF.checkBPRKnownFacts(*)(*) returnsF response
      cache.readOption[BusinessPartnerRecord](BusinessPartnerRecord.format, *, *) returns Future
        .successful(None)
      cache.readOption[CompleteXMLInfo](CompleteXMLInfo.format, *, *) returns Future.successful(
        None)
      cache.save[Utr](*)(*, *, *) returns Future.successful(
        CacheMap("cache", Map.empty[String, JsValue]))
      subService.retrieveSubscriptionData(*)(*, *) returnsF None
      val result = controller.checkKnownFacts(fakeRequestSubscribe)
      status(result) shouldBe Status.SEE_OTHER
      header("Location", result).get should endWith("/known-facts/match")
    }

    "return 404 when CBCId in KF does not match CBCId in submitted XML" in {
      val response = BusinessPartnerRecord(
        "safeid",
        Some(OrganisationResponse("My Corp")),
        EtmpAddress("Line1", None, None, None, Some("SW46NR"), "GB"))
      val fakeRequestSubscribe = addToken(
        FakeRequest("POST", "/checkKnownFacts").withFormUrlEncodedBody("utr" -> "7000000002", "postCode" -> "SW46NR"))
      authC.authorise[Option[AffinityGroup]](*, *)(*, *) returns Future.successful(
        Some(AffinityGroup.Agent))
      bprKF.checkBPRKnownFacts(*)(*) returnsF response
      subService.retrieveSubscriptionData(*)(*, *) returnsF Some(subDetails)
      cache.readOption[BusinessPartnerRecord](BusinessPartnerRecord.format, *, *) returns Future
        .successful(None)
      cache.readOption[Utr](Utr.utrRead, *, *) returns Future.successful(None)
      cache.readOption[CompleteXMLInfo](CompleteXMLInfo.format, *, *) returns Future.successful(
        None)
      cache.save[BusinessPartnerRecord](*)(*, *, *) returns Future.successful(
        CacheMap("cache", Map.empty[String, JsValue]))
      cache.save[Utr](*)(*, *, *) returns Future.successful(
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
      webPageAsString should include(getMessages(request)("sessionExpired.title"))
    }

    "redirect to GG page and return 303" in {
      val request = addToken(FakeRequest())
      val result = controller.signOutGG(request)
      status(result) shouldBe Status.SEE_OTHER
    }

    "redirect to signOutSurvey page and return 200" in {
      val request = addToken(FakeRequest())
      authC.authorise[Any](*, *)(*, *) returns Future.successful((): Unit)
      feConfig.cbcrFrontendHost returns "http://localhost:9696"
      feConfig.governmentGatewaySignOutUrl returns "http://localhost:9553"
      val result = controller.signOutSurvey(request)
      status(result) shouldBe Status.SEE_OTHER
    }

    "keepSessionAlive returns 200" in {
      val request = addToken(FakeRequest())
      authC.authorise[Any](*, *)(*, *) returns Future.successful((): Unit)
      val result = controller.keepSessionAlive(request)
      status(result) shouldBe Status.OK
    }
  }

  "unsupportedAffinityGroup" should {
    "return 401 for individuals" in {
      val request = addToken(FakeRequest())
      authC.authorise[Option[AffinityGroup]](*, *)(*, *) returns Future.successful(
        Some(AffinityGroup.Individual))
      val result = controller.unsupportedAffinityGroup(request)
      status(result) shouldBe Status.UNAUTHORIZED
    }

    "return 500 when no affinity group found" in {
      val request = addToken(FakeRequest())
      authC.authorise[Option[AffinityGroup]](*, *)(*, *) returns Future.successful(None)
      val result = controller.unsupportedAffinityGroup(request)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }

    "return 401 when unexpected affinity group found" in {
      val request = addToken(FakeRequest())
      authC.authorise[Option[AffinityGroup]](*, *)(*, *) returns Future.successful(
        Some(AffinityGroup.Organisation))
      val result = controller.unsupportedAffinityGroup(request)
      status(result) shouldBe Status.UNAUTHORIZED
    }
  }

  "knownFactsMatch" should {
    "return 500 when no affinity group found" in {
      val request = addToken(FakeRequest())
      authC.authorise[Option[AffinityGroup]](*, *)(*, *) returns Future.successful(None)
      val result = controller.knownFactsMatch(request)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }

    "return 200 when affinity group found" in {
      val request = addToken(FakeRequest())
      authC.authorise[Option[AffinityGroup]](*, *)(*, *) returns Future.successful(
        Some(AffinityGroup.Organisation))
      cache.read[BusinessPartnerRecord](BusinessPartnerRecord.format, *, *) returnsF bpr
      cache.read[Utr](Utr.utrRead, *, *) returnsF Utr("700000002")
      val result = controller.knownFactsMatch(request)
      status(result) shouldBe Status.OK
    }
  }
}
