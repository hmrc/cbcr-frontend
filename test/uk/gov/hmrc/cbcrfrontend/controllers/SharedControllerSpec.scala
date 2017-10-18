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

import java.time.{LocalDate, LocalDateTime}

import akka.stream.{ActorMaterializer, Materializer}
import akka.util.Timeout
import org.mockito.Matchers.{eq => EQ, _}
import cats.data.{EitherT, OptionT}
import cats.instances.future._
import org.scalatest.concurrent.ScalaFutures
import org.scalatestplus.play.OneAppPerSuite
import play.api.http.Status
import play.api.i18n.MessagesApi
import play.api.libs.json.{JsValue, Json}
import play.api.test.FakeRequest
import uk.gov.hmrc.cbcrfrontend.controllers.auth.{SecuredActionsTest, TestUsers}
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.{Await, ExecutionContext, Future}
import org.mockito.Mockito._
import org.mockito.Matchers._
import org.scalatest.mock.MockitoSugar
import play.api.libs.json.{JsValue, Json}
import play.api.test.Helpers.contentAsString
import uk.gov.hmrc.cbcrfrontend.connectors.{BPRKnownFactsConnector, EnrolmentsConnector}
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.services.{BPRKnownFactsService, CBCSessionCache, SubscriptionDataService}
import uk.gov.hmrc.emailaddress.EmailAddress
import uk.gov.hmrc.http.cache.client.CacheMap
import uk.gov.hmrc.play.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.duration._
import akka.util.Timeout
import play.Logger
import play.api.Configuration
import play.api.mvc.Result
import uk.gov.hmrc.play.frontend.auth.connectors.AuthConnector

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.ExecutionContext.Implicits.global

class SharedControllerSpec extends UnitSpec with ScalaFutures with OneAppPerSuite with CSRFTest with FakeAuthConnector with MockitoSugar{

  implicit val ec = app.injector.instanceOf[ExecutionContext]
  implicit val messagesApi = app.injector.instanceOf[MessagesApi]

  implicit val authCon = authConnector(TestUsers.cbcrUser)
  val securedActions = new SecuredActionsTest(TestUsers.cbcrUser, authCon)
  implicit val cache = mock[CBCSessionCache]
  implicit val enrol = mock[EnrolmentsConnector]
  val subService = mock[SubscriptionDataService]
  val bprKF    = mock[BPRKnownFactsService]
  val configuration = mock[Configuration]

  val id: CBCId = CBCId("XGCBC0000000001").getOrElse(fail("unable to create cbcid"))

  val docRefId="GB2016RGXVCBC0000000056CBC40120170311T090000X_7000000002OECD1ENTZ"

  private lazy val keyXMLInfo = {
    XMLInfo(
      MessageSpec(
        MessageRefID("GB2016RGXVCBC0000000056CBC40120170311T090000X").getOrElse(fail("waaaaa")),
        "GB",
        CBCId.create(99).getOrElse(fail("booo")),
        LocalDateTime.now(),
        LocalDate.parse("2017-01-30"),
        None
      ),
      ReportingEntity(CBC701,DocSpec(OECD1,DocRefId(docRefId).get,None),Utr("7000000002"),"name"),
      Some(CbcReports(DocSpec(OECD1,DocRefId(docRefId).get,None))),
      Some(AdditionalInfo(DocSpec(OECD1,DocRefId(docRefId).get,None)))
    )
  }

  when(cache.read[AffinityGroup](any(),any(),any())) thenReturn Future.successful(Some(AffinityGroup("Organisation")))
  when(cache.save[Utr](any())(any(),any(),any())) thenReturn Future.successful(CacheMap("id",Map.empty[String,JsValue]))
  when(enrol.getEnrolments(any())) thenReturn Future.successful(List.empty)

  val schemaVer: String = "1.0"
  when(configuration.getString("oecd-schema-version")) thenReturn Future.successful(Some(schemaVer))

  val controller = new SharedController(securedActions, subService, enrol,authCon,bprKF,configuration)

  val subDetails = SubscriptionDetails(
    BusinessPartnerRecord("safeid",None,EtmpAddress("Line1",None,None,None,None,"GB")),
    SubscriberContact("firstName","lastName", "lkasjdf",EmailAddress("max@max.com")),
    Some(id),
    Utr("utr")
  )

  implicit val hc = HeaderCarrier()

  implicit val timeout: Timeout = Duration.apply(20, "s")

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
      when(subService.retrieveSubscriptionData(any())(any(),any())) thenReturn EitherT.right[Future,CBCErrors, Option[SubscriptionDetails]](None)
      val result = controller.submitCBCId(fakeRequestEnterCBCId.withJsonBody(Json.obj("cbcId" -> id.toString)))
      status(result) shouldBe Status.BAD_REQUEST
    }
    "return 403 if the CBCId has been registered but doesnt match the CBCId in the bearer token" in {
      when(subService.retrieveSubscriptionData(any())(any(),any())) thenReturn EitherT.right[Future,CBCErrors, Option[SubscriptionDetails]](Some(subDetails))
      when(enrol.getEnrolments(any())) thenReturn Future.successful(List(Enrolment("HMRC-CBC-ORG",List(Identifier("cbcId","XLCBC0000000006"),Identifier("utr","7000000002")))))
      val result = controller.submitCBCId(fakeRequestEnterCBCId.withJsonBody(Json.obj("cbcId" -> id.toString)))
      status(result) shouldBe Status.BAD_REQUEST
    }
    "return a redirect if successful" in {
      when(subService.retrieveSubscriptionData(any())(any(),any())) thenReturn EitherT.right[Future,CBCErrors, Option[SubscriptionDetails]](Some(subDetails))
      when(enrol.getEnrolments(any())) thenReturn Future.successful(List(Enrolment("HMRC-CBC-ORG",List(Identifier("cbcId",id.value),Identifier("utr","7000000002")))))
      val result = controller.submitCBCId(fakeRequestEnterCBCId.withJsonBody(Json.obj("cbcId" -> id.toString)))
      status(result) shouldBe Status.SEE_OTHER
    }
  }

  val fakeRequestSignOut = addToken(FakeRequest("GET", "/signOut"))

  "GET /signOut" should {
    "return 303 to Company Auth" in {
      val result: Result = Await.result(controller.signOut(fakeRequestSignOut), 5.second)
      status(result) shouldBe Status.SEE_OTHER
      val maybeUri = result.header.headers.getOrElse("location", "")
      Logger.debug(s"location: ${maybeUri}")
      maybeUri shouldBe "http://localhost:9025/gg/sign-out?continue=http://localhost:9696/country-by-country-reporting-private-beta-1/guidance"

    }
  }
   "GET /known-facts-check" should {
   "return 406 if we have already subscribed" in {
     when(enrol.getEnrolments(any())) thenReturn List(Enrolment("HMRC-CBC-ORG",List.empty))
     val fakeRequestSubscribe = addToken(FakeRequest("GET", "/known-facts-check"))
     status(controller.verifyKnownFactsOrganisation(fakeRequestSubscribe)) shouldBe Status.NOT_ACCEPTABLE

   }
    "return 200 if we haven't already subscribed" in {
      when(enrol.getEnrolments(any())) thenReturn List.empty
      val fakeRequestSubscribe = addToken(FakeRequest("GET", "/known-facts-check"))
      status(controller.verifyKnownFactsOrganisation(fakeRequestSubscribe)) shouldBe Status.OK
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
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/checkKnownFacts").withJsonBody(Json.toJson(kf)))
      when(bprKF.checkBPRKnownFacts(any())(any())) thenReturn OptionT.none[Future,BusinessPartnerRecord]
      when(subService.retrieveSubscriptionData(any())(any(),any())) thenReturn EitherT.right[Future,CBCErrors, Option[SubscriptionDetails]](None)
      status(controller.checkKnownFacts(fakeRequestSubscribe)) shouldBe Status.NOT_FOUND
    }
    "return 303 when we have already used that utr and we are an Organisation"  in {
      val kf = BPRKnownFacts(Utr("7000000002"), "SW46NR")
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/checkKnownFacts").withJsonBody(Json.toJson(kf)))
      when(cache.read[AffinityGroup](EQ(AffinityGroup.format),any(),any())) thenReturn Future.successful(Some(AffinityGroup("Organisation")))
      when(cache.read[XMLInfo](EQ(XMLInfo.format),any(),any())) thenReturn Future.successful(None)
      val response = BusinessPartnerRecord("safeid", Some(OrganisationResponse("My Corp")), EtmpAddress("line1", None, None, None, Some("SW46NR"), "GB"))
      when(bprKF.checkBPRKnownFacts(any())(any())) thenReturn OptionT.some[Future,BusinessPartnerRecord](response)
      when(cache.save[BusinessPartnerRecord](any())(any(),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      when(cache.save[Utr](any())(any(),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      when(subService.retrieveSubscriptionData(any())(any(),any())) thenReturn EitherT.right[Future,CBCErrors, Option[SubscriptionDetails]](Some(subDetails))
      val result = controller.checkKnownFacts(fakeRequestSubscribe)
      status(result) shouldBe Status.SEE_OTHER
      result.header.headers("Location") should endWith("/already-subscribed")

    }
    "return 303 when that utr has not been registered and we are an Agent"  in {
      val kf = BPRKnownFacts(Utr("7000000002"), "SW46NR")
      val response = BusinessPartnerRecord("safeid", Some(OrganisationResponse("My Corp")), EtmpAddress("Line1", None, None, None, Some("SW46NR"), "GB"))
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/checkKnownFacts").withJsonBody(Json.toJson(kf)))
      when(bprKF.checkBPRKnownFacts(any())(any())) thenReturn OptionT.some[Future,BusinessPartnerRecord](response)
      when(cache.read[AffinityGroup](EQ(AffinityGroup.format),any(),any())) thenReturn Future.successful(Some(AffinityGroup("Agent")))
      when(cache.read[XMLInfo](EQ(XMLInfo.format),any(),any())) thenReturn Future.successful(Some(keyXMLInfo))
      when(cache.save[BusinessPartnerRecord](any())(any(),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      when(cache.save[Utr](any())(any(),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      val result = controller.checkKnownFacts(fakeRequestSubscribe)
      status(result) shouldBe Status.NOT_FOUND
    }

    "return 303 when the utr and postcode are valid" in {
      val kf = BPRKnownFacts(Utr("7000000002"), "SW46NR")
      val response = BusinessPartnerRecord("safeid", Some(OrganisationResponse("My Corp")), EtmpAddress("Line1", None, None, None, Some("SW46NR"), "GB"))
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/chehttps://github.com/hmrc/cbcr/pull/14ckKnownFacts").withJsonBody(Json.toJson(kf)))
      when(bprKF.checkBPRKnownFacts(any())(any())) thenReturn OptionT.some[Future,BusinessPartnerRecord](response)
      when(cache.read[AffinityGroup](EQ(AffinityGroup.format),any(),any())) thenReturn Future.successful(Some(AffinityGroup("Organisation")))
      when(cache.read[XMLInfo](EQ(XMLInfo.format),any(),any())) thenReturn Future.successful(None)
      when(cache.save[BusinessPartnerRecord](any())(any(),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      when(cache.save[Utr](any())(any(),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      when(subService.retrieveSubscriptionData(any())(any(),any())) thenReturn EitherT.right[Future,CBCErrors, Option[SubscriptionDetails]](None)
      val result = controller.checkKnownFacts(fakeRequestSubscribe)
      status(result) shouldBe Status.SEE_OTHER
      result.header.headers("Location") should endWith("/known-facts/match")
    }

    "return 303 when CBCId in KF does not match CBCId in submitted XML"  in {
      val kf = BPRKnownFacts(Utr("7000000002"), "SW46NR")
      val response = BusinessPartnerRecord("safeid", Some(OrganisationResponse("My Corp")), EtmpAddress("Line1", None, None, None, Some("SW46NR"), "GB"))
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/checkKnownFacts").withJsonBody(Json.toJson(kf)))
      when(bprKF.checkBPRKnownFacts(any())(any())) thenReturn OptionT.some[Future,BusinessPartnerRecord](response)
      when(cache.read[AffinityGroup](EQ(AffinityGroup.format),any(),any())) thenReturn Future.successful(Some(AffinityGroup("Agent")))
      when(cache.read[XMLInfo](EQ(XMLInfo.format),any(),any())) thenReturn Future.successful(Some(keyXMLInfo.copy(reportingEntity = keyXMLInfo.reportingEntity.copy(tin = Utr("7000000003")))))
      when(cache.save[BusinessPartnerRecord](any())(any(),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      when(cache.save[Utr](any())(any(),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      val result = controller.checkKnownFacts(fakeRequestSubscribe)
      status(result) shouldBe Status.NOT_FOUND
    }

  }


}