/*
 * Copyright 2018 HM Revenue & Customs
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

import akka.util.Timeout
import org.mockito.Matchers.{eq => EQ, _}
import org.mockito.Mockito._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mock.MockitoSugar
import org.scalatestplus.play.OneAppPerSuite
import play.api.Configuration
import play.api.http.Status
import play.api.i18n.MessagesApi
import play.api.libs.json.{JsValue, Json}
import play.api.mvc.Result
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.{AffinityGroup, AuthConnector}
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.services._
import uk.gov.hmrc.emailaddress.EmailAddress
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.cache.client.CacheMap
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{Duration, _}
import scala.concurrent.{Await, ExecutionContext, Future}

class ExitSurveyControllerSpec extends UnitSpec with ScalaFutures with OneAppPerSuite with CSRFTest with MockitoSugar with BeforeAndAfterEach{


  implicit val ec                     = app.injector.instanceOf[ExecutionContext]
  implicit val messagesApi            = app.injector.instanceOf[MessagesApi]
  implicit val authCon                = mock[AuthConnector]
  implicit val conf                   = mock[FrontendAppConfig]

  implicit val cache                  = app.injector.instanceOf[CBCSessionCache]
  val subService                      = mock[SubscriptionDataService]
  val bprKF                           = mock[BPRKnownFactsService]
  val configuration                   = mock[Configuration]
  val reDeEnrol:DeEnrolReEnrolService = mock[DeEnrolReEnrolService]
  val auditC: AuditConnector          = mock[AuditConnector]
  val runMode                         = mock[RunMode]

  val id: CBCId = CBCId.create(42).getOrElse(fail("unable to create cbcid"))
  val id2: CBCId = CBCId.create(99).getOrElse(fail("unable to create cbcid"))

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
      Some(ReportingEntity(CBC701,DocSpec(OECD1,DocRefId(docRefId).get,None),TIN("7000000002","GB"),"name")),
      List(CbcReports(DocSpec(OECD1,DocRefId(docRefId).get,None))),
      Some(AdditionalInfo(DocSpec(OECD1,DocRefId(docRefId).get,None)))
    )
  }

  override protected def afterEach(): Unit = {
    reset(cache,subService,bprKF,reDeEnrol,auditC,runMode)
    super.afterEach()
  }

//  when(cache.read[AffinityGroup](any(),any(),any())) thenReturn rightE(AffinityGroup.Organisation)

  when(cache.save[Utr](any())(any(),any(),any())) thenReturn Future.successful(CacheMap("id",Map.empty[String,JsValue]))
  when(runMode.env) thenReturn "Dev"

  val schemaVer: String = "1.0"
  when(configuration.getString(s"${runMode.env}.oecd-schema-version")) thenReturn Future.successful(Some(schemaVer))

  val controller = new ExitSurveyController(configuration, auditC)

  val utr = Utr("7000000001")
  val bpr = BusinessPartnerRecord("safeid",None,EtmpAddress("Line1",None,None,None,None,"GB"))
  val subDetails = SubscriptionDetails(
    bpr,
    SubscriberContact("firstName","lastName", "lkasjdf",EmailAddress("max@max.com")),
    Some(id),
    utr
  )

  implicit val hc = HeaderCarrier()

  implicit val timeout: Timeout = Duration.apply(20, "s")

  val sa = SurveyAnswers("","")

  val fakeSubmit = addToken(FakeRequest("POST", "/exit-survey/submit"))

  val fakeRequest  = addToken(FakeRequest("GET", "/exit-survey"))

  "GET /exit-survey" should {
    "return 200" in {
      val result: Result = Await.result(controller.doSurvey(fakeRequest), 5.second)
      status(result) shouldBe Status.OK
    }
  }

  "POST /exit-survey/submit" should {
    "return 400 when the satisfied selection is missing and not audit" in {
      val result = Await.result(
        controller.submit(fakeSubmit.withJsonBody(Json.obj())),
        5.seconds
      )
      status(result) shouldBe 400
      verify(auditC, times(0)).sendEvent(any())(any(),any())
    }
    "return a 303 to the guidance page if satisfied selection is provided and should audit" in {
      when(auditC.sendEvent(any())(any(),any())) thenReturn Future.successful(AuditResult.Success)
      val result = Await.result(
        controller.submit(fakeSubmit.withJsonBody(Json.toJson(SurveyAnswers("splendid","")))),
        5.seconds
      )
      status(result) shouldBe 303
      val redirect = result.header.headers.getOrElse("location", "")
      redirect should endWith("guidance")
      verify(auditC, times(1)).sendEvent(any())(any(),any())
    }
  }


}