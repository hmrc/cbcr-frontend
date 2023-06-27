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

import org.mockito.ArgumentMatchers.any
import org.mockito.MockitoSugar
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Configuration
import play.api.http.Status
import play.api.i18n.MessagesApi
import play.api.libs.json.Json
import play.api.mvc.{MessagesControllerComponents, Result}
import play.api.test.FakeRequest
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.services._
import uk.gov.hmrc.cbcrfrontend.util.UnitSpec
import uk.gov.hmrc.cbcrfrontend.views.Views
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

class ExitSurveyControllerSpec
    extends UnitSpec with GuiceOneAppPerSuite with CSRFTest with MockitoSugar {

  private implicit val ec: ExecutionContext = app.injector.instanceOf[ExecutionContext]
  private implicit val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
  private implicit val conf: FrontendAppConfig = mock[FrontendAppConfig]

  private val configuration = mock[Configuration]
  private val auditC = mock[AuditConnector]
  private val mcc = app.injector.instanceOf[MessagesControllerComponents]
  private val runMode = mock[RunMode]
  private val views = app.injector.instanceOf[Views]

  when(runMode.env) thenReturn "Dev"

  private val controller = new ExitSurveyController(configuration, auditC, mcc, views)

  private val fakeSubmit = addToken(FakeRequest("POST", "/exit-survey/submit"))

  private val fakeRequest = addToken(FakeRequest("GET", "/exit-survey"))

  private val fakeAcknowledge = addToken(FakeRequest("GET", "/exit-survey/acknowledge"))

  "GET /exit-survey" should {
    "return 200" in {
      val result: Result = Await.result(controller.doSurvey().apply(fakeRequest), 5.second)
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
      verify(auditC, times(0)).sendEvent(any())(any(), any())
    }
    "return a 303 to the guidance page if satisfied selection is provided and should audit" in {
      when(auditC.sendExtendedEvent(any())(any(), any())) thenReturn Future.successful(AuditResult.Success)
      val result = Await.result(
        controller
          .submit(
            fakeSubmit
              .withMethod("POST")
              .withFormUrlEncodedBody("satisfied" -> "splendid", "suggestions" -> "")),
        5.seconds
      )
      status(result) shouldBe 303
      val redirect = result.header.headers.getOrElse("location", "")
      redirect should endWith("acknowledge")
      verify(auditC, times(1)).sendExtendedEvent(any())(any(), any())
    }
  }

  "GET exit-survey/acknowledge" should {
    "return 200" in {
      val result: Result = Await.result(controller.surveyAcknowledge(fakeAcknowledge), 5.second)
      status(result) shouldBe Status.OK
    }
  }

}
