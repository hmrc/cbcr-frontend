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

import org.mockito.ArgumentMatchersSugar.*
import org.mockito.IdiomaticMockito
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Configuration
import play.api.http.Status
import play.api.i18n.MessagesApi
import play.api.libs.json.Json
import play.api.mvc.MessagesControllerComponents
import play.api.test.FakeRequest
import play.api.test.Helpers.{defaultAwaitTimeout, header, status}
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.views.Views
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}

import scala.concurrent.{ExecutionContext, Future}

class ExitSurveyControllerSpec
    extends AnyWordSpecLike with Matchers with GuiceOneAppPerSuite with CSRFTest with IdiomaticMockito {

  private implicit val ec: ExecutionContext = app.injector.instanceOf[ExecutionContext]
  private implicit val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
  private implicit val conf: FrontendAppConfig = mock[FrontendAppConfig]

  private val configuration = mock[Configuration]
  private val auditC = mock[AuditConnector]
  private val mcc = app.injector.instanceOf[MessagesControllerComponents]
  private val views = app.injector.instanceOf[Views]

  private val controller = new ExitSurveyController(configuration, auditC, mcc, views)

  private val fakeSubmit = addToken(FakeRequest("POST", "/exit-survey/submit"))

  private val fakeRequest = addToken(FakeRequest("GET", "/exit-survey"))

  private val fakeAcknowledge = addToken(FakeRequest("GET", "/exit-survey/acknowledge"))

  "GET /exit-survey" should {
    "return 200" in {
      val result = controller.doSurvey().apply(fakeRequest)
      status(result) shouldBe Status.OK
    }
  }

  "POST /exit-survey/submit" should {
    "return 400 when the satisfied selection is missing and not audit" in {
      val result = controller.submit(fakeSubmit.withJsonBody(Json.obj()))
      status(result) shouldBe 400
      auditC.sendEvent(*)(*, *) wasNever called
    }

    "return a 303 to the guidance page if satisfied selection is provided and should audit" in {
      auditC.sendExtendedEvent(*)(*, *) returns Future.successful(AuditResult.Success)
      val result = controller.submit(fakeSubmit.withMethod("POST")
              .withFormUrlEncodedBody("satisfied" -> "splendid", "suggestions" -> ""))
      status(result) shouldBe 303
      header("Location", result).get endsWith "acknowledge"
      auditC.sendExtendedEvent(*)(*, *) was called
    }
  }

  "GET exit-survey/acknowledge" should {
    "return 200" in {
      val result = controller.surveyAcknowledge(fakeAcknowledge)
      status(result) shouldBe Status.OK
    }
  }
}
