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

import org.mockito.MockitoSugar
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.ScalaFutures
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.i18n.MessagesApi
import play.api.mvc.MessagesControllerComponents
import play.api.test.FakeRequest
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.util.UnitSpec
import uk.gov.hmrc.cbcrfrontend.views.Views

import scala.concurrent.ExecutionContext

class CBCEnhancementsControllerSpec
    extends UnitSpec with ScalaFutures with GuiceOneAppPerSuite with CSRFTest
    with BeforeAndAfterEach with MockitoSugar {
  private implicit val ec: ExecutionContext = app.injector.instanceOf[ExecutionContext]
  private implicit val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
  private val mcc = app.injector.instanceOf[MessagesControllerComponents]
  private val views = app.injector.instanceOf[Views]
  private implicit val feConfig: FrontendAppConfig = mock[FrontendAppConfig]

  val controller = new CBCEnhancementsController(messagesApi, mcc, views)(ec, feConfig)

  "enhancementUnavailable" should {
    "redirect user to Unauthorised for enhancement page" in {
      val request = addToken(FakeRequest())
      val result = controller.enhancementUnavailable(request)
      status(result) shouldBe Status.UNAUTHORIZED
    }
  }

}
