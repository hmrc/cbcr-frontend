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

package uk.gov.hmrc.cbcrfrontend.controllers.actions

import com.google.inject.Inject
import org.mockito.MockitoSugar
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Application
import play.api.http.Status.{OK, SEE_OTHER}
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.mvc.{Action, AnyContent, InjectedController, Request}
import play.api.test.FakeRequest
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.controllers.CSRFTest
import uk.gov.hmrc.cbcrfrontend.util.UnitSpec

class Harness @Inject()(cbcEnhancementAction: CBCEnhancementAction) extends InjectedController {

  def onPageLoad(): Action[AnyContent] = cbcEnhancementAction { _: Request[AnyContent] =>
    Ok
  }
}

class CBCEnhancementActionSpec
    extends UnitSpec with GuiceOneAppPerSuite with CSRFTest with MockitoSugar {

  private val mockFrontendAppConfig = mock[FrontendAppConfig]

  implicit override lazy val app: Application = GuiceApplicationBuilder()
    .overrides(bind[FrontendAppConfig].toInstance(mockFrontendAppConfig))
    .configure(Map("metrics.enabled" -> false))
    .build()

  "when CBCEnhancementFeatures are switched off" should {
    "redirect requests to unauthorised" in {
      when(mockFrontendAppConfig.cbcEnhancementFeature) thenReturn false

      val harness = app.injector.instanceOf[Harness]
      val result = harness.onPageLoad()(FakeRequest("GET", "/"))

      status(result) shouldBe SEE_OTHER

    }
  }
  "when CBCEnhancementFeatures are switched on" should {
    "allow requests through" in {
      when(mockFrontendAppConfig.cbcEnhancementFeature) thenReturn true

      val harness = app.injector.instanceOf[Harness]
      val result = harness.onPageLoad()(FakeRequest("GET", "/"))

      status(result) shouldBe OK
    }
  }
}
