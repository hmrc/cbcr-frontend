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

import org.mockito.IdiomaticMockito
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.HeaderNames._
import play.api.mvc.MessagesControllerComponents
import play.api.test.FakeRequest
import play.api.test.Helpers.{cookies, defaultAwaitTimeout, flash, header, status}
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig

import scala.concurrent.ExecutionContext.Implicits.global

class LanguageControllerSpec
    extends AnyWordSpec with Matchers with GuiceOneAppPerSuite with CSRFTest with IdiomaticMockito {

  private implicit val conf: FrontendAppConfig = mock[FrontendAppConfig]
  private val mcc = app.injector.instanceOf[MessagesControllerComponents]

  conf.fallbackURLForLanguageSwitcher returns "#"

  private val controller = new LanguageController(conf, mcc)
  private val requestWithReferer = addToken(FakeRequest("GET", "/report/upload-form").withHeaders(REFERER -> "/somewhere"))
  private val requestNoReferer = addToken(FakeRequest("GET", "/report/upload-form"))

  "Switching Language" should {
    "return 303 and set lang=en " when {
      "switching to welsh enableLanguageSwitching = false and referer set in request header" in {
        val result = controller.switchToWelsh()(requestWithReferer)
        status(result) shouldBe 303

        flash(result).get("switching-language") shouldBe Some("true")
        cookies(result).get("PLAY_LANG").get.value shouldBe "en"
      }
    }

    "return 303 and set lang=cy " when {
      "switching to english, because Welsh is not supported" in {
        val result = controller.switchToWelsh()(requestWithReferer)
        status(result) shouldBe 303

        flash(result).get("switching-language") shouldBe Some("true")
        cookies(result).get("PLAY_LANG").get.value shouldBe "en"
      }
    }

    "return 303 and set lang=en" when {
      "switching to english and referer set in request header" in {
        val result = controller.switchToEnglish()(requestWithReferer)
        status(result) shouldBe 303

        flash(result).get("switching-language") shouldBe Some("true")
        cookies(result).get("PLAY_LANG").get.value shouldBe "en"
      }
    }

    "redirect to the fallbackURLForLanguageSwitcher" in {
      val result = controller.switchToWelsh()(requestNoReferer)
      status(result) shouldBe 303
      header("Location", result) shouldBe Some("#")
    }
  }
}
