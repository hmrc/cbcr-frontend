/*
 * Copyright 2021 HM Revenue & Customs
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

import org.mockito.Mockito._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mockito.MockitoSugar
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.HeaderNames._
import play.api.mvc.{MessagesControllerComponents, Result}
import play.api.test.FakeRequest
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.util.FeatureSwitch
import uk.gov.hmrc.cbcrfrontend.util.UnitSpec

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class LanguageControllerSpec
    extends UnitSpec with ScalaFutures with GuiceOneAppPerSuite with CSRFTest with MockitoSugar
    with BeforeAndAfterEach {

  implicit val conf = mock[FrontendAppConfig]
  val mcc = app.injector.instanceOf[MessagesControllerComponents]

  when(conf.fallbackURLForLanguageSwitcher) thenReturn "#"

  val controller = new LanguageController(conf, mcc)
  val requestWithReferer = addToken(FakeRequest("GET", "/report/upload-form").withHeaders(REFERER -> "/somewhere"))
  val requestNoReferer = addToken(FakeRequest("GET", "/report/upload-form"))

  "Switching Language" should {
    "return 303 and set lang=en " when {
      "switching to welsh enableLanguageSwitching = false and referer set in request header" in {
        val result: Result = Await.result(controller.switchToWelsh()(requestWithReferer), 5.second)
        status(result) shouldBe 303
        val headers = result.header.headers.getOrElse("Set-Cookie", "")

        val switchingFlashValue = result.newFlash.head.data("switching-language")
        val cookieName = result.newCookies.head.name
        val cookieValue = result.newCookies.head.value
        switchingFlashValue shouldBe "true"
        cookieName shouldBe "PLAY_LANG"
        cookieValue shouldBe "en"
      }
    }
    "return 303 and set lang=cy " when {
      "switching to welsh enableLanguageSwitching = true and referer set in request header" in {
        FeatureSwitch.enable(FeatureSwitch("enableLanguageSwitching", true))
        val result: Result = Await.result(controller.switchToWelsh()(requestWithReferer), 5.second)
        status(result) shouldBe 303

        val switchingFlashValue = result.newFlash.head.data("switching-language")
        val cookieName = result.newCookies.head.name
        val cookieValue = result.newCookies.head.value
        switchingFlashValue shouldBe "true"
        cookieName shouldBe "PLAY_LANG"
        cookieValue shouldBe "cy"
      }
    }
    "return 303 and set lang=en" when {
      "switching to english enableLanguageSwitching = true and referer set in request header" in {
        FeatureSwitch.enable(FeatureSwitch("enableLanguageSwitching", true))
        val result: Result = Await.result(controller.switchToEnglish()(requestWithReferer), 5.second)
        status(result) shouldBe 303

        val switchingFlashValue = result.newFlash.head.data("switching-language")
        val cookieName = result.newCookies.head.name
        val cookieValue = result.newCookies.head.value
        switchingFlashValue shouldBe "true"
        cookieName shouldBe "PLAY_LANG"
        cookieValue shouldBe "en"
      }
    }
    "redirect to the fallbackURLForLanguageSwitcher" in {
      val result: Result = Await.result(controller.switchToWelsh()(requestNoReferer), 5.second)
      status(result) shouldBe 303
      val headers = result.header.headers.getOrElse("location", "")
      headers should equal("#")
    }
  }

}
