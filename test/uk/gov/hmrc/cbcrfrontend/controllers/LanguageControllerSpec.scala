/*
 * Copyright 2019 HM Revenue & Customs
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

import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mockito.MockitoSugar
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.play.test.UnitSpec
import org.mockito.Mockito._
import play.api.mvc.Result
import play.api.test.FakeRequest

import scala.concurrent.duration.{Duration, _}
import scala.concurrent.{Await, ExecutionContext, Future}
import play.api.http.HeaderNames._
import uk.gov.hmrc.cbcrfrontend.util
import scala.concurrent.ExecutionContext.Implicits.global
import uk.gov.hmrc.cbcrfrontend.util.{CbcrSwitches, FeatureSwitch}




class LanguageControllerSpec extends UnitSpec with ScalaFutures with GuiceOneAppPerSuite with CSRFTest with MockitoSugar with BeforeAndAfterEach{

  implicit val conf  = mock[FrontendAppConfig]


  when(conf.fallbackURLForLanguageSwitcher) thenReturn "#"

  val controller = new LanguageController(conf)
  val requestWithReferer = addToken(FakeRequest("GET","/report/upload-form").withHeaders(REFERER -> "/somewhere"))
  val requestNoReferer = addToken(FakeRequest("GET","/report/upload-form"))

  "Switching Language" should {
    "return 303 and set lang=en " when {
      "switching to welsh enableLanguageSwitching = false and referer set in request header" in {
        val result: Result = Await.result(controller.switchToWelsh()(requestWithReferer), 5.second)
        status(result) shouldBe 303
        val headers = result.header.headers.getOrElse("Set-Cookie", "")
        headers should include("PLAY_FLASH=switching-language=true")
        headers should include("PLAY_LANG=en")
      }
    }
    "return 303 and set lang=cy " when {
      "switching to welsh enableLanguageSwitching = true and referer set in request header" in {
        FeatureSwitch.enable(FeatureSwitch("enableLanguageSwitching", true))
        val result: Result = Await.result(controller.switchToWelsh()(requestWithReferer), 5.second)
        status(result) shouldBe 303
        val headers = result.header.headers.getOrElse("Set-Cookie", "").toString
        headers should include("PLAY_FLASH=switching-language=true")
        headers should include("PLAY_LANG=cy")
      }
    }
    "return 303 and set lang=en" when {
      "switching to english enableLanguageSwitching = true and referer set in request header" in {
        FeatureSwitch.enable(FeatureSwitch("enableLanguageSwitching",true))
        val result: Result = Await.result(controller.switchToEnglish()(requestWithReferer), 5.second)
        status(result) shouldBe 303
        val headers = result.header.headers.getOrElse("Set-Cookie", "")
        headers should include("PLAY_FLASH=switching-language=true")
        headers should include("PLAY_LANG=en")
      }
    }
    "redirect to the fallbackURLForLanguageSwitcher" in {
      val result: Result = Await.result(controller.switchToWelsh()(requestNoReferer), 5.second)
      status(result) shouldBe 303
      val headers = result.header.headers.getOrElse("location", "")
      headers should equal ("#")
    }
  }

}
