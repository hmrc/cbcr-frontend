/*
 * Copyright 2024 HM Revenue & Customs
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

package uk.gov.hmrc.cbcrfrontend.auth

import java.io.File

import play.api.http.HeaderNames
import play.api.mvc.Result
import play.api.{Configuration, Environment, Mode}
import play.test.WithApplication
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class AuthRedirectsExternalSpec extends AnyWordSpec with ScalaFutures with Matchers {

  trait Dev {
    val mode = Mode.Dev
  }

  trait Prod {
    val mode = Mode.Prod
  }

  trait BaseUri {
    val strideService = "http://localhost:9041"
    val stridePath = "/stride/sign-in"
    val ggLoginService: String = "http://localhost:9553"
    val ggLoginPath: String = "/bas-gateway/sign-in"
  }

  trait Setup extends WithApplication with BaseUri {

    def mode: Mode

    def extraConfig: Map[String, Any] = Map("accountType" -> "Organisation")

    trait TestRedirects extends AuthRedirectsExternal {

      val env = Environment(new File("."), getClass.getClassLoader, mode)

      val config = Configuration.from(
        Map(
          "appName"  -> "app",
          "run.mode" -> mode.toString
        ) ++ extraConfig
      )
    }

    object Redirect extends TestRedirects

    def validate(redirect: Result)(expectedLocation: String): Unit = {
      redirect.header.status shouldBe 303
      redirect.header.headers(HeaderNames.LOCATION) shouldBe expectedLocation
    }
  }

  "redirect with defaults from config" should {

    "redirect to stride auth in Dev without failureURL" in new Setup with Dev {
      validate(Redirect.toStrideLogin("/success"))(
        expectedLocation = s"$strideService$stridePath?successURL=%2Fsuccess&origin=app"
      )
    }

    "redirect to stride auth in Dev with failureURL" in new Setup with Dev {
      validate(Redirect.toStrideLogin("/success", Some("/failure")))(
        expectedLocation = s"$strideService$stridePath?successURL=%2Fsuccess&origin=app&failureURL=%2Ffailure"
      )
    }

    "redirect to stride auth in Prod without failureURL" in new Setup with Prod {
      validate(Redirect.toStrideLogin("/success"))(expectedLocation = s"$stridePath?successURL=%2Fsuccess&origin=app")
    }

    "redirect to stride auth in Prod with failureURL" in new Setup with Prod {
      validate(Redirect.toStrideLogin("/success", Some("/failure")))(
        expectedLocation = s"$stridePath?successURL=%2Fsuccess&origin=app&failureURL=%2Ffailure"
      )
    }
  }

  "AuthRedirectsExternal" when {
    "redirecting with defaults from config" should {
      "redirect to GG login in Dev" in new Setup with Dev {
        validate(Redirect.toGGLogin("/continue"))(
          expectedLocation = s"$ggLoginService$ggLoginPath?continue_url=%2Fcontinue&origin=app&accountType=Organisation"
        )
      }

      "redirect to GG login in Prod" in new Setup with Prod {
        validate(Redirect.toGGLogin("/continue"))(
          expectedLocation = s"$ggLoginPath?continue_url=%2Fcontinue&origin=app&accountType=Organisation"
        )
      }
    }
  }
}
