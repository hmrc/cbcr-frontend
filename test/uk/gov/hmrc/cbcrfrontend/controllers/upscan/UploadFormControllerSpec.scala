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

package uk.gov.hmrc.cbcrfrontend.controllers.upscan

import base.SpecBase
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.when
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers.{GET, route, status, _}
import uk.gov.hmrc.cbcrfrontend.connectors.UpscanConnector
import uk.gov.hmrc.cbcrfrontend.services.CBCSessionCache
import uk.gov.hmrc.cbcrfrontend.util.FakeUpscanConnector
import uk.gov.hmrc.http.cache.client.CacheMap

import scala.concurrent.Future

class UploadFormControllerSpec extends SpecBase {

  val mockUpscanConnector: FakeUpscanConnector = app.injector.instanceOf[FakeUpscanConnector]
  val mockCache = mock[CBCSessionCache]

  override def guiceApplicationBuilder(): GuiceApplicationBuilder =
    super.
      guiceApplicationBuilder()
      .overrides(
        bind[UpscanConnector].to[FakeUpscanConnector],
        bind[CBCSessionCache].toInstance(mockCache)
      )

  lazy val UploadFormRoutes: String = routes.UploadFormController.onPageLoad.url

  "upload form controller" - {
    "must initiate a request to upscan to bring back an upload form" in {

      when(mockCache.save(any())(any(),any(),any())).thenReturn(Future.successful(CacheMap("x",Map("x"  -> Json.toJson[String]("x")))))

      val request = FakeRequest(GET, UploadFormRoutes)

      val result = route(app, request).value

      status(result) mustBe OK

    }
  }

}
