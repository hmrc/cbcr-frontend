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
import uk.gov.hmrc.cbcrfrontend.controllers.CSRFTest
import uk.gov.hmrc.cbcrfrontend.model.upscan.{Reference, UploadId, UploadedSuccessfully, UpscanInitiateResponse}
import uk.gov.hmrc.cbcrfrontend.util.FakeUpscanConnector
import uk.gov.hmrc.cbcrfrontend.views.html.upscan.uploadForm
import uk.gov.hmrc.http.cache.client.CacheMap

import scala.concurrent.Future

class UploadFormControllerSpec extends SpecBase with CSRFTest {

  val mockUpscanConnector: FakeUpscanConnector = app.injector.instanceOf[FakeUpscanConnector]

  override def guiceApplicationBuilder(): GuiceApplicationBuilder =
    super
      .guiceApplicationBuilder()
      .overrides(
        bind[UpscanConnector].to(mockUpscanConnector)
      )

  val upscanInitiateResponse: UpscanInitiateResponse = UpscanInitiateResponse(
    fileReference = Reference("file-reference"),
    postTarget = "target",
    formFields = Map.empty
  )

  lazy val UploadFormRoutes: String = routes.UploadFormController.onPageLoad.url

  "upload form controller" - {
    "must initiate a request to upscan to bring back an upload form" in {

      when(mockCache.save(any())(any(), any(), any()))
        .thenReturn(Future.successful(CacheMap("x", Map("x" -> Json.toJson[String]("x")))))

      val request = addToken(FakeRequest(GET, UploadFormRoutes))

      val result = route(app, request).value

      val view = app.injector.instanceOf[uploadForm]

      status(result) shouldBe OK

      contentAsString(result).contains(messages("uploadReport.mainHeading")) shouldBe true

    }
    "must return ACCEPTED from fileUploadResponse for successfully uploaded file" in {
      val uploadedSuccessfully = UploadedSuccessfully("x", "x", "http://", Some(1))
      mockUpscanConnector.statusBuffer = Some(uploadedSuccessfully)

      val url = routes.UploadFormController.fileUploadResponse(UploadId("123")).url

      val request = addToken(FakeRequest(GET, url))

      val result = route(app, request).value

      status(result) shouldBe ACCEPTED
    }
  }

}
