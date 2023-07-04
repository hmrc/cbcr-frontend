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

package uk.gov.hmrc.cbcrfrontend.connectors

import org.mockito.MockitoSugar
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.http.HeaderNames.LOCATION
import play.api.libs.json.{JsNull, Json}
import uk.gov.hmrc.cbcrfrontend.model.{EnvelopeId, FileMetadata, UnexpectedState}
import uk.gov.hmrc.http.HttpResponse

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

class FileUploadControllerServiceConnectorSpec extends AnyWordSpec with Matchers with EitherValues with MockitoSugar {

  "A FileUploadControllerServiceConnectorSpec " should {
    "create the expected Json Object when the expiry Date is specified" in {
      val formatter = DateTimeFormatter.ofPattern("YYYY-MM-dd'T'HH:mm:ss'Z'")
      val envelopeExpiryDate = LocalDateTime.now.plusDays(7).format(formatter)

      val expectedEnvelopeRequest = Json.obj(
        "callbackUrl" -> "http://localhost:9797/cbcr/file-upload-response",
        "expiryDate"  -> s"$envelopeExpiryDate",
        "metadata" -> Json.obj(
          "application" -> "Country By Country Reporting Service"
        ),
        "constraints" -> Json.obj(
          "maxSize"        -> "50MB",
          "maxSizePerItem" -> "50MB",
          "contentTypes"   -> List("application/xml", "text/xml")
        )
      )

      val actualEnvelopeRequest =
        new FileUploadServiceConnector().envelopeRequest("http://localhost:9797", Some(envelopeExpiryDate))

      actualEnvelopeRequest should be(expectedEnvelopeRequest)
    }

    "return the expected Json Object when the expiry Date is Not specified" in {

      val expectedEnvelopeRequest = Json.obj(
        "callbackUrl" -> "http://localhost:9797/cbcr/file-upload-response",
        "metadata" -> Json.obj(
          "application" -> "Country By Country Reporting Service"
        ),
        "constraints" -> Json.obj(
          "maxSize"        -> "50MB",
          "maxSizePerItem" -> "50MB",
          "contentTypes"   -> List("application/xml", "text/xml")
        )
      )

      val actualEnvelopeRequest = new FileUploadServiceConnector().envelopeRequest("http://localhost:9797", None)

      actualEnvelopeRequest should be(expectedEnvelopeRequest)
    }

    "return the envelopeId if call to extractEnvelopId with valid location header" in {
      val response: HttpResponse = mock[HttpResponse]
      when(response.header(LOCATION))
        .thenReturn(Option("localhost:8898/file-upload/envelopes/0f23a63e-d448-41af-a159-6ba23d88943e"))

      val result = new FileUploadServiceConnector().extractEnvelopId(response)
      result should equal(Right(EnvelopeId("0f23a63e-d448-41af-a159-6ba23d88943e")))
    }

    "return an error if call to extractEnvelopId with location header but no envelopeId" in {
      val response: HttpResponse = mock[HttpResponse]
      when(response.header(LOCATION)).thenReturn(Option("localhost:8898/file-upload/envelopes"))

      val result = new FileUploadServiceConnector().extractEnvelopId(response)

      result.fold(
        cbcError =>
          cbcError.shouldBe(
            UnexpectedState(s"EnvelopeId in $LOCATION header: localhost:8898/file-upload/envelopes not found", None)),
        _ => fail("No error generated")
      )
    }

    "return an error if call to extractEnvelopId with no location header" in {
      val response: HttpResponse = mock[HttpResponse]
      when(response.header(LOCATION)).thenReturn(None)

      val result = new FileUploadServiceConnector().extractEnvelopId(response)

      result.fold(
        cbcError => cbcError.shouldBe(UnexpectedState(s"Header $LOCATION not found", None)),
        _ => fail("No error generated")
      )
    }

    "return the response body if call to extractFileUploadMessage with status = 200" in {
      val response: HttpResponse = mock[HttpResponse]
      when(response.body).thenReturn("Test Body")
      when(response.status).thenReturn(200)

      val result = new FileUploadServiceConnector().extractFileUploadMessage(response)
      result should equal(Right("Test Body"))
    }

    "return an error if call to extractFileUploadMessage with status not 200" in {
      val response: HttpResponse = mock[HttpResponse]
      when(response.body).thenReturn("Test Body")
      when(response.status).thenReturn(400)

      val result = new FileUploadServiceConnector().extractFileUploadMessage(response)

      result.fold(
        cbcError => cbcError.shouldBe(UnexpectedState("Problems uploading the file", None)),
        _ => fail("No error generated")
      )
    }

    "return the response body if call to extractEnvelopeDeleteMessage with status = 200" in {
      val response: HttpResponse = mock[HttpResponse]
      when(response.body).thenReturn("Test Body")
      when(response.status).thenReturn(200)

      val result = new FileUploadServiceConnector().extractEnvelopeDeleteMessage(response)
      result should equal(Right("Test Body"))
    }

    "return an error if call to extractEnvelopeDeleteMessage with status not 200" in {
      val response: HttpResponse = mock[HttpResponse]
      when(response.body).thenReturn("Test Body")
      when(response.status).thenReturn(400)

      val result = new FileUploadServiceConnector().extractEnvelopeDeleteMessage(response)

      result.fold(
        cbcError => cbcError.shouldBe(UnexpectedState("Problems deleting the envelope", None)),
        _ => fail("No error generated")
      )
    }

    "return the file meta data if call to extractFileMetadata with status = 200" in {
      val response: HttpResponse = mock[HttpResponse]
      val md = FileMetadata("", "", "something.xml", "", 1.0, "", JsNull, "")
      when(response.json).thenReturn(Json.toJson(md))
      when(response.status).thenReturn(200)

      val result = new FileUploadServiceConnector().extractFileMetadata(response)
      result should equal(Right(Option(md)))
    }

    "return an error if call to extractFileMetadata with status not 200" in {
      val response: HttpResponse = mock[HttpResponse]
      when(response.status).thenReturn(400)

      val result = new FileUploadServiceConnector().extractFileMetadata(response)

      result.fold(
        cbcError => cbcError.shouldBe(UnexpectedState("Problems getting File Metadata", None)),
        _ => fail("No error generated")
      )
    }
  }
}
