/*
 * Copyright 2017 HM Revenue & Customs
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

import org.scalatest.{EitherValues, FlatSpec, Matchers}
import uk.gov.hmrc.cbcrfrontend.exceptions.InvalidState
import uk.gov.hmrc.cbcrfrontend.model.EnvelopeId
import uk.gov.hmrc.play.http.HttpResponse


class FileUploadServiceConnectorSpec extends FlatSpec with Matchers with EitherValues {

  "createEnvelope" should "return invalid state when response is missing Location header" in {

    val responseFromFus = HttpResponse(201, responseHeaders = Map.empty[String, List[String]])

    val res = new FileUploadServiceConnector().extractEnvelopId(responseFromFus)

    res.left.value should be(InvalidState("Header Location not found"))

  }

  it should "return invalid state when Location header don't include envelopId" in {

    val responseFromFus = HttpResponse(201, responseHeaders = Map("Location" -> List("invalid-location")))

    val res = new FileUploadServiceConnector().extractEnvelopId(responseFromFus)

    res.left.value should be(InvalidState("EnvelopeId in Location header: invalid-location not found"))

  }

  it should "return envelopId" in {

    val responseFromFus = HttpResponse(201, responseHeaders = Map("Location" -> List("envelopes/123")))

    val res = new FileUploadServiceConnector().extractEnvelopId(responseFromFus)

    res.right.value should be(EnvelopeId("123"))

  }

}
