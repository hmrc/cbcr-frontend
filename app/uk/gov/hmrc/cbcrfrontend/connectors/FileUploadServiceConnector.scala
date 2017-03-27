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

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import play.api.http.HeaderNames.LOCATION
import play.api.libs.json.{JsObject, Json}
import uk.gov.hmrc.cbcrfrontend.core.Opt
import uk.gov.hmrc.cbcrfrontend.exceptions.InvalidState
import uk.gov.hmrc.cbcrfrontend.model.{EnvelopeId}
import uk.gov.hmrc.play.http.HttpResponse

class FileUploadServiceConnector() {

  val EnvelopeIdExtractor = "envelopes/([\\w\\d-]+)$".r.unanchored
  val formatter = DateTimeFormatter.ofPattern("YYYY-MM-dd'T'HH:mm:ss'Z'")


  def envelopeRequest(formTypeRef: String): JsObject = {

    def envelopeExpiryDate(numberOfDays: Int) = LocalDateTime.now.plusDays(numberOfDays).format(formatter)

    Json.obj(
      "constraints" -> Json.obj(
        "contentTypes" -> Json.arr(
          "application/xml",
          "image/jpeg"
        ),
        "maxItems" -> 5,
        "masSize" -> "30MB",
        "maxSizePerItem" -> "5MB"
      ),
      "callbackUrl" -> "https://www-dev.***REMOVED***/country-by-country-reporting/fileUploadCallback",
      "expiryDate" -> s"${envelopeExpiryDate(7)}",
      "metadata" -> Json.obj(
        "application" -> "Digital Forms Service",
        "formTypeRef" -> s"$formTypeRef"
      )
    )
  }

  def extractEnvelopId(
                        resp: HttpResponse
                      ): Opt[EnvelopeId] = {
    resp.header(LOCATION) match {
      case Some(location) => location match {
        case EnvelopeIdExtractor(envelopeId) => Right(EnvelopeId(envelopeId))
        case otherwise => Left(InvalidState(s"EnvelopeId in $LOCATION header: $location not found"))
      }
      case None => Left(InvalidState(s"Header $LOCATION not found"))

    }
  }


}
