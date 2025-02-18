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

package uk.gov.hmrc.cbcrfrontend.model.upscan

import play.api.Logging
import play.api.libs.json._

import java.net.URL
import java.time.Instant
import scala.util.Try

sealed trait UpscanCallback {
  def reference: String
}

case class UpscanReadyCallback(
  reference: String,
  fileStatus: String,
  downloadUrl: URL,
  uploadDetails: UploadDetails
) extends UpscanCallback

case class UpscanFailedCallback(
  reference: String,
  failureDetails: ErrorDetails
) extends UpscanCallback

object UpscanCallback extends Logging {
  implicit val uploadDetailsFormat: Format[UploadDetails] = Json.format[UploadDetails]
  implicit val errorDetailsFormat: Format[ErrorDetails] = Json.format[ErrorDetails]
  implicit val formatURL: Format[URL] = new Format[URL] {
    override def reads(json: JsValue): JsResult[URL] = json match {
      case JsString(s) =>
        parseUrl(s)
          .map(JsSuccess(_))
          .getOrElse(JsError(Seq(JsPath() -> Seq(JsonValidationError("error.expected.url")))))
      case _ => JsError(Seq(JsPath() -> Seq(JsonValidationError("error.expected.url"))))
    }

    private def parseUrl(s: String): Option[URL] = Try(new URL(s)).toOption

    override def writes(o: URL): JsValue = JsString(o.toString)
  }

  implicit val readyCallbackBodyFormat: Format[UpscanReadyCallback] = Json.format[UpscanReadyCallback]
  implicit val failedCallbackBodyReads: Reads[UpscanFailedCallback] = Json.reads[UpscanFailedCallback]

  implicit val reads: Reads[UpscanCallback] = new Reads[UpscanCallback] {
    override def reads(json: JsValue): JsResult[UpscanCallback] = json \ "fileStatus" match {
      case JsDefined(JsString("READY")) =>
        logger.info("READY")
        implicitly[Reads[UpscanReadyCallback]].reads(json)
      case JsDefined(JsString("FAILED")) => implicitly[Reads[UpscanFailedCallback]].reads(json)
      case JsDefined(value)              => JsError(s"Invalid type discriminator: $value")
      case JsUndefined()                 => JsError(s"Missing type discriminator")
    }
  }
}

case class UploadDetails(uploadTimestamp: Instant, checksum: String, fileMimeType: String, fileName: String)

case class ErrorDetails(failureReason: String, message: String)
