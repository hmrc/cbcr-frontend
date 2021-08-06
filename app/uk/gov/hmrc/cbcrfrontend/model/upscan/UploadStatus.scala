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

package uk.gov.hmrc.cbcrfrontend.model.upscan

import play.api.libs.json._

sealed trait UploadStatus
case object NotStarted extends UploadStatus
case object InProgress extends UploadStatus
case object Failed extends UploadStatus
case object Quarantined extends UploadStatus

case class UploadedSuccessfully(name: String, downloadUrl: String) extends UploadStatus

case class UploadRejected(details: ErrorDetails) extends UploadStatus

object UploadStatus {

  implicit val uploadedSuccessfullyFormat: OFormat[UploadedSuccessfully] = Json.format[UploadedSuccessfully]
  implicit val uploadRejectedFormat: OFormat[UploadRejected] = Json.format[UploadRejected]

  implicit val read: Reads[UploadStatus] = new Reads[UploadStatus] {

    override def reads(json: JsValue): JsResult[UploadStatus] = {
      val jsObject = json.asInstanceOf[JsObject]

      jsObject.value.get("_type") match {
        case Some(JsString("NotStarted"))  => JsSuccess(NotStarted)
        case Some(JsString("InProgress"))  => JsSuccess(InProgress)
        case Some(JsString("Failed"))      => JsSuccess(Failed)
        case Some(JsString("Quarantined")) => JsSuccess(Quarantined)
        case Some(JsString("UploadedSuccessfully")) =>
          Json.fromJson[UploadedSuccessfully](jsObject)(uploadedSuccessfullyFormat)
        case Some(JsString("UploadRejected")) => Json.fromJson[UploadRejected](jsObject)(uploadRejectedFormat)
        case Some(value)                      => JsError(s"Unexpected value of _type: $value")
        case None                             => JsError("Missing _type field")
      }
    }
  }

  implicit val write: Writes[UploadStatus] = new Writes[UploadStatus] {

    override def writes(p: UploadStatus): JsValue =
      p match {
        case NotStarted  => JsObject(Map("_type" -> JsString("NotStarted")))
        case InProgress  => JsObject(Map("_type" -> JsString("InProgress")))
        case Failed      => JsObject(Map("_type" -> JsString("Failed")))
        case Quarantined => JsObject(Map("_type" -> JsString("Quarantined")))
        case s: UploadRejected =>
          Json.toJson(s)(uploadRejectedFormat).as[JsObject] + ("_type" -> JsString("UploadRejected"))
        case s: UploadedSuccessfully =>
          Json.toJson(s)(uploadedSuccessfullyFormat).as[JsObject] + ("_type" -> JsString("UploadedSuccessfully"))
      }
  }
}
