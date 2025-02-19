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

import play.api.libs.json.{Format, JsString, Json, Reads, Writes}

case class UpscanInitiateResponse(
  fileReference: UpscanFileReference,
  postTarget: String,
  formFields: Map[String, String]
)

case class UploadForm(
  href: String,
  fields: Map[String, String]
)

case class Reference(value: String) extends AnyVal

object Reference {
  implicit val referenceReader: Reads[Reference] = Reads.StringReads.map(Reference(_))
  implicit val referenceWrites: Writes[Reference] = Writes[Reference](x => JsString(x.value))
}

case class PreparedUpload(
  reference: Reference,
  uploadRequest: UploadForm
)

object PreparedUpload {
  implicit val uploadFormFormat: Format[UploadForm] = Json.format[UploadForm]
  implicit val format: Format[PreparedUpload] = Json.format[PreparedUpload]
}
