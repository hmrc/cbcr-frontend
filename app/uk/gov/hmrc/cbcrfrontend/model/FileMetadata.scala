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

package uk.gov.hmrc.cbcrfrontend.model

import play.api.libs.functional.syntax.unlift
import play.api.libs.json.{Format, JsPath, JsValue}
import play.api.libs.functional.syntax._


case class FileMetadata(
                         id: String,
                         status: String,
                         name: String,
                         contentType: String,
                         length: BigDecimal,
                         created: String,
                         metadata: JsValue,
                         href: String)

object FileMetadata {

  implicit val fileMetadataFormat: Format[FileMetadata] = (
    (JsPath \ "id").format[String] and
    (JsPath \ "status").format[String] and
    (JsPath \ "name").format[String] and
    (JsPath \ "contentType").format[String] and
    (JsPath \ "length").format[BigDecimal] and
    (JsPath \ "created").format[String] and
    (JsPath \ "metadata").format[JsValue] and
    (JsPath \ "href").format[String]
    ) (FileMetadata.apply, unlift(FileMetadata.unapply))

}