/*
 * Copyright 2018 HM Revenue & Customs
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

import play.api.libs.json._

case class FileId(value: String) extends AnyVal {
  override def toString = value
}
object FileId {
  implicit val fileIdFormat = new Format[FileId] {
    override def reads(json: JsValue): JsResult[FileId] = json.asOpt[String] match {
      case Some(s) => JsSuccess(FileId(s))
      case None => JsError(s"Could not parse fileId: $json")
    }

    override def writes(o: FileId): JsValue = JsString(o.value)
  }
}
