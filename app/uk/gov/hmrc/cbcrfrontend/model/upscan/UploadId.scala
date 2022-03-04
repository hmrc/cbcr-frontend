/*
 * Copyright 2022 HM Revenue & Customs
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
import play.api.mvc.PathBindable

import java.util.UUID

case class UploadId(value: String) extends AnyVal

object UploadId {
  def generate = UploadId(UUID.randomUUID().toString)

  implicit val uploadIdFormat: OFormat[UploadId] = Json.format[UploadId]

  implicit lazy val pathBindable: PathBindable[UploadId] = new PathBindable[UploadId] {
    override def bind(key: String, value: String): Either[String, UploadId] =
      implicitly[PathBindable[String]].bind(key, value).right.map(UploadId(_))

    override def unbind(key: String, value: UploadId): String =
      value.value
  }
}
