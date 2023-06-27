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

package uk.gov.hmrc.cbcrfrontend.model

import play.api.libs.json._

sealed trait DocTypeIndic

case object OECD0 extends DocTypeIndic
case object OECD1 extends DocTypeIndic
case object OECD2 extends DocTypeIndic
case object OECD3 extends DocTypeIndic

object DocTypeIndic {

  def fromString(s: String): Option[DocTypeIndic] =
    format
      .reads(JsString(s))
      .fold(
        _ => None,
        d => Some(d)
      )

  implicit val format: Format[DocTypeIndic] = new Format[DocTypeIndic] {
    override def writes(o: DocTypeIndic): JsValue = JsString(o.toString)

    override def reads(json: JsValue): JsResult[DocTypeIndic] = json match {
      case JsString("OECD0") => JsSuccess(OECD0)
      case JsString("OECD1") => JsSuccess(OECD1)
      case JsString("OECD2") => JsSuccess(OECD2)
      case JsString("OECD3") => JsSuccess(OECD3)
      case other             => JsError(s"Unable to parse DocTypeIndic: $other")
    }
  }
}
