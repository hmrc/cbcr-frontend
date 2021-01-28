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

package uk.gov.hmrc.cbcrfrontend.model

import play.api.libs.json._

trait ParentGroupElement

case object ENT extends ParentGroupElement
case object REP extends ParentGroupElement
case object ADD extends ParentGroupElement

object ParentGroupElement {
  def fromString(s: String): Option[ParentGroupElement] =
    format
      .reads(JsString(s))
      .fold(
        _ => None,
        p => Some(p)
      )

  implicit val format = new Format[ParentGroupElement] {
    override def reads(json: JsValue): JsResult[ParentGroupElement] = json match {
      case JsString("ENT") => JsSuccess(ENT)
      case JsString("REP") => JsSuccess(REP)
      case JsString("ADD") => JsSuccess(ADD)
      case other           => JsError(s"Unable to deserialise $other as ParentGroupElement")
    }

    override def writes(o: ParentGroupElement): JsValue = JsString(o.toString)

  }
}
