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

package uk.gov.hmrc.cbcrfrontend.model

import play.api.libs.json._

/** Case object to be written to cache to indicate subscription has occurred */
case object Subscribed {
  implicit val format: Format[Subscribed.type] = new Format[Subscribed.type] {
    override def writes(o: Subscribed.type): JsValue = JsString(o.toString)

    override def reads(json: JsValue): JsResult[Subscribed.type] = json match {
      case JsString("Subscribed") => JsSuccess(Subscribed)
      case other                  => JsError(s"Failed to serialise Subscribed: $other")
    }
  }
}
