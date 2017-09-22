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

import play.api.libs.json._

case class AgencyBusinessName(name:String)
object AgencyBusinessName{
  implicit val format = new Format[AgencyBusinessName] {

    override def writes(o: AgencyBusinessName): JsValue = JsString(o.toString)

    override def reads(json: JsValue): JsResult[AgencyBusinessName] = json match {
      case o:JsString => Option(AgencyBusinessName(o.value)).fold[JsResult[AgencyBusinessName]](JsError(s"Failed to parse $json as AgencyBusinessName"))(JsSuccess(_))
      case _          => JsError(s"Failed to parse $json as AgencyBusinessName")
    }
  }
}
