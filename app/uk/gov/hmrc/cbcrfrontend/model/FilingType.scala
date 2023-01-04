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
import cats.syntax.show._

case class FilingType(value: ReportingRole)
object FilingType {
  implicit val format = new Format[FilingType] {
    override def writes(o: FilingType): JsValue = Json.obj("filingType" -> o.value.show)

    override def reads(json: JsValue): JsResult[FilingType] = {
      val value = for {
        obj <- json.asOpt[JsObject]
        ft  <- obj.value.get("filingType")
        v   <- ft.asOpt[String]
      } yield v

      value match {
        case Some("PRIMARY")          => JsSuccess(FilingType(CBC701))
        case Some("VOLUNTARY")        => JsSuccess(FilingType(CBC702))
        case Some("LOCAL")            => JsSuccess(FilingType(CBC703))
        case Some("LOCAL INCOMPLETE") => JsSuccess(FilingType(CBC704))
        case _                        => JsError(s"Unable to parse $json as FilingType")
      }
    }
  }
}
