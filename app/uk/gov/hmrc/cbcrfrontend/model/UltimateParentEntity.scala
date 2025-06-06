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

case class UltimateParentEntity(ultimateParentEntity: String)

object UltimateParentEntity {
  implicit val format: Format[UltimateParentEntity] = new Format[UltimateParentEntity] {
    override def reads(json: JsValue): JsResult[UltimateParentEntity] =
      json
        .asOpt[String]
        .map(s => JsSuccess(UltimateParentEntity(s)))
        .getOrElse(JsError(s"Unable to parse $json as UltimateParentEntity"))

    override def writes(o: UltimateParentEntity): JsString = JsString(o.ultimateParentEntity)
  }
  def unapply(upe: UltimateParentEntity): Option[String] = Some(upe.ultimateParentEntity)
}
