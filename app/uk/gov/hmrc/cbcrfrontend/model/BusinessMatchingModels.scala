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

import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._ // Combinator syntax

case class EtmpAddress(
  addressLine1: String,
  addressLine2: Option[String],
  addressLine3: Option[String],
  addressLine4: Option[String],
  postalCode: Option[String],
  countryCode: String
)

object EtmpAddress {
  implicit val bprFormat: OFormat[EtmpAddress] = Json.format[EtmpAddress]

  val subscriptionFormat: Format[EtmpAddress] = new Format[EtmpAddress] {
    override def writes(o: EtmpAddress): JsObject = Json.obj(
      "line1"       -> o.addressLine1,
      "line2"       -> o.addressLine1,
      "line3"       -> o.addressLine1,
      "line4"       -> o.addressLine1,
      "postalCode"  -> o.postalCode,
      "countryCode" -> o.countryCode
    )

    implicit val etmpAddressReads: Reads[EtmpAddress] =
      ((JsPath \ "line1").read[String] and
        (JsPath \ "line2").readNullable[String] and
        (JsPath \ "line3").readNullable[String] and
        (JsPath \ "line4").readNullable[String] and
        (JsPath \ "postalCode").readNullable[String] and
        (JsPath \ "countryCode").read[String])(EtmpAddress.apply _)

    override def reads(json: JsValue): JsResult[EtmpAddress] = etmpAddressReads.reads(json)
  }

}

case class OrganisationResponse(organisationName: String)

object OrganisationResponse {
  implicit val formats: OFormat[OrganisationResponse] = Json.format[OrganisationResponse]
}

case class BusinessPartnerRecord(safeId: String, organisation: Option[OrganisationResponse], address: EtmpAddress)

object BusinessPartnerRecord {
  implicit val format: OFormat[BusinessPartnerRecord] = Json.format[BusinessPartnerRecord]
}
