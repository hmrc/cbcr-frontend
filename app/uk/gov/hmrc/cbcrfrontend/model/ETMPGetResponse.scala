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

import play.api.libs.json.Reads._
import play.api.libs.json._
import uk.gov.hmrc.cbcrfrontend.emailaddress.{EmailAddress, PlayJsonFormats}

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

class PhoneNumber private (val number: String)

//Must match telephone type from API docs
//***REMOVED***

object PhoneNumber {

  private val pattern = "^[0-9 )/(-*#]+$"

  def apply(number: String): Option[PhoneNumber] =
    if (number.matches(pattern)) {
      Some(new PhoneNumber(number))
    } else {
      None
    }

  implicit val format: Format[PhoneNumber] = new Format[PhoneNumber] {
    override def writes(o: PhoneNumber): JsString = JsString(o.number)

    override def reads(json: JsValue): JsResult[PhoneNumber] = json match {
      case JsString(v) =>
        PhoneNumber(v).fold[JsResult[PhoneNumber]](
          JsError(s"Unable to serialise $json as a PhoneNumber")
        )(pn => JsSuccess(pn))
      case _ => JsError(s"Unable to serialise $json as a PhoneNumber")
    }
  }
}

case class ContactDetails(email: String, phoneNumber: String)

object ContactDetails {
  implicit val emailFormat: Format[EmailAddress] =
    Format(PlayJsonFormats.emailAddressReads, PlayJsonFormats.emailAddressWrites)
  implicit val format: OFormat[ContactDetails] = Json.format[ContactDetails]
}

case class ContactName(name1: String, name2: String)

object ContactName {
  implicit val format: OFormat[ContactName] = Json.format[ContactName]
}
case class ETMPSubscription(safeId: String, names: ContactName, contact: ContactDetails, address: EtmpAddress)
object ETMPSubscription {
  implicit val addressFormat: Format[EtmpAddress] = EtmpAddress.subscriptionFormat
  implicit val format: OFormat[ETMPSubscription] = Json.format[ETMPSubscription]
}

case class CorrespondenceDetails(contactAddress: EtmpAddress, contactDetails: ContactDetails, contactName: ContactName)

object CorrespondenceDetails {
  implicit val format: OFormat[CorrespondenceDetails] = Json.format[CorrespondenceDetails]
}
case class UpdateResponse(processingDate: LocalDateTime)
object UpdateResponse {
  private val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss'Z'")
  implicit val format: Writes[UpdateResponse] = (o: UpdateResponse) =>
    Json.obj(
      "processingDate" -> o.processingDate.format(formatter)
    )

  implicit val reads: Reads[UpdateResponse] = (JsPath \ "processingDate").read[LocalDateTime].map(UpdateResponse(_))
}
