/*
 * Copyright 2022HM Revenue & Customs
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

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import play.api.libs.json._
import uk.gov.hmrc.emailaddress.EmailAddress
import uk.gov.hmrc.emailaddress.PlayJsonFormats._
import play.api.libs.json.Reads._ // Custom validation helpers

class PhoneNumber private (val number: String)

//Must match telephone type from API docs
//***REMOVED***

object PhoneNumber {

  val pattern = "^[0-9 )/(-*#]+$"

  def apply(number: String): Option[PhoneNumber] =
    if (number.matches(pattern)) {
      Some(new PhoneNumber(number))
    } else {
      None
    }

  implicit val format = new Format[PhoneNumber] {
    override def writes(o: PhoneNumber) = JsString(o.number)

    override def reads(json: JsValue) = json match {
      case JsString(v) =>
        PhoneNumber(v).fold[JsResult[PhoneNumber]](
          JsError(s"Unable to serialise $json as a PhoneNumber")
        )(pn => JsSuccess(pn))
      case _ => JsError(s"Unable to serialise $json as a PhoneNumber")
    }
  }
}

case class ContactDetails(email: EmailAddress, phoneNumber: String)

object ContactDetails {
  val emailFormat = new Format[EmailAddress] {
    override def writes(o: EmailAddress) = Json.obj("emailAddress" -> o.value)

    override def reads(json: JsValue) = json match {
      case JsObject(m) =>
        m.get("emailAddress")
          .flatMap(_.asOpt[String].map(EmailAddress(_)))
          .fold[JsResult[EmailAddress]](
            JsError("Unable to serialise emailAddress")
          )(emailAddress => JsSuccess(emailAddress))
      case other => JsError(s"Unable to serialise emailAddress: $other")
    }
  }
  implicit val format = Json.format[ContactDetails]
}

case class ContactName(name1: String, name2: String)

object ContactName {
  implicit val format = Json.format[ContactName]
}
case class ETMPSubscription(safeId: String, names: ContactName, contact: ContactDetails, address: EtmpAddress)
object ETMPSubscription {
  implicit val addressFormat = EtmpAddress.subscriptionFormat
  implicit val format = Json.format[ETMPSubscription]
}

case class CorrespondenceDetails(contactAddress: EtmpAddress, contactDetails: ContactDetails, contactName: ContactName)

object CorrespondenceDetails {
  implicit val format = Json.format[CorrespondenceDetails]
}
case class UpdateResponse(processingDate: LocalDateTime)
object UpdateResponse {
  val formatter = DateTimeFormatter.ofPattern("YYYY-MM-dd'T'HH:mm:ss'Z'")
  implicit val format = new Writes[UpdateResponse] {
    override def writes(o: UpdateResponse) = Json.obj(
      "processingDate" -> o.processingDate.format(formatter)
    )
  }

  implicit val reads: Reads[UpdateResponse] = (JsPath \ "processingDate").read[LocalDateTime].map(UpdateResponse(_))
}
