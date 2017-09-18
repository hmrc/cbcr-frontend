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

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.syntax.all._
import cats.instances.all._
import cats.kernel.Eq
import play.api.data.FormError
import play.api.data.format.{Formats, Formatter}
import play.api.libs.json._
import uk.gov.hmrc.domain.Modulus23Check
import uk.gov.hmrc.emailaddress.EmailAddress
import uk.gov.hmrc.emailaddress.PlayJsonFormats._

/**
  * A CBCId defined as at 15 digit reference using a modulus 23 check digit
  * Digit  1     is an 'X'
  * Digit  2     is the check digit
  * Digits 3-5   is the short name 'CBC'
  * Digits 6-9   are '0000'
  * Digits 10-15 are for the id sequence e.g. '000001' - '999999'
  *
  * Note: This is a hard limit of 999999 unique CBCIds
  */
class CBCId private(val value: String) {
  override def toString: String = value

  override def hashCode(): Int = value.hashCode

  override def equals(obj: scala.Any): Boolean = obj match {
    case c: CBCId => c.value === this.value
    case _ => false
  }
}

object CBCId extends Modulus23Check {

  implicit val cbcIdEq = Eq.instance[CBCId]((a, b) => a.value.equalsIgnoreCase(b.value))

  implicit val cbcFormatter = new Formatter[CBCId] {
    override def bind(key: String, data: Map[String, String]): Either[Seq[FormError], CBCId] =
      Formats.stringFormat.bind(key, data).right.flatMap { s =>
        CBCId(s).toRight(Seq(FormError(key, "error.cbcid", Nil)))
      }

    override def unbind(key: String, value: CBCId): Map[String, String] = Map(key -> value.value)

  }

  implicit val cbcIdFormat = new Format[CBCId] {
    override def writes(o: CBCId): JsValue = JsString(o.value)

    override def reads(json: JsValue): JsResult[CBCId] = json match {
      case JsString(value) => CBCId(value).fold[JsResult[CBCId]](
        JsError(s"CBCId is invalid: $value")
      )(cbcid => JsSuccess(cbcid))
      case other => JsError(s"CBCId is invalid: $other")
    }
  }

  def apply(s: String): Option[CBCId] =
    if (isValidCBC(s) && isCheckCorrect(s, 1)) {
      Some(new CBCId(s))
    } else {
      None
    }

  val cbcRegex = """^X[A-Z]CBC\d{10}$"""

  private def isValidCBC(s: String): Boolean = s.matches(cbcRegex)

  def create(i: Int): Validated[Throwable, CBCId] = if (i > 999999 || i < 0) {
    Invalid(new IllegalArgumentException("CBCId ranges from 0-999999"))
  } else {
    val sequenceNumber = i.formatted("%06d")
    val id = s"CBC0000$sequenceNumber"
    val checkChar = calculateCheckCharacter(id)
    CBCId(s"X$checkChar" + id).fold[Validated[Throwable, CBCId]](
      Invalid(new Exception(s"Generated CBCId did not validate: $id"))
    )(
      cbcId => Valid(cbcId)
    )
  }

}

case class SubscriberContact(firstName: String, lastName: String, phoneNumber: String, email: EmailAddress)

object SubscriberContact {
  implicit val subscriptionFormat: Format[SubscriberContact] = Json.format[SubscriberContact]
}

case class SubscriptionDetails(businessPartnerRecord: BusinessPartnerRecord, subscriberContact: SubscriberContact, cbcId: Option[CBCId], utr: Utr)

object SubscriptionDetails {
  implicit val subscriptionDetailsFormat: Format[SubscriptionDetails] = Json.format[SubscriptionDetails]
}

