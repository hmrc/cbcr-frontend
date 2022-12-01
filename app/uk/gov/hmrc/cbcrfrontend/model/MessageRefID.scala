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

import java.time._
import java.time.format.DateTimeFormatter

import cats.Show
import cats.data.ValidatedNel
import cats.syntax.validated._
import cats.syntax.either._
import cats.syntax.show._
import play.api.libs.json._

import scala.util.control.Exception._

class MessageRefID private (
  val sendingRJ: String,
  val reportingPeriod: Year,
  val receivingRJ: String,
  val cBCId: CBCId,
  val messageType: MessageTypeIndic,
  val creationTimestamp: LocalDateTime,
  val uniqueElement: String)
object MessageRefID {
  val dateFmt = DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmss")
  val cbcRegex: String = CBCId.cbcRegex.init.tail // strip the ^ and $ characters from the cbcRegex
  val dateRegex = """\d{8}T\d{6}"""
  val messageRefIDRegex = ("""GB(\d{4})(\w{2})(""" + cbcRegex + """)(CBC40[1,2])(""" + dateRegex + """)(\w{1,56})""").r

  implicit val show = Show.show[MessageRefID](m =>
    s"${m.sendingRJ}${m.reportingPeriod}${m.receivingRJ}${m.cBCId}${m.messageType}${m.creationTimestamp
      .format(dateFmt)}${m.uniqueElement}")

  implicit val format = new Format[MessageRefID] {
    override def writes(o: MessageRefID): JsValue = JsString(o.show)

    override def reads(json: JsValue): JsResult[MessageRefID] = json match {
      case JsString(value) =>
        MessageRefID(value).fold(
          _ => JsError(s"Unable to parse MessageRefID: $value"),
          m => JsSuccess(m)
        )
      case other => JsError(s"Unable to parse MessageRefID: $other")
    }
  }

  def apply(value: String): ValidatedNel[MessageRefIDError, MessageRefID] = value match {
    case "" | "null" => MessageRefIDMissing.invalidNel[MessageRefID]
    case messageRefIDRegex(reportingPeriod, receivingRJ, cbcid, messageType, timestamp, uniq) =>
      val result: Either[MessageRefIDError, MessageRefID] = for {
        rp <- allCatch opt Year.parse(reportingPeriod) toRight MessageRefIDFormatError
        c  <- CBCId(cbcid) toRight MessageRefIDFormatError
        mt <- MessageTypeIndic.parseFrom(messageType) toRight MessageRefIDFormatError
        ts <- allCatch opt LocalDateTime.parse(timestamp, dateFmt) toRight MessageRefIDTimestampError
      } yield new MessageRefID("GB", rp, receivingRJ, c, mt, ts, uniq)
      result.toValidatedNel
    case _ => MessageRefIDFormatError.invalidNel
  }

}
