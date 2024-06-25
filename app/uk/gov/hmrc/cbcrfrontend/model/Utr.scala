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

import play.api.libs.json.{Format, Reads, Writes}
import play.api.mvc.PathBindable
import uk.gov.hmrc.domain.{Modulus11Check, SimpleObjectReads, SimpleObjectWrites, TaxIdentifier}

import scala.annotation.unused

case class Utr(utr: String) extends TaxIdentifier {
  override def value: String = utr

  def isValid: Boolean = CheckUTR.isValid(utr)

  def stripUtr: String = CheckUTR.stripUtr(utr)

  private object CheckUTR extends Modulus11Check {
    def isValid(utr: String): Boolean = utr match {
      case Utr.utrRegex(_*) =>
        val suffix: String = utr.substring(1)
        val checkCharacter: Char = calculateCheckCharacter(suffix)
        checkCharacter == utr.charAt(0)
      case _ => false
    }
    def stripUtr(utr: String): String =
      utr
        .replaceAll("\\s+", "")
        .replaceAll("[a-zA-Z]", "")
        .takeRight(Math.min(utr.length, 10))
  }
}

object Utr {

  @unused
  def safeApply(utr: String): Option[Utr] = {
    val u = Utr(utr)
    if (u.isValid) Some(u)
    else None
  }

  implicit val pathFormat: PathBindable[Utr] = new PathBindable[Utr] {
    override def bind(key: String, value: String): Either[String, Utr] =
      if (Utr(value).isValid) {
        Right(Utr(value))
      } else {
        Left(s"Invalid Utr: $value")
      }
    override def unbind(key: String, value: Utr): String = value.value
  }

  private val utrRegex = "^[0-9]{10}$".r

  private val writes: Writes[Utr] = new SimpleObjectWrites[Utr](_.value)
  private val reads: Reads[Utr] = new SimpleObjectReads[Utr]("utr", Utr.apply)
  implicit val format: Format[Utr] = Format[Utr](reads, writes)
}
