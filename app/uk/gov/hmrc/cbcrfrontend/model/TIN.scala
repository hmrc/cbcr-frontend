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

import play.api.libs.json.{Format, Reads, Writes}
import uk.gov.hmrc.domain.{SimpleObjectReads, SimpleObjectWrites}

case class TIN(value: String, issuedBy: String)
object TIN {
  implicit val writes: Writes[TIN] = new SimpleObjectWrites[TIN](_.value)
  implicit val reads: Reads[TIN] =
    new SimpleObjectReads[TIN]("tin", TIN.apply(_, "")).orElse(new SimpleObjectReads[TIN]("utr", TIN.apply(_, "")))
  implicit val format: Format[TIN] = Format[TIN](reads, writes)
}
