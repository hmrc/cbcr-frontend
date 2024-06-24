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

import java.time.LocalDate

case class PartialReportingEntityData(
  cbcReportsDRI: List[DocRefIdPair],
  additionalInfoDRI: List[DocRefIdPair],
  reportingEntityDRI: DocRefIdPair,
  tin: TIN,
  ultimateParentEntity: UltimateParentEntity,
  reportingRole: ReportingRole,
  creationDate: Option[LocalDate],
  reportingPeriod: Option[LocalDate],
  currencyCode: Option[String],
  entityReportingPeriod: Option[EntityReportingPeriod]
)

object PartialReportingEntityData {
  implicit val format: OFormat[PartialReportingEntityData] = Json.format[PartialReportingEntityData]
  def extract(x: CompleteXMLInfo): PartialReportingEntityData =
    PartialReportingEntityData(
      x.cbcReport.map(cr => DocRefIdPair(cr.docSpec.docRefId, cr.docSpec.corrDocRefId)),
      x.additionalInfo.map(ai => DocRefIdPair(ai.docSpec.docRefId, ai.docSpec.corrDocRefId)),
      DocRefIdPair(x.reportingEntity.docSpec.docRefId, x.reportingEntity.docSpec.corrDocRefId),
      x.reportingEntity.tin,
      UltimateParentEntity(x.reportingEntity.name),
      x.reportingEntity.reportingRole,
      x.creationDate,
      Some(x.messageSpec.reportingPeriod),
      x.currencyCodes.headOption,
      Some(x.reportingEntity.entityReportingPeriod)
    )
}

case class DocRefIdPair(docRefId: DocRefId, corrDocRefId: Option[CorrDocRefId])
object DocRefIdPair {
  implicit val format: OFormat[DocRefIdPair] = Json.format[DocRefIdPair]
}
