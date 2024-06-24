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

import cats.data.NonEmptyList
import play.api.libs.json._

import java.time.LocalDate

case class ReportingEntityDataModel(
  cbcReportsDRI: NonEmptyList[DocRefId],
  additionalInfoDRI: List[DocRefId],
  reportingEntityDRI: DocRefId,
  tin: TIN,
  ultimateParentEntity: UltimateParentEntity,
  reportingRole: ReportingRole,
  creationDate: Option[LocalDate],
  reportingPeriod: Option[LocalDate],
  oldModel: Boolean,
  currencyCode: Option[String],
  entityReportingPeriod: Option[EntityReportingPeriod]
)

object ReportingEntityDataModel {
  implicit def formatNEL: Format[NonEmptyList[DocRefId]] =
    ReportingEntityData.formatNEL[DocRefId]
  implicit val format: OFormat[ReportingEntityDataModel] = Json.format[ReportingEntityDataModel]
}
