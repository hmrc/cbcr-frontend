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

import cats.data.{NonEmptyList, ValidatedNel}
import cats.syntax.all._
import play.api.libs.json._

import java.time.LocalDate

/** This Data is stored in our mongo store on initial submission of an XML On subsequent update/delete submissions, if
  * the [[ReportingEntity]] section is missing, we can use the [[DocRefId]] from one of the other sections
  * ([[CbcReports]]/[[AdditionalInfo]]) to query this data, and fill in the missing [[Utr]], [[UltimateParentEntity]]
  * and [[FilingType]] If the [[ReportingEntity]] is included in an update, we need to update our data in our mongo
  * store
  *
  * @param cbcReportsDRI
  *   The [[DocRefId]] from the [[CbcReports]] section of the XML document
  * @param additionalInfoDRI
  *   The [[DocRefId]] from the [[AdditionalInfo]] section of the XML document
  * @param reportingEntityDRI
  *   The [[DocRefId]] from the [[ReportingEntity]] section of the XML document
  * @param tin
  *   The [[Utr]] (or other tax identifier) from the [[ReportingEntity]] section of the XML document
  * @param ultimateParentEntity
  *   The [[UltimateParentEntity]] from the [[ReportingEntity]] section of the XML document
  * @param reportingRole
  *   The [[ReportingRole]] from the [[ReportingEntity]] section of the XML document
  */
case class ReportingEntityData(
  cbcReportsDRI: NonEmptyList[DocRefId],
  additionalInfoDRI: List[DocRefId],
  reportingEntityDRI: DocRefId,
  tin: TIN,
  ultimateParentEntity: UltimateParentEntity,
  reportingRole: ReportingRole,
  creationDate: Option[LocalDate],
  reportingPeriod: Option[LocalDate],
  currencyCode: Option[String],
  entityReportingPeriod: Option[EntityReportingPeriod]
)

object ReportingEntityData {
  implicit def formatNEL[A: Format]: Format[NonEmptyList[A]] = new Format[NonEmptyList[A]] {
    override def writes(o: NonEmptyList[A]): JsArray = JsArray(o.map(Json.toJson(_)).toList)

    override def reads(json: JsValue): JsResult[NonEmptyList[A]] =
      json
        .validate[List[A]]
        .flatMap(l =>
          NonEmptyList.fromList(l) match {
            case None    => JsError(s"Unable to serialise $json as NonEmptyList")
            case Some(a) => JsSuccess(a)
          }
        )
        .orElse {
          json.validate[A].map(a => NonEmptyList(a, Nil))
        }
  }
  implicit val format: OFormat[ReportingEntityData] = Json.format[ReportingEntityData]

  def extract(x: CompleteXMLInfo): ValidatedNel[CBCErrors, ReportingEntityData] =
    x.cbcReport.toNel.toValidNel(UnexpectedState("CBCReport DocRefId not found")).map { c =>
      ReportingEntityData(
        c.map(_.docSpec.docRefId),
        x.additionalInfo.map(_.docSpec.docRefId),
        x.reportingEntity.docSpec.docRefId,
        x.reportingEntity.tin,
        UltimateParentEntity(x.reportingEntity.name),
        x.reportingEntity.reportingRole,
        x.creationDate,
        Some(x.messageSpec.reportingPeriod),
        x.currencyCodes.headOption,
        Some(x.reportingEntity.entityReportingPeriod)
      )
    }
}
