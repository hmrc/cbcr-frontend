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

import play.api.libs.json.Json


/**
  * This Data is stored in our mongo store on initial submission of an XML
  * On subsequent update/delete submissions, if the [[ReportingEntity]] section is missing, we can use the [[DocRefId]]
  * from one of the other sections ([[CbcReports]]/[[AdditionalInfo]]) to query this data, and fill in the missing
  * [[Utr]], [[UltimateParentEntity]] and [[FilingType]]
  * If the [[ReportingEntity]] is included in an update, we need to update our data in our mongo store
  *
  * @param cbcReportsDRI The [[DocRefId]] from the [[CbcReports]] section of the XML document
  * @param additionalInfoDRI The [[DocRefId]] from the [[AdditionalInfo]] section of the XML document
  * @param reportingEntityDRI The [[DocRefId]] from the [[ReportingEntity]] section of the XML document
  * @param utr The [[Utr]] from the [[ReportingEntity]] section of the XML document
  * @param ultimateParentEntity The [[UltimateParentEntity]] from the [[ReportingEntity]] section of the XML document
  * @param reportingRole The [[ReportingRole]] from the [[ReportingEntity]] section of the XML document
  */
case class ReportingEntityData(cbcReportsDRI:DocRefId,
                               additionalInfoDRI:DocRefId,
                               reportingEntityDRI:DocRefId,
                               utr:Utr,
                               ultimateParentEntity: UltimateParentEntity,
                               reportingRole: ReportingRole)

object ReportingEntityData{
  implicit val format = Json.format[ReportingEntityData]

  def extract(x:XMLInfo):ReportingEntityData =
    ReportingEntityData(
      x.cbcReport.docSpec.docRefId,
      x.additionalInfo.docSpec.docRefId,
      x.reportingEntity.docSpec.docRefId,
      x.reportingEntity.tin,
      UltimateParentEntity(x.reportingEntity.name),
      x.reportingEntity.reportingRole
    )
}

