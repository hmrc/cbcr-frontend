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

package uk.gov.hmrc.cbcrfrontend.util
import uk.gov.hmrc.cbcrfrontend.model.{DocSpec, XMLInfo}

object BusinessRulesUtil {
  def isFullyCorrected(listOne: List[String], listTwo: List[String]): Boolean = listOne.sorted == listTwo.sorted
  def allDocSpecs(in: XMLInfo): List[DocSpec] =
    in.reportingEntity.map(_.docSpec).toList ++ in.cbcReport.map(_.docSpec) ++ in.additionalInfo.map(_.docSpec)
  def extractAllCorrDocRefIds(in: XMLInfo): List[String] = allDocSpecs(in).flatMap(_.corrDocRefId).map(_.cid.toString)
  def extractAllDocTypes(in: XMLInfo): List[String] = allDocSpecs(in).map(_.docType.toString)
}
