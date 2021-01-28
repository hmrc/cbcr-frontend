/*
 * Copyright 2021 HM Revenue & Customs
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
import uk.gov.hmrc.cbcrfrontend.model.XMLInfo
import uk.gov.hmrc.cbcrfrontend.model._

object BusinessRulesUtil {

  def isFullyCorrected(listOne: List[String], listTwo: List[String]): Boolean = {
    val sortedFirst = listOne.sorted
    val sortedSecond = listTwo.sorted
    sortedFirst.equals(sortedSecond)
  }

  def extractAllCorrDocRefIds(in: XMLInfo): List[String] = {
    val addDocSpec = in.additionalInfo
      .filter(_.docSpec.corrDocRefId.isDefined)
      .map(_.docSpec.corrDocRefId.get.cid.toString)
    val entDocSpecs = in.reportingEntity
      .filter(_.docSpec.corrDocRefId.isDefined)
      .map(_.docSpec.corrDocRefId.get.cid.toString) match {
      case Some(entDoc: String) => List(entDoc)
      case None                 => List()
    }
    val repDocSpec = in.cbcReport
      .filter(_.docSpec.corrDocRefId.isDefined)
      .map(_.docSpec.corrDocRefId.get.cid.toString)

    entDocSpecs ++ repDocSpec ++ addDocSpec
  }

  def extractAllDocTypes(in: XMLInfo): List[String] = {
    val addDocSpec = in.additionalInfo.map(_.docSpec.docType.toString)
    val entDocSpecs = in.reportingEntity match {
      case Some(ent) => List(ent.docSpec.docType.toString)
      case None      => List()
    }
    val repDocSpec = in.cbcReport.map(_.docSpec.docType.toString)

    entDocSpecs ++ repDocSpec ++ addDocSpec
  }
}
