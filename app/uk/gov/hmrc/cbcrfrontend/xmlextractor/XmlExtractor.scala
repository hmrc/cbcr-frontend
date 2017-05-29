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

package uk.gov.hmrc.cbcrfrontend.xmlextractor

import java.io.File

import cats.data.Validated
import uk.gov.hmrc.cbcrfrontend.model.KeyXMLFileInfo

class XmlExtractor {
  import scala.xml.XML
  def getKeyXMLFileInfo(file: File): Validated[Exception, KeyXMLFileInfo] = {

    Validated.catchOnly[Exception] {

    val xmlFile = XML.loadFile(file)
    KeyXMLFileInfo(
      (xmlFile \ "MessageSpec" \ "MessageRefId").text,
      (xmlFile \ "MessageSpec" \ "ReportingPeriod").text,
      (xmlFile \ "MessageSpec" \ "Timestamp").text)

    }
  }
}

object XmlExtractor extends XmlExtractor