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

package uk.gov.hmrc.cbcrfrontend.services

import java.io.File
import javax.xml.stream.XMLInputFactory

import com.scalawilliam.xs4s.XmlElementExtractor
import com.scalawilliam.xs4s.Implicits._
import uk.gov.hmrc.cbcrfrontend.model._


import scala.util.control.Exception.nonFatalCatch
import scala.io.Source
import scala.xml.{Elem, NodeSeq}
import cats.instances.all._
import cats.syntax.all._

class XmlInfoExtract {
  private val xmlInputfactory: XMLInputFactory = XMLInputFactory.newInstance()

  implicit class NodeSeqPimp(n: NodeSeq) {
    def textOption: Option[String] = n map (_.text) headOption
    def text: String = textOption.orEmpty
  }

  private def getDocSpec(e: Elem): RawDocSpec = {
    val docType = (e \ "DocSpec" \ "DocTypeIndic").text
    val docRefId = (e \ "DocSpec" \ "DocRefId").text
    val corrDocRefId = (e \ "DocSpec" \ "CorrDocRefId").textOption
    RawDocSpec(docType, docRefId, corrDocRefId)
  }

  private val splitter: XmlElementExtractor[RawXmlFields] = XmlElementExtractor{

    case List("CBC_OECD", "MessageSpec") => ms => {
      val msgRefId         = (ms \ "MessageRefId").text
      val receivingCountry = (ms \ "ReceivingCountry").text
      val sendingEntityIn  = (ms \ "SendingEntityIN").text
      val timestamp        = (ms \ "Timestamp").text
      val msgType          = (ms \ "MessageTypeIndic").textOption
      val reportingPeriod  = (ms \ "ReportingPeriod").text
      RawMessageSpec(msgRefId,receivingCountry,sendingEntityIn,timestamp,reportingPeriod,msgType)
    }

    case List("CBC_OECD", "CbcBody", "ReportingEntity") => re => {
      val tin  = (re \ "Entity" \ "TIN").text
      val name = (re \ "Entity" \ "Name").text
      val rr   = (re \ "ReportingRole").text
      val ds   = getDocSpec(re)
      RawReportingEntity(rr,ds,tin,name)
    }

    case List("CBC_OECD", "CbcBody", "CbcReports") => cr => RawCbcReports(getDocSpec(cr))

    case List("CBC_OECD", "CbcBody", "AdditionalInfo") => cr => RawAdditionalInfo(getDocSpec(cr))

  }

  private val splitter2: XmlElementExtractor[RawXmlFields] = XmlElementExtractor {

    case List("CBC_OECD") => cv => {
      val cbcValue = cv.attributes.get("version").getOrElse("").toString
      RawCbcVal(cbcValue)
    }
  }


  def extract(file:File): RawXMLInfo = {

    val collectedData: List[RawXmlFields] = {
      val xmlEventReader = nonFatalCatch opt xmlInputfactory.createXMLEventReader(Source.fromFile(file).bufferedReader())

      try xmlEventReader.map(_.toIterator.scanCollect(splitter.Scan).toList).toList.flatten
      finally xmlEventReader.foreach(_.close())
    }

    val collectedData2: List[RawXmlFields] = {
      val xmlEventReader2 = nonFatalCatch opt xmlInputfactory.createXMLEventReader(Source.fromFile(file).bufferedReader())

      try xmlEventReader2.map(_.toIterator.scanCollect(splitter2.Scan).toList).toList.flatten
      finally xmlEventReader2.foreach(_.close())
    }

    val cv = collectedData2.collectFirst{ case cv:RawCbcVal=> cv}.getOrElse(RawCbcVal(""))
    val ms = collectedData.collectFirst{ case ms:RawMessageSpec => ms}.getOrElse(RawMessageSpec("","","","","",None))
    val re = collectedData.collectFirst{ case re:RawReportingEntity => re}.getOrElse(RawReportingEntity("",RawDocSpec("","",None),"",""))
    val ai = collectedData.collectFirst{ case ai:RawAdditionalInfo => ai}.getOrElse(RawAdditionalInfo(RawDocSpec("","",None)))
    val cr = collectedData.collectFirst{ case cr:RawCbcReports=> cr}.getOrElse(RawCbcReports(RawDocSpec("","",None)))

    RawXMLInfo(ms,re,cr,ai,cv)
//    RawXMLInfo(ms,re,cr,ai)

  }

}
