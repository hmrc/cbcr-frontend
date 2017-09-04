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

import java.io.{File, InputStream}
import javax.xml.stream.{XMLInputFactory, XMLStreamConstants}

import cats.instances.all._
import cats.syntax.all._
import com.scalawilliam.xs4s.Implicits._
import com.scalawilliam.xs4s.XmlElementExtractor
import uk.gov.hmrc.cbcrfrontend.model._

import scala.io.Source
import scala.util.control.Exception.nonFatalCatch
import scala.xml.{Node, NodeSeq}
import org.codehaus.stax2.{XMLInputFactory2, XMLStreamReader2}
import play.api.Logger

class XmlInfoExtract {

  private val xmlInputFactory: XMLInputFactory2 = XMLInputFactory.newInstance.asInstanceOf[XMLInputFactory2]

  implicit class NodeSeqPimp(n: NodeSeq) {
    def textOption: Option[String] = n map (_.text) headOption
    def text: String = textOption.orEmpty
  }

  private def getDocSpec(e: Node): RawDocSpec = {
    val docType = (e \ "DocTypeIndic").text
    val docRefId = (e \ "DocRefId").text
    val corrDocRefId = (e \ "CorrDocRefId").textOption
    RawDocSpec(docType, docRefId, corrDocRefId)
  }

  private def extractEncoding(input: File): RawXmlEncodingVal = {
    val xmlStreamReader: XMLStreamReader2  = xmlInputFactory.createXMLStreamReader(input)

    val encodingVal: String = xmlStreamReader.getCharacterEncodingScheme
    xmlStreamReader.closeCompletely()
    RawXmlEncodingVal(encodingVal)
  }

  private def extractCbcVal(input: File): RawCbcVal = {
    val xmlStreamReader: XMLStreamReader2  = xmlInputFactory.createXMLStreamReader(input)

    val value = nonFatalCatch either {
      RawCbcVal(
        if(xmlStreamReader.hasNext()) {
          xmlStreamReader.nextTag()
          xmlStreamReader.getAttributeValue("","version")
        } else ""
      )
    }

    xmlStreamReader.closeCompletely()

    value.fold(
      e => {
        Logger.warn(s"extractCbcVal encountered the following error: ${e.getMessage}")
        RawCbcVal("")
      },
      cbcVal => cbcVal
    )

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
      val ds   = getDocSpec((re \ "DocSpec").head) //DocSpec is required in ReportingEntity so this will exist!
      RawReportingEntity(rr,ds,tin,name)
    }

    case List("CBC_OECD", "CbcBody", "CbcReports", "DocSpec") => ds => RawCbcReports(getDocSpec(ds))

    case List("CBC_OECD", "CbcBody", "AdditionalInfo", "DocSpec") => ds => RawAdditionalInfo(getDocSpec(ds))

  }

  def extract(file:File): RawXMLInfo = {

    val collectedData: List[RawXmlFields] = {

      val xmlEventReader = nonFatalCatch opt xmlInputFactory.createXMLEventReader(Source.fromFile(file).bufferedReader())

      try xmlEventReader.map(_.toIterator.scanCollect(splitter.Scan).toList).toList.flatten
      finally xmlEventReader.foreach(_.close())
    }


    val xe = extractEncoding(file)
    val cv = extractCbcVal(file)
    val ms = collectedData.collectFirst{ case ms:RawMessageSpec => ms}.getOrElse(RawMessageSpec("","","","","",None))
    val re = collectedData.collectFirst{ case re:RawReportingEntity => re}.getOrElse(RawReportingEntity("",RawDocSpec("","",None),"",""))
    val ai = collectedData.collectFirst{ case ai:RawAdditionalInfo => ai}.getOrElse(RawAdditionalInfo(RawDocSpec("","",None)))
    val cr = collectedData.collectFirst{ case cr:RawCbcReports=> cr}.getOrElse(RawCbcReports(RawDocSpec("","",None)))

    RawXMLInfo(ms,re,cr,ai,cv,xe)

  }

}
