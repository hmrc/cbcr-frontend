/*
 * Copyright 2018 HM Revenue & Customs
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

import scala.util.control.NonFatal

class XmlInfoExtract {

  private val xmlInputFactory: XMLInputFactory2 = XMLInputFactory.newInstance.asInstanceOf[XMLInputFactory2]
  xmlInputFactory.setProperty(XMLInputFactory.SUPPORT_DTD, false)
  xmlInputFactory.setProperty("javax.xml.stream.isSupportingExternalEntities", false)

  implicit class NodeSeqPimp(n: NodeSeq) {
    def textOption: Option[String] = n.map(_.text).headOption
    def text: String = textOption.orEmpty
  }

  private def getDocSpec(e: Node): RawDocSpec = {
    val docType = (e \ "DocTypeIndic").text
    val docRefId = (e \ "DocRefId").text
    val corrDocRefId = (e \ "CorrDocRefId").textOption
    val corrMessageRefId = (e \ "CorrMessageRefId").textOption
    RawDocSpec(docType, docRefId, corrDocRefId, corrMessageRefId)
  }

  // sorry but speed
  private def countBodys(input:File): Int ={
    val xmlStreamReader: XMLStreamReader2  = xmlInputFactory.createXMLStreamReader(input)
    var count = 0
    try {
      while (xmlStreamReader.hasNext) {
        val event = xmlStreamReader.next()
        if (event == XMLStreamConstants.START_ELEMENT && xmlStreamReader.getLocalName().equalsIgnoreCase("CbcBody")) count = count + 1
      }
    } catch {
      case NonFatal(e)  => Logger.warn(s"Error counting CBCBody elements: ${e.getMessage}")
    }
    count
  }

  private def extractEncoding(input: File): Option[RawXmlEncodingVal] = {
    val xmlStreamReader: XMLStreamReader2  = xmlInputFactory.createXMLStreamReader(input)

    val encodingVal: String = xmlStreamReader.getCharacterEncodingScheme
    xmlStreamReader.closeCompletely()

    encodingVal match {
      case null => None
      case _ => Some(RawXmlEncodingVal(encodingVal))
    }
  }

  private def extractCbcVal(input: File): RawCbcVal = {
    val xmlStreamReader: XMLStreamReader2  = xmlInputFactory.createXMLStreamReader(input)

    val value = nonFatalCatch either {
      RawCbcVal(
        if(xmlStreamReader.hasNext) {
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
      val corrMsgRefId     = (ms \ "CorrMessageRefId").textOption
      RawMessageSpec(msgRefId,receivingCountry,sendingEntityIn,timestamp,reportingPeriod,msgType,corrMsgRefId)
    }

    case List("CBC_OECD", "CbcBody", "ReportingEntity") => re => {
      val tin  = (re \ "Entity" \ "TIN").text
      val tinIB  = (re \ "Entity" \ "TIN") \@ "issuedBy"
      val name = (re \ "Entity" \ "Name").text
      val rr   = (re \ "ReportingRole").text
      val ds   = getDocSpec((re \ "DocSpec").head) //DocSpec is required in ReportingEntity so this will exist!
      RawReportingEntity(rr,ds,tin,tinIB,name)
    }

    case List("CBC_OECD", "CbcBody", "CbcReports", "DocSpec") => ds => RawCbcReports(getDocSpec(ds))

    case List("CBC_OECD", "CbcBody", "CbcReports", "ConstEntities", "ConstEntity", "Name") => ds => RawConstEntityName(ds.text)

    case List("CBC_OECD", "CbcBody", "AdditionalInfo", "DocSpec") => ds => RawAdditionalInfo(getDocSpec(ds))

  }

  def extract(file:File): RawXMLInfo = {

    val collectedData: (List[RawXmlFields],Int) = {

      val xmlEventReader = nonFatalCatch opt xmlInputFactory.createXMLEventReader(Source.fromFile(file).bufferedReader())

      try {
        val fields    = xmlEventReader.map(_.toIterator.scanCollect(splitter.Scan).toList).toList.flatten
        val numBodies = countBodys(file)
        (fields,numBodies)
      }

      finally {
        xmlEventReader.foreach(_.close())
      }
    }

    val xe = extractEncoding(file)
    val cv = extractCbcVal(file)
    val ms = collectedData._1.collectFirst{ case ms:RawMessageSpec => ms}.getOrElse(RawMessageSpec("","","","","",None,None))
    val re = collectedData._1.collectFirst{ case re:RawReportingEntity => re}
    val ai = collectedData._1.collect{ case ai:RawAdditionalInfo => ai}
    val cr = collectedData._1.collect{ case cr:RawCbcReports=> cr }
    val cen = collectedData._1.collect{ case cen:RawConstEntityName => cen.name}

    RawXMLInfo(ms,re,cr,ai.headOption,cv,xe, collectedData._2, cen)

  }

}
