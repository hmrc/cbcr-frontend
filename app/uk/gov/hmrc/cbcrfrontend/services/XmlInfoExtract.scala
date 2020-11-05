/*
 * Copyright 2020 HM Revenue & Customs
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

import scala.io.{BufferedSource, Source}
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

  private def getAddress(e: Node): Address = {
    val countryCode = (e \ "CountryCode").text
    val addressFix = getAddressFix((e \ "AddressFix").headOption)
    val addressFree = (e \ "AddressFree").text
    Address(countryCode, addressFree, addressFix)
  }

  private def getAddressFix(e: Option[Node]): AddressFix =
    e match {
      case Some(node) => {
        val city = (node \ "City").text
        AddressFix(Some(city))
      }
      case None => AddressFix(None)
    }

  // sorry but speed
  private def countBodys(input: File): Int = {
    val xmlStreamReader: XMLStreamReader2 = xmlInputFactory.createXMLStreamReader(input)
    var count = 0
    try {
      while (xmlStreamReader.hasNext) {
        val event = xmlStreamReader.next()
        if (event == XMLStreamConstants.START_ELEMENT && xmlStreamReader.getLocalName().equalsIgnoreCase("CbcBody"))
          count = count + 1
      }
    } catch {
      case NonFatal(e) => Logger.warn(s"Error counting CBCBody elements: ${e.getMessage}")
    }
    count
  }

  private def extractEncoding(input: File): Option[RawXmlEncodingVal] = {
    val xmlStreamReader: XMLStreamReader2 = xmlInputFactory.createXMLStreamReader(input)

    val encodingVal: String = xmlStreamReader.getCharacterEncodingScheme
    xmlStreamReader.closeCompletely()

    encodingVal match {
      case null => None
      case _    => Some(RawXmlEncodingVal(encodingVal))
    }
  }

  private def extractCbcVal(input: File): RawCbcVal = {
    val xmlStreamReader: XMLStreamReader2 = xmlInputFactory.createXMLStreamReader(input)

    val value = nonFatalCatch either {
      RawCbcVal(
        if (xmlStreamReader.hasNext) {
          xmlStreamReader.nextTag()
          xmlStreamReader.getAttributeValue("", "version")
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

  private val splitter: XmlElementExtractor[RawXmlFields] = XmlElementExtractor {

    case List("CBC_OECD", "MessageSpec") =>
      ms =>
        {
          val msgRefId = (ms \ "MessageRefId").text
          val receivingCountry = (ms \ "ReceivingCountry").text
          val sendingEntityIn = (ms \ "SendingEntityIN").text
          val timestamp = (ms \ "Timestamp").text
          val msgType = (ms \ "MessageTypeIndic").textOption
          val reportingPeriod = (ms \ "ReportingPeriod").text
          val corrMsgRefId = (ms \ "CorrMessageRefId").textOption
          RawMessageSpec(msgRefId, receivingCountry, sendingEntityIn, timestamp, reportingPeriod, msgType, corrMsgRefId)
        }

    case List("CBC_OECD", "CbcBody", "ReportingEntity") =>
      re =>
        {
          val tin = (re \ "Entity" \ "TIN").text
          val tinIB = (re \ "Entity" \ "TIN") \@ "issuedBy"
          val name = (re \ "Entity" \ "Name").text
          val address = getAddress((re \ "Entity" \ "Address").head)
          val rr = (re \ "ReportingRole").text
          val ds = getDocSpec((re \ "DocSpec").head) //DocSpec is required in ReportingEntity so this will exist!
          RawReportingEntity(rr, ds, tin, tinIB, name, address)
        }

    case List("CBC_OECD", "CbcBody", "CbcReports", "DocSpec") =>
      ds =>
        RawCbcReports(getDocSpec(ds))

    case List("CBC_OECD", "CbcBody", "CbcReports", "ConstEntities", "ConstEntity", "Name") =>
      ds =>
        RawConstEntityName(ds.text)

    case List("CBC_OECD", "CbcBody", "CbcReports", "Summary") =>
      su =>
        {
          val unrelated = (su \ "Revenues" \ "Unrelated") \@ "currCode"
          val related = (su \ "Revenues" \ "Related") \@ "currCode"
          val total = (su \ "Revenues" \ "Total") \@ "currCode"
          val profitOrLoss = (su \ "ProfitOrLoss") \@ "currCode"
          val taxPaid = (su \ "TaxPaid") \@ "currCode"
          val taxAccrued = (su \ "TaxAccrued") \@ "currCode"
          val capital = (su \ "Capital") \@ "currCode"
          val earnings = (su \ "Earnings") \@ "currCode"
          val assets = (su \ "Assets") \@ "currCode"

          RawCurrencyCodes(
            List(unrelated, related, total, profitOrLoss, taxPaid, taxAccrued, capital, earnings, assets))
        }

    case List("CBC_OECD", "CbcBody", "AdditionalInfo") =>
      ds =>
        {
          val otherInfo = (ds \ "OtherInfo").text
          RawAdditionalInfo(getDocSpec((ds \ "DocSpec").head), otherInfo)
        }

  }

  def extract(file: File): RawXMLInfo = {

    val collectedData: (List[RawXmlFields], Int) = {

      val xmlEventReader = nonFatalCatch opt xmlInputFactory.createXMLEventReader(
        Source.fromFile(file).bufferedReader())
      try {
        val fields = xmlEventReader.map(_.toIterator.scanCollect(splitter.Scan).toList).toList.flatten
        val numBodies = countBodys(file)
        (fields, numBodies)
      } finally {
        xmlEventReader.foreach(_.close())
      }
    }

    val xe = extractEncoding(file)
    val cv = extractCbcVal(file)
    val ms = collectedData._1
      .collectFirst { case ms: RawMessageSpec => ms }
      .getOrElse(RawMessageSpec("", "", "", "", "", None, None))
    val re = collectedData._1.collectFirst { case re: RawReportingEntity                             => re }
    val ai = collectedData._1.collect { case ai: RawAdditionalInfo                                   => ai }
    val cr = collectedData._1.collect { case cr: RawCbcReports                                       => cr }
    val cen = collectedData._1.collect { case cen: RawConstEntityName                                => cen.name }
    val currencyCodes: List[RawCurrencyCodes] = collectedData._1.collect { case cc: RawCurrencyCodes => cc }

    RawXMLInfo(ms, re, cr, ai, cv, xe, collectedData._2, cen, currencyCodes)

  }

}
