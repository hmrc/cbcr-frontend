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

package uk.gov.hmrc.cbcrfrontend.services

import cats.instances.all._
import cats.syntax.all._
import org.codehaus.stax2.XMLInputFactory2
import play.api.Logging
import uk.gov.hmrc.cbcrfrontend.model._

import java.io.File
import javax.xml.stream.{XMLInputFactory, XMLStreamConstants}
import scala.util.control.Exception.nonFatalCatch
import scala.util.control.NonFatal
import scala.xml.{Node, NodeSeq}

class XmlInfoExtract extends Logging {
  private val xmlInputFactory = XMLInputFactory.newInstance.asInstanceOf[XMLInputFactory2]
  xmlInputFactory.setProperty(XMLInputFactory.SUPPORT_DTD, false)
  xmlInputFactory.setProperty("javax.xml.stream.isSupportingExternalEntities", false)

  implicit class NodeSeqPimp(n: NodeSeq) {
    def textOption: Option[String] = n.map(_.text).headOption
    def text: String = textOption.orEmpty
  }

  private def getDocSpec(e: Node) = {
    val docType = (e \ "DocTypeIndic").text
    val docRefId = (e \ "DocRefId").text
    val corrDocRefId = (e \ "CorrDocRefId").textOption
    val corrMessageRefId = (e \ "CorrMessageRefId").textOption
    RawDocSpec(docType, docRefId, corrDocRefId, corrMessageRefId)
  }

  def extract(file: File): RawXMLInfo = {
    val nodes = scala.xml.XML.loadFile(file)

    val cbc = nodes \\ "CBC_OECD"

    val cbcBody = cbc \\ "CbcBody"

    val cbcReports = cbc \\ "CbcReports"

    def extractMessageSpec =
      (cbc \ "MessageSpec").iterator
        .map(ms => {
          val msgRefId = (ms \ "MessageRefId").text
          val receivingCountry = (ms \ "ReceivingCountry").text
          val sendingEntityIn = (ms \ "SendingEntityIN").text
          val timestamp = (ms \ "Timestamp").text
          val msgType = (ms \ "MessageTypeIndic").textOption
          val reportingPeriod = (ms \ "ReportingPeriod").text
          val corrMsgRefId = (ms \ "CorrMessageRefId").textOption
          RawMessageSpec(msgRefId, receivingCountry, sendingEntityIn, timestamp, reportingPeriod, msgType, corrMsgRefId)
        })
        .toList
        .headOption
        .getOrElse(RawMessageSpec("", "", "", "", "", None, None))

    def extractReportingEntity = {
      def getAddressCity(e: Option[Node]) =
        e match {
          case Some(node) => (node \ "AddressFix" \ "City").textOption
          case None       => None
        }

      (cbcBody \ "ReportingEntity").iterator
        .map(re => {
          val tin = (re \ "Entity" \ "TIN").text
          val tinIB = (re \ "Entity" \ "TIN") \@ "issuedBy"
          val name = (re \ "Entity" \ "Name").text
          val city = getAddressCity((re \ "Entity" \ "Address").headOption)
          val rr = (re \ "ReportingRole").text
          val ds = getDocSpec((re \ "DocSpec").head) //DocSpec is required in ReportingEntity so this will exist!
          val startDate = (re \ "ReportingPeriod" \ "StartDate").text
          val endDate = (re \ "ReportingPeriod" \ "EndDate").text
          RawReportingEntity(rr, ds, tin, tinIB, name, city, startDate, endDate)
        })
        .toList
        .headOption
    }

    def extractCbcReports =
      (cbcReports \ "DocSpec").iterator
        .map(
          ds => RawCbcReports(getDocSpec(ds))
        )
        .toList

    def extractAdditionalInfo =
      (cbcBody \ "AdditionalInfo").iterator
        .map(ds => {
          val otherInfo = (ds \ "OtherInfo").text
          RawAdditionalInfo(getDocSpec((ds \ "DocSpec").head), otherInfo)
        })
        .toList

    def extractCbcVal(input: File) = {
      val xmlStreamReader = xmlInputFactory.createXMLStreamReader(input)

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
          logger.warn(s"extractCbcVal encountered the following error: ${e.getMessage}")
          RawCbcVal("")
        },
        cbcVal => cbcVal
      )
    }

    def extractEncoding(input: File) = {
      val xmlStreamReader = xmlInputFactory.createXMLStreamReader(input)

      val encodingVal = xmlStreamReader.getCharacterEncodingScheme
      xmlStreamReader.closeCompletely()

      encodingVal match {
        case null => None
        case _    => Some(RawXmlEncodingVal(encodingVal))
      }
    }

    // sorry but speed
    def countBodies(input: File) = {
      val xmlStreamReader = xmlInputFactory.createXMLStreamReader(input)
      var count = 0
      try {
        while (xmlStreamReader.hasNext) {
          val event = xmlStreamReader.next()
          if (event == XMLStreamConstants.START_ELEMENT && xmlStreamReader.getLocalName.equalsIgnoreCase("CbcBody"))
            count = count + 1
        }
      } catch {
        case NonFatal(e) => logger.warn(s"Error counting CBCBody elements: ${e.getMessage}")
      }
      count
    }

    def extractEntityNames =
      (cbcReports \ "ConstEntities" \ "ConstEntity" \ "Name").iterator
        .map(ds => RawConstEntityName(ds.text).name)
        .toList

    def extractCurrencyCodes =
      (cbcReports \ "Summary").iterator
        .map(su => {
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
        })
        .toList

    val ms = extractMessageSpec

    val re = extractReportingEntity

    val cr = extractCbcReports

    val ai = extractAdditionalInfo

    val cv = extractCbcVal(file)

    val xe = extractEncoding(file)

    val numBodies = countBodies(file)

    val cen = extractEntityNames

    val currencyCodes = extractCurrencyCodes

    RawXMLInfo(ms, re, cr, ai, cv, xe, numBodies, cen, currencyCodes)
  }
}
