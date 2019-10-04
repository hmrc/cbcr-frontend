/*
 * Copyright 2019 HM Revenue & Customs
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

import java.io.{File, FileInputStream}

import uk.gov.hmrc.cbcrfrontend.model.RawCbcReports
import uk.gov.hmrc.cbcrfrontend.util.UnitSpec

class XmlInfoExtractSpec extends UnitSpec {

  private def loadFile(filename: String) = new File(s"test/resources/$filename")

  "An XmlInfoExtract object" should {
    "provide an extract method what extracts the correct information from an xml" in {
      val f = loadFile("cbcr-valid-dup.xml")

      val xmlInfoExtract = new XmlInfoExtract()

      val e = xmlInfoExtract.extract(f)

      e.messageSpec.messageRefID shouldBe "GB2016RGXLCBC0100000056CBC40120170311T090000X"
      e.messageSpec.receivingCountry shouldBe "GB"
      e.messageSpec.reportingPeriod shouldBe "2016-03-31"
      e.messageSpec.timestamp shouldBe "2016-11-01T15:00:00"

      e.xmlEncoding.get.xmlEncodingVal shouldBe "UTF-8"
      e.cbcVal.cbcVer shouldBe "1.0"

      val re = e.reportingEntity.get
      re.name shouldBe "ABCCorp"
      re.tin shouldBe "7000000002"
      re.docSpec.docType shouldBe "OECD1"
      re.docSpec.docRefId shouldBe "GB2016RGXLCBC0100000056CBC40120170311T090000X_7000000002OECD1ENT"
      re.docSpec.corrDocRefId shouldBe None

      val rs: List[RawCbcReports] = e.cbcReport

      rs.size shouldBe 4

      val r = rs.head
      r.docSpec.docType shouldBe "OECD1"
      r.docSpec.docRefId shouldBe "GB2016RGXLCBC0100000056CBC40120170311T090000X_7000000002OECD1REP"
      r.docSpec.corrDocRefId shouldBe None

      val r2 = rs.tail.head
      r2.docSpec.docType shouldBe "OECD1"
      r2.docSpec.docRefId shouldBe "GB2016RGXLCBC0100000056CBC40120170311T090000X_7000000002OECD1REP"
      r2.docSpec.corrDocRefId shouldBe None

      val r3 = rs.tail.tail.head
      r3.docSpec.docType shouldBe "OECD1"
      r3.docSpec.docRefId shouldBe "GB2016RGXLCBC0100000056CBC40120170311T090000X_7000000002OECD1REP3"
      r3.docSpec.corrDocRefId shouldBe None

      val r4 = rs.tail.tail.tail.head
      r4.docSpec.docType shouldBe "OECD2"
      r4.docSpec.docRefId shouldBe "GB2016RGXLCBC0100000056CBC40120170311T090000X_7000000002OECD1REP4"
      r4.docSpec.corrDocRefId shouldBe Some("GB2016RGXLCBC0100000056CBC40120170311T090000X_7000000002OECD1REP5")

      val a = e.additionalInfo.head

      a.docSpec.docType shouldBe "OECD1"
      a.docSpec.docRefId shouldBe "GB2016RGXLCBC0100000056CBC40120170311T090000X_7000000002OECD1ADD"
      a.docSpec.corrDocRefId shouldBe None

      val a2 = e.additionalInfo.tail.head

      a2.docSpec.docType shouldBe "OECD1"
      a2.docSpec.docRefId shouldBe "GB2016RGXLCBC0100000056CBC40120170311T090000X_7000000002OECD1ADD2"
      a2.docSpec.corrDocRefId shouldBe None

      e.constEntityNames should contain("name1")
      e.constEntityNames should contain("name2")
      e.constEntityNames should contain("name3")
      e.constEntityNames should contain("name4")

    }
  }

}
