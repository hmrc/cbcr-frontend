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
import java.time.{LocalDate, LocalDateTime}

import uk.gov.hmrc.cbcrfrontend.model._

class BusinessRulesUtilSpec extends UnitSpec {

  val docRefId = "GB2016RGXVCBC0000000056CBC40120170311T090000X_7000000002OECD1"
  val corrDocRefId = "GB2016RGXVCBC0000000056CBC40220170311T090000X_7000000002OECD2"
  val reportingEntity =
    ReportingEntity(
      CBC701,
      DocSpec(OECD1, DocRefId(docRefId + "ENT").get, Some(CorrDocRefId(DocRefId(corrDocRefId + "ENT").get)), None),
      TIN("1000000019", "GB"),
      "Mne",
      None,
      EntityReportingPeriod(LocalDate.parse("2016-01-31"), LocalDate.parse("2017-01-30"))
    )

  def xmlinfo(reportingEntity: Option[ReportingEntity]) = XMLInfo(
    MessageSpec(
      MessageRefID("GB2016RGXVCBC0000000056CBC40120170311T090000X").getOrElse(fail("waaaaa")),
      "GB",
      CBCId.create(99).getOrElse(fail("booo")),
      LocalDateTime.now(),
      LocalDate.parse("2017-01-30"),
      None,
      None
    ),
    reportingEntity,
    List(CbcReports(DocSpec(OECD1, DocRefId(docRefId + "REP").get, None, None))),
    List(AdditionalInfo(DocSpec(OECD1, DocRefId(docRefId + "ADD").get, None, None), "Some Other Info")),
    Some(LocalDate.now()),
    List.empty[String],
    List.empty[String]
  )

  val xmlInfo2 = XMLInfo(
    MessageSpec(
      MessageRefID("GB2016RGXVCBC0000000056CBC40120170311T090000X").getOrElse(fail("waaaaa")),
      "GB",
      CBCId.create(99).getOrElse(fail("booo")),
      LocalDateTime.now(),
      LocalDate.parse("2017-01-30"),
      None,
      None
    ),
    Some(reportingEntity),
    List(
      CbcReports(
        DocSpec(OECD1, DocRefId(docRefId + "REP").get, Some(CorrDocRefId(DocRefId(corrDocRefId + "REP").get)), None))),
    List(
      AdditionalInfo(
        DocSpec(OECD1, DocRefId(docRefId + "ADD").get, Some(CorrDocRefId(DocRefId(corrDocRefId + "ADD").get)), None),
        "Some Other Info")),
    Some(LocalDate.now()),
    List.empty[String],
    List.empty[String]
  )
  val list1 = List("docref1", "docref2", "docref3")
  val list2 = List("docref2", "docref1", "docref3")
  val list3 = List("docref1")
  val expectedList = List(
    "GB2016RGXVCBC0000000056CBC40220170311T090000X_7000000002OECD2ENT",
    "GB2016RGXVCBC0000000056CBC40220170311T090000X_7000000002OECD2REP",
    "GB2016RGXVCBC0000000056CBC40220170311T090000X_7000000002OECD2ADD"
  )

  "The business rules util" should {

    "isFullyCorrected should return true when the lists are the same no matter of the order of elements" in {
      BusinessRulesUtil.isFullyCorrected(list1, list2) shouldBe true
    }

    "isFullrCorrected should return false when lists are not the same" in {
      BusinessRulesUtil.isFullyCorrected(list1, list3) shouldBe false
    }

    "Extract all doc types should work as expected" in {
      BusinessRulesUtil.extractAllDocTypes(xmlinfo(None)) shouldBe List("OECD1", "OECD1")
      BusinessRulesUtil.extractAllDocTypes(xmlinfo(Some(reportingEntity))) shouldBe List("OECD1", "OECD1", "OECD1")
    }

    "Extract all corr doc ref ids should extract every occurrence of corr doc ref id" in {
      BusinessRulesUtil.extractAllCorrDocRefIds(xmlinfo(Some(reportingEntity))) shouldBe List(
        "GB2016RGXVCBC0000000056CBC40220170311T090000X_7000000002OECD2ENT")
      BusinessRulesUtil.extractAllCorrDocRefIds(xmlinfo(None)) shouldBe List()
      BusinessRulesUtil.extractAllCorrDocRefIds(xmlInfo2) shouldBe expectedList
    }
  }
}
