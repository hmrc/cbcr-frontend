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

import cats.data.NonEmptyList
import cats.implicits.catsStdInstancesForFuture
import org.mockito.ArgumentMatchersSugar.*
import org.mockito.IdiomaticMockito
import org.mockito.cats.IdiomaticMockitoCats.StubbingOpsCats
import org.mockito.cats.MockitoCats
import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Configuration
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.http.HeaderCarrier

import java.time.{LocalDate, LocalDateTime}
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}

class CreationDateSpec
    extends AnyWordSpec with Matchers with BeforeAndAfterEach with GuiceOneAppPerSuite with IdiomaticMockito with MockitoCats {

  private val reportingEntity = mock[ReportingEntityDataService]
  private val configuration = mock[Configuration]
  private val runMode: RunMode = mock[RunMode]

  runMode.env returns  "Dev"
  configuration.getOptional[Int](s"${runMode.env}.default-creation-date.day") returns  Some(23)
  configuration.getOptional[Int](s"${runMode.env}.default-creation-date.month") returns  Some(12)
  configuration.getOptional[Int](s"${runMode.env}.default-creation-date.year") returns  Some(2020)

  private implicit val ec: ExecutionContext = app.injector.instanceOf[ExecutionContext]
  private implicit val hc: HeaderCarrier = HeaderCarrier()

  private val cds = new CreationDateService(configuration, runMode, reportingEntity)

  private val docRefId = "GB2016RGXVCBC0000000056CBC40120170311T090000X_7000000002OECD1"
  private val actualDocRefId = DocRefId("GB2016RGXGCBC0100000132CBC40120170311T090000X_4590617080OECD2ADD62").get
  private val actualDocRefId2 = DocRefId("GB2016RGXGCBC0100000132CBC40120170311T090000X_4590617080OECD2ADD63").get

  private val redNoCreationDate = ReportingEntityData(
    NonEmptyList.of(actualDocRefId),
    List(actualDocRefId2),
    actualDocRefId,
    TIN("asdf", "lkajsdf"),
    UltimateParentEntity("someone"),
    CBC701,
    None,
    None,
    Some("USD"),
    None
  )

  private val redOldCreationDate = ReportingEntityData(
    NonEmptyList.of(actualDocRefId),
    List(actualDocRefId2),
    actualDocRefId,
    TIN("asdf", "lkajsdf"),
    UltimateParentEntity("someone"),
    CBC701,
    Some(LocalDate.parse("2010-01-01")),
    None,
    Some("USD"),
    None
  )

  private val red = ReportingEntityData(
    NonEmptyList.of(actualDocRefId),
    List(actualDocRefId2),
    actualDocRefId,
    TIN("asdf", "lkajsdf"),
    UltimateParentEntity("someone"),
    CBC701,
    Some(LocalDate.now()),
    None,
    Some("USD"),
    Some(EntityReportingPeriod(LocalDate.parse("2017-05-05"), LocalDate.parse("2018-01-01")))
  )

  private val messageSpec = MessageSpec(
    MessageRefID("GB2016RGXVCBC0000000056CBC40120170311T090000X").getOrElse(fail("waaaaa")),
    "GB",
    CBCId.create(99).getOrElse(fail("booo")),
    LocalDateTime.now(),
    LocalDate.parse("2017-01-30"),
    None,
    None
  )

  private val xmlInfo = XMLInfo(
    messageSpec,
    None,
    List(CbcReports(DocSpec(OECD2, DocRefId(docRefId + "ENT").get, Some(CorrDocRefId(actualDocRefId)), None))),
    List(AdditionalInfo(DocSpec(OECD1, DocRefId(docRefId + "ADD").get, None, None), "Some Other Info")),
    Some(LocalDate.now()),
    List.empty[String],
    List.empty[String]
  )

  override protected def afterEach(): Unit = {
    reset(configuration)
    super.afterEach()
  }

  "The CreationDateService" should {
    "return true" when {
      "repotingEntity creationDate is Null and default date of 2020/12/23 is less than 3 years ago" in {
        reportingEntity.queryReportingEntityData(*)(*) returnsF Some(redNoCreationDate)
        val result = Await.result(cds.isDateValid(xmlInfo), 5.seconds)
        result shouldBe true
      }

      "repotingEntity creationDate is less than 3 years ago" in {
        reportingEntity.queryReportingEntityData(*)(*) returnsF Some(red)
        val result = Await.result(cds.isDateValid(xmlInfo), 5.seconds)
        result shouldBe true
      }
    }

    "return false" when {
      "reportingEntity creationDate is older than 3 years ago" in {
        reportingEntity.queryReportingEntityData(*)(*) returnsF Some(redOldCreationDate)
        val result = Await.result(cds.isDateValid(xmlInfo), 5.seconds)
        result shouldBe false
      }

      "reportingEntity creationDate is Null and default date is more than 3 years ago" in {
        configuration.getOptional[Int](s"${runMode.env}.default-creation-date.year") returns Some(2017)
        runMode.env returns  "Dev"
        configuration.getOptional[Int](s"${runMode.env}.default-creation-date.year") returns Some(2010)
        configuration.getOptional[Int](s"${runMode.env}.default-creation-date.day") returns Some(23)
        configuration.getOptional[Int](s"${runMode.env}.default-creation-date.month") returns Some(12)
        val cds2 = new CreationDateService(configuration, runMode, reportingEntity)
        val result = Await.result(cds2.isDateValid(xmlInfo), 5.seconds)
        result shouldBe false
      }
    }
  }
}
