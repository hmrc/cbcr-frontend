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

import java.time.{LocalDate, LocalDateTime}

import cats.data.{EitherT, NonEmptyList}
import org.mockito.Mockito._
import org.mockito.Matchers._
import org.scalatest.mockito.MockitoSugar
import play.api.Configuration
import uk.gov.hmrc.cbcrfrontend.connectors.CBCRBackendConnector
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.Await
import cats.instances.future._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.ScalaFutures
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}


class CreationDateSpec extends UnitSpec with ScalaFutures with MockitoSugar with BeforeAndAfterEach{

  val connector        = mock[CBCRBackendConnector]
  val reportingEntity  = mock[ReportingEntityDataService]
  val configuration    = mock[Configuration]
  val runMode:RunMode  = mock[RunMode]

  when(runMode.env) thenReturn "Dev"
  when(configuration.getInt(s"${runMode.env}.default-creation-date.day")) thenReturn Future.successful(Some(23))
  when(configuration.getInt(s"${runMode.env}.default-creation-date.month")) thenReturn Future.successful(Some(12))
  when(configuration.getInt(s"${runMode.env}.default-creation-date.year")) thenReturn Future.successful(Some(2017))


  implicit val ec:ExecutionContext = mock[ExecutionContext]
  implicit val hc:HeaderCarrier = HeaderCarrier()

  val cds = new CreationDateService(connector,configuration,runMode,reportingEntity)

  val docRefId="GB2016RGXVCBC0000000056CBC40120170311T090000X_7000000002OECD1"
  val actualDocRefId = DocRefId("GB2016RGXGCBC0100000132CBC40120170311T090000X_4590617080OECD2ADD62").get

  val redNoCreationDate = ReportingEntityData(NonEmptyList.of(actualDocRefId),None,actualDocRefId,TIN("asdf","lkajsdf"),UltimateParentEntity("someone"),CBC701,None)
  val redOldCreationDate = ReportingEntityData(NonEmptyList.of(actualDocRefId),None,actualDocRefId,TIN("asdf","lkajsdf"),UltimateParentEntity("someone"),CBC701,Some(LocalDate.parse("2010-01-01")))
  val red = ReportingEntityData(NonEmptyList.of(actualDocRefId),None,actualDocRefId,TIN("asdf","lkajsdf"),UltimateParentEntity("someone"),CBC701,Some(LocalDate.now()))

  val messageSpec = MessageSpec(
    MessageRefID("GB2016RGXVCBC0000000056CBC40120170311T090000X").getOrElse(fail("waaaaa")),
    "GB",
    CBCId.create(99).getOrElse(fail("booo")),
    LocalDateTime.now(),
    LocalDate.parse("2017-01-30"),
    None,
    None
  )

  val xmlinfo = XMLInfo(
    messageSpec,
    None,
    List(CbcReports(DocSpec(OECD2,DocRefId(docRefId + "ENT").get,Some(CorrDocRefId(actualDocRefId))))),
    Some(AdditionalInfo(DocSpec(OECD1,DocRefId(docRefId + "ADD").get,None))),
    Some(LocalDate.now()),
    List.empty[String]
  )

  override protected def afterEach(): Unit = {
    reset(configuration)
    super.afterEach()
  }


  "The CreationDateService" should {
    "return true" when {
      "repotingEntity creationDate is Null and default date of 2017/12/23 is less than 3 years ago" in {
        when(reportingEntity.queryReportingEntityData(any())(any())) thenReturn EitherT.pure[Future,CBCErrors,Option[ReportingEntityData]](Some(redNoCreationDate))
       val result = Await.result(cds.isDateValid(xmlinfo), 5.seconds)
        result shouldBe true
      }
      "repotingEntity creationDate is less than 3 years ago" in {
        when(reportingEntity.queryReportingEntityData(any())(any())) thenReturn EitherT.pure[Future,CBCErrors,Option[ReportingEntityData]](Some(red))
        val result = Await.result(cds.isDateValid(xmlinfo), 5.seconds)
        result shouldBe true
      }
    }
    "return false" when {
      "reportingEntity creationDate is older than 3 years ago" in {
        when(reportingEntity.queryReportingEntityData(any())(any())) thenReturn EitherT.pure[Future,CBCErrors,Option[ReportingEntityData]](Some(redOldCreationDate))
        val result = Await.result(cds.isDateValid(xmlinfo), 5.seconds)
        result shouldBe false
      }
      "repotingEntity creationDate is Null and default date is more than 3 years ago" in {when(configuration.getInt(s"${runMode.env}.default-creation-date.year")) thenReturn Future.successful(Some(2017))
        when(runMode.env) thenReturn "Dev"
        when(configuration.getInt(s"${runMode.env}.default-creation-date.year")) thenReturn Future.successful(Some(2010))
        when(configuration.getInt(s"${runMode.env}.default-creation-date.day")) thenReturn Future.successful(Some(23))
        when(configuration.getInt(s"${runMode.env}.default-creation-date.month")) thenReturn Future.successful(Some(12))
        val cds2 = new CreationDateService(connector,configuration,runMode,reportingEntity)
        val result = Await.result(cds2.isDateValid(xmlinfo), 5.seconds)
        result shouldBe false
      }
    }
  }
}
