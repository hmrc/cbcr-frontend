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
import org.mockito.ArgumentMatchersSugar.*
import org.mockito.IdiomaticMockito
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.libs.json.{JsNull, JsString, Json}
import play.api.test.Helpers.{await, defaultAwaitTimeout}
import uk.gov.hmrc.cbcrfrontend.connectors.CBCRBackendConnector
import uk.gov.hmrc.cbcrfrontend.controllers.CSRFTest
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import java.time.LocalDate
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class ReportingEntityDataServiceSpec
    extends AnyWordSpec with Matchers with GuiceOneAppPerSuite with CSRFTest with IdiomaticMockito {

  private val connector = mock[CBCRBackendConnector]
  private val reds = new ReportingEntityDataService(connector)
  private val docRefId = DocRefId("GB2016RGXVCBC0000000056CBC40120170311T090000X_7000000002OECD1REP").get
  private val crnDocRefId = DocRefId("GB2016RGXVCBC0000000056CBC40120170311T090000A_7000000002OECD1REP").get
  private val corrDocRefId = CorrDocRefId(crnDocRefId)
  private val cbcid = CBCId.create(1).toOption
  private val tin = TIN("90000000001", "")
  private val reportingPeriod = "2016-03-31"
  private val docRefIdpair = DocRefIdPair(docRefId: DocRefId, Some(corrDocRefId))

  private val red = ReportingEntityData(
    NonEmptyList(docRefId, Nil),
    List(docRefId),
    docRefId,
    TIN("90000000001", ""),
    UltimateParentEntity("Foo Corp"),
    CBC701,
    Some(LocalDate.now()),
    None,
    Some("USD"),
    Some(EntityReportingPeriod(LocalDate.parse("2016-01-01"), LocalDate.parse("2016-03-31")))
  )

  private val redModel = ReportingEntityDataModel(
    NonEmptyList(docRefId, Nil),
    List(docRefId),
    docRefId,
    TIN("90000000001", ""),
    UltimateParentEntity("Foo Corp"),
    CBC701,
    Some(LocalDate.now()),
    None,
    oldModel = true,
    Some("USD"),
    None
  )

  DocRefIdPair(docRefId: DocRefId, Some(corrDocRefId))

  private val partialRed = PartialReportingEntityData(
    List(docRefIdpair),
    List(docRefIdpair),
    docRefIdpair,
    TIN("90000000001", ""),
    UltimateParentEntity("Foo Corp"),
    CBC701,
    Some(LocalDate.now()),
    None,
    Some("USD"),
    Some(EntityReportingPeriod(LocalDate.parse("2016-01-01"), LocalDate.parse("2016-03-31")))
  )

  implicit val hc: HeaderCarrier = HeaderCarrier()

  "ReportingEntityDataService" should {
    "provide a query service" which {
      "returns a reportingEntityData object the call to the connector returns one" in {
        connector.reportingEntityDataQuery(*)(*) returns Future.successful(
          HttpResponse(Status.OK, Json.toJson(red), Map.empty[String, Seq[String]])
        )
        val result = await(reds.queryReportingEntityData(docRefId).value)
        result shouldBe Right(Some(red))
      }

      "return NONE if the connector returns a NotFoundException" in {
        connector.reportingEntityDataQuery(*)(*) returns Future.successful(
          HttpResponse(Status.NOT_FOUND, JsNull, Map.empty[String, Seq[String]])
        )
        val result = await(reds.queryReportingEntityData(docRefId).value)
        result shouldBe Right(None)
      }

      "return an error if there is a serialisation error" in {
        connector.reportingEntityDataQuery(*)(*) returns Future.successful(
          HttpResponse(Status.OK, JsString("Not the correct json"), Map.empty[String, Seq[String]])
        )
        val result = await(reds.queryReportingEntityData(docRefId).value)
        result.isLeft shouldBe true
      }

      "return an error if anything else goes wrong" in {
        connector.reportingEntityDataQuery(*)(*) returns Future.failed(new Exception("The sky is falling"))
        val result = await(reds.queryReportingEntityData(docRefId).value)
        result.isLeft shouldBe true
      }
    }

    "provide a query service by cbc id and reporting period to prevent multiple file upload for original submission within same reporting period" which {
      "returns a reportingEntityData object the call to the connector returns one" in {
        connector.reportingEntityCBCIdAndReportingPeriod(*, *)(*) returns Future.successful(
          HttpResponse(Status.OK, Json.toJson(red), Map.empty[String, Seq[String]])
        )
        val result = await(reds.queryReportingEntityDataByCbcId(cbcid.get, LocalDate.now()).value)
        result shouldBe Right(Some(red))
      }
      "return NONE if the connector returns a NotFoundException" in {
        connector.reportingEntityCBCIdAndReportingPeriod(*, *)(*) returns Future.successful(
          HttpResponse(Status.NOT_FOUND, JsNull, Map.empty[String, Seq[String]])
        )
        val result = await(reds.queryReportingEntityDataByCbcId(cbcid.get, LocalDate.now()).value)
        result shouldBe Right(None)
      }
      "return an error if there is a serialisation error" in {
        connector.reportingEntityCBCIdAndReportingPeriod(*, *)(*) returns Future.successful(
          HttpResponse(Status.OK, JsString("Not the correct json"), Map.empty[String, Seq[String]])
        )
        val result = await(reds.queryReportingEntityDataByCbcId(cbcid.get, LocalDate.now()).value)
        result.isLeft shouldBe true
      }
      "return an error if anything else goes wrong" in {
        connector.reportingEntityCBCIdAndReportingPeriod(*, *)(*) returns Future.failed(
          new Exception("The sky is falling")
        )
        val result = await(reds.queryReportingEntityDataByCbcId(cbcid.get, LocalDate.now()).value)
        result.isLeft shouldBe true
      }
    }
  }

  "ReportingEntityDataService" should {
    "return ReportingEntityDataModel if it exists in the DB store" in {
      connector.reportingEntityDataModelQuery(*)(*) returns Future.successful(
        HttpResponse(Status.OK, Json.toJson(redModel), Map.empty[String, Seq[String]])
      )
      val result = await(reds.queryReportingEntityDataModel(docRefId).value)
      result shouldBe Right(Some(redModel))
    }

    "return an error if there is a serialisation error" in {
      connector.reportingEntityDataModelQuery(*)(*) returns Future.successful(
        HttpResponse(Status.OK, JsString("Not the correct json"), Map.empty[String, Seq[String]])
      )
      val result = await(reds.queryReportingEntityDataModel(docRefId).value)
      result.isLeft shouldBe true
    }

    "return NONE if the connector returns a NotFoundException" in {
      connector.reportingEntityDataModelQuery(*)(*) returns Future.successful(
        HttpResponse(Status.NOT_FOUND, JsNull, Map.empty[String, Seq[String]])
      )
      val result = await(reds.queryReportingEntityDataModel(docRefId).value)
      result shouldBe Right(None)
    }

    "return an error if anything else goes wrong" in {
      connector.reportingEntityDataModelQuery(*)(*) returns Future.failed(new Exception("The sky is falling"))
      val result = await(reds.queryReportingEntityDataModel(docRefId).value)
      result.isLeft shouldBe true
    }
  }

  "ReportingEntityDataService on a call to queryReportingEntityDataDocRefId" should {
    "return ReportingEntityData if it exists in the DB store" in {
      connector.reportingEntityDocRefId(*)(*) returns Future.successful(
        HttpResponse(Status.OK, Json.toJson(red), Map.empty[String, Seq[String]])
      )
      val result = await(reds.queryReportingEntityDataDocRefId(docRefId).value)
      result shouldBe Right(Some(red))
    }

    "return an error if there is a serialisation error while parsing for ReportingEntityData" in {
      connector.reportingEntityDocRefId(*)(*) returns Future.successful(
        HttpResponse(Status.OK, JsString("Not the correct json"), Map.empty[String, Seq[String]])
      )
      val result = await(reds.queryReportingEntityDataDocRefId(docRefId).value)
      result.isLeft shouldBe true
    }

    "return NONE if the connector returns a NotFoundException" in {
      connector.reportingEntityDocRefId(*)(*) returns Future.successful(
        HttpResponse(Status.NOT_FOUND, JsNull, Map.empty[String, Seq[String]])
      )
      val result = await(reds.queryReportingEntityDataDocRefId(docRefId).value)
      result shouldBe Right(None)
    }

    "return an error if anything else goes wrong" in {
      connector.reportingEntityDocRefId(*)(*) returns Future.failed(new Exception("The sky is falling"))
      val result = await(reds.queryReportingEntityDataDocRefId(docRefId).value)
      result.isLeft shouldBe true
    }
  }

  "ReportingEntityDataService on a call to queryReportingEntityDataTin" should {
    "return ReportingEntityData if it exists in the DB store" in {
      connector.reportingEntityDataQueryTin(*, *)(*) returns Future.successful(
        HttpResponse(Status.OK, Json.toJson(red), Map.empty[String, Seq[String]])
      )
      val result = await(reds.queryReportingEntityDataTin(tin.value, reportingPeriod).value)
      result shouldBe Right(Some(red))
    }

    "return an error if there is a serialisation error while parsing for ReportingEntityData" in {
      connector.reportingEntityDataQueryTin(*, *)(*) returns Future.successful(
        HttpResponse(Status.OK, JsString("Not the correct json"), Map.empty[String, Seq[String]])
      )
      val result = await(reds.queryReportingEntityDataTin(tin.value, reportingPeriod).value)
      result.isLeft shouldBe true
    }

    "return NONE if the connector returns a NotFoundException" in {
      connector.reportingEntityDataQueryTin(*, *)(*) returns Future.successful(
        HttpResponse(Status.NOT_FOUND, JsNull, Map.empty[String, Seq[String]])
      )
      val result = await(reds.queryReportingEntityDataTin(tin.value, reportingPeriod).value)
      result shouldBe Right(None)
    }

    "return an error if anything else goes wrong" in {
      connector.reportingEntityDataQueryTin(*, *)(*) returns Future.failed(new Exception("The sky is falling"))
      val result = await(reds.queryReportingEntityDataTin(tin.value, reportingPeriod).value)
      result.isLeft shouldBe true
    }
  }

  "ReportingEntityDataService on a call to updateReportingEntityData" should {
    "update ReportingEntityData if it exists in the DB store" in {
      connector.reportingEntityDataUpdate(*)(*) returns Future.successful(
        HttpResponse(Status.OK, JsNull, Map.empty[String, Seq[String]])
      )
      val result = await(reds.updateReportingEntityData(partialRed).value)
      result.isRight shouldBe true
    }

    "return an error if anything else goes wrong" in {
      connector.reportingEntityDataUpdate(*)(*) returns Future.failed(new Exception("The sky is falling"))
      val result = await(reds.updateReportingEntityData(partialRed).value)
      result.isLeft shouldBe true
    }
  }

  "ReportingEntityDataService on a call to saveReportingEntityData" should {
    "save ReportingEntityData if it does not exist in the DB store" in {
      connector.reportingEntityDataSave(*)(*) returns Future.successful(
        HttpResponse(Status.OK, JsNull, Map.empty[String, Seq[String]])
      )
      val result = await(reds.saveReportingEntityData(red).value)
      result.isRight shouldBe true
    }

    "return an error if anything else goes wrong" in {
      connector.reportingEntityDataSave(*)(*) returns Future.failed(new Exception("The sky is falling"))
      val result = await(reds.saveReportingEntityData(red).value)
      result.isLeft shouldBe true
    }
  }
}
