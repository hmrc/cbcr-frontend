/*
 * Copyright 2022 HM Revenue & Customs
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

import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.when
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mockito.MockitoSugar
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.libs.json.JsNull
import uk.gov.hmrc.cbcrfrontend.connectors.CBCRBackendConnector
import uk.gov.hmrc.cbcrfrontend.controllers.CSRFTest
import uk.gov.hmrc.cbcrfrontend.model.DocRefIdResponses.{DoesNotExist, Invalid, Valid}
import uk.gov.hmrc.cbcrfrontend.model.{CBCId, CorrDocRefId, DocRefId, DocRefIdPair, TIN}
import uk.gov.hmrc.cbcrfrontend.util.UnitSpec
import uk.gov.hmrc.http.{HeaderCarrier, HttpException, HttpResponse}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class DocRefIdServiceSpec extends UnitSpec with ScalaFutures with GuiceOneAppPerSuite with CSRFTest with MockitoSugar {

  val connector = mock[CBCRBackendConnector]
  val docRefIdService = new DocRefIdService(connector)
  val docRefId = DocRefId("GB2016RGXVCBC0000000056CBC40120170311T090000X_7000000002OECD1REP").get
  val crnDocRefId = DocRefId("GB2016RGXVCBC0000000056CBC40120170311T090000A_7000000002OECD1REP").get
  val corrDocRefId = CorrDocRefId(crnDocRefId)
  val cbcid = CBCId.create(1).toOption
  val tin = TIN("90000000001", "")
  val reportingPeriod = "31-03-2016"
  val docRefIdpair = DocRefIdPair(docRefId: DocRefId, Some(corrDocRefId))
  implicit val hc: HeaderCarrier = HeaderCarrier()

  "DocRefIdService" should {
    "save a DocRefId in backend CBCR" in {
      when(connector.docRefIdSave(any())(any())) thenReturn Future.successful(
        HttpResponse(Status.OK, JsNull, Map.empty[String, Seq[String]]))
      val result = Await.result(docRefIdService.saveDocRefId(docRefId).value, 2.seconds)
      result shouldBe None
    }
    "return Unexpected error state with message if the connector returns a HttpException" in {
      when(connector.docRefIdSave(any())(any())) thenReturn Future.failed(
        new HttpException("HttpException occurred", 400))
      val result = Await.result(docRefIdService.saveDocRefId(docRefId).value, 2.seconds)
      result.get.getClass.getName shouldBe "uk.gov.hmrc.cbcrfrontend.model.UnexpectedState"
    }

    "return an error if anything else goes wrong and if the connector returns any other Exception" in {
      when(connector.docRefIdSave(any())(any())) thenReturn Future.failed(new Exception("The sky is falling"))
      val result = Await.result(docRefIdService.saveDocRefId(docRefId).value, 2.seconds)
      result.get.errorMsg shouldBe "The sky is falling"
    }
  }

  "DocRefIdService" should {
    "save a CorrDocRefId in backend CBCR" in {
      when(connector.corrDocRefIdSave(any(), any())(any())) thenReturn Future.successful(
        HttpResponse(Status.OK, JsNull, Map.empty[String, Seq[String]]))
      val result = Await.result(docRefIdService.saveCorrDocRefID(corrDocRefId, docRefId).value, 2.seconds)
      result shouldBe None
    }
    "return Unexpected error state with message if the connector returns a HttpException if it fails to save CorrDocRefId" in {
      when(connector.corrDocRefIdSave(any(), any())(any())) thenReturn Future.failed(
        new HttpException("HttpException occurred", 400))
      val result = Await.result(docRefIdService.saveCorrDocRefID(corrDocRefId, docRefId).value, 2.seconds)
      result.get.getClass.getName shouldBe "uk.gov.hmrc.cbcrfrontend.model.UnexpectedState"
    }

    "return an error if anything else goes wrong and if the connector returns any other Exception if it fails to save CorrDocRefId" in {
      when(connector.corrDocRefIdSave(any(), any())(any())) thenReturn Future.failed(
        new Exception("The sky is falling"))
      val result = Await.result(docRefIdService.saveCorrDocRefID(corrDocRefId, docRefId).value, 2.seconds)
      result.get.errorMsg shouldBe "The sky is falling"
    }
  }

  "DocRefIdService" should {
    "return a docRefId valid state from backend CBCR if it exists in the DB" in {
      when(connector.docRefIdQuery(any())(any())) thenReturn Future.successful(
        HttpResponse(Status.OK, JsNull, Map.empty[String, Seq[String]]))
      val result = Await.result(docRefIdService.queryDocRefId(docRefId), 2.seconds)
      result shouldBe Valid
    }
    "return DoesNotExist error state with message if the docRefId is not found in the DB" in {
      when(connector.docRefIdQuery(any())(any())) thenReturn Future.successful(
        HttpResponse(Status.NOT_FOUND, JsNull, Map.empty[String, Seq[String]]))
      val result = Await.result(docRefIdService.queryDocRefId(docRefId), 2.seconds)
      result shouldBe DoesNotExist
    }

    "return an Invalid state of DocerefId if an exception occurs while retrieving " in {
      when(connector.docRefIdQuery(any())(any())) thenReturn Future.successful(
        HttpResponse(Status.CONFLICT, JsNull, Map.empty[String, Seq[String]]))
      val result = Await.result(docRefIdService.queryDocRefId(docRefId), 2.seconds)
      result shouldBe Invalid
    }
  }
}
