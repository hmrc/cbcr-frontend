/*
 * Copyright 2024 HM Revenue & Customs
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

import org.mockito.ArgumentMatchers.{any, eq => eqTo}
import org.mockito.Mockito.when
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.mockito.MockitoSugar
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.libs.json.JsNull
import play.api.test.Helpers.{await, defaultAwaitTimeout}
import uk.gov.hmrc.cbcrfrontend.connectors.CBCRBackendConnector
import uk.gov.hmrc.cbcrfrontend.controllers.CSRFTest
import uk.gov.hmrc.cbcrfrontend.model.DocRefIdResponses.{DoesNotExist, Invalid, Valid}
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.http.{HeaderCarrier, HttpException, HttpResponse}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class DocRefIdServiceSpec extends AnyWordSpec with Matchers with GuiceOneAppPerSuite with CSRFTest with MockitoSugar {

  private val connector = mock[CBCRBackendConnector]
  private val docRefIdService = new DocRefIdService(connector)
  private val docRefId = DocRefId("GB2016RGXVCBC0000000056CBC40120170311T090000X_7000000002OECD1REP").get
  private val crnDocRefId = DocRefId("GB2016RGXVCBC0000000056CBC40120170311T090000A_7000000002OECD1REP").get
  private val corrDocRefId = CorrDocRefId(crnDocRefId)
  private implicit val hc: HeaderCarrier = HeaderCarrier()

  "DocRefIdService" should {
    "save a DocRefId in backend CBCR" in {
      when(connector.docRefIdSave(any)(any)).thenReturn(
        Future.successful(
          HttpResponse(Status.OK, JsNull, Map.empty[String, Seq[String]])
        )
      )
      val result = await(docRefIdService.saveDocRefId(docRefId).value)
      result shouldBe None
    }

    "return Unexpected error state with message if the connector returns a HttpException" in {
      when(connector.docRefIdSave(any)(any)).thenReturn(Future.failed(new HttpException("HttpException occurred", 400)))
      val result = await(docRefIdService.saveDocRefId(docRefId).value)
      result.get.getClass.getName shouldBe "uk.gov.hmrc.cbcrfrontend.model.UnexpectedState"
    }

    "return an error if anything else goes wrong and if the connector).thenReturn(any other Exception" in {
      when(connector.docRefIdSave(any)(any)).thenReturn(Future.failed(new Exception("The sky is falling")))
      val result = await(docRefIdService.saveDocRefId(docRefId).value)
      result.get.errorMsg shouldBe "The sky is falling"
    }
  }

  "DocRefIdService" should {
    "save a CorrDocRefId in backend CBCR" in {
      when(connector.corrDocRefIdSave(any, any)(any)).thenReturn(
        Future.successful(
          HttpResponse(Status.OK, JsNull, Map.empty[String, Seq[String]])
        )
      )
      val result = await(docRefIdService.saveCorrDocRefID(corrDocRefId, docRefId).value)
      result shouldBe None
    }

    "return Unexpected error state with message if the connector returns a HttpException if it fails to save CorrDocRefId" in {
      when(connector.corrDocRefIdSave(any, any)(any))
        .thenReturn(Future.failed(new HttpException("HttpException occurred", 400)))
      val result = await(docRefIdService.saveCorrDocRefID(corrDocRefId, docRefId).value)
      result.get.getClass.getName shouldBe "uk.gov.hmrc.cbcrfrontend.model.UnexpectedState"
    }

    "return an error if anything else goes wrong and if the connector returns any other Exception if it fails to save CorrDocRefId" in {
      when(connector.corrDocRefIdSave(any, any)(any)).thenReturn(Future.failed(new Exception("The sky is falling")))
      val result = await(docRefIdService.saveCorrDocRefID(corrDocRefId, docRefId).value)
      result.get.errorMsg shouldBe "The sky is falling"
    }
  }

  "DocRefIdService" should {
    "return a docRefId valid state from backend CBCR if it exists in the DB" in {
      when(connector.docRefIdQuery(any)(any)).thenReturn(
        Future.successful(
          HttpResponse(Status.OK, JsNull, Map.empty[String, Seq[String]])
        )
      )
      val result = await(docRefIdService.queryDocRefId(docRefId))
      result shouldBe Valid
    }

    "return DoesNotExist error state with message if the docRefId is not found in the DB" in {
      when(connector.docRefIdQuery(any)(any)).thenReturn(
        Future.successful(
          HttpResponse(Status.NOT_FOUND, JsNull, Map.empty[String, Seq[String]])
        )
      )
      val result = await(docRefIdService.queryDocRefId(docRefId))
      result shouldBe DoesNotExist
    }

    "return an Invalid state of DocRefId if an exception occurs while retrieving " in {
      when(connector.docRefIdQuery(any)(any)).thenReturn(
        Future.successful(
          HttpResponse(Status.CONFLICT, JsNull, Map.empty[String, Seq[String]])
        )
      )
      val result = await(docRefIdService.queryDocRefId(docRefId))
      result shouldBe Invalid
    }
  }
}
