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

import cats.data.NonEmptyList
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mock.MockitoSugar
import org.scalatestplus.play.OneAppPerSuite
import play.api.http.Status
import play.api.libs.json.{JsString, Json}
import uk.gov.hmrc.cbcrfrontend.connectors.CBCRBackendConnector
import uk.gov.hmrc.cbcrfrontend.controllers.CSRFTest
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse, NotFoundException}
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class ReportingEntityDataServiceSpec extends UnitSpec with ScalaFutures with OneAppPerSuite with CSRFTest with MockitoSugar {

  val connector = mock[CBCRBackendConnector]
  val reds = new ReportingEntityDataService(connector)
  val docRefId=DocRefId("GB2016RGXVCBC0000000056CBC40120170311T090000X_7000000002OECD1REP").get


  val red = ReportingEntityData(NonEmptyList(docRefId,Nil),Some(docRefId),docRefId,TIN("90000000001",""),UltimateParentEntity("Foo Corp"),CBC701)

  implicit val hc:HeaderCarrier = HeaderCarrier()

  "ReportingEntityDataService" should {
    "provide a query service" which {
      "returns a reportingEntityData object the call to the connector returns one" in{
        when(connector.reportingEntityDataQuery(any())(any())) thenReturn Future.successful(HttpResponse(Status.OK,Some(Json.toJson(red))))
        val result = Await.result(reds.queryReportingEntityData(docRefId).value, 2.seconds)
        result shouldBe Right(Some(red))
      }
      "return NONE if the connector returns a NotFoundException" in {
        when(connector.reportingEntityDataQuery(any())(any())) thenReturn Future.failed(new NotFoundException("Not found"))
        val result = Await.result(reds.queryReportingEntityData(docRefId).value, 2.seconds)
        result shouldBe Right(None)
      }
      "return an error if there is a serialisation error" in {
        when(connector.reportingEntityDataQuery(any())(any())) thenReturn Future.successful(HttpResponse(Status.OK,Some(JsString("Not the correct json"))))
        val result = Await.result(reds.queryReportingEntityData(docRefId).value, 2.seconds)
        result.isLeft shouldBe true
      }
      "return an error if anything else goes wrong" in {
        when(connector.reportingEntityDataQuery(any())(any())) thenReturn Future.failed(new Exception("The sky is falling"))
        val result = Await.result(reds.queryReportingEntityData(docRefId).value, 2.seconds)
        result.isLeft shouldBe true
      }
    }
  }

}
