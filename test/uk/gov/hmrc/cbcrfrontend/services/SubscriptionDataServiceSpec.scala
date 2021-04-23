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

package uk.gov.hmrc.cbcrfrontend.services

import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.when
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mockito.MockitoSugar
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.libs.json.{JsNull, Json}
import play.api.{Configuration, Environment}
import uk.gov.hmrc.cbcrfrontend.connectors.CBCRBackendConnector
import uk.gov.hmrc.cbcrfrontend.controllers.CSRFTest
import uk.gov.hmrc.cbcrfrontend.model.{BusinessPartnerRecord, CBCId, EtmpAddress, OrganisationResponse, SubscriberContact, SubscriptionDetails, Utr}
import uk.gov.hmrc.cbcrfrontend.typesclasses.{CbcrsUrl, ServiceUrl}
import uk.gov.hmrc.cbcrfrontend.util.UnitSpec
import uk.gov.hmrc.emailaddress.EmailAddress
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.http._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class SubscriptionDataServiceSpec
    extends UnitSpec with ScalaFutures with GuiceOneAppPerSuite with CSRFTest with MockitoSugar {
  implicit val hc: HeaderCarrier = HeaderCarrier()
  implicit val cbcrsUrl = new ServiceUrl[CbcrsUrl] { val url = "cbcr" }
  val connector = mock[CBCRBackendConnector]
  val environment = mock[Environment]
  val runModeConfiguration = mock[Configuration]
  val mockHttp = mock[HttpClient]
  val servicesConfig = mock[ServicesConfig]
  val mockUrl = mock[ServiceUrl[CbcrsUrl]]
  val sds = new SubscriptionDataService(environment, runModeConfiguration, mockHttp, servicesConfig)
  val cbcId = CBCId.create(56).toOption
  val utr = Utr("7000000001")
  val idUtr: Either[Utr, CBCId] = Left(utr)
  val idCbcId: Either[Utr, CBCId] = Right(cbcId.get)
  val subscriberContact = SubscriberContact("Brian", "Lastname", "phonenum", EmailAddress("test@test.com"))
  val subscriptionDetails = SubscriptionDetails(
    BusinessPartnerRecord(
      "SAFEID",
      Some(OrganisationResponse("blagh")),
      EtmpAddress("Line1", None, None, None, Some("TF3 XFE"), "GB")),
    subscriberContact,
    cbcId,
    Utr("7000000002")
  )

  "SubscriptionDataService on a call to saveReportingEntityData" should {
    "save ReportingEntityData if it does not exist in the DB store" in {
      val json = Json.toJson(Some(subscriptionDetails)).toString()
      when(mockHttp.GET[HttpResponse](any(), any(), any())(any(), any(), any()))
        .thenReturn(Future.successful(HttpResponse(Status.OK, json)))
      val result = Await.result(sds.retrieveSubscriptionData(idUtr).value, 2.seconds)
      result shouldBe Right(Some(subscriptionDetails))

      val result1 = Await.result(sds.retrieveSubscriptionData(idCbcId).value, 2.seconds)
      result1 shouldBe Right(Some(subscriptionDetails))
    }

    "return an error if there is a serialisation error while parsing for SubscriptionDetails" in {
      val invalidJson = Json.toJson("Invalid Json, so we should have Left() error").toString()
      when(mockHttp.GET[HttpResponse](any(), any(), any())(any(), any(), any()))
        .thenReturn(Future.successful(HttpResponse(Status.OK, invalidJson)))
      val result = Await.result(sds.retrieveSubscriptionData(idUtr).value, 2.seconds)
      result.isLeft shouldBe true
    }
    "return NONE if the connector returns a NotFoundException" in {
      when(mockHttp.GET[HttpResponse](any(), any(), any())(any(), any(), any())) thenReturn Future.successful(
        HttpResponse(Status.NOT_FOUND, JsNull, Map.empty[String, Seq[String]]))
      val result = Await.result(sds.retrieveSubscriptionData(idUtr).value, 2.seconds)
      result shouldBe Right(None)
    }
    "return an error if anything else goes wrong" in {
      when(mockHttp.GET[HttpResponse](any(), any(), any())(any(), any(), any())) thenReturn Future.failed(
        new Exception("The sky is falling"))
      val result = Await.result(sds.retrieveSubscriptionData(idUtr).value, 2.seconds)
      result.isLeft shouldBe true
    }
  }

  "SubscriptionDataService on a call to saveSubscriptionData" should {
    "save saveSubscriptionData if it exists in the DB store" in {
      when(mockHttp.POST[SubscriptionDetails, HttpResponse](any(), any(), any())(any(), any(), any(), any()))
        .thenReturn(Future.successful(HttpResponse(Status.OK, JsNull, Map.empty[String, Seq[String]])))
      val result = Await.result(sds.saveSubscriptionData(subscriptionDetails).value, 2.seconds)
      result.isRight shouldBe true

    }

    "return Left() unexpected error if it does not exist in the DB store" in {
      when(mockHttp.POST[SubscriptionDetails, HttpResponse](any(), any(), any())(any(), any(), any(), any()))
        .thenReturn(Future.successful(HttpResponse(Status.NOT_FOUND, JsNull, Map.empty[String, Seq[String]])))
      val result = Await.result(sds.saveSubscriptionData(subscriptionDetails).value, 2.seconds)
      result.isLeft shouldBe true

    }

    "return Left() and throw an exception" in {
      when(mockHttp.POST[SubscriptionDetails, HttpResponse](any(), any(), any())(any(), any(), any(), any()))
        .thenReturn(Future.failed(new Exception("Error occurred")))
      val result = Await.result(sds.saveSubscriptionData(subscriptionDetails).value, 2.seconds)
      result.isLeft shouldBe true

    }
  }

  "SubscriptionDataService on a call to updateSubscriptionData" should {
    "update subscriptionDetails if it exists in the DB store" in {
      when(mockHttp.PUT[SubscriptionDetails, HttpResponse](any(), any(), any())(any(), any(), any(), any()))
        .thenReturn(Future.successful(HttpResponse(200, JsNull, Map.empty[String, Seq[String]])))
      val result = Await.result(sds.updateSubscriptionData(cbcId.get, subscriberContact).value, 2.seconds)
      result.isRight shouldBe true
    }
    "not update subscriptionDetails and return unexpected error if it fails to exist in the DB store" in {
      when(mockHttp.PUT[SubscriptionDetails, HttpResponse](any(), any(), any())(any(), any(), any(), any()))
        .thenReturn(Future.successful(HttpResponse(300, JsNull, Map.empty[String, Seq[String]])))
      val result = Await.result(sds.updateSubscriptionData(cbcId.get, subscriberContact).value, 2.seconds)
      result.isLeft shouldBe true
    }
    "return Left() and throw an exception" in {
      when(mockHttp.PUT[SubscriptionDetails, HttpResponse](any(), any(), any())(any(), any(), any(), any()))
        .thenReturn(Future.failed(new Exception("Error occurred")))
      val result = Await.result(sds.saveSubscriptionData(subscriptionDetails).value, 2.seconds)
      result.isLeft shouldBe true

    }
  }

}
