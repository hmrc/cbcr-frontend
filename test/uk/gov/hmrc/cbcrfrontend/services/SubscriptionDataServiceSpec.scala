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

import org.mockito.ArgumentMatchers.any
import org.mockito.MockitoSugar
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Configuration
import play.api.http.Status
import play.api.libs.json.{JsNull, Json}
import uk.gov.hmrc.cbcrfrontend.controllers.CSRFTest
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.emailaddress.EmailAddress
import uk.gov.hmrc.http._
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class SubscriptionDataServiceSpec
    extends AnyWordSpec with Matchers with GuiceOneAppPerSuite with CSRFTest with MockitoSugar {
  private implicit val hc: HeaderCarrier = HeaderCarrier()
  private val runModeConfiguration = mock[Configuration]
  private val mockHttp = mock[HttpClient]
  private val servicesConfig = mock[ServicesConfig]
  private val sds = new SubscriptionDataService(runModeConfiguration, mockHttp, servicesConfig)
  private val cbcId = CBCId.create(56).toOption
  private val utr = Utr("7000000001")
  private val idUtr: Either[Utr, CBCId] = Left(utr)
  private val idCbcId: Either[Utr, CBCId] = Right(cbcId.get)
  private val subscriberContact = SubscriberContact("Brian", "Lastname", "phonenum", EmailAddress("test@test.com"))
  private val subscriptionDetails = SubscriptionDetails(
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
