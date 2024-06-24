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

import org.mockito.ArgumentMatchersSugar.*
import org.mockito.IdiomaticMockito
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.libs.json.{JsNull, Json}
import play.api.test.Helpers.{await, defaultAwaitTimeout}
import uk.gov.hmrc.cbcrfrontend.controllers.CSRFTest
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.emailaddress.EmailAddress
import uk.gov.hmrc.http.{HeaderCarrier, HttpClient, HttpReads, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class SubscriptionDataServiceSpec
    extends AnyWordSpec with Matchers with GuiceOneAppPerSuite with CSRFTest with IdiomaticMockito {
  private implicit val hc: HeaderCarrier = HeaderCarrier()
  implicit val reads: HttpReads[HttpResponse] = uk.gov.hmrc.http.HttpReads.Implicits.readRaw
  private val mockHttp = mock[HttpClient]
  private val servicesConfig = mock[ServicesConfig]
  private val sds = new SubscriptionDataService(mockHttp, servicesConfig)
  private val cbcId = CBCId.create(56).toOption
  private val utr = Utr("7000000001")
  private val idUtr: Either[Utr, CBCId] = Left(utr)
  private val idCbcId: Either[Utr, CBCId] = Right(cbcId.get)
  private val subscriberContact = SubscriberContact("Brian", "Lastname", "phonenum", EmailAddress("test@test.com"))
  private val subscriptionDetails = SubscriptionDetails(
    BusinessPartnerRecord(
      "SAFEID",
      Some(OrganisationResponse("blagh")),
      EtmpAddress("Line1", None, None, None, Some("TF3 XFE"), "GB")
    ),
    subscriberContact,
    cbcId,
    Utr("7000000002")
  )

  "SubscriptionDataService on a call to saveReportingEntityData" should {
    "save ReportingEntityData if it does not exist in the DB store" in {
      val json = Json.toJson(Some(subscriptionDetails)).toString()
      mockHttp.GET[HttpResponse](*, *, *)(*, *, *) returns Future.successful(HttpResponse(Status.OK, json))
      val result = await(sds.retrieveSubscriptionData(idUtr).value)
      result shouldBe Right(Some(subscriptionDetails))

      val result1 = await(sds.retrieveSubscriptionData(idCbcId).value)
      result1 shouldBe Right(Some(subscriptionDetails))
    }

    "return an error if there is a serialisation error while parsing for SubscriptionDetails" in {
      val invalidJson = Json.toJson("Invalid Json, so we should have Left() error").toString()
      mockHttp.GET[HttpResponse](*, *, *)(*, *, *) returns Future.successful(HttpResponse(Status.OK, invalidJson))
      val result = await(sds.retrieveSubscriptionData(idUtr).value)
      result.isLeft shouldBe true
    }

    "return NONE if the connector returns a NotFoundException" in {
      mockHttp.GET[HttpResponse](*, *, *)(*, *, *) returns Future.successful(
        HttpResponse(Status.NOT_FOUND, JsNull, Map.empty[String, Seq[String]])
      )
      val result = await(sds.retrieveSubscriptionData(idUtr).value)
      result shouldBe Right(None)
    }

    "return an error if anything else goes wrong" in {
      mockHttp.GET(*, *, *)(*, *, *) returns Future.failed(new Exception("The sky is falling"))
      val result = await(sds.retrieveSubscriptionData(idUtr).value)
      result.isLeft shouldBe true
    }
  }

  "SubscriptionDataService on a call to saveSubscriptionData" should {
    "save saveSubscriptionData if it exists in the DB store" in {
      mockHttp.POST[SubscriptionDetails, HttpResponse](*, *, *)(*, *, *, *) returns Future.successful(
        HttpResponse(Status.OK, JsNull, Map.empty[String, Seq[String]])
      )
      val result = await(sds.saveSubscriptionData(subscriptionDetails).value)
      result.isRight shouldBe true
    }

    "return Left() unexpected error if it does not exist in the DB store" in {
      mockHttp.POST[SubscriptionDetails, HttpResponse](*, *, *)(*, *, *, *) returns Future.successful(
        HttpResponse(Status.NOT_FOUND, JsNull, Map.empty[String, Seq[String]])
      )
      val result = await(sds.saveSubscriptionData(subscriptionDetails).value)
      result.isLeft shouldBe true
    }

    "return Left() and throw an exception" in {
      mockHttp.POST[SubscriptionDetails, HttpResponse](*, *, *)(*, *, *, *) returns Future.failed(
        new Exception("Error occurred")
      )
      val result = await(sds.saveSubscriptionData(subscriptionDetails).value)
      result.isLeft shouldBe true
    }
  }

  "SubscriptionDataService on a call to updateSubscriptionData" should {
    "update subscriptionDetails if it exists in the DB store" in {
      mockHttp.PUT[SubscriberContact, HttpResponse](*, *, *)(*, *, *, *) returns Future.successful(
        HttpResponse(200, JsNull, Map.empty[String, Seq[String]])
      )
      val result = await(sds.updateSubscriptionData(cbcId.get, subscriberContact).value)
      result.isRight shouldBe true
    }

    "not update subscriptionDetails and return unexpected error if it fails to exist in the DB store" in {
      mockHttp.PUT[SubscriberContact, HttpResponse](*, *, *)(*, *, *, *) returns Future.successful(
        HttpResponse(300, JsNull, Map.empty[String, Seq[String]])
      )
      val result = await(sds.updateSubscriptionData(cbcId.get, subscriberContact).value)
      result.isLeft shouldBe true
    }

    "return Left() and throw an exception" in {
      mockHttp.PUT[SubscriberContact, HttpResponse](*, *, *)(*, *, *, *) returns Future.failed(
        new Exception("Error occurred")
      )
      val result = await(sds.saveSubscriptionData(subscriptionDetails).value)
      result.isLeft shouldBe true
    }
  }
}
