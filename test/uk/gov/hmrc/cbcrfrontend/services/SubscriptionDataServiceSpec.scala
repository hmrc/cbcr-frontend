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

import com.typesafe.config.ConfigFactory
import org.mockito.IdiomaticMockito
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.Json
import play.api.test.Helpers.{await, defaultAwaitTimeout}
import play.api.{Application, Configuration}
import uk.gov.hmrc.cbcrfrontend.controllers.CSRFTest
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.util.WireMockMethods
import uk.gov.hmrc.emailaddress.EmailAddress
import uk.gov.hmrc.http.test.WireMockSupport
import uk.gov.hmrc.http.{HeaderCarrier, HttpClient, HttpReads, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import scala.concurrent.ExecutionContext.Implicits.global

class SubscriptionDataServiceSpec
    extends AnyWordSpec with Matchers with GuiceOneAppPerSuite with CSRFTest with IdiomaticMockito with WireMockSupport
    with WireMockMethods {

  private val config = Configuration(
    ConfigFactory.parseString(
      s"""
         |microservice {
         |  services {
         |      cbcr {
         |      host     = $wireMockHost
         |      port     = $wireMockPort
         |    }
         |  }
         |}
         |""".stripMargin
    )
  )

  override def fakeApplication(): Application = new GuiceApplicationBuilder().configure(config).build()

  private implicit val hc: HeaderCarrier = HeaderCarrier()
  implicit val reads: HttpReads[HttpResponse] = uk.gov.hmrc.http.HttpReads.Implicits.readRaw
  private val mockHttp = fakeApplication().injector.instanceOf[HttpClient]
  private val servicesConfig = fakeApplication().injector.instanceOf[ServicesConfig]
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

  "SubscriptionDataService on a call to retrieveSubscriptionData" should {
    "return ReportingEntityData if available in DB store" in {
      val json = Json.toJson(Some(subscriptionDetails)).toString()
      when(GET, s"/cbcr/subscription-data/utr/${utr.utr}").thenReturn(Status.OK, json)
      val utrResponse = await(sds.retrieveSubscriptionData(idUtr).value)
      utrResponse shouldBe Right(Some(subscriptionDetails))

      when(GET, s"/cbcr/subscription-data/cbc-id/${cbcId.get.value}").thenReturn(Status.OK, json)
      val idResponse = await(sds.retrieveSubscriptionData(idCbcId).value)
      idResponse shouldBe Right(Some(subscriptionDetails))
    }

    "return an error if there is a serialisation error while parsing for SubscriptionDetails" in {
      val invalidJsonString = "Invalid Json, so we should have Left() error"
      when(GET, s"/cbcr/subscription-data/utr/${utr.utr}").thenReturn(Status.OK, invalidJsonString)
      val result = await(sds.retrieveSubscriptionData(idUtr).value)
      result.isLeft shouldBe true
    }

    "return NONE if the connector returns a NotFoundException" in {
      when(GET, s"/cbcr/subscription-data/utr/${utr.utr}").thenReturn(Status.NOT_FOUND)
      val result = await(sds.retrieveSubscriptionData(idUtr).value)
      result shouldBe Right(None)
    }
  }

  "SubscriptionDataService on a call to saveSubscriptionData" should {
    "save saveSubscriptionData if it exists in the DB store" in {
      when(POST, "/cbcr/subscription-data").thenReturn(Status.OK)
      val result = await(sds.saveSubscriptionData(subscriptionDetails).value)
      result.isRight shouldBe true
    }

    "return Left() unexpected error if it does not exist in the DB store" in {
      when(POST, "/cbcr/subscription-data").thenReturn(Status.NOT_FOUND)
      val result = await(sds.saveSubscriptionData(subscriptionDetails).value)
      result.isLeft shouldBe true
    }
  }

  "SubscriptionDataService on a call to updateSubscriptionData" should {
    "update subscriptionDetails if it exists in the DB store" in {
      when(PUT, s"/cbcr/subscription-data/${cbcId.get}").thenReturn(Status.OK)

      val result = await(sds.updateSubscriptionData(cbcId.get, subscriberContact).value)
      result.isRight shouldBe true
    }

    "not update subscriptionDetails and return unexpected error if it fails to exist in the DB store" in {
      when(PUT, s"/cbcr/subscription-data/${cbcId.get}").thenReturn(Status.NOT_FOUND)

      val result = await(sds.updateSubscriptionData(cbcId.get, subscriberContact).value)
      result.isLeft shouldBe true
    }
  }

  "SubscriptionDataService on call to unavailable service" should {
    "return Left() and throw an exception" in {
      wireMockServer.stop()

      when(GET, s"/cbcr/subscription-data/utr/$idUtr")
      val getSubscriptionDetailsResponse = await(sds.retrieveSubscriptionData(idUtr).value)
      getSubscriptionDetailsResponse.isLeft shouldBe true

      when(POST, "/cbcr/subscription-data")
      val saveSubscriptionDetailsResponse = await(sds.saveSubscriptionData(subscriptionDetails).value)
      saveSubscriptionDetailsResponse.isLeft shouldBe true

      when(PUT, s"/cbcr/subscription-data/${cbcId.get}")
      val updateSubscriptionDetailsResponse = await(sds.updateSubscriptionData(cbcId.get, subscriberContact).value)
      updateSubscriptionDetailsResponse.isLeft shouldBe true

      wireMockServer.start()
    }
  }
}
