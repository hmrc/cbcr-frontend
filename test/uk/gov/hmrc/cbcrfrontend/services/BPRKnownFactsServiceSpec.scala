/*
 * Copyright 2017 HM Revenue & Customs
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

import cats.data.OptionT
import org.scalatest.{Matchers, WordSpec}
import org.scalatest.mock.MockitoSugar
import org.scalatestplus.play.OneAppPerSuite
import uk.gov.hmrc.cbcrfrontend.connectors.BPRKnownFactsConnector
import org.mockito.Mockito._
import org.mockito.Matchers._
import org.scalatest.concurrent.ScalaFutures
import play.api.libs.json.Json
import uk.gov.hmrc.cbcrfrontend.model.{BPRKnownFacts, BusinessPartnerRecord, Utr}
import uk.gov.hmrc.play.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}


class BPRKnownFactsServiceSpec extends WordSpec with Matchers with OneAppPerSuite with MockitoSugar {


  val mockConnector = mock[BPRKnownFactsConnector]
  val bprKnownFactsService = new BPRKnownFactsService(mockConnector)
  implicit val hc = HeaderCarrier()

  val bodyKnownFact1: String =
    """{
      |    "safeId": "XX0000114342996",
      |    "organisation": {
      |      "organisationName": "TEVA PHARMA HOLDINGS LIMITED"
      |    },
      |    "address": {
      |      "addressLine1": "RIDINGS POINT WHISTLER DRIVE",
      |      "addressLine2": "CASTLEFORD",
      |      "addressLine3": "ENGLAND",
      |      "postalCode": "WF10 5HX",
      |      "countryCode": "GB"
      |    }}""".stripMargin

  val businessPartnerRecord1: BusinessPartnerRecord = Json.parse(bodyKnownFact1).validate[BusinessPartnerRecord].asOpt.get
  val kf1 = BPRKnownFacts(Utr("5525118664"), "WF10 5HX")



  "The BPRKnowFactsService" should {
    "return a match for a check with an exact matching post code " in {

      val result: HttpResponse = mock[HttpResponse]
      when(mockConnector.lookup(kf1.utr.utr)).thenReturn(Future.successful(result))
      when(result.body).thenReturn(bodyKnownFact1)

      val maybeKnownFact = Await.result(bprKnownFactsService.checkBPRKnownFacts(kf1).value, 2.second)

      maybeKnownFact.isDefined shouldBe true

    }

    "return a non match for a check with an non matching post code " in {

      val result: HttpResponse = mock[HttpResponse]
      when(mockConnector.lookup(kf1.utr.utr)).thenReturn(Future.successful(result))
      when(result.body).thenReturn(bodyKnownFact1)

      val maybeKnownFact = Await.result(bprKnownFactsService.checkBPRKnownFacts(BPRKnownFacts(Utr("5525118664"), "BN3 5XB")).value, 2.second)

      maybeKnownFact.isDefined shouldBe false

    }

    "return a  match for a check with no space in the post code when the DES version has a space" in {

      val result: HttpResponse = mock[HttpResponse]
      when(mockConnector.lookup(kf1.utr.utr)).thenReturn(Future.successful(result))
      when(result.body).thenReturn(bodyKnownFact1)

      val maybeKnownFact = Await.result(bprKnownFactsService.checkBPRKnownFacts(BPRKnownFacts(Utr("5525118664"), "WF105HX")).value, 2.second)

      maybeKnownFact.isDefined shouldBe true

    }

  }

}
