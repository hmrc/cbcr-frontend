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
import uk.gov.hmrc.cbcrfrontend.connectors.BPRKnownFactsConnector
import uk.gov.hmrc.cbcrfrontend.model.{BPRKnownFacts, Utr}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class BPRKnownFactsServiceSpec extends AnyWordSpec with Matchers with GuiceOneAppPerSuite with IdiomaticMockito {

  private val mockConnector = mock[BPRKnownFactsConnector]
  private val mockAudit = mock[AuditConnector]
  private val bprKnownFactsService = new BPRKnownFactsService(mockConnector, mockAudit)
  private implicit val hc: HeaderCarrier = HeaderCarrier()

  private val bodyKnownFact1 =
    """{
      |    "safeId": "XX0000114342888",
      |    "organisation": {
      |      "organisationName": "Foo Ltd"
      |    },
      |    "address": {
      |      "addressLine1": "1 The Street",
      |      "addressLine2": "HOVE",
      |      "addressLine3": "ENGLAND",
      |      "postalCode": "BN5 4ZZ",
      |      "countryCode": "GB"
      |    }}""".stripMargin

  private val kf1 = BPRKnownFacts(Utr("7000000002"), "BN5 4ZZ")

  "The BPRKnowFactsService" should {
    "return a match for a check with an exact matching post code " in {
      val result = mock[HttpResponse]
      mockConnector.lookup(kf1.utr.utr) returns Future.successful(result)
      mockAudit.sendExtendedEvent(*)(*, *) returns Future.successful(AuditResult.Success)
      result.body returns bodyKnownFact1

      val maybeKnownFact = Await.result(bprKnownFactsService.checkBPRKnownFacts(kf1).value, 2.second)

      maybeKnownFact.isDefined shouldBe false
    }

    "return a non match for a check with an non matching post code " in {
      val result = mock[HttpResponse]
      mockConnector.lookup(kf1.utr.utr) returns Future.successful(result)
      mockAudit.sendExtendedEvent(*)(*, *) returns Future.successful(AuditResult.Success)
      result.body returns bodyKnownFact1

      val maybeKnownFact = Await
        .result(bprKnownFactsService.checkBPRKnownFacts(BPRKnownFacts(Utr("7000000002"), "BN3 5XB")).value, 2.second)

      maybeKnownFact.isDefined shouldBe false
    }

    "return a match for a check with no space in the post code when the DES version has a space" in {
      val result = mock[HttpResponse]
      mockConnector.lookup(kf1.utr.utr) returns Future.successful(result)
      mockAudit.sendExtendedEvent(*)(*, *) returns Future.successful(AuditResult.Success)
      result.body returns bodyKnownFact1

      val maybeKnownFact = Await
        .result(bprKnownFactsService.checkBPRKnownFacts(BPRKnownFacts(Utr("7000000002"), "BN54ZZ")).value, 2.second)

      maybeKnownFact.isDefined shouldBe false
    }
  }
}
