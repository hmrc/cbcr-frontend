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

import org.apache.http.HttpStatus
import org.mockito.ArgumentMatchersSugar.*
import org.mockito.IdiomaticMockito
import org.mockito.Mockito.{never, verify}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.test.Helpers.{await, defaultAwaitTimeout}
import uk.gov.hmrc.cbcrfrontend.connectors.CBCRBackendConnector
import uk.gov.hmrc.cbcrfrontend.controllers.CSRFTest
import uk.gov.hmrc.cbcrfrontend.emailaddress.EmailAddress
import uk.gov.hmrc.cbcrfrontend.model.{ContactDetails, ContactName, ETMPSubscription, EtmpAddress}
import uk.gov.hmrc.http.{HeaderCarrier, HttpException, HttpResponse}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class CBCIdServiceSpec extends AnyWordSpec with Matchers with GuiceOneAppPerSuite with CSRFTest with IdiomaticMockito {

  private val connector = mock[CBCRBackendConnector]
  private val cbcidService = new CBCIdService(connector)

  private val safeId = "safe-id"

  private implicit val hc: HeaderCarrier = HeaderCarrier()

  "CBCIdService" should {

    "return valid etmp subscription if the connector returns valid json object response" in {
      val desResponse = s"""
                           |{
                           |   "safeId":"XP0000100099577",
                           |   "names": {
                           |     "name1": "name1",
                           |     "name2": "name2"
                           |   },
                           |   "contact": {
                           |     "email": "test@test.com",
                           |     "phoneNumber": "08287623651"
                           |   },
                           |   "address":{
                           |     "line1":"Delivery Day",
                           |     "countryCode":"GB"
                           |   }
                           |}
       """.stripMargin

      connector.getETMPSubscriptionData(*)(*) returns Future.successful(HttpResponse(Status.OK, desResponse))
      val result = await(cbcidService.getETMPSubscriptionData(safeId).value)
      result should not be None
      result.get shouldBe ETMPSubscription(
        "XP0000100099577",
        ContactName("name1", "name2"),
        ContactDetails(EmailAddress("test@test.com"), "08287623651"),
        EtmpAddress("Delivery Day", None, None, None, None, "GB")
      )
    }

    "return NONE if the connector returns empty string or empty json object response" in {
      List("", "{}").map { responseBody =>
        connector.getETMPSubscriptionData(*)(*) returns Future.successful(HttpResponse(Status.OK, responseBody))
        val result = await(cbcidService.getETMPSubscriptionData(safeId).value)
        result shouldBe None
      }
    }

    "not parse response body if the returned status is not a 200" in {
      val response = mock[HttpResponse]
      connector.getETMPSubscriptionData(*)(*) returns Future.successful(response)
      response.status returns HttpStatus.SC_INTERNAL_SERVER_ERROR

      await(cbcidService.getETMPSubscriptionData(safeId).value)
      verify(response, never()).body
    }

    "throw exception if the connector fails to responds for given request" in {
      connector.getETMPSubscriptionData(*)(*) returns Future.failed(
        new HttpException("HttpException occurred", HttpStatus.SC_BAD_REQUEST)
      )
      intercept[HttpException] {
        await(cbcidService.getETMPSubscriptionData(safeId).value)
      }
    }
  }
}
