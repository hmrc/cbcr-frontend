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

package uk.gov.hmrc.cbcrfrontend.connectors

import com.github.tomakehurst.wiremock.client.WireMock.{aResponse, post, urlEqualTo}
import com.github.tomakehurst.wiremock.stubbing.StubMapping
import org.scalacheck.Gen
import org.scalatest.MustMatchers.convertToAnyMustWrapper
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.{FreeSpec, Matchers}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.Application
import play.api.http.Status.OK
import play.api.inject.guice.GuiceApplicationBuilder
import uk.gov.hmrc.cbcrfrontend.util.WireMockHelper
import uk.gov.hmrc.http.HeaderCarrier

class CBCRBackendConnectorSpec
    extends FreeSpec with GuiceOneAppPerSuite with WireMockHelper with ScalaFutures with Matchers
    with ScalaCheckPropertyChecks with IntegrationPatience {

  override def fakeApplication(): Application =
    new GuiceApplicationBuilder()
      .configure(
        "microservice.services.cbcr.port" -> server.port(),
        "auditing.enabled"                -> false
      )
      .build()

  lazy val connector: CBCRBackendConnector = app.injector.instanceOf[CBCRBackendConnector]
  implicit val hc: HeaderCarrier = HeaderCarrier()

  private val errorResponsesCodes: Gen[Int] = Gen.chooseNum(400, 599)

  "CBCRBackendConnector" - {
    "submitDocument" - {
      "should return a GeneratedIDs" - {
        "when the backend returns a valid successful response" in {

          stubResponse(OK, "/cbcr/submit")

          val xml = <test></test>
          val result = connector.submitDocument(xml)
          result.futureValue.status mustBe OK
        }
      }

      "when submitDocument returns an error status as a response" in {
        forAll(errorResponsesCodes) { errorResponseCode =>
          stubResponse(errorResponseCode, "/cbcr/submit")

          val xml = <test></test>
          val result = connector.submitDocument(xml)
          result.futureValue.status mustBe errorResponseCode
        }
      }
    }
  }

  private def stubResponse(expectedStatus: Int, stubUrl: String): StubMapping =
    server.stubFor(
      post(urlEqualTo(stubUrl))
        .willReturn(
          aResponse()
            .withStatus(expectedStatus)
        )
    )
}
