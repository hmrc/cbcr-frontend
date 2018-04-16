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

import org.mockito.Matchers._
import org.mockito.Mockito._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mockito.MockitoSugar
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import uk.gov.hmrc.cbcrfrontend.connectors.GGConnector
import uk.gov.hmrc.cbcrfrontend.controllers.CSRFTest
import uk.gov.hmrc.cbcrfrontend.model.{CBCId, CBCKnownFacts, Utr}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class CBCKnownFactsServiceSpec extends UnitSpec with ScalaFutures with GuiceOneAppPerSuite with CSRFTest with MockitoSugar {

  val connector = mock[GGConnector]
  val service = new CBCKnownFactsService(connector)

  implicit val hc = HeaderCarrier()
  val kf = CBCKnownFacts(Utr("1234567890"),CBCId.create(100).getOrElse(fail("Could not create CBCID")))

  "The CBCKnownFactsService" should {
    "return a success if both calls to addKnownFacts and enrol are successful" in {
      when(connector.addKnownFacts(any())(any())) thenReturn HttpResponse(Status.OK)
      when(connector.enrolCBC(any())(any())) thenReturn HttpResponse(Status.OK)
      val result = service.addKnownFactsToGG(kf)
      assert(Await.result(result.value, 10 second span).isRight)
    }
    "return a failure if the call to addKnownFacts fails" in {
      when(connector.addKnownFacts(any())(any())) thenReturn HttpResponse(Status.INTERNAL_SERVER_ERROR)
      when(connector.enrolCBC(any())(any())) thenReturn HttpResponse(Status.OK)
      val result = service.addKnownFactsToGG(kf)
      assert(Await.result(result.value, 10 second span).isLeft)
    }
    "return a failure if the call to enrol fails" in {
      when(connector.addKnownFacts(any())(any())) thenReturn HttpResponse(Status.OK)
      when(connector.enrolCBC(any())(any())) thenReturn HttpResponse(Status.INTERNAL_SERVER_ERROR)
      val result = service.addKnownFactsToGG(kf)
      assert(Await.result(result.value, 10 second span).isLeft)
    }
  }

}
