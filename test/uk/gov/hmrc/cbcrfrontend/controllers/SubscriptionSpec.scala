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

package uk.gov.hmrc.cbcrfrontend.controllers

import cats.data.OptionT
import org.mockito.Mockito._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mock.MockitoSugar
import org.scalatestplus.play.OneAppPerSuite
import play.api.http.Status
import play.api.i18n.MessagesApi
import play.api.libs.json.Json
import play.api.test.FakeRequest
import uk.gov.hmrc.cbcrfrontend.model.{KnownFacts, OrganisationResponse, Utr}
import uk.gov.hmrc.cbcrfrontend.services.KnownFactsCheckService
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.{ExecutionContext, Future}

/**
  * Created by max on 06/04/17.
  *
  *
POST       /checkKnownFacts             uk.gov.hmrc.cbcrfrontend.controllers.Subscription.checkKnownFacts
GET        /subscribeFirst              uk.gov.hmrc.cbcrfrontend.controllers.Subscription.subscribeFirst
GET        /subscribeMatchFound         uk.gov.hmrc.cbcrfrontend.controllers.Subscription.subscribeMatchFound
GET        /contactInfoSubscriber       uk.gov.hmrc.cbcrfrontend.controllers.Subscription.contactInfoSubscriber
GET        /subscribeSuccessCbcId       uk.gov.hmrc.cbcrfrontend.controllers.Subscription.subscribeSuccessCbcId
  *
  *
  *
  */
class SubscriptionSpec extends UnitSpec with ScalaFutures with OneAppPerSuite with CSRFTest with MockitoSugar{

  val kfcs = mock[KnownFactsCheckService]

  implicit val ec = app.injector.instanceOf[ExecutionContext]
  implicit val messagesApi = app.injector.instanceOf[MessagesApi]


  val controller = new Subscription {
    val knownFactsService: KnownFactsCheckService = kfcs
  }

  "GET /subscribeFirst" should {
    "return 200" in {
      val fakeRequestSubscribe = addToken(FakeRequest("GET", "/subscribeFirst"))
      status(controller.subscribeFirst(fakeRequestSubscribe)) shouldBe Status.OK
    }
  }
  "GET /subscribeMatchFound" should {
    "return 200" in {
      val fakeRequestSubscribe = addToken(FakeRequest("GET", "/subscribeMatchFound"))
      status(controller.subscribeMatchFound(fakeRequestSubscribe)) shouldBe Status.OK
    }
  }
  "GET /contactInfoSubscriber" should {
    "return 200" in {
      val fakeRequestSubscribe = addToken(FakeRequest("GET", "/contactInfoSubscriber"))
      status(controller.contactInfoSubscriber(fakeRequestSubscribe)) shouldBe Status.OK
    }
  }
  "GET /subscribeSuccessCbcId" should {
    "return 200" in {
      val fakeRequestSubscribe = addToken(FakeRequest("GET", "/subscribeSuccessCbcId"))
      status(controller.subscribeSuccessCbcId(fakeRequestSubscribe)) shouldBe Status.OK
    }
  }
  "POST /checkKnownFacts" should {
    "return 400 when KnownFacts are missing" in {
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/checkKnownFacts"))
      status(controller.checkKnownFacts(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }
    "return 400 when the postcode is invalid" in {
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/checkKnownFacts").withJsonBody(Json.toJson(KnownFacts(Utr("1234567890"),"NOTAPOSTCODE"))))
      status(controller.checkKnownFacts(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }
    "return 400 when the utr is invalid" in {
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/checkKnownFacts").withJsonBody(Json.toJson(KnownFacts(Utr("IAMNOTAUTR"),"SW4 6NR"))))
      status(controller.checkKnownFacts(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }
    "return 200 when the utr and postcode are valid" in {
      val kf = KnownFacts(Utr("7000000002"),"SW46NR")
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/checkKnownFacts").withJsonBody(Json.toJson(kf)))
      when(kfcs.checkKnownFacts(kf)) thenReturn OptionT[Future,OrganisationResponse](Future.successful(Some(OrganisationResponse("name",None,None))))
      status(controller.checkKnownFacts(fakeRequestSubscribe)) shouldBe Status.OK
    }
  }



}
