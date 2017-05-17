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

import org.mockito.Matchers._
import org.mockito.Mockito.when
import org.scalatest.mock.MockitoSugar
import org.scalatestplus.play.OneAppPerSuite
import play.api.http.Status
import play.api.i18n.MessagesApi
import play.api.libs.json.{JsValue, Json}
import play.api.test.FakeRequest
import uk.gov.hmrc.cbcrfrontend.controllers.auth.{SecuredActionsTest, TestUsers}
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.services.CBCSessionCache
import uk.gov.hmrc.emailaddress.EmailAddress
import uk.gov.hmrc.http.cache.client.CacheMap
import uk.gov.hmrc.play.http.HeaderCarrier
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.{ExecutionContext, Future}


class SubmissionSpec  extends UnitSpec with OneAppPerSuite with CSRFTest with MockitoSugar with FakeAuthConnector {

  implicit val ec = app.injector.instanceOf[ExecutionContext]
  implicit val messagesApi = app.injector.instanceOf[MessagesApi]
  val authCon = authConnector(TestUsers.cbcrUser)
  val securedActions = new SecuredActionsTest(TestUsers.cbcrUser, authCon)
  val cache = mock[CBCSessionCache]

  implicit val hc = HeaderCarrier()
  val controller = new Submission(securedActions, cache)


  "POST /submitFilingType" should {
    "return 400 when the there is no data" in {
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/submitFilingType"))
      status(controller.submitFilingType(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }
  }

  "POST /submitFilingType" should {
    "return 200 when the data exists" in {
      val filingType = FilingType("PRIMARY")
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/submitFilingType").withJsonBody(Json.toJson(filingType)))
      when(cache.save[FilingType](any())(any(),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      status(controller.submitFilingType(fakeRequestSubscribe)) shouldBe Status.OK
    }
  }

  "POST /submitUltimateParentEntity " should {
    "return 400 when the there is no data" in {
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/submitUltimateParentEntity "))
      status(controller.submitFilingType(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }
  }

  "POST /submitUltimateParentEntity " should {
    "return 200 when the data exists" in {
      val ultimateParentEntity  = UltimateParentEntity("UlitmateParentEntity")
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/submitUltimateParentEntity ").withJsonBody(Json.toJson(ultimateParentEntity)))
      when(cache.save[UltimateParentEntity](any())(any(),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      status(controller.submitUltimateParentEntity(fakeRequestSubscribe)) shouldBe Status.OK
    }
  }

  "POST /submitFilingCapacity" should {
    "return 400 when the there is no data" in {
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/submitFilingCapacity"))
      status(controller.submitFilingCapacity(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }
  }

  "POST /submitFilingCapacity" should {
    "return 200 when the data exists" in {
      val filingCapacity = FilingCapacity("MNE_USER")
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/submitFilingCapacity").withJsonBody(Json.toJson(filingCapacity)))
      when(cache.save[FilingCapacity](any())(any(),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      status(controller.submitFilingCapacity(fakeRequestSubscribe)) shouldBe Status.OK
    }
  }

  "POST /submitSubmitterInfo" should {
    "return 400 when the there is no data at all" in {
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/submitSubmitterInfo"))
      status(controller.submitSubmitterInfo(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }
  }

  "POST /submitSubmitterInfo" should {
    "return 400 when the all data exists but Fullname" in {
      val submitterInfo = SubmitterInfo("", "AAgency", "jobRole", "07923456708", EmailAddress("abc@xyz.com"))
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/submitSubmitterInfo").withJsonBody(Json.toJson(submitterInfo)))
      status(controller.submitSubmitterInfo(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }
  }

  "POST /submitSubmitterInfo" should {
    "return 400 when the all data exists but AgencyOrBusinessname" in {
      val submitterInfo = SubmitterInfo("Fullname", "", "jobRole", "07923456708", EmailAddress("abc@xyz.com"))
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/submitSubmitterInfo").withJsonBody(Json.toJson(submitterInfo)))
      status(controller.submitSubmitterInfo(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }
  }

  "POST /submitSubmitterInfo" should {
    "return 400 when the all data exists but JobRole" in {
      val submitterInfo = SubmitterInfo("Fullname", "AAgency", "", "07923456708", EmailAddress("abc@xyz.com"))
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/submitSubmitterInfo").withJsonBody(Json.toJson(submitterInfo)))
      status(controller.submitSubmitterInfo(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }
  }

  "POST /submitSubmitterInfo" should {
    "return 400 when the all data exists but Contact Phone" in {
      val submitterInfo = SubmitterInfo("Fullname", "AAgency", "jobRole", "", EmailAddress("abc@xyz.com"))
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/submitSubmitterInfo").withJsonBody(Json.toJson(submitterInfo)))
      status(controller.submitSubmitterInfo(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }
  }

  "POST /submitSubmitterInfo" should {
    "return 400 when the all data exists but Email Address" in {

      val submitterInfo = Json.obj(
        "fullName" ->"Fullname",
        "agencyBusinessName" ->"AAgency",
        "jobRole" -> "jobRole",
        "contactPhone" -> "07923456708",
        "email" -> ""
      )
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/submitSubmitterInfo").withJsonBody(Json.toJson(submitterInfo)))
      status(controller.submitSubmitterInfo(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }
  }


  "POST /submitSubmitterInfo" should {
    "return 400 when the all data exists but Email Address is in Invalid format" in {
      val submitterInfo = Json.obj(
        "fullName" ->"Fullname",
        "agencyBusinessName" ->"AAgency",
        "jobRole" -> "jobRole",
        "contactPhone" -> "07923456708",
        "email" -> "abc.xyz"
      )

      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/submitSubmitterInfo").withJsonBody(Json.toJson(submitterInfo)))
      status(controller.submitSubmitterInfo(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }
  }

  "POST /submitSubmitterInfo" should {
    "return 400 when the empty fields of data exists" in {
      val submitterInfo = Json.obj(
        "fullName" ->"",
        "agencyBusinessName" ->"",
        "jobRole" -> "",
        "contactPhone" -> "",
        "email" -> ""
      )
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/submitSubmitterInfo").withJsonBody(Json.toJson(submitterInfo)))
      status(controller.submitSubmitterInfo(fakeRequestSubscribe)) shouldBe Status.BAD_REQUEST
    }
  }

  "POST /submitSubmitterInfo" should {
    "return 200 when all of the data exists & valid" in {
      val submitterInfo = SubmitterInfo("Fullname", "AAgency", "jobRole", "07923456708", EmailAddress("abc@xyz.com"))
      val fakeRequestSubscribe = addToken(FakeRequest("POST", "/submitSubmitterInfo").withJsonBody(Json.toJson(submitterInfo)))
      when(cache.save[SubmitterInfo](any())(any(),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      status(controller.submitSubmitterInfo(fakeRequestSubscribe)) shouldBe Status.OK
    }
  }
}
