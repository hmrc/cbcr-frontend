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

import org.scalatest.concurrent.ScalaFutures
import org.scalatestplus.play.OneAppPerSuite
import play.api.http.Status
import play.api.i18n.MessagesApi
import play.api.test.FakeRequest
import uk.gov.hmrc.cbcrfrontend.controllers.auth._
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.ExecutionContext


class FileUploadSpec extends UnitSpec with ScalaFutures with OneAppPerSuite with CSRFTest with FakeAuthConnector {

  implicit val ec = app.injector.instanceOf[ExecutionContext]
  implicit val messagesApi = app.injector.instanceOf[MessagesApi]

  val fakeRequestChooseXMLFile = addToken(FakeRequest("GET", "/upload-report"))

  "GET /upload-report" should {
    "return 200" in {
      val controller = fileUploadController

      val result = controller.chooseXMLFile(fakeRequestChooseXMLFile).futureValue
      status(result) shouldBe Status.OK

    }
  }

  val fakeRequestUploadProgress = addToken(FakeRequest("GET", "/upload-progress"))

  "GET /upload-progress" should {
    "return 200" in {
      val controller = fileUploadController

      val result = controller.fileUploadProgress(fakeRequestUploadProgress).futureValue
      status(result) shouldBe Status.OK

    }
  }


  val fakeRequestSuccessFileUpload = addToken(FakeRequest("GET", "/successFileUpload"))

  "GET /successFileUpload" should {
    "return 200" in {
      val controller = fileUploadController

      val result = controller.successFileUpload(fakeRequestSuccessFileUpload).futureValue
      status(result) shouldBe Status.OK

    }
  }

  val fakeRequestErrorFileUpload = addToken(FakeRequest("GET", "/errorFileUpload"))

  "GET /errorFileUpload" should {
    "return 200" in {
      val controller = fileUploadController

      val result = controller.successFileUpload(fakeRequestErrorFileUpload).futureValue
      status(result) shouldBe Status.OK

    }
  }



  def fileUploadController(implicit messagesApi: MessagesApi) = {

    val authCon = authConnector(TestUsers.cbcrUser)
    val securedActions = new SecuredActionsTest(TestUsers.cbcrUser, authCon)

    new FileUpload(securedActions) {}
  }


}
