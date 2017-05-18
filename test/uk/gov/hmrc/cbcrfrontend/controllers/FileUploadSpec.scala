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

import java.io.File

import cats.data.{EitherT, Validated}
import org.scalatest.concurrent.ScalaFutures
import org.scalatestplus.play.OneAppPerSuite
import play.api.http.Status
import play.api.i18n.MessagesApi
import play.api.test.FakeRequest
import uk.gov.hmrc.cbcrfrontend.controllers.auth._
import uk.gov.hmrc.play.test.UnitSpec
import org.mockito.Mockito._
import org.mockito.Matchers._
import org.scalatest.mock.MockitoSugar
import uk.gov.hmrc.cbcrfrontend.core.fromFutureOptA
import uk.gov.hmrc.cbcrfrontend.exceptions.UnexpectedState
import uk.gov.hmrc.cbcrfrontend.model.{EnvelopeId, FileMetadata, FileUploadCallbackResponse}
import uk.gov.hmrc.cbcrfrontend.services.{CBCSessionCache, FileUploadService}
import uk.gov.hmrc.cbcrfrontend.typesclasses.{CbcrsUrl, FusFeUrl, FusUrl, ServiceUrl}
import cats.instances.future._
import org.xml.sax.{Locator, SAXParseException}
import play.api.libs.json.{JsObject, Json}
import uk.gov.hmrc.cbcrfrontend.connectors.FileUploadServiceConnector
import uk.gov.hmrc.cbcrfrontend.xmlvalidator.{CBCRXMLValidator, XmlErorHandler}
import uk.gov.hmrc.play.http.{HeaderCarrier, HttpResponse}
import cats.data.Validated
import org.scalacheck.Prop.Exception

import scala.concurrent.{ExecutionContext, Future}


class FileUploadSpec extends UnitSpec with ScalaFutures with OneAppPerSuite with CSRFTest with MockitoSugar with FakeAuthConnector {

  implicit val ec = app.injector.instanceOf[ExecutionContext]
  implicit val messagesApi = app.injector.instanceOf[MessagesApi]
  val fusConnector = mock[FileUploadServiceConnector]
  val fuService = mock[FileUploadService]
  val schemaValidator = mock[CBCRXMLValidator]
  val cache = mock[CBCSessionCache]

  implicit val hc = HeaderCarrier()
  implicit val fusUrl = new ServiceUrl[FusUrl] { val url = "file-upload"}
  implicit val fusFeUrl = new ServiceUrl[FusFeUrl] { val url = "file-upload-frontend"}
  implicit val cbcrsUrl = new ServiceUrl[CbcrsUrl] { val url = "cbcr"}

  val fakeRequestChooseXMLFile = addToken(FakeRequest("GET", "/upload-report"))


  "GET /upload-report" should {
    "return 200" in {
      val controller = fileUploadController

      when(fuService.createEnvelope(anyObject(), anyObject(), anyObject(), anyObject())) thenReturn (EitherT.right[Future, UnexpectedState, EnvelopeId](Future.successful(EnvelopeId("someEnvelopeId"))))
      val result = controller.chooseXMLFile(fakeRequestChooseXMLFile)
      status(result) shouldBe Status.OK

    }
  }


  "GET /upload-report" should {
    "return 500" in {
      val controller = fileUploadController

      when(fuService.createEnvelope(anyObject(), anyObject(), anyObject(), anyObject())) thenReturn (EitherT.left[Future, UnexpectedState, EnvelopeId](Future.successful(UnexpectedState("server error"))))
      val result = controller.chooseXMLFile(fakeRequestChooseXMLFile)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR

    }
  }

  val fakeRequestGetFileUploadResponse = addToken(FakeRequest("GET", "/fileUploadResponse/envelopeId/fileId"))

  "GET /fileUploadResponse/envelopeId/fileId" should {
    "return 202" in {
      val controller = fileUploadController
      val file = mock[File]

      when(fuService.getFileUploadResponse(anyString, anyString)(anyObject(), anyObject(), anyObject())) thenReturn (EitherT.right[Future, UnexpectedState, Option[FileUploadCallbackResponse]]
        (Some(FileUploadCallbackResponse("envelopeId", "fileId", "AVAILABLE"))))
      when(fuService.getFile(anyString, anyString)(anyObject(), anyObject(), anyObject())) thenReturn (EitherT.right[Future, UnexpectedState, File](file))
      when(schemaValidator.validate(file)) thenReturn Validated.Valid(file)
      when(fuService.deleteEnvelope(anyString)(anyObject(), anyObject(), anyObject())) thenReturn (EitherT.right[Future, UnexpectedState, String]("FileDeleted"))
      when(fuService.getFileMetaData(anyString, anyString)(anyObject(), anyObject(), anyObject())) thenReturn (EitherT.right[Future, UnexpectedState, Option[FileMetadata]]
        (Some(FileMetadata("9f34660e-edab-4b23-a957-f434d0469c6d", "AVAILABLE", "oecd-2017-05-07T23:18:43.849-cbcr.xml", "application/xml; charset=UTF-8", 4772, "017-05-07T22:18:44Z"
        ,Json.toJson("{}"), "/file-upload/envelopes/d842d9bc-c920-4e21-ae8d-2c57efab5fc6/files/9f34660e-edab-4b23-a957-f434d0469c6d/content"))))

      val result = controller.fileUploadResponse("envelopeId", "fileId")(fakeRequestGetFileUploadResponse)
      status(result) shouldBe Status.ACCEPTED

    }
  }

  "GET /fileUploadResponse/envelopeId/fileId" should {
    "return 406" in {
      val controller = fileUploadController
      val file = mock[File]

      when(fuService.getFileUploadResponse(anyString, anyString)(anyObject(), anyObject(), anyObject())) thenReturn (EitherT.right[Future, UnexpectedState, Option[FileUploadCallbackResponse]]
        (Some(FileUploadCallbackResponse("envelopeId", "fileId", "AVAILABLE"))))
      when(fuService.getFile(anyString, anyString)(anyObject(), anyObject(), anyObject())) thenReturn (EitherT.right[Future, UnexpectedState, File](file))
      when(schemaValidator.validate(file)) thenReturn Validated.Invalid(new XmlErorHandler)
      when(fuService.deleteEnvelope(anyString)(anyObject(), anyObject(), anyObject())) thenReturn (EitherT.right[Future, UnexpectedState, String]("FileDeleted"))

      val result = controller.fileUploadResponse("envelopeId", "fileId")(fakeRequestGetFileUploadResponse)
      status(result) shouldBe Status.NOT_ACCEPTABLE

    }
  }

  "GET /fileUploadResponse/envelopeId/fileId" should {
    "return 204" in {
      val controller = fileUploadController

      when(fuService.getFileUploadResponse(anyString, anyString)(anyObject(), anyObject(), anyObject())) thenReturn (EitherT.right[Future, UnexpectedState, Option[FileUploadCallbackResponse]]
        (None))
      val result = (controller.fileUploadResponse("envelopeId", "fileId")(fakeRequestGetFileUploadResponse)).futureValue
      status(result) shouldBe Status.NO_CONTENT

    }
  }


  "GET /fileUploadResponse/envelopeId/fileId" should {
    "return 500" in {
      val controller = fileUploadController

      when(fuService.getFileUploadResponse(anyString, anyString)(anyObject(), anyObject(), anyObject())) thenReturn (EitherT.right[Future, UnexpectedState, Option[FileUploadCallbackResponse]]
        (Some(FileUploadCallbackResponse("envelopeId", "fileId", "AVAILABLE"))))
      when(fuService.getFile(anyString, anyString)(anyObject(), anyObject(), anyObject())) thenReturn (EitherT.left[Future, UnexpectedState, File](UnexpectedState("Problem getting file")))

      val result = controller.fileUploadResponse("envelopeId", "fileId")(fakeRequestGetFileUploadResponse)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR

    }
  }


  val fakeRequestSuccessFileUpload = addToken(FakeRequest("GET", "/successFileUpload"))

  "GET /successFileUpload" should {
    "return 200" in {
      val controller = fileUploadController

      val result = controller.successFileUpload("fileName", "fileSize")(fakeRequestSuccessFileUpload).futureValue
      status(result) shouldBe Status.OK

    }
  }

  val fakeRequestErrorFileUpload = addToken(FakeRequest("GET", "/errorFileUpload"))

  "GET /errorFileUpload" should {
    "return 200" in {
      val controller = fileUploadController

      val result = controller.errorFileUpload("errorMessage")(fakeRequestErrorFileUpload).futureValue
      status(result) shouldBe Status.OK

    }
  }





  def fileUploadController(implicit messagesApi: MessagesApi) = {

    val authCon = authConnector(TestUsers.cbcrUser)
    val securedActions = new SecuredActionsTest(TestUsers.cbcrUser, authCon)
    new FileUpload(securedActions, fusConnector, schemaValidator,cache) {
     override lazy val fileUploadService = fuService
    }
  }


}
