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

import java.io.{File, IOException}

import cats.data.{EitherT, Validated}
import cats.instances.future._
import org.mockito.Matchers.{eq => EQ,_}
import org.mockito.Mockito._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mock.MockitoSugar
import org.scalatestplus.play.OneAppPerSuite
import play.api.http.Status
import play.api.i18n.MessagesApi
import play.api.libs.Files
import play.api.libs.Files.TemporaryFile
import play.api.libs.json.{JsNull, JsValue, Json}
import play.api.mvc.MultipartFormData.FilePart
import play.api.mvc.{AnyContentAsEmpty, MultipartFormData}
import play.api.test.FakeRequest
import uk.gov.hmrc.cbcrfrontend.connectors.FileUploadServiceConnector
import uk.gov.hmrc.cbcrfrontend.controllers.auth._
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.exceptions.UnexpectedState
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.services._
import uk.gov.hmrc.cbcrfrontend.typesclasses.{CbcrsUrl, FusFeUrl, FusUrl, ServiceUrl}
import uk.gov.hmrc.http.cache.client.CacheMap
import uk.gov.hmrc.play.http.HeaderCarrier
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.{ExecutionContext, Future}


class FileUploadSpec extends UnitSpec with ScalaFutures with OneAppPerSuite with CSRFTest with MockitoSugar with FakeAuthConnector {

  implicit val ec: ExecutionContext                    = app.injector.instanceOf[ExecutionContext]
  implicit val messagesApi: MessagesApi                = app.injector.instanceOf[MessagesApi]
  implicit val authCon                                 = authConnector(TestUsers.cbcrUser)
  val securedActions: SecuredActionsTest               = new SecuredActionsTest(TestUsers.cbcrUser, authCon)

  val fuService: FileUploadService                     = mock[FileUploadService]
  val schemaValidator: CBCRXMLValidator                = mock[CBCRXMLValidator]
  val businessRulesValidator: CBCBusinessRuleValidator = mock[CBCBusinessRuleValidator]
  val cache: CBCSessionCache                           = mock[CBCSessionCache]

  implicit val hc = HeaderCarrier()
  implicit val fusUrl = new ServiceUrl[FusUrl] { val url = "file-upload"}
  implicit val fusFeUrl = new ServiceUrl[FusFeUrl] { val url = "file-upload-frontend"}
  implicit val cbcrsUrl = new ServiceUrl[CbcrsUrl] { val url = "cbcr"}

  def right[A](a:Future[A]) : ServiceResponse[A] = EitherT.right[Future,UnexpectedState, A](a)
  def left[A](s:String) : ServiceResponse[A] = EitherT.left[Future,UnexpectedState, A](UnexpectedState(s))
  def pure[A](a:A) : ServiceResponse[A] = EitherT.pure[Future,UnexpectedState, A](a)

  val md = FileMetadata("","","","",1.0,"",JsNull,"")

  val controller = new FileUpload(securedActions, schemaValidator, businessRulesValidator,cache,fuService)


  val file = Files.TemporaryFile("","")

  "GET /upload-report" should {
    val fakeRequestChooseXMLFile = addToken(FakeRequest("GET", "/upload-report"))
    "return 200 when the envelope is created successfully" in {
      when(fuService.createEnvelope(any(), any(), any(), any())) thenReturn pure(EnvelopeId("someEnvelopeId"))
      when(cache.save(any())(any(),any(),any())) thenReturn Future.successful(CacheMap("id",Map.empty))
      val result = controller.chooseXMLFile(fakeRequestChooseXMLFile)
      status(result) shouldBe Status.OK
    }
    "return 500 when the is an error creating the envelope" in {
      when(fuService.createEnvelope(any(), any(), any(), any())) thenReturn left[EnvelopeId]("server error")
      val result = controller.chooseXMLFile(fakeRequestChooseXMLFile)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }
  }

  "GET /fileUploadResponse/envelopeId/fileId" should {
    val fakeRequestGetFileUploadResponse  = addToken(FakeRequest("GET", "/fileUploadResponse/envelopeId/fileId"))
    "return 202 when the file is available" in {
      when(fuService.getFileUploadResponse(any(), any())(any(), any(), any())) thenReturn right(Some(FileUploadCallbackResponse("envelopeId", "fileId", "AVAILABLE")):Option[FileUploadCallbackResponse])
      val result = controller.fileUploadResponse("envelopeId", "fileId")(fakeRequestGetFileUploadResponse)
      status(result) shouldBe Status.ACCEPTED
    }
    "return 204 when the FUS hasn't updated the backend yet" in {
      when(fuService.getFileUploadResponse(any(), any())(any(), any(), any())) thenReturn right[Option[FileUploadCallbackResponse]](None)
      val result = controller.fileUploadResponse("envelopeId", "fileId")(fakeRequestGetFileUploadResponse).futureValue
      status(result) shouldBe Status.NO_CONTENT
    }
    "return 204 when the file is not yet available" in {
      when(fuService.getFileUploadResponse(any(), any())(any(), any(), any())) thenReturn right[Option[FileUploadCallbackResponse]](Some(FileUploadCallbackResponse("envelopeId", "fileId", "QUARENTEENED")):Option[FileUploadCallbackResponse])
      val result = controller.fileUploadResponse("envelopeId", "fileId")(fakeRequestGetFileUploadResponse).futureValue
      status(result) shouldBe Status.NO_CONTENT
    }
  }


  "GET /fileUploadProgress/envelopeId/fileId" should {
    "return a 200" in {
      val request = addToken(FakeRequest("GET", "fileUploadProgress/envelopeId/fileId"))
      val result = controller.fileUploadProgress("test","test")(request)
      status(result) shouldBe Status.OK
    }
  }

  "GET /fileUploadReady/envelopeId/fileId" should {
    "return a 500" when {
      "the call to get the file metadata fails" in{
        val request = addToken(FakeRequest("GET", "fileUploadReady/envelopeId/fileId"))
        when(fuService.getFileMetaData(any(),any())(any(),any(),any())) thenReturn right[Option[FileMetadata]](None)
        when(fuService.deleteEnvelope(EQ("test"))(any(),any(),any())) thenReturn right("yeah")
        val result = controller.fileValidate("test","test")(request)
        status(result) shouldBe Status.INTERNAL_SERVER_ERROR
      }
      "the call to cache.save fails" in {
        val request = addToken(FakeRequest("GET", "fileUploadReady/envelopeId/fileId"))
        when(fuService.getFileMetaData(any(),any())(any(),any(),any())) thenReturn right[Option[FileMetadata]](Some(md))
        when(cache.save(any())(any(),any(),any())) thenReturn Future.failed(new Exception("bad"))
        when(fuService.deleteEnvelope(EQ("test"))(any(),any(),any())) thenReturn right("yeah")
        val result = controller.fileValidate("test","test")(request)
        status(result) shouldBe Status.INTERNAL_SERVER_ERROR
      }
      "the call to get the file fails" in {
        val request = addToken(FakeRequest("GET", "fileUploadReady/envelopeId/fileId"))
        when(fuService.getFileMetaData(any(),any())(any(),any(),any())) thenReturn right[Option[FileMetadata]](Some(md))
        when(cache.save(any())(any(),any(),any())) thenReturn Future.successful(CacheMap("cache",Map.empty))
        when(fuService.getFile(any(),any())(any(),any(),any())) thenReturn left[File]("oops")
        when(fuService.deleteEnvelope(EQ("test"))(any(),any(),any())) thenReturn right("yeah")
        val result = controller.fileValidate("test","test")(request)
        status(result) shouldBe Status.INTERNAL_SERVER_ERROR
      }
    }
    "return a 200" when {
      "all the calls succeed" in {
        val request = addToken(FakeRequest("GET", "fileUploadReady/envelopeId/fileId"))
        when(fuService.getFileMetaData(any(),any())(any(),any(),any())) thenReturn right[Option[FileMetadata]](Some(md))
        when(cache.save(any())(any(),any(),any())) thenReturn Future.successful(CacheMap("cache",Map.empty))
        when(fuService.getFile(any(),any())(any(),any(),any())) thenReturn right(file.file)
        val result = controller.fileValidate("test","test")(request)
        status(result) shouldBe Status.INTERNAL_SERVER_ERROR
      }
    }
  }

}
