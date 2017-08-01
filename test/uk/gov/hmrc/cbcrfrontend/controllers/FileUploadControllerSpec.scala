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

import cats.data.{EitherT, OptionT}
import cats.instances.future._
import com.typesafe.config.ConfigFactory
import org.compass.core.config.CompassConfigurationFactory
import org.mockito.Matchers.{eq => EQ, _}
import org.mockito.Mockito._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mock.MockitoSugar
import org.scalatestplus.play.OneAppPerSuite
import play.api.Configuration
import play.api.http.Status
import play.api.i18n.MessagesApi
import play.api.libs.Files
import play.api.libs.json.{Format, JsNull}
import play.api.test.FakeRequest
import uk.gov.hmrc.cbcrfrontend.controllers.auth._
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.services._
import uk.gov.hmrc.cbcrfrontend.typesclasses.{CbcrsUrl, FusFeUrl, FusUrl, ServiceUrl}
import uk.gov.hmrc.http.cache.client.CacheMap
import uk.gov.hmrc.play.http.{HeaderCarrier, HttpDelete, HttpGet, HttpPut}
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.reflect.runtime.universe


class FileUploadControllerSpec extends UnitSpec with ScalaFutures with OneAppPerSuite with CSRFTest with MockitoSugar with FakeAuthConnector with BeforeAndAfterEach{

  implicit val ec: ExecutionContext                    = app.injector.instanceOf[ExecutionContext]
  implicit val messagesApi: MessagesApi                = app.injector.instanceOf[MessagesApi]
  implicit val authCon                                 = authConnector(TestUsers.cbcrUser)
  val securedActions: SecuredActionsTest               = new SecuredActionsTest(TestUsers.cbcrUser, authCon)

  val fuService: FileUploadService                     = mock[FileUploadService]
  val schemaValidator: CBCRXMLValidator                = mock[CBCRXMLValidator]
  val businessRulesValidator: CBCBusinessRuleValidator = mock[CBCBusinessRuleValidator]
  val cache: CBCSessionCache                           = mock[CBCSessionCache]
  val extractor: XmlInfoExtract                        = new XmlInfoExtract()

  override protected def afterEach(): Unit = {
    reset(cache,businessRulesValidator,schemaValidator,fuService)
    super.afterEach()
  }

  object TestSessionCache {

    var succeed = true

    val http = mock[HttpGet with HttpPut with HttpDelete]
    val configuration = new Configuration(ConfigFactory.load("application.conf"))

    def apply(): CBCSessionCache = new SessionCache(configuration, http)

    private class SessionCache(_config:Configuration, _http:HttpGet with HttpPut with HttpDelete) extends CBCSessionCache(_config, _http) {
      override def readOrCreate[T: Format : universe.TypeTag](f: => OptionT[Future, T])(implicit hc: HeaderCarrier): OptionT[Future, T] = universe.typeOf[T] match {
        case t if t =:= universe.typeOf[FileId] => OptionT.some[Future, FileId](FileId("fileId")).asInstanceOf[OptionT[Future, T]]
        case t if t =:= universe.typeOf[EnvelopeId] => succeed match {
          case true =>  OptionT.some[Future, EnvelopeId](EnvelopeId("envId")).asInstanceOf[OptionT[Future, T]]
          case false => OptionT.none
        }
      }
    }
  }

  implicit val hc = HeaderCarrier()
  implicit val fusUrl = new ServiceUrl[FusUrl] { val url = "file-upload"}
  implicit val fusFeUrl = new ServiceUrl[FusFeUrl] { val url = "file-upload-frontend"}
  implicit val cbcrsUrl = new ServiceUrl[CbcrsUrl] { val url = "cbcr"}

  def right[A](a:Future[A]) : ServiceResponse[A] = EitherT.right[Future,CBCErrors, A](a)
  def left[A](s:String) : ServiceResponse[A] = EitherT.left[Future,CBCErrors, A](UnexpectedState(s))
  def pure[A](a:A) : ServiceResponse[A] = EitherT.pure[Future,CBCErrors, A](a)

  val md = FileMetadata("","","something.xml","",1.0,"",JsNull,"")

  val partiallyMockedController = new FileUploadController(securedActions, schemaValidator, businessRulesValidator, TestSessionCache(),fuService, extractor)
  val controller = new FileUploadController(securedActions, schemaValidator, businessRulesValidator, cache,fuService, extractor)

  val file = Files.TemporaryFile("","")
  val validFile = new File("test/resources/cbcr-valid.xml")

  "GET /upload-report" should {
    val fakeRequestChooseXMLFile = addToken(FakeRequest("GET", "/upload-report"))

    "return 200 when the envelope is created successfully" in {
      val result = partiallyMockedController.chooseXMLFile(fakeRequestChooseXMLFile)
      status(result) shouldBe Status.OK
    }
    "return 500 when the is an error creating the envelope" in {
      TestSessionCache.succeed = false
      when(fuService.createEnvelope(any(), any(), any(), any())) thenReturn left[EnvelopeId]("server error")
      val result = partiallyMockedController.chooseXMLFile(fakeRequestChooseXMLFile)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
      TestSessionCache.succeed = true
    }
  }

  "GET /fileUploadResponse/envelopeId/fileId" should {
    val fakeRequestGetFileUploadResponse  = addToken(FakeRequest("GET", "/fileUploadResponse/envelopeId/fileId"))
    "return 202 when the file is available" in {
      when(fuService.getFileUploadResponse(any(), any())(any(), any(), any())) thenReturn right(Some(FileUploadCallbackResponse("envelopeId", "fileId", "AVAILABLE", None)):Option[FileUploadCallbackResponse])
      val result = partiallyMockedController.fileUploadResponse("envelopeId", "fileId")(fakeRequestGetFileUploadResponse)
      status(result) shouldBe Status.ACCEPTED
    }
    "return 204" when {
      "the FUS hasn't updated the backend yet" in {
        when(fuService.getFileUploadResponse(any(), any())(any(), any(), any())) thenReturn right[Option[FileUploadCallbackResponse]](None)
        val result = partiallyMockedController.fileUploadResponse("envelopeId", "fileId")(fakeRequestGetFileUploadResponse).futureValue
        status(result) shouldBe Status.NO_CONTENT
      }
      "file is not yet available" in {
        when(fuService.getFileUploadResponse(any(), any())(any(), any(), any())) thenReturn right[Option[FileUploadCallbackResponse]](Some(FileUploadCallbackResponse("envelopeId", "fileId", "QUARENTEENED",None)): Option[FileUploadCallbackResponse])
        val result = partiallyMockedController.fileUploadResponse("envelopeId", "fileId")(fakeRequestGetFileUploadResponse).futureValue
        status(result) shouldBe Status.NO_CONTENT
      }
    }
    "return a 200" in {
      val request = addToken(FakeRequest("GET", "fileUploadProgress/envelopeId/fileId"))
      val result = partiallyMockedController.fileUploadProgress("test","test")(request)
      status(result) shouldBe Status.OK
    }
    "direct to technical-difficulties" when {
      "the call to get the file metadata fails" in{
        val request = addToken(FakeRequest("GET", "fileUploadReady/envelopeId/fileId"))
        when(fuService.getFile(any(),any())(any(),any(),any())) thenReturn right(validFile)
        when(fuService.getFileMetaData(any(),any())(any(),any(),any())) thenReturn right[Option[FileMetadata]](None)
        val result = Await.result(controller.fileValidate("test","test")(request), 2.second)
        result.header.headers("Location") should endWith("technical-difficulties")
        status(result) shouldBe Status.SEE_OTHER
        verify(fuService).getFile(any(),any())(any(),any(),any())
        verify(fuService).getFileMetaData(any(),any())(any(),any(),any())
      }
      "the call to get the file fails" in {
        val request = addToken(FakeRequest("GET", "fileUploadReady/envelopeId/fileId"))
        when(fuService.getFile(any(),any())(any(),any(),any())) thenReturn left[File]("oops")
        when(fuService.getFileMetaData(any(),any())(any(),any(),any())) thenReturn right[Option[FileMetadata]](Some(md))
        val result = Await.result(controller.fileValidate("test","test")(request), 2.second)
        result.header.headers("Location") should endWith("technical-difficulties")
        status(result) shouldBe Status.SEE_OTHER
        verify(fuService).getFile(any(),any())(any(),any(),any())
        verify(fuService).getFileMetaData(any(),any())(any(),any(),any())
      }
      "the call to cache.save fails" in {
        val request = addToken(FakeRequest("GET", "fileUploadReady/envelopeId/fileId"))
        when(fuService.getFile(any(),any())(any(),any(),any())) thenReturn right(validFile)
        when(fuService.getFileMetaData(any(),any())(any(),any(),any())) thenReturn right[Option[FileMetadata]](Some(md))
        when(cache.save(any())(any(),any(),any())) thenReturn Future.failed(new Exception("bad"))
        val result = Await.result(controller.fileValidate("test","test")(request), 2.second)
        result.header.headers("Location") should endWith("technical-difficulties")
        status(result) shouldBe Status.SEE_OTHER
        verify(fuService).getFile(any(),any())(any(),any(),any())
        verify(fuService).getFileMetaData(any(),any())(any(),any(),any())
        verify(cache,atLeastOnce()).save(any())(any(),any(),any())
      }
    }

    "be redirected to an error page" when {
      "the file extension is invalid" in {
        val request = addToken(FakeRequest("GET", "fileUploadReady/envelopeId/fileId"))
        when(fuService.getFile(any(),any())(any(),any(),any())) thenReturn right(validFile)
        when(fuService.getFileMetaData(any(),any())(any(),any(),any())) thenReturn right[Option[FileMetadata]](Some(md.copy(name = "bad.zip")))
        when(cache.read[CBCId](EQ(CBCId.cbcIdFormat),any(),any())) thenReturn Future.successful(CBCId.create(1).toOption)
        when(cache.save(any())(any(),any(),any())) thenReturn Future.successful(CacheMap("cache",Map.empty))
        val result = Await.result(controller.fileValidate("test","test")(request), 5.second)
        result.header.headers("Location") should endWith("invalid-file-type")
        status(result) shouldBe Status.SEE_OTHER
      }
    }
  }

  "The file-upload error call back" should {
    "cause a redirect to file-too-large if the response has status-code 413" in {
      val request = addToken(FakeRequest())
      val result = Await.result(partiallyMockedController.handleError(413, "no reason")(request), 5.second)
      result.header.headers("Location") should endWith("file-too-large")
      status(result) shouldBe Status.SEE_OTHER
    }
    "cause a redirect to invalid-file-type if the response has status-code 415" in {
      val request = addToken(FakeRequest())
      val result = Await.result(partiallyMockedController.handleError(415, "no reason")(request), 5.second)
      result.header.headers("Location") should endWith("invalid-file-type")
      status(result) shouldBe Status.SEE_OTHER
    }
  }

}
