/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.cbcrfrontend.controllers.upscan

import base.SpecBase
import cats.data.EitherT
import cats.implicits.catsStdInstancesForFuture
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{reset, when}
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.FakeRequest
import play.api.test.Helpers.{GET, route, status, _}
import uk.gov.hmrc.auth.core.AffinityGroup
import uk.gov.hmrc.cbcrfrontend.controllers.CSRFTest
import uk.gov.hmrc.cbcrfrontend.model.{AllBusinessRuleErrors, CBC701, CBCErrors, FatalSchemaErrors, InvalidFileType, InvalidSession, TestDataError, ValidationErrors, XMLErrors}
import uk.gov.hmrc.cbcrfrontend.model.upscan.FileValidationResult
import uk.gov.hmrc.cbcrfrontend.services.FileValidationService
import uk.gov.hmrc.cbcrfrontend.controllers.{routes => mainRoutes}
import uk.gov.hmrc.cbcrfrontend.util.ErrorUtil

import java.io.File
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class FileValidationControllerSpec extends SpecBase with CSRFTest {

  val mockFileValidationService: FileValidationService = mock[FileValidationService]
  val mockErrorUtil: ErrorUtil = mock[ErrorUtil]

  override def beforeEach: Unit = {
    reset(mockFileValidationService)
    super.beforeEach
  }

  override def guiceApplicationBuilder(): GuiceApplicationBuilder =
    super
      .guiceApplicationBuilder()
      .overrides(
        bind[FileValidationService].toInstance(mockFileValidationService),
        bind[ErrorUtil].toInstance(mockErrorUtil)
      )

  lazy val UploadFormRoutes: String = routes.FileValidationController.fileValidate().url

  private val validationResult =
    FileValidationResult(Some(AffinityGroup.Organisation), Some("test.xml"), Some(12), None, None, Some(CBC701))

  private val invalidFileResult: InvalidFileType = InvalidFileType("test.txt")

  private val schmemaFatalError = FatalSchemaErrors(Some(1))

  val testFile: File = new File("test/resources/cbcr-valid.xml")
  val tempFile: File = File.createTempFile("test", ".xml")
  val validFile: File = java.nio.file.Files.copy(testFile.toPath, tempFile.toPath, REPLACE_EXISTING).toFile

  "File validation controller" - {
    "must validate the uploaded file and return OK" in {
      val test: EitherT[Future, CBCErrors, FileValidationResult] = EitherT.right(Future.successful(validationResult))
      when(mockFileValidationService.fileValidate()(any(), any(), any()))
        .thenReturn(test)

      val request = addToken(FakeRequest(GET, UploadFormRoutes))

      val result = route(app, request).value

      status(result) shouldBe OK

      contentAsString(result).contains(messages("fileUploadResult.title.ok")) shouldBe true

    }

    "must validate the uploaded file and return OK with Fatal Schema Error" in {
      val test: EitherT[Future, CBCErrors, FileValidationResult] = EitherT.left(Future.successful(schmemaFatalError))

      when(mockFileValidationService.fileValidate()(any(), any(), any()))
        .thenReturn(test)

      val request = addToken(FakeRequest(GET, UploadFormRoutes))

      val result = route(app, request).value

      status(result) shouldBe OK

      contentAsString(result).contains(messages("fileUploadResult.error.title")) shouldBe true

    }

    "must redirect to the Invalid File controller when the file is not xml" in {
      val invalidFileType: EitherT[Future, CBCErrors, FileValidationResult] =
        EitherT.left(Future.successful(invalidFileResult))
      when(mockFileValidationService.fileValidate()(any(), any(), any()))
        .thenReturn(invalidFileType)

      val request = addToken(FakeRequest(GET, UploadFormRoutes))

      val result = route(app, request).value

      status(result) shouldBe SEE_OTHER
      redirectLocation(result) shouldBe Some(routes.FileValidationController.fileInvalid.url)

    }

    "must redirect to the technical difficulties page when other CBCError occurs" in {
      val invalidSession: EitherT[Future, CBCErrors, FileValidationResult] =
        EitherT.left(Future.successful(InvalidSession))
      when(mockFileValidationService.fileValidate()(any(), any(), any()))
        .thenReturn(invalidSession)

      val request = addToken(FakeRequest(GET, UploadFormRoutes))

      val result = route(app, request).value

      status(result) shouldBe SEE_OTHER
      redirectLocation(result) shouldBe Some(mainRoutes.SharedController.technicalDifficulties.url)
    }

    "must redirect to the technical difficulties page when exception occurs" in {
      val exceptionOccurs: EitherT[Future, CBCErrors, FileValidationResult] =
        EitherT.left(Future.failed(new Exception("error")))
      when(mockFileValidationService.fileValidate()(any(), any(), any()))
        .thenReturn(exceptionOccurs)

      val request = addToken(FakeRequest(GET, UploadFormRoutes))

      val result = route(app, request).value

      status(result) shouldBe SEE_OTHER
      redirectLocation(result) shouldBe Some(mainRoutes.SharedController.technicalDifficulties.url)
    }

    "getBusiness errors must return 200 if error details found in cache" in {

      when(mockErrorUtil.errorsToFile(any(), any())(any())).thenReturn(validFile)
      when(mockCache.readOption[AllBusinessRuleErrors](any(), any(), any())) thenReturn Future
        .successful(Some(AllBusinessRuleErrors(List(TestDataError))))

      val getBusinessErrorsUrl = routes.FileValidationController.getBusinessRuleErrors.url

      val request = addToken(FakeRequest(GET, getBusinessErrorsUrl))

      val result = route(app, request).value

      status(result) shouldBe OK

    }

    "getBusiness errors must return No content if no error details found in cache" in {

      when(mockCache.readOption[AllBusinessRuleErrors](any(), any(), any())) thenReturn Future
        .successful(None)

      val getBusinessErrorsUrl = routes.FileValidationController.getBusinessRuleErrors.url

      val request = addToken(FakeRequest(GET, getBusinessErrorsUrl))

      val result = route(app, request).value

      status(result) shouldBe NO_CONTENT

    }

    "getXML errors must return 200 if error details found in cache" in {

      when(mockErrorUtil.errorsToFile(any(), any())(any())).thenReturn(validFile)
      when(mockCache.readOption[XMLErrors](any(), any(), any())) thenReturn Future.successful(
        Some(XMLErrors(List("Big xml error"))))

      val getXmlErrors = routes.FileValidationController.getXmlSchemaErrors.url

      val request = addToken(FakeRequest(GET, getXmlErrors))

      val result = route(app, request).value

      status(result) shouldBe OK

    }

    "getXml errors must return No content if no error details found in cache" in {

      when(mockCache.readOption[XMLErrors](any(), any(), any())) thenReturn Future.successful(None)

      val getXmlErrors = routes.FileValidationController.getXmlSchemaErrors.url

      val request = addToken(FakeRequest(GET, getXmlErrors))

      val result = route(app, request).value

      status(result) shouldBe NO_CONTENT

    }
  }
}
