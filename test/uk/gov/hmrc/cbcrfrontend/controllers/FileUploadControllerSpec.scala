/*
 * Copyright 2023 HM Revenue & Customs
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

import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import cats.implicits.catsStdInstancesForFuture
import com.ctc.wstx.exc.WstxException
import com.typesafe.config.ConfigFactory
import org.codehaus.stax2.validation.XMLValidationProblem
import org.mockito.ArgumentMatchers.{any, eq => EQ}
import org.mockito.MockitoSugar
import org.mockito.cats.MockitoCats
import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.Configuration
import play.api.http.Status
import play.api.libs.json.JsNull
import play.api.test.Helpers.{charset, contentType, defaultAwaitTimeout, header, status}
import play.api.test.{FakeRequest, Helpers}
import play.twirl.api.Html
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.retrieve.{Credentials, Retrieval, ~}
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.services._
import uk.gov.hmrc.cbcrfrontend.views.Views
import uk.gov.hmrc.cbcrfrontend.views.html.error_template
import uk.gov.hmrc.cbcrfrontend.views.html.submission.fileupload.{chooseFile, fileUploadError, fileUploadProgress, fileUploadResult}
import uk.gov.hmrc.cbcrfrontend.views.html.submission.unregisteredGGAccount
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.cache.client.CacheMap
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}

import java.io.File
import java.nio.file.StandardCopyOption._
import java.time.{LocalDate, LocalDateTime}
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContextExecutor, Future}
import scala.language.{implicitConversions, postfixOps}

class FileUploadControllerSpec
    extends AnyWordSpec with Matchers with BeforeAndAfterEach with MockitoSugar
    with MockitoCats {

  implicit val ec: ExecutionContextExecutor = scala.concurrent.ExecutionContext.global

  private val fuService: FileUploadService = mock[FileUploadService]
  private val schemaValidator: CBCRXMLValidator = mock[CBCRXMLValidator]
  private val businessRulesValidator: CBCBusinessRuleValidator = mock[CBCBusinessRuleValidator]
  private val cache: CBCSessionCache = mock[CBCSessionCache]
  private val extractor: XmlInfoExtract = new XmlInfoExtract()
  private val auditC: AuditConnector = mock[AuditConnector]
  private val authConnector = mock[AuthConnector]
  private val file = mock[File]
  private val views = mock[Views]
  private val unregisteredGGAccountView = mock[unregisteredGGAccount]
  private val chooseFileView = mock[chooseFile]
  private val fileUploadErrorView = mock[fileUploadError]
  private val errorTemplateView = mock[error_template]
  private val fileUploadProgressView = mock[fileUploadProgress]
  private val fileUploadResultView = mock[fileUploadResult]

  private implicit val configuration: Configuration = new Configuration(ConfigFactory.load("application.conf"))
  private implicit val feConfig: FrontendAppConfig = mock[FrontendAppConfig]

  private val creds = Credentials("totally", "legit")

  implicit def liftFuture[A](v: A): Future[A] = Future.successful(v)

  override protected def afterEach(): Unit = {
    reset(cache, businessRulesValidator, schemaValidator, fuService, file, authConnector, auditC, feConfig)
    super.afterEach()
  }

  private val hc = HeaderCarrier()

  private val docRefId = "GB2016RGXVCBC0000000056CBC40120170311T090000X_7000000002OECD1"
  private val xmlInfo = XMLInfo(
    MessageSpec(
      MessageRefID("GB2016RGXVCBC0000000056CBC40120170311T090000X").getOrElse(fail("waaaaa")),
      "GB",
      CBCId.create(99).getOrElse(fail("booo")),
      LocalDateTime.now(),
      LocalDate.parse("2017-01-30"),
      None,
      None
    ),
    Some(
      ReportingEntity(
        CBC701,
        DocSpec(OECD1, DocRefId(docRefId + "REP").get, None, None),
        TIN("7000000002", "gb"),
        "name",
        None,
        EntityReportingPeriod(LocalDate.parse("2016-03-31"), LocalDate.parse("2017-03-30"))
      )),
    List(CbcReports(DocSpec(OECD1, DocRefId(docRefId + "ENT").get, None, None))),
    List(AdditionalInfo(DocSpec(OECD1, DocRefId(docRefId + "ADD").get, None, None), "Some Other Info")),
    Some(LocalDate.now()),
    List.empty[String],
    List.empty[String]
  )

  private val completeXmlInfo = CompleteXMLInfo(
    xmlInfo,
    ReportingEntity(
      CBC701,
      DocSpec(OECD1, DocRefId(docRefId + "REP").get, None, None),
      TIN("7000000002", "gb"),
      "name",
      None,
      EntityReportingPeriod(LocalDate.parse("2016-03-31"), LocalDate.parse("2017-03-30"))
    )
  )

  private val md = FileMetadata("", "", "something.xml", "", 1.0, "", JsNull, "")

  when(views.unregisteredGGAccount) thenReturn unregisteredGGAccountView
  when(views.chooseFile) thenReturn chooseFileView
  when(views.fileUploadError) thenReturn fileUploadErrorView
  when(views.errorTemplate) thenReturn errorTemplateView
  when(views.fileUploadProgress) thenReturn fileUploadProgressView
  when(views.fileUploadResult) thenReturn fileUploadResultView

  when(unregisteredGGAccountView.apply()(any(), any())) thenReturn Html("some html content")
  when(chooseFileView.apply(any(), any(), any())(any(), any(), any())) thenReturn Html("some html content")
  when(fileUploadErrorView.apply(any())(any(), any())) thenReturn Html("some html content")
  when(errorTemplateView.apply(any(), any(), any())(any(), any())) thenReturn Html("some html content")
  when(fileUploadProgressView.apply(any(), any(), any())(any(), any())) thenReturn Html("some html content")
  when(fileUploadResultView.apply(any(), any(), any(), any(), any(), any())(any(), any(), any())) thenReturn Html("some html content")

  private val controller = new FileUploadController(
    authConnector,
    schemaValidator,
    businessRulesValidator,
    fuService,
    extractor,
    auditC,
    Helpers.stubMessagesControllerComponents(),
    views,
    cache,
    configuration)(ec, feConfig)

  private val testFile = new File("test/resources/cbcr-valid.xml")
  private val tempFile = File.createTempFile("test", ".xml")
  private val validFile = java.nio.file.Files.copy(testFile.toPath, tempFile.toPath, REPLACE_EXISTING).toFile

  private val newCBCEnrolment = CBCEnrolment(CBCId.create(99).getOrElse(fail("booo")), Utr("1234567890"))

  "GET /upload-report" should {
    val fakeRequestChooseXMLFile = FakeRequest("GET", "/upload-report")

    "return 200 when the envelope is created successfully" in {
      when(authConnector.authorise(any(), any[Retrieval[Option[AffinityGroup] ~ Option[CBCEnrolment]]]())(any(), any()))
        .thenReturn(Future.successful(
          new ~[Option[AffinityGroup], Option[CBCEnrolment]](Some(AffinityGroup.Organisation), Some(newCBCEnrolment))))
      whenF(fuService.createEnvelope(any(), any())) thenReturn EnvelopeId("1234")
      whenF(cache.create[EnvelopeId](any())(EQ(EnvelopeId.format), any(), any())) thenReturn EnvelopeId("1234")
      whenF(cache.create[FileId](any())(EQ(FileId.fileIdFormat), any(), any())) thenReturn FileId("abcd")
      val result = controller.chooseXMLFile(fakeRequestChooseXMLFile)
      status(result) shouldBe Status.OK
    }

    "displays gateway account not registered page if Organisation user is not enrolled" in {
      when(authConnector.authorise(any(), any[Retrieval[Option[AffinityGroup] ~ Option[CBCEnrolment]]]())(any(), any()))
        .thenReturn(
          Future.successful(new ~[Option[AffinityGroup], Option[CBCEnrolment]](Some(AffinityGroup.Organisation), None)))
      when(cache.readOption[CBCId](any(), any(), any())) thenReturn Future.successful(None)

      val result = controller.chooseXMLFile(fakeRequestChooseXMLFile)
      status(result) shouldBe Status.OK

      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")

      verify(unregisteredGGAccountView).apply()(any(), any())
    }

    "allow agent to submit even when no enrolment" in {
      when(authConnector.authorise(any(), any[Retrieval[Option[AffinityGroup] ~ Option[CBCEnrolment]]]())(any(), any()))
        .thenReturn(Future.successful(
          new~[Option[AffinityGroup], Option[CBCEnrolment]](Some(AffinityGroup.Organisation), None)))
      when(cache.readOption[CBCId](any(), any(), any())) thenReturn Future.successful(None)
      val result = controller.chooseXMLFile(fakeRequestChooseXMLFile)
      status(result) shouldBe Status.OK
    }

    "redirect  when user is an individual" in {
      when(authConnector.authorise(any(), any[Retrieval[Option[AffinityGroup] ~ Option[CBCEnrolment]]]())(any(), any()))
        .thenReturn(
          Future.successful(new ~[Option[AffinityGroup], Option[CBCEnrolment]](Some(AffinityGroup.Individual), None)))
      val result = controller.chooseXMLFile(fakeRequestChooseXMLFile)
      status(result) shouldBe Status.SEE_OTHER
    }

    "return 500 when there is an error creating the envelope" in {
      when(authConnector.authorise(any(), any[Retrieval[Option[AffinityGroup] ~ Option[CBCEnrolment]]]())(any(), any()))
        .thenReturn(Future.successful(
          new ~[Option[AffinityGroup], Option[CBCEnrolment]](Some(AffinityGroup.Organisation), Some(newCBCEnrolment))))
      whenF(fuService.createEnvelope(any(), any())) thenFailWith UnexpectedState("server error")
      val result = controller.chooseXMLFile(fakeRequestChooseXMLFile)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }
  }

  "GET /unregistered-gg-account" should {
    val fakeRequestUnregisteredGGId = FakeRequest("GET", "/unregistered-gg-account")

    "return 200 when the envelope is created successfully" in {
      when(authConnector.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful((): Unit)
      whenF(fuService.createEnvelope(any(), any())) thenReturn EnvelopeId("1234")
      whenF(cache.create[EnvelopeId](any())(EQ(EnvelopeId.format), any(), any())) thenReturn EnvelopeId("1234")
      whenF(cache.create[FileId](any())(EQ(FileId.fileIdFormat), any(), any())) thenReturn FileId("abcd")
      val result = controller.unregisteredGGAccount(fakeRequestUnregisteredGGId)
      status(result) shouldBe Status.OK
    }

    "return 500 when there is an error creating the envelope" in {
      when(authConnector.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful((): Unit)
      whenF(fuService.createEnvelope(any(), any())) thenFailWith UnexpectedState("server error")
      val result = controller.unregisteredGGAccount(fakeRequestUnregisteredGGId)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }
  }

  "GET /fileUploadResponse/envelopeId/fileId" should {
    val fakeRequestGetFileUploadResponse = FakeRequest("GET", "/fileUploadResponse/envelopeId/fileId")
    "return 202 when the file is available" in {
      when(authConnector.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful((): Unit)
      whenF(fuService.getFileUploadResponse(any())(any(), any())) thenReturn
        Some(FileUploadCallbackResponse("envelopeId", "fileId", "AVAILABLE", None))
      val result = controller.fileUploadResponse("envelopeId")(fakeRequestGetFileUploadResponse)
      status(result) shouldBe Status.ACCEPTED
    }

    "return 204" when {
      "the FUS hasn't updated the backend yet" in {
        when(authConnector.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful((): Unit)
        whenF(fuService.getFileUploadResponse(any())(any(), any())) thenReturn None
        val result =
          controller.fileUploadResponse("envelopeId")(fakeRequestGetFileUploadResponse)
        status(result) shouldBe Status.NO_CONTENT
      }

      "file is not yet available" in {
        when(authConnector.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful((): Unit)
        whenF(fuService.getFileUploadResponse(any())(any(), any())) thenReturn
          Some(FileUploadCallbackResponse("envelopeId", "fileId", "QUARENTEENED", None))
        val result =
          controller.fileUploadResponse("envelopeId")(fakeRequestGetFileUploadResponse)
        status(result) shouldBe Status.NO_CONTENT
      }
    }

    "return a 200" in {
      when(authConnector.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful((): Unit)
      whenF(cache.read[EnvelopeId](any(), any(), any())) thenReturn EnvelopeId("test")
      val request = FakeRequest("GET", "fileUploadProgress/envelopeId/fileId")
      val result = controller.fileUploadProgress("test", "test")(request)

      status(result) shouldBe Status.OK
    }

    "return a 500 if the envelopeId doesn't match with the cache" in {
      when(authConnector.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful((): Unit)
      whenF(cache.read[EnvelopeId](any(), any(), any())) thenReturn EnvelopeId("test")
      val request = FakeRequest("GET", "fileUploadProgress/envelopeId/fileId")
      val result = controller.fileUploadProgress("test2", "test")(request)

      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }

    "direct to technical-difficulties" when {
      "the call to get the file metadata fails" in {
        val request = FakeRequest("GET", "fileUploadReady/envelopeId/fileId")
        val cbcId = CBCId("XLCBC0100000056").getOrElse(fail("booo"))
        val enrole: CBCEnrolment = CBCEnrolment(cbcId, Utr("7000000002"))
        whenF(fuService.getFile(any(), any())(any())) thenReturn validFile
        whenF(fuService.getFileMetaData(any(), any())(any(), any())) thenReturn None
        when(
          authConnector.authorise[~[~[Option[Credentials], Option[AffinityGroup]], Option[CBCEnrolment]]](any(), any())(
            any(),
            any())) thenReturn Future.successful(
          new ~[~[Option[Credentials], Option[AffinityGroup]], Option[CBCEnrolment]](
            new ~(Some(creds), Some(AffinityGroup.Organisation)),
            Some(enrole)))
        val result = controller.fileValidate("test", "test")(request)
        header("Location", result).get should endWith("technical-difficulties")
        status(result) shouldBe Status.SEE_OTHER
        verify(fuService).getFile(any(), any())(any())
        verify(fuService).getFileMetaData(any(), any())(any(), any())
      }

      "the call to get the file fails" in {
        val request = FakeRequest("GET", "fileUploadReady/envelopeId/fileId")
        val cbcId = CBCId("XLCBC0100000056").getOrElse(fail("booo"))
        val enrole: CBCEnrolment = CBCEnrolment(cbcId, Utr("7000000002"))
        whenF(fuService.getFile(any(), any())(any())) thenFailWith UnexpectedState("oops")
        whenF(fuService.getFileMetaData(any(), any())(any(), any())) thenReturn Some(md)
        when(
          authConnector.authorise[~[~[Option[Credentials], Option[AffinityGroup]], Option[CBCEnrolment]]](any(), any())(
            any(),
            any())) thenReturn Future.successful(
          new ~[~[Option[Credentials], Option[AffinityGroup]], Option[CBCEnrolment]](
            new ~(Some(creds), Some(AffinityGroup.Organisation)),
            Some(enrole)))
        val result = controller.fileValidate("test", "test")(request)
        header("Location", result).get should endWith("technical-difficulties")
        status(result) shouldBe Status.SEE_OTHER
        verify(fuService).getFile(any(), any())(any())
        verify(fuService).getFileMetaData(any(), any())(any(), any())
      }

      "the call to cache.save fails" in {
        val request = FakeRequest("GET", "fileUploadReady/envelopeId/fileId")
        val cbcId = CBCId("XLCBC0100000056").getOrElse(fail("booo"))
        val enrole: CBCEnrolment = CBCEnrolment(cbcId, Utr("7000000002"))
        whenF(fuService.getFile(any(), any())(any())) thenReturn validFile
        whenF(fuService.getFileMetaData(any(), any())(any(), any())) thenReturn Some(md)
        when(
          authConnector.authorise[~[~[Option[Credentials], Option[AffinityGroup]], Option[CBCEnrolment]]](any(), any())(
            any(),
            any())) thenReturn Future.successful(
          new ~[~[Option[Credentials], Option[AffinityGroup]], Option[CBCEnrolment]](
            new ~(Some(creds), Some(AffinityGroup.Organisation)),
            Some(enrole)))
        when(cache.save(any())(any(), any(), any())) thenReturn Future.failed(new Exception("bad"))
        val result = controller.fileValidate("test", "test")(request)
        header("Location", result).get should endWith("technical-difficulties")
        status(result) shouldBe Status.SEE_OTHER
        verify(fuService).getFile(any(), any())(any())
        verify(fuService).getFileMetaData(any(), any())(any(), any())
        verify(cache, atLeastOnce).save(any())(any(), any(), any())
      }
    }

    "return a 200 when the fileValidate call is successful and all dependant calls return successfully" in {
      val evenMoreValidFile = java.nio.file.Files.copy(testFile.toPath, tempFile.toPath, REPLACE_EXISTING).toFile
      val request = FakeRequest("GET", "fileUploadReady/envelopeId/fileId")
      val cbcId = CBCId("XLCBC0100000056").getOrElse(fail("booo"))
      val enrolment: CBCEnrolment = CBCEnrolment(cbcId, Utr("7000000002"))
      whenF(fuService.getFile(any(), any())(any())) thenReturn evenMoreValidFile
      whenF(fuService.getFileMetaData(any(), any())(any(), any())) thenReturn Some(md)
      when(schemaValidator.validateSchema(any())) thenReturn new XmlErrorHandler()
      when(cache.save(any())(any(), any(), any())) thenReturn Future.successful(new CacheMap("", Map.empty))
      when(cache.readOption(EQ(AffinityGroup.jsonFormat), any(), any())) thenReturn Future.successful(
        Option(AffinityGroup.Organisation))
      when(businessRulesValidator.validateBusinessRules(any(), any(), any(), any())(any())) thenReturn Future
        .successful(Valid(xmlInfo))
      when(businessRulesValidator.recoverReportingEntity(any())) thenReturn Future.successful(
        Valid(completeXmlInfo))
      when(
        authConnector.authorise[~[~[Option[Credentials], Option[AffinityGroup]], Option[CBCEnrolment]]](any(), any())(
          any(),
          any())) thenReturn Future.successful(
        new ~[~[Option[Credentials], Option[AffinityGroup]], Option[CBCEnrolment]](
          new ~(Some(creds), Some(AffinityGroup.Organisation)),
          Some(enrolment)))
      val result = controller.fileValidate("test", "test")(request)
      status(result) shouldBe Status.OK
      verify(fuService).getFile(any(), any())(any())
      verify(fuService).getFileMetaData(any(), any())(any(), any())
      verify(cache, atLeastOnce).save(any())(any(), any(), any())
      verify(businessRulesValidator).validateBusinessRules(any(), any(), any(), any())(any())
      verify(schemaValidator).validateSchema(any())
    }

    "return a 303 when the fileValidate call is successful and schemaValidator returns errors" in {
      val evenMoreValidFile = java.nio.file.Files.copy(testFile.toPath, tempFile.toPath, REPLACE_EXISTING).toFile
      val request = FakeRequest("GET", "fileUploadReady/envelopeId/fileId")
      val cbcId = CBCId("XLCBC0100000056").getOrElse(fail("booo"))
      val enrol: CBCEnrolment = CBCEnrolment(cbcId, Utr("7000000002"))
      val xmlErrorHandler = new XmlErrorHandler()
      val e = new WstxException("error")
      xmlErrorHandler.reportProblem(new XMLValidationProblem(e.getLocation, "", XMLValidationProblem.SEVERITY_FATAL))

      whenF(fuService.getFile(any(), any())(any())) thenReturn evenMoreValidFile
      whenF(fuService.getFileMetaData(any(), any())(any(), any())) thenReturn Some(md)
      when(schemaValidator.validateSchema(any())) thenReturn xmlErrorHandler
      when(cache.save(any())(any(), any(), any())) thenReturn Future.successful(new CacheMap("", Map.empty))
      when(cache.readOption(EQ(AffinityGroup.jsonFormat), any(), any())) thenReturn Future.successful(
        Option(AffinityGroup.Organisation))
      when(businessRulesValidator.validateBusinessRules(any(), any(), any(), any())(any())) thenReturn Future
        .successful(Valid(xmlInfo))
      when(businessRulesValidator.recoverReportingEntity(any())) thenReturn Future.successful(
        Valid(completeXmlInfo))
      when(
        authConnector.authorise[~[~[Option[Credentials], Option[AffinityGroup]], Option[CBCEnrolment]]](any(), any())(
          any(),
          any())) thenReturn Future.successful(
        new ~[~[Option[Credentials], Option[AffinityGroup]], Option[CBCEnrolment]](
          new ~(Some(creds), Some(AffinityGroup.Organisation)),
          Some(enrol)))
      val result = controller.fileValidate("test", "test")(request)
      status(result) shouldBe Status.SEE_OTHER
      verify(fuService).getFile(any(), any())(any())
      verify(fuService).getFileMetaData(any(), any())(any(), any())
      verify(cache, atLeastOnce).save(any())(any(), any(), any())
      verify(schemaValidator).validateSchema(any())
    }

    "return a 303 when the fileValidate call is successful but validateBusinessRules returns errors" in {
      val evenMoreValidFile = java.nio.file.Files.copy(testFile.toPath, tempFile.toPath, REPLACE_EXISTING).toFile
      val request = FakeRequest("GET", "fileUploadReady/envelopeId/fileId")
      val cbcId = CBCId("XLCBC0100000056").getOrElse(fail("booo"))
      val enrole: CBCEnrolment = CBCEnrolment(cbcId, Utr("7000000002"))
      val businessRuleErrors = NonEmptyList.of(TestDataError)
      whenF(fuService.getFile(any(), any())(any())) thenReturn evenMoreValidFile
      whenF(fuService.getFileMetaData(any(), any())(any(), any())) thenReturn Some(md)
      when(schemaValidator.validateSchema(any())) thenReturn new XmlErrorHandler()
      when(cache.save(any())(any(), any(), any())) thenReturn Future.successful(new CacheMap("", Map.empty))
      when(cache.readOption(EQ(AffinityGroup.jsonFormat), any(), any())) thenReturn Future.successful(
        Option(AffinityGroup.Organisation))
      when(businessRulesValidator.validateBusinessRules(any(), any(), any(), any())(any())) thenReturn Future
        .successful(Invalid(businessRuleErrors))
      when(businessRulesValidator.recoverReportingEntity(any())) thenReturn Future.successful(
        Valid(completeXmlInfo))
      when(
        authConnector.authorise[~[~[Option[Credentials], Option[AffinityGroup]], Option[CBCEnrolment]]](any(), any())(
          any(),
          any())) thenReturn Future.successful(
        new ~[~[Option[Credentials], Option[AffinityGroup]], Option[CBCEnrolment]](
          new ~(Some(creds), Some(AffinityGroup.Organisation)),
          Some(enrole)))
      val result = controller.fileValidate("test", "test")(request)
      status(result) shouldBe Status.SEE_OTHER
      verify(fuService).getFile(any(), any())(any())
      verify(fuService).getFileMetaData(any(), any())(any(), any())
      verify(cache, atLeastOnce).save(any())(any(), any(), any())
      verify(businessRulesValidator).validateBusinessRules(any(), any(), any(), any())(any())
      verify(schemaValidator).validateSchema(any())
    }

    "be redirected to an error page" when {
      "the file extension is invalid" in {
        val request = FakeRequest("GET", "fileUploadReady/envelopeId/fileId")
        val cbcId = CBCId("XLCBC0100000056").getOrElse(fail("booo"))
        val enrole: CBCEnrolment = CBCEnrolment(cbcId, Utr("7000000002"))
        whenF(fuService.getFile(any(), any())(any())) thenReturn validFile
        whenF(fuService.getFileMetaData(any(), any())(any(), any())) thenReturn Some(md.copy(name = "bad.zip"))
        whenF(cache.read[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn CBCId.create(1).getOrElse(fail("baaa"))
        when(cache.save(any())(any(), any(), any())) thenReturn Future.successful(CacheMap("cache", Map.empty))
        when(
          authConnector.authorise[~[~[Option[Credentials], Option[AffinityGroup]], Option[CBCEnrolment]]](any(), any())(
            any(),
            any())) thenReturn Future.successful(
          new ~[~[Option[Credentials], Option[AffinityGroup]], Option[CBCEnrolment]](
            new ~(Some(creds), Some(AffinityGroup.Organisation)),
            Some(enrole)))
        val result = controller.fileValidate("test", "test")(request)
        header("Location", result).get should endWith("invalid-file-type")
        status(result) shouldBe Status.SEE_OTHER
      }
    }
  }

  "fileInvalid" should {
    "return 200" in {
      val request = FakeRequest("GET", "invalid-file-type")
      val cbcId = CBCId("XLCBC0100000056").getOrElse(fail("booo"))
      val enrole: CBCEnrolment = CBCEnrolment(cbcId, Utr("7000000002"))
      when(cache.readOption[AllBusinessRuleErrors](EQ(AllBusinessRuleErrors.format), any(), any())) thenReturn Future
        .successful(Some(AllBusinessRuleErrors(List(TestDataError))))
      when(cache.readOption[XMLErrors](EQ(XMLErrors.format), any(), any())) thenReturn Future.successful(
        Some(XMLErrors(List("Big xml error"))))
      when(cache.readOption[FileMetadata](EQ(FileMetadata.fileMetadataFormat), any(), any())) thenReturn Future
        .successful(Some(md))
      when(cache.readOption[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn Future.successful(Some(cbcId))
      when(cache.readOption[Utr](EQ(Utr.utrRead), any(), any())) thenReturn Future.successful(Some(Utr("1234567890")))
      when(
        authConnector.authorise[~[~[Option[Credentials], Option[AffinityGroup]], Option[CBCEnrolment]]](any(), any())(
          any(),
          any())) thenReturn Future.successful(
        new ~[~[Option[Credentials], Option[AffinityGroup]], Option[CBCEnrolment]](
          new ~(Some(creds), Some(AffinityGroup.Organisation)),
          Some(enrole)))
      when(auditC.sendExtendedEvent(any())(any(), any())) thenReturn Future.successful(AuditResult.Success)
      when(fuService.errorsToMap(any())(any())) thenReturn Map("error" -> "error message")
      val result = controller.fileInvalid(request)
      status(result) shouldBe Status.OK
    }

    "return 303" in {
      val request = FakeRequest("GET", "invalid-file-type")
      val cbcId = CBCId("XLCBC0100000056").getOrElse(fail("booo"))
      val enrole: CBCEnrolment = CBCEnrolment(cbcId, Utr("7000000002"))
      val failure = AuditResult.Failure("boo hoo")
      when(cache.readOption[AllBusinessRuleErrors](EQ(AllBusinessRuleErrors.format), any(), any())) thenReturn Future
        .successful(Some(AllBusinessRuleErrors(List(TestDataError))))
      when(cache.readOption[XMLErrors](EQ(XMLErrors.format), any(), any())) thenReturn Future.successful(
        Some(XMLErrors(List("Big xml error"))))
      when(cache.readOption[FileMetadata](EQ(FileMetadata.fileMetadataFormat), any(), any())) thenReturn Future
        .successful(Some(md))
      when(cache.readOption[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn Future.successful(Some(cbcId))
      when(cache.readOption[Utr](EQ(Utr.utrRead), any(), any())) thenReturn Future.successful(Some(Utr("1234567890")))
      when(
        authConnector.authorise[~[~[Option[Credentials], Option[AffinityGroup]], Option[CBCEnrolment]]](any(), any())(
          any(),
          any())) thenReturn Future.successful(
        new ~[~[Option[Credentials], Option[AffinityGroup]], Option[CBCEnrolment]](
          new ~(Some(creds), Some(AffinityGroup.Organisation)),
          Some(enrole)))
      when(auditC.sendExtendedEvent(any())(any(), any())) thenReturn Future.successful(failure)
      when(fuService.errorsToMap(any())(any())) thenReturn Map("error" -> "error message")
      val result = controller.fileInvalid(request)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }
  }

  "fileToLarge" should {
    "return 200" in {
      val request = FakeRequest("GET", "invalid-file-type")
      val cbcId = CBCId("XLCBC0100000056").getOrElse(fail("booo"))
      val enrole: CBCEnrolment = CBCEnrolment(cbcId, Utr("7000000002"))
      when(cache.readOption[AllBusinessRuleErrors](EQ(AllBusinessRuleErrors.format), any(), any())) thenReturn Future
        .successful(Some(AllBusinessRuleErrors(List(TestDataError))))
      when(cache.readOption[XMLErrors](EQ(XMLErrors.format), any(), any())) thenReturn Future.successful(
        Some(XMLErrors(List("Big xml error"))))
      when(cache.readOption[FileMetadata](EQ(FileMetadata.fileMetadataFormat), any(), any())) thenReturn Future
        .successful(Some(md))
      when(cache.readOption[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn Future.successful(Some(cbcId))
      when(cache.readOption[Utr](EQ(Utr.utrRead), any(), any())) thenReturn Future.successful(Some(Utr("1234567890")))
      when(
        authConnector.authorise[~[~[Option[Credentials], Option[AffinityGroup]], Option[CBCEnrolment]]](any(), any())(
          any(),
          any())) thenReturn Future.successful(
        new ~[~[Option[Credentials], Option[AffinityGroup]], Option[CBCEnrolment]](
          new ~(Some(creds), Some(AffinityGroup.Organisation)),
          Some(enrole)))
      when(auditC.sendExtendedEvent(any())(any(), any())) thenReturn Future.successful(AuditResult.Success)
      when(fuService.errorsToMap(any())(any())) thenReturn Map("error" -> "error message")
      val result = controller.fileTooLarge(request)
      status(result) shouldBe Status.OK
    }
  }

  "fileContainsVirus" should {
    "return 200" in {
      val request = FakeRequest("GET", "invalid-file-type")
      val cbcId = CBCId("XLCBC0100000056").getOrElse(fail("booo"))
      val enrole: CBCEnrolment = CBCEnrolment(cbcId, Utr("7000000002"))
      when(cache.readOption[AllBusinessRuleErrors](EQ(AllBusinessRuleErrors.format), any(), any())) thenReturn Future
        .successful(Some(AllBusinessRuleErrors(List(TestDataError))))
      when(cache.readOption[XMLErrors](EQ(XMLErrors.format), any(), any())) thenReturn Future.successful(
        Some(XMLErrors(List("Big xml error"))))
      when(cache.readOption[FileMetadata](EQ(FileMetadata.fileMetadataFormat), any(), any())) thenReturn Future
        .successful(Some(md))
      when(cache.readOption[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn Future.successful(Some(cbcId))
      when(cache.readOption[Utr](EQ(Utr.utrRead), any(), any())) thenReturn Future.successful(Some(Utr("1234567890")))
      when(
        authConnector.authorise[~[~[Option[Credentials], Option[AffinityGroup]], Option[CBCEnrolment]]](any(), any())(
          any(),
          any())) thenReturn Future.successful(
        new ~[~[Option[Credentials], Option[AffinityGroup]], Option[CBCEnrolment]](
          new ~(Some(creds), Some(AffinityGroup.Organisation)),
          Some(enrole)))
      when(auditC.sendExtendedEvent(any())(any(), any())) thenReturn Future.successful(AuditResult.Success)
      when(fuService.errorsToMap(any())(any())) thenReturn Map("error" -> "error message")
      val result = controller.fileContainsVirus(request)
      status(result) shouldBe Status.OK
    }
  }

  "The file-upload error call back" should {
    "cause a redirect to file-too-large if the response has status-code 413" in {
      val request = FakeRequest()
      when(authConnector.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful((): Unit)
      val result = controller.handleError(413, "no reason")(request)
      header("Location", result).get should endWith("file-too-large")
      status(result) shouldBe Status.SEE_OTHER
    }

    "cause a redirect to invalid-file-type if the response has status-code 415" in {
      val request = FakeRequest()
      when(authConnector.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful((): Unit)
      val result = controller.handleError(415, "no reason")(request)
      header("Location", result).get should endWith("invalid-file-type")
      status(result) shouldBe Status.SEE_OTHER
    }
  }

  "getBusinessRuleErrors" should {
    "return 200 if error details found in cache" in {
      val request = FakeRequest()
      when(authConnector.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful((): Unit)
      when(cache.readOption[AllBusinessRuleErrors](EQ(AllBusinessRuleErrors.format), any(), any())) thenReturn Future
        .successful(Some(AllBusinessRuleErrors(List(TestDataError))))
      when(file.delete) thenReturn true
      when(fuService.errorsToFile(any(), any())(any())) thenReturn validFile
      val result = controller.getBusinessRuleErrors(request)
      status(result) shouldBe Status.OK
    }

    "return 203 if no error content found in cache" in {
      val request = FakeRequest()
      when(authConnector.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful((): Unit)
      when(cache.readOption[AllBusinessRuleErrors](EQ(AllBusinessRuleErrors.format), any(), any())) thenReturn Future
        .successful(None)
      when(file.delete) thenReturn true
      val result = controller.getBusinessRuleErrors(request)
      status(result) shouldBe Status.NO_CONTENT
    }
  }

  "getXmlSchemaErrors" should {
    "return 200 if error details found in cache" in {
      val request = FakeRequest()
      when(authConnector.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful((): Unit)
      when(cache.readOption[XMLErrors](EQ(XMLErrors.format), any(), any())) thenReturn Future.successful(
        Some(XMLErrors(List("Big xml error"))))
      when(file.delete) thenReturn true
      when(fuService.errorsToFile(any(), any())(any())) thenReturn validFile
      val result = controller.getXmlSchemaErrors(request)
      status(result) shouldBe Status.OK
    }

    "return 203 if no error content found in cache" in {
      val request = FakeRequest()
      when(authConnector.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful((): Unit)
      when(cache.readOption[XMLErrors](EQ(XMLErrors.format), any(), any())) thenReturn Future.successful(None)
      when(file.delete) thenReturn true
      val result = controller.getXmlSchemaErrors(request)
      status(result) shouldBe Status.NO_CONTENT
    }
  }

  "a call to auditFailedSubmission" should {
    "return success if audit enabled and sendExtendedEvent succeeds" in {
      val cbcId = CBCId("XLCBC0100000056").getOrElse(fail("booo"))
      val enrole: CBCEnrolment = CBCEnrolment(cbcId, Utr("7000000002"))
      val request = FakeRequest()
      when(auditC.sendExtendedEvent(any())(any(), any())) thenReturn Future.successful(AuditResult.Success)
      when(cache.readOption[AllBusinessRuleErrors](EQ(AllBusinessRuleErrors.format), any(), any())) thenReturn Future
        .successful(Some(AllBusinessRuleErrors(List(TestDataError))))
      when(cache.readOption[XMLErrors](EQ(XMLErrors.format), any(), any())) thenReturn Future.successful(
        Some(XMLErrors(List("Big xml error"))))
      when(cache.readOption[FileMetadata](EQ(FileMetadata.fileMetadataFormat), any(), any())) thenReturn Future
        .successful(Some(md))
      when(cache.readOption[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn Future.successful(Some(cbcId))
      when(cache.readOption[Utr](EQ(Utr.utrRead), any(), any())) thenReturn Future.successful(Some(Utr("1234567890")))
      val result = Await.result(controller.auditFailedSubmission(creds, Some(AffinityGroup.Organisation), Some(enrole), "just because")(hc, request), 5 seconds)
      result.map(r => r shouldBe AuditResult.Success)    }

    "return success if audit disabled and sendExtendedEvent succeeds" in {
      val cbcId = CBCId("XLCBC0100000056").getOrElse(fail("booo"))
      val request = FakeRequest()
      when(auditC.sendExtendedEvent(any())(any(), any())) thenReturn Future.successful(AuditResult.Disabled)
      when(cache.readOption[AllBusinessRuleErrors](EQ(AllBusinessRuleErrors.format), any(), any())) thenReturn Future
        .successful(Some(AllBusinessRuleErrors(List(TestDataError))))
      when(cache.readOption[XMLErrors](EQ(XMLErrors.format), any(), any())) thenReturn Future.successful(
        Some(XMLErrors(List("Big xml error"))))
      when(cache.readOption[FileMetadata](EQ(FileMetadata.fileMetadataFormat), any(), any())) thenReturn Future
        .successful(Some(md))
      when(cache.readOption[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn Future.successful(Some(cbcId))
      when(cache.readOption[Utr](EQ(Utr.utrRead), any(), any())) thenReturn Future.successful(Some(Utr("1234567890")))
      val result = Await.result(
        controller.auditFailedSubmission(creds, Some(AffinityGroup.Organisation), None, "just because")(hc, request),
        2.second)
      result.map(r => r shouldBe AuditResult.Success)
    }

    "return error if sendExtendedEvent fails" in {
      val cbcId = CBCId("XLCBC0100000056").getOrElse(fail("booo"))
      val enrole: CBCEnrolment = CBCEnrolment(cbcId, Utr("7000000002"))
      val request = FakeRequest()
      val failure = AuditResult.Failure("boo hoo")
      when(auditC.sendExtendedEvent(any())(any(), any())) thenReturn Future.successful(failure)
      when(cache.readOption[AllBusinessRuleErrors](EQ(AllBusinessRuleErrors.format), any(), any())) thenReturn Future
        .successful(Some(AllBusinessRuleErrors(List(TestDataError))))
      when(cache.readOption[XMLErrors](EQ(XMLErrors.format), any(), any())) thenReturn Future.successful(
        Some(XMLErrors(List("Big xml error"))))
      when(cache.readOption[FileMetadata](EQ(FileMetadata.fileMetadataFormat), any(), any())) thenReturn Future
        .successful(Some(md))
      when(cache.readOption[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn Future.successful(Some(cbcId))
      when(cache.readOption[Utr](EQ(Utr.utrRead), any(), any())) thenReturn Future.successful(Some(Utr("1234567890")))
      val result = Await.result(
        controller
          .auditFailedSubmission(creds, Some(AffinityGroup.Organisation), Some(enrole), "just because")(hc, request),
        2.second)
      result.fold(
        error => error.toString should contain("boo hoo"),
        _ => fail("No error generated")
      )
    }
  }

}
