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

import akka.actor.ActorSystem
import cats.data.Validated.{Invalid, Valid}
import cats.data.{EitherT, NonEmptyList, OptionT}
import cats.implicits.catsStdInstancesForFuture
import com.ctc.wstx.exc.WstxException
import com.typesafe.config.ConfigFactory
import org.codehaus.stax2.validation.{XMLValidationProblem, XMLValidationSchema, XMLValidationSchemaFactory}
import org.mockito.ArgumentMatchers.{any, eq => EQ}
import org.mockito.MockitoSugar
import org.mockito.cats.MockitoCats
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.ScalaFutures
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.i18n.MessagesApi
import play.api.libs.json.{Format, JsNull, Reads}
import play.api.mvc.MessagesControllerComponents
import play.api.test.FakeRequest
import play.api.{Configuration, Environment}
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.retrieve.{Credentials, Retrieval, ~}
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.services._
import uk.gov.hmrc.cbcrfrontend.typesclasses.{CbcrsUrl, FusFeUrl, FusUrl, ServiceUrl}
import uk.gov.hmrc.cbcrfrontend.util.UnitSpec
import uk.gov.hmrc.cbcrfrontend.views.Views
import uk.gov.hmrc.http.cache.client.CacheMap
import uk.gov.hmrc.http.{HeaderCarrier, HttpClient}
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}

import java.io.File
import java.nio.file.StandardCopyOption._
import java.time.{LocalDate, LocalDateTime}
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.reflect.runtime.universe

class FileUploadControllerSpec
    extends UnitSpec with ScalaFutures with GuiceOneAppPerSuite with CSRFTest with BeforeAndAfterEach with MockitoSugar
    with MockitoCats {

  implicit val ec: ExecutionContext = app.injector.instanceOf[ExecutionContext]
  implicit val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
  implicit val env = app.injector.instanceOf[Environment]
  implicit val as = app.injector.instanceOf[ActorSystem]

  val fuService: FileUploadService = mock[FileUploadService]
  val schemaValidator: CBCRXMLValidator = mock[CBCRXMLValidator]
  val businessRulesValidator: CBCBusinessRuleValidator = mock[CBCBusinessRuleValidator]
  val cache: CBCSessionCache = mock[CBCSessionCache]
  val extractor: XmlInfoExtract = new XmlInfoExtract()
  val auditC: AuditConnector = mock[AuditConnector]
  var runMode = mock[RunMode]
  val authConnector = mock[AuthConnector]
  val file = mock[File]
  val mcc = app.injector.instanceOf[MessagesControllerComponents]
  val views: Views = app.injector.instanceOf[Views]

  implicit val configuration = new Configuration(ConfigFactory.load("application.conf"))
  implicit val feConfig = mock[FrontendAppConfig]

  val creds: Credentials = Credentials("totally", "legit")

  override protected def afterEach(): Unit = {
    reset(cache, businessRulesValidator, schemaValidator, fuService)
    super.afterEach()
  }

  object TestSessionCache {

    var succeed = true
    var agent = false
    var individual = false
    val http = mock[HttpClient]

    def apply(): CBCSessionCache = new SessionCache(configuration, http)

    private class SessionCache(_config: Configuration, _http: HttpClient) extends CBCSessionCache(_config, _http) {

      override def read[T: Reads: universe.TypeTag](implicit hc: HeaderCarrier): EitherT[Future, ExpiredSession, T] =
        universe.typeOf[T] match {
          case t if t =:= universe.typeOf[EnvelopeId] =>
            EitherT.right(EnvelopeId("test").asInstanceOf[T])
          case t if t =:= universe.typeOf[CBCId] => EitherT.left(ExpiredSession("meh"))
        }

      override def readOption[T: Reads: universe.TypeTag](implicit hc: HeaderCarrier): Future[Option[T]] =
        universe.typeOf[T] match {
          case t if t =:= universe.typeOf[CBCId] => Future.successful(None)
        }

      override def readOrCreate[T: Format: universe.TypeTag](f: => OptionT[Future, T])(
        implicit hc: HeaderCarrier): OptionT[Future, T] = universe.typeOf[T] match {
        case t if t =:= universe.typeOf[FileId] =>
          OptionT.some(FileId("fileId")).asInstanceOf[OptionT[Future, T]]
        case t if t =:= universe.typeOf[EnvelopeId] =>
          succeed match {
            case true  => OptionT.some(EnvelopeId("test")).asInstanceOf[OptionT[Future, T]]
            case false => OptionT.none
          }
      }

      override def create[T: Format: universe.TypeTag](f: => OptionT[Future, T])(
        implicit hc: HeaderCarrier): OptionT[Future, T] = universe.typeOf[T] match {
        case t if t =:= universe.typeOf[FileId] =>
          OptionT.some(FileId("fileId")).asInstanceOf[OptionT[Future, T]]
        case t if t =:= universe.typeOf[EnvelopeId] =>
          succeed match {
            case true  => OptionT.some(EnvelopeId("test")).asInstanceOf[OptionT[Future, T]]
            case false => OptionT.none
          }
      }
    }
  }

  implicit val hc = HeaderCarrier()
  implicit val fusUrl = new ServiceUrl[FusUrl] { val url = "file-upload" }
  implicit val fusFeUrl = new ServiceUrl[FusFeUrl] { val url = "file-upload-frontend" }
  implicit val cbcrsUrl = new ServiceUrl[CbcrsUrl] { val url = "cbcr" }

  val docRefId = "GB2016RGXVCBC0000000056CBC40120170311T090000X_7000000002OECD1"
  val xmlinfo = XMLInfo(
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

  val completeXmlInfo = CompleteXMLInfo(
    xmlinfo,
    ReportingEntity(
      CBC701,
      DocSpec(OECD1, DocRefId(docRefId + "REP").get, None, None),
      TIN("7000000002", "gb"),
      "name",
      None,
      EntityReportingPeriod(LocalDate.parse("2016-03-31"), LocalDate.parse("2017-03-30"))
    )
  )

  val md = FileMetadata("", "", "something.xml", "", 1.0, "", JsNull, "")

  val xmlValidationSchemaFactory: XMLValidationSchemaFactory =
    XMLValidationSchemaFactory.newInstance(XMLValidationSchema.SCHEMA_ID_W3C_SCHEMA)
  when(runMode.env) thenReturn "Dev"
  val schemaVer: String = configuration
    .getOptional[String](s"${runMode.env}.oecd-schema-version")
    .getOrElse(throw new Exception(s"Missing configuration ${runMode.env}.oecd-schema-version"))
  val schemaFile: File = new File(s"conf/schema/$schemaVer/CbcXML_v$schemaVer.xsd")

  val partiallyMockedController = new FileUploadController(
    messagesApi,
    authConnector,
    schemaValidator,
    businessRulesValidator,
    fuService,
    extractor,
    auditC,
    env,
    mcc,
    views)(ec, TestSessionCache(), configuration, feConfig)
  val controller = new FileUploadController(
    messagesApi,
    authConnector,
    schemaValidator,
    businessRulesValidator,
    fuService,
    extractor,
    auditC,
    env,
    mcc,
    views)(ec, cache, configuration, feConfig)

  val testFile: File = new File("test/resources/cbcr-valid.xml")
  val tempFile: File = File.createTempFile("test", ".xml")
  val validFile = java.nio.file.Files.copy(testFile.toPath, tempFile.toPath, REPLACE_EXISTING).toFile
  val newEnrolments = Set(
    Enrolment(
      "HMRC-CBC-ORG",
      Seq(
        EnrolmentIdentifier("cbcId", (CBCId.create(99).getOrElse(fail("booo"))).toString),
        EnrolmentIdentifier("UTR", Utr("1234567890").utr)),
      state = "",
      delegatedAuthRule = None
    ))
  val newCBCEnrolment = CBCEnrolment(CBCId.create(99).getOrElse(fail("booo")), Utr("1234567890"))

  "GET /upload-report" should {
    val fakeRequestChooseXMLFile = addToken(FakeRequest("GET", "/upload-report"))

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

    "displays gateway account not registered page if Organisation user has is not enrolled" in {
      when(authConnector.authorise(any(), any[Retrieval[Option[AffinityGroup] ~ Option[CBCEnrolment]]]())(any(), any()))
        .thenReturn(
          Future.successful(new ~[Option[AffinityGroup], Option[CBCEnrolment]](Some(AffinityGroup.Organisation), None)))
      val result = partiallyMockedController.chooseXMLFile(fakeRequestChooseXMLFile)
      status(result) shouldBe Status.OK
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
      val result = partiallyMockedController.chooseXMLFile(fakeRequestChooseXMLFile)
      status(result) shouldBe Status.SEE_OTHER

    }
    TestSessionCache.succeed = false
    "return 500 when there is an error creating the envelope" in {
      when(authConnector.authorise(any(), any[Retrieval[Option[AffinityGroup] ~ Option[CBCEnrolment]]]())(any(), any()))
        .thenReturn(Future.successful(
          new ~[Option[AffinityGroup], Option[CBCEnrolment]](Some(AffinityGroup.Organisation), Some(newCBCEnrolment))))
      whenF(fuService.createEnvelope(any(), any())) thenFailWith UnexpectedState("server error")
      val result = partiallyMockedController.chooseXMLFile(fakeRequestChooseXMLFile)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
      TestSessionCache.succeed = true
    }
  }

  "GET /unregistered-gg-account" should {
    val fakeRequestUnregisteredGGId = addToken(FakeRequest("GET", "/unregistered-gg-account"))

    "return 200 when the envelope is created successfully" in {
      when(authConnector.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful((): Unit)
      whenF(cache.readOrCreate[EnvelopeId](any())(any(), any(), any())) thenReturn EnvelopeId("12345678")
      val result = partiallyMockedController.unregisteredGGAccount(fakeRequestUnregisteredGGId)
      status(result) shouldBe Status.OK
    }

    "return 500 when the is an error creating the envelope\"" in {
      TestSessionCache.succeed = false
      when(authConnector.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful((): Unit)
      whenF(fuService.createEnvelope(any(), any())) thenFailWith UnexpectedState("server error")
      val result = partiallyMockedController.unregisteredGGAccount(fakeRequestUnregisteredGGId)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
      TestSessionCache.succeed = true
    }
  }

  "GET /fileUploadResponse/envelopeId/fileId" should {
    val fakeRequestGetFileUploadResponse = addToken(FakeRequest("GET", "/fileUploadResponse/envelopeId/fileId"))
    "return 202 when the file is available" in {
      when(authConnector.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful((): Unit)
      whenF(fuService.getFileUploadResponse(any())(any(), any())) thenReturn
        Some(FileUploadCallbackResponse("envelopeId", "fileId", "AVAILABLE", None))
      val result = partiallyMockedController.fileUploadResponse("envelopeId")(fakeRequestGetFileUploadResponse)
      status(result) shouldBe Status.ACCEPTED
    }

    "return 204" when {
      "the FUS hasn't updated the backend yet" in {
        when(authConnector.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful((): Unit)
        whenF(fuService.getFileUploadResponse(any())(any(), any())) thenReturn None
        val result =
          partiallyMockedController.fileUploadResponse("envelopeId")(fakeRequestGetFileUploadResponse).futureValue
        status(result) shouldBe Status.NO_CONTENT
      }

      "file is not yet available" in {
        when(authConnector.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful((): Unit)
        whenF(fuService.getFileUploadResponse(any())(any(), any())) thenReturn
          Some(FileUploadCallbackResponse("envelopeId", "fileId", "QUARENTEENED", None))
        val result =
          partiallyMockedController.fileUploadResponse("envelopeId")(fakeRequestGetFileUploadResponse).futureValue
        status(result) shouldBe Status.NO_CONTENT
      }
    }

    "return a 200" in {
      when(authConnector.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful((): Unit)
      val request = addToken(FakeRequest("GET", "fileUploadProgress/envelopeId/fileId"))
      val result = partiallyMockedController.fileUploadProgress("test", "test")(request)
      status(result) shouldBe Status.OK
    }

    "return a 500 if the envelopeId doesn't match with the cache" in {
      when(authConnector.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful((): Unit)
      val request = addToken(FakeRequest("GET", "fileUploadProgress/envelopeId/fileId"))
      val result = partiallyMockedController.fileUploadProgress("test2", "test")(request)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }

    "direct to technical-difficulties" when {
      "the call to get the file metadata fails" in {
        val request = addToken(FakeRequest("GET", "fileUploadReady/envelopeId/fileId"))
        val cbcId = CBCId("XLCBC0100000056").getOrElse(fail("booo"))
        val enrole: CBCEnrolment = CBCEnrolment(cbcId, Utr("7000000002"))
        whenF(fuService.getFile(any(), any())(any(), any())) thenReturn validFile
        whenF(fuService.getFileMetaData(any(), any())(any(), any())) thenReturn None
        when(
          authConnector.authorise[~[~[Option[Credentials], Option[AffinityGroup]], Option[CBCEnrolment]]](any(), any())(
            any(),
            any())) thenReturn Future.successful(
          new ~[~[Option[Credentials], Option[AffinityGroup]], Option[CBCEnrolment]](
            new ~(Some(creds), Some(AffinityGroup.Organisation)),
            Some(enrole)))
        val result = Await.result(controller.fileValidate("test", "test")(request), 2.second)
        result.header.headers("Location") should endWith("technical-difficulties")
        status(result) shouldBe Status.SEE_OTHER
        verify(fuService).getFile(any(), any())(any(), any())
        verify(fuService).getFileMetaData(any(), any())(any(), any())
      }

      "the call to get the file fails" in {
        val request = addToken(FakeRequest("GET", "fileUploadReady/envelopeId/fileId"))
        val cbcId = CBCId("XLCBC0100000056").getOrElse(fail("booo"))
        val enrole: CBCEnrolment = CBCEnrolment(cbcId, Utr("7000000002"))
        whenF(fuService.getFile(any(), any())(any(), any())) thenFailWith UnexpectedState("oops")
        whenF(fuService.getFileMetaData(any(), any())(any(), any())) thenReturn Some(md)
        when(
          authConnector.authorise[~[~[Option[Credentials], Option[AffinityGroup]], Option[CBCEnrolment]]](any(), any())(
            any(),
            any())) thenReturn Future.successful(
          new ~[~[Option[Credentials], Option[AffinityGroup]], Option[CBCEnrolment]](
            new ~(Some(creds), Some(AffinityGroup.Organisation)),
            Some(enrole)))
        val result = Await.result(controller.fileValidate("test", "test")(request), 2.second)
        result.header.headers("Location") should endWith("technical-difficulties")
        status(result) shouldBe Status.SEE_OTHER
        verify(fuService).getFile(any(), any())(any(), any())
        verify(fuService).getFileMetaData(any(), any())(any(), any())
      }

      "the call to cache.save fails" in {
        val request = addToken(FakeRequest("GET", "fileUploadReady/envelopeId/fileId"))
        val cbcId = CBCId("XLCBC0100000056").getOrElse(fail("booo"))
        val enrole: CBCEnrolment = CBCEnrolment(cbcId, Utr("7000000002"))
        whenF(fuService.getFile(any(), any())(any(), any())) thenReturn validFile
        whenF(fuService.getFileMetaData(any(), any())(any(), any())) thenReturn Some(md)
        when(
          authConnector.authorise[~[~[Option[Credentials], Option[AffinityGroup]], Option[CBCEnrolment]]](any(), any())(
            any(),
            any())) thenReturn Future.successful(
          new ~[~[Option[Credentials], Option[AffinityGroup]], Option[CBCEnrolment]](
            new ~(Some(creds), Some(AffinityGroup.Organisation)),
            Some(enrole)))
        when(cache.save(any())(any(), any(), any())) thenReturn Future.failed(new Exception("bad"))
        val result = Await.result(controller.fileValidate("test", "test")(request), 2.second)
        result.header.headers("Location") should endWith("technical-difficulties")
        status(result) shouldBe Status.SEE_OTHER
        verify(fuService).getFile(any(), any())(any(), any())
        verify(fuService).getFileMetaData(any(), any())(any(), any())
        verify(cache, atLeastOnce).save(any())(any(), any(), any())
      }
    }

    "return a 200 when the fileValidate call is successful and all dependant calls return successfully" in {
      val evenMoreValidFile = java.nio.file.Files.copy(testFile.toPath, tempFile.toPath, REPLACE_EXISTING).toFile
      val request = addToken(FakeRequest("GET", "fileUploadReady/envelopeId/fileId"))
      val cbcId = CBCId("XLCBC0100000056").getOrElse(fail("booo"))
      val enrole: CBCEnrolment = CBCEnrolment(cbcId, Utr("7000000002"))
      whenF(fuService.getFile(any(), any())(any(), any())) thenReturn evenMoreValidFile
      whenF(fuService.getFileMetaData(any(), any())(any(), any())) thenReturn Some(md)
      when(schemaValidator.validateSchema(any())) thenReturn new XmlErrorHandler()
      when(cache.save(any())(any(), any(), any())) thenReturn Future.successful(new CacheMap("", Map.empty))
      when(cache.readOption(EQ(AffinityGroup.jsonFormat), any(), any())) thenReturn Future.successful(
        Option(AffinityGroup.Organisation))
      when(businessRulesValidator.validateBusinessRules(any(), any(), any(), any())(any())) thenReturn Future
        .successful(Valid(xmlinfo))
      when(businessRulesValidator.recoverReportingEntity(any())(any())) thenReturn Future.successful(
        Valid(completeXmlInfo))
      when(
        authConnector.authorise[~[~[Option[Credentials], Option[AffinityGroup]], Option[CBCEnrolment]]](any(), any())(
          any(),
          any())) thenReturn Future.successful(
        new ~[~[Option[Credentials], Option[AffinityGroup]], Option[CBCEnrolment]](
          new ~(Some(creds), Some(AffinityGroup.Organisation)),
          Some(enrole)))
      val result = Await.result(controller.fileValidate("test", "test")(request), 2.second)
      val returnVal = status(result)
      returnVal shouldBe Status.OK
      verify(fuService).getFile(any(), any())(any(), any())
      verify(fuService).getFileMetaData(any(), any())(any(), any())
      verify(cache, atLeastOnce).save(any())(any(), any(), any())
      verify(businessRulesValidator).validateBusinessRules(any(), any(), any(), any())(any())
      verify(schemaValidator).validateSchema(any())
    }

    "return a 303 when the fileValidate call is successful and schemaValidator returns errors" in {
      val evenMoreValidFile = java.nio.file.Files.copy(testFile.toPath, tempFile.toPath, REPLACE_EXISTING).toFile
      val request = addToken(FakeRequest("GET", "fileUploadReady/envelopeId/fileId"))
      val cbcId = CBCId("XLCBC0100000056").getOrElse(fail("booo"))
      val enrole: CBCEnrolment = CBCEnrolment(cbcId, Utr("7000000002"))
      val xmlErrorHandler = new XmlErrorHandler()
      val e = new WstxException("error")
      xmlErrorHandler.reportProblem(new XMLValidationProblem(e.getLocation, "", XMLValidationProblem.SEVERITY_FATAL))

      whenF(fuService.getFile(any(), any())(any(), any())) thenReturn evenMoreValidFile
      whenF(fuService.getFileMetaData(any(), any())(any(), any())) thenReturn Some(md)
      when(schemaValidator.validateSchema(any())) thenReturn xmlErrorHandler
      when(cache.save(any())(any(), any(), any())) thenReturn Future.successful(new CacheMap("", Map.empty))
      when(cache.readOption(EQ(AffinityGroup.jsonFormat), any(), any())) thenReturn Future.successful(
        Option(AffinityGroup.Organisation))
      when(businessRulesValidator.validateBusinessRules(any(), any(), any(), any())(any())) thenReturn Future
        .successful(Valid(xmlinfo))
      when(businessRulesValidator.recoverReportingEntity(any())(any())) thenReturn Future.successful(
        Valid(completeXmlInfo))
      when(
        authConnector.authorise[~[~[Option[Credentials], Option[AffinityGroup]], Option[CBCEnrolment]]](any(), any())(
          any(),
          any())) thenReturn Future.successful(
        new ~[~[Option[Credentials], Option[AffinityGroup]], Option[CBCEnrolment]](
          new ~(Some(creds), Some(AffinityGroup.Organisation)),
          Some(enrole)))
      val result = Await.result(controller.fileValidate("test", "test")(request), 2.second)
      val returnVal = status(result)
      returnVal shouldBe Status.SEE_OTHER
      verify(fuService).getFile(any(), any())(any(), any())
      verify(fuService).getFileMetaData(any(), any())(any(), any())
      verify(cache, atLeastOnce).save(any())(any(), any(), any())
      verify(schemaValidator).validateSchema(any())
    }

    "return a 303 when the fileValidate call is successful but validateBusinessRules returns errors" in {
      val evenMoreValidFile = java.nio.file.Files.copy(testFile.toPath, tempFile.toPath, REPLACE_EXISTING).toFile
      val request = addToken(FakeRequest("GET", "fileUploadReady/envelopeId/fileId"))
      val cbcId = CBCId("XLCBC0100000056").getOrElse(fail("booo"))
      val enrole: CBCEnrolment = CBCEnrolment(cbcId, Utr("7000000002"))
      val businessRuleErrors = NonEmptyList.of(TestDataError)
      whenF(fuService.getFile(any(), any())(any(), any())) thenReturn evenMoreValidFile
      whenF(fuService.getFileMetaData(any(), any())(any(), any())) thenReturn Some(md)
      when(schemaValidator.validateSchema(any())) thenReturn new XmlErrorHandler()
      when(cache.save(any())(any(), any(), any())) thenReturn Future.successful(new CacheMap("", Map.empty))
      when(cache.readOption(EQ(AffinityGroup.jsonFormat), any(), any())) thenReturn Future.successful(
        Option(AffinityGroup.Organisation))
      when(businessRulesValidator.validateBusinessRules(any(), any(), any(), any())(any())) thenReturn Future
        .successful(Invalid(businessRuleErrors))
      when(businessRulesValidator.recoverReportingEntity(any())(any())) thenReturn Future.successful(
        Valid(completeXmlInfo))
      when(
        authConnector.authorise[~[~[Option[Credentials], Option[AffinityGroup]], Option[CBCEnrolment]]](any(), any())(
          any(),
          any())) thenReturn Future.successful(
        new ~[~[Option[Credentials], Option[AffinityGroup]], Option[CBCEnrolment]](
          new ~(Some(creds), Some(AffinityGroup.Organisation)),
          Some(enrole)))
      val result = Await.result(controller.fileValidate("test", "test")(request), 2.second)
      val returnVal = status(result)
      returnVal shouldBe Status.SEE_OTHER
      verify(fuService).getFile(any(), any())(any(), any())
      verify(fuService).getFileMetaData(any(), any())(any(), any())
      verify(cache, atLeastOnce).save(any())(any(), any(), any())
      verify(businessRulesValidator).validateBusinessRules(any(), any(), any(), any())(any())
      verify(schemaValidator).validateSchema(any())
    }

    "be redirected to an error page" when {
      "the file extension is invalid" in {
        val request = addToken(FakeRequest("GET", "fileUploadReady/envelopeId/fileId"))
        val cbcId = CBCId("XLCBC0100000056").getOrElse(fail("booo"))
        val enrole: CBCEnrolment = CBCEnrolment(cbcId, Utr("7000000002"))
        whenF(fuService.getFile(any(), any())(any(), any())) thenReturn validFile
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
        val result = Await.result(controller.fileValidate("test", "test")(request), 5.second)
        result.header.headers("Location") should endWith("invalid-file-type")
        status(result) shouldBe Status.SEE_OTHER
      }
    }
  }

  "fileInvalid" should {
    "return 200" in {
      val request = addToken(FakeRequest("GET", "invalid-file-type"))
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
      val request = addToken(FakeRequest("GET", "invalid-file-type"))
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
      val request = addToken(FakeRequest("GET", "invalid-file-type"))
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
      val request = addToken(FakeRequest("GET", "invalid-file-type"))
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
      val request = addToken(FakeRequest())
      when(authConnector.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful((): Unit)
      val result = Await.result(partiallyMockedController.handleError(413, "no reason")(request), 5.second)
      result.header.headers("Location") should endWith("file-too-large")
      status(result) shouldBe Status.SEE_OTHER
    }
    "cause a redirect to invalid-file-type if the response has status-code 415" in {
      val request = addToken(FakeRequest())
      when(authConnector.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful((): Unit)
      val result = Await.result(partiallyMockedController.handleError(415, "no reason")(request), 5.second)
      result.header.headers("Location") should endWith("invalid-file-type")
      status(result) shouldBe Status.SEE_OTHER
    }
  }

  "getBusinessRuleErrors" should {
    "return 200 if error details found in cache" in {
      val request = addToken(FakeRequest())
      when(authConnector.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful((): Unit)
      when(cache.readOption[AllBusinessRuleErrors](EQ(AllBusinessRuleErrors.format), any(), any())) thenReturn Future
        .successful(Some(AllBusinessRuleErrors(List(TestDataError))))
      when(file.delete) thenReturn Future.successful(true)
      when(fuService.errorsToFile(any(), any())(any())) thenReturn validFile
      val result = controller.getBusinessRuleErrors(request)
      status(result) shouldBe Status.OK
    }

    "return 203 if no error content found in cache" in {
      val request = addToken(FakeRequest())
      when(authConnector.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful((): Unit)
      when(cache.readOption[AllBusinessRuleErrors](EQ(AllBusinessRuleErrors.format), any(), any())) thenReturn Future
        .successful(None)
      when(file.delete) thenReturn Future.successful(true)
      val result = controller.getBusinessRuleErrors(request)
      status(result) shouldBe Status.NO_CONTENT
    }
  }

  "getXmlSchemaErrors" should {
    "return 200 if error details found in cache" in {
      val request = addToken(FakeRequest())
      when(authConnector.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful((): Unit)
      when(cache.readOption[XMLErrors](EQ(XMLErrors.format), any(), any())) thenReturn Future.successful(
        Some(XMLErrors(List("Big xml error"))))
      when(file.delete) thenReturn Future.successful(true)
      when(fuService.errorsToFile(any(), any())(any())) thenReturn validFile
      val result = controller.getXmlSchemaErrors(request)
      status(result) shouldBe Status.OK
    }

    "return 203 if no error content found in cache" in {
      val request = addToken(FakeRequest())
      when(authConnector.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful((): Unit)
      when(cache.readOption[XMLErrors](EQ(XMLErrors.format), any(), any())) thenReturn Future.successful(None)
      when(file.delete) thenReturn Future.successful(true)
      val result = controller.getXmlSchemaErrors(request)
      status(result) shouldBe Status.NO_CONTENT
    }
  }

  "a call to auditFailedSubmission" should {
    "return success if audit enabled and sendExtendedEvent succeeds" in {
      val cbcId = CBCId("XLCBC0100000056").getOrElse(fail("booo"))
      val enrole: CBCEnrolment = CBCEnrolment(cbcId, Utr("7000000002"))
      val request = addToken(FakeRequest())
      when(auditC.sendExtendedEvent(any())(any(), any())) thenReturn Future.successful(AuditResult.Success)
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
      result.map(r => r shouldBe AuditResult.Success)
    }

    "return success if audit disabled and sendExtendedEvent succeeds" in {
      val cbcId = CBCId("XLCBC0100000056").getOrElse(fail("booo"))
      val request = addToken(FakeRequest())
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

    "return error if sendExtendedEvent failes" in {
      val cbcId = CBCId("XLCBC0100000056").getOrElse(fail("booo"))
      val enrole: CBCEnrolment = CBCEnrolment(cbcId, Utr("7000000002"))
      val request = addToken(FakeRequest())
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
