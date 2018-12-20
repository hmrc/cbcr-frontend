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

package uk.gov.hmrc.cbcrfrontend.controllers

import java.io.File
import java.nio.file.Files
import java.nio.file.StandardCopyOption._
import java.time.{LocalDate, LocalDateTime}

import akka.actor.ActorSystem
import cats.data.Validated.Valid
import cats.data.{EitherT, OptionT}
import cats.instances.future._
import com.typesafe.config.ConfigFactory
import org.codehaus.stax2.validation.{XMLValidationSchema, XMLValidationSchemaFactory}
import org.mockito.Matchers.{eq => EQ, _}
import org.mockito.Mockito._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mockito.MockitoSugar
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.i18n.MessagesApi
import play.api.libs.Files
import play.api.libs.json.{Format, JsNull, Reads}
import play.api.test.FakeRequest
import play.api.{Configuration, Environment}
import uk.gov.hmrc.auth.core.authorise.Predicate
import uk.gov.hmrc.auth.core.retrieve.{Credentials, Retrieval, ~}
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.services._
import uk.gov.hmrc.cbcrfrontend.typesclasses.{CbcrsUrl, FusFeUrl, FusUrl, ServiceUrl}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.cache.client.CacheMap
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}
import uk.gov.hmrc.play.bootstrap.http.HttpClient
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.reflect.runtime.universe


class FileUploadControllerSpec extends UnitSpec with ScalaFutures with GuiceOneAppPerSuite with CSRFTest with MockitoSugar with BeforeAndAfterEach{

  implicit val ec: ExecutionContext                    = app.injector.instanceOf[ExecutionContext]
  implicit val messagesApi: MessagesApi                = app.injector.instanceOf[MessagesApi]
  implicit val env                                     = app.injector.instanceOf[Environment]
  implicit val as                                      = app.injector.instanceOf[ActorSystem]

  val fuService: FileUploadService                     = mock[FileUploadService]
  val schemaValidator: CBCRXMLValidator                = mock[CBCRXMLValidator]
  val businessRulesValidator: CBCBusinessRuleValidator = mock[CBCBusinessRuleValidator]
  val cache: CBCSessionCache                           = mock[CBCSessionCache]
  val extractor: XmlInfoExtract                        = new XmlInfoExtract()
  val deEnrolReEnrolService                            = mock[DeEnrolReEnrolService]
  val auditC: AuditConnector                           = mock[AuditConnector]
  var runMode                                          = mock[RunMode]
  val authConnector                                    = mock[AuthConnector]

  implicit val configuration = new Configuration(ConfigFactory.load("application.conf"))
  implicit val feConfig = mock[FrontendAppConfig]

  when(feConfig.analyticsHost) thenReturn "host"
  when(feConfig.analyticsToken) thenReturn "token"

  val creds:Credentials       = Credentials("totally", "legit")

  override protected def afterEach(): Unit = {
    reset(cache,businessRulesValidator,schemaValidator,fuService)
    super.afterEach()
  }

  object TestSessionCache {

    var succeed = true
    var agent = false
    var individual = false
    val http = mock[HttpClient]

    def apply(): CBCSessionCache = new SessionCache(configuration, http)

    private class SessionCache(_config:Configuration, _http:HttpClient) extends CBCSessionCache(_config, _http) {

      override def read[T: Reads : universe.TypeTag](implicit hc: HeaderCarrier): EitherT[Future,ExpiredSession,T] = universe.typeOf[T] match {
        case t if t =:= universe.typeOf[EnvelopeId] => EitherT.pure[Future,ExpiredSession,T](EnvelopeId("test").asInstanceOf[T])
        case t if t =:= universe.typeOf[CBCId] => leftE[T](ExpiredSession("meh"))
      }

      override def readOption[T: Reads : universe.TypeTag](implicit hc: HeaderCarrier): Future[Option[T]] = universe.typeOf[T] match {
        case t if t =:= universe.typeOf[CBCId] => Future.successful(None)
      }

      override def readOrCreate[T: Format : universe.TypeTag](f: => OptionT[Future, T])(implicit hc: HeaderCarrier): OptionT[Future, T] = universe.typeOf[T] match {
        case t if t =:= universe.typeOf[FileId] => OptionT.some[Future, FileId](FileId("fileId")).asInstanceOf[OptionT[Future, T]]
        case t if t =:= universe.typeOf[EnvelopeId] => succeed match {
          case true =>  OptionT.some[Future, EnvelopeId](EnvelopeId("test")).asInstanceOf[OptionT[Future, T]]
          case false => OptionT.none
        }
      }
    }
  }

  implicit val hc = HeaderCarrier()
  implicit val fusUrl = new ServiceUrl[FusUrl] { val url = "file-upload"}
  implicit val fusFeUrl = new ServiceUrl[FusFeUrl] { val url = "file-upload-frontend"}
  implicit val cbcrsUrl = new ServiceUrl[CbcrsUrl] { val url = "cbcr"}

  val docRefId="GB2016RGXVCBC0000000056CBC40120170311T090000X_7000000002OECD1"
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
    Some(ReportingEntity(CBC701,DocSpec(OECD1,DocRefId(docRefId+"REP").get,None,None),TIN("7000000002","gb"),"name")),
    List(CbcReports(DocSpec(OECD1,DocRefId(docRefId + "ENT").get,None,None))),
    Some(AdditionalInfo(DocSpec(OECD1,DocRefId(docRefId + "ADD").get,None,None))),
    Some(LocalDate.now()),
    List.empty[String]
  )

  val completeXmlInfo = CompleteXMLInfo(xmlinfo,ReportingEntity(CBC701,DocSpec(OECD1,DocRefId(docRefId+"REP").get,None,None),TIN("7000000002","gb"),"name"))

  def right[A](a:Future[A]) : ServiceResponse[A] = EitherT.right[Future,CBCErrors, A](a)
  def left[A](s:String) : ServiceResponse[A] = EitherT.left[Future,CBCErrors, A](UnexpectedState(s))
  def pure[A](a:A) : ServiceResponse[A] = EitherT.pure[Future,CBCErrors, A](a)

  val md = FileMetadata("","","something.xml","",1.0,"",JsNull,"")

  val xmlValidationSchemaFactory: XMLValidationSchemaFactory =
    XMLValidationSchemaFactory.newInstance(XMLValidationSchema.SCHEMA_ID_W3C_SCHEMA)
  when(runMode.env) thenReturn "Dev"
  val schemaVer: String = configuration.getString(s"${runMode.env}.oecd-schema-version").getOrElse(throw new Exception(s"Missing configuration ${runMode.env}.oecd-schema-version"))
  val schemaFile: File = new File(s"conf/schema/$schemaVer/CbcXML_v$schemaVer.xsd")

  val partiallyMockedController = new FileUploadController(messagesApi,authConnector,schemaValidator, businessRulesValidator, fuService, extractor,auditC,deEnrolReEnrolService,env)(ec,TestSessionCache(), configuration, feConfig)
  val controller = new FileUploadController(messagesApi,authConnector,schemaValidator, businessRulesValidator, fuService, extractor,auditC,deEnrolReEnrolService,env)(ec,cache, configuration, feConfig)

  val testFile:File= new File("test/resources/cbcr-valid.xml")
  val tempFile:File=File.createTempFile("test",".xml")
  val validFile = java.nio.file.Files.copy(testFile.toPath,tempFile.toPath,REPLACE_EXISTING).toFile
  val newEnrolments = Set(Enrolment("HMRC-CBC-ORG", Seq(EnrolmentIdentifier("cbcId", (CBCId.create(99).getOrElse(fail("booo"))).toString), EnrolmentIdentifier("UTR", Utr("1234567890").utr)),state = "",delegatedAuthRule = None))
  val newCBCEnrolment = CBCEnrolment(CBCId.create(99).getOrElse(fail("booo")), Utr("1234567890"))

  "GET /upload-report" should {
    val fakeRequestChooseXMLFile = addToken(FakeRequest("GET", "/upload-report"))

    "return 200 when the envelope is created successfully" in {
      when(authConnector.authorise(any(), any[Retrieval[Option[AffinityGroup] ~ Option[CBCEnrolment]]]())(any(), any()))
        .thenReturn(Future.successful(new ~[Option[AffinityGroup], Option[CBCEnrolment]](Some(AffinityGroup.Organisation), Some(newCBCEnrolment))))
      when(cache.readOrCreate[EnvelopeId](any())) thenReturn OptionT.some[Future,EnvelopeId](EnvelopeId("12345678"))
      val result = partiallyMockedController.chooseXMLFile(fakeRequestChooseXMLFile)
      status(result) shouldBe Status.OK
    }
    "displays gateway account not registered page if Organisation user has is not enrolled" in {
      when(authConnector.authorise(any(), any[Retrieval[Option[AffinityGroup] ~ Option[CBCEnrolment]]]())(any(), any()))
        .thenReturn(Future.successful(new ~[Option[AffinityGroup], Option[CBCEnrolment]](Some(AffinityGroup.Organisation), None)))
      val result = partiallyMockedController.chooseXMLFile(fakeRequestChooseXMLFile)
      status(result) shouldBe Status.OK
    }
    "allow agent to submit even when no enrolment" in {
      when(authConnector.authorise(any(), any[Retrieval[Option[AffinityGroup] ~ Option[CBCEnrolment]]]())(any(), any()))
        .thenReturn(Future.successful(new ~[Option[AffinityGroup], Option[CBCEnrolment]](Some(AffinityGroup.Organisation), Some(newCBCEnrolment))))
      val result = partiallyMockedController.chooseXMLFile(fakeRequestChooseXMLFile)
      status(result) shouldBe Status.OK

    }
    "redirect  when user is an individual" in {
      when(authConnector.authorise(any(), any[Retrieval[Option[AffinityGroup] ~ Option[CBCEnrolment]]]())(any(), any()))
        .thenReturn(Future.successful(new ~[Option[AffinityGroup], Option[CBCEnrolment]](Some(AffinityGroup.Individual), None)))
      val result = partiallyMockedController.chooseXMLFile(fakeRequestChooseXMLFile)
      status(result) shouldBe Status.SEE_OTHER

    }
    "return 500 when the is an error creating the envelope" in {
      TestSessionCache.succeed = false
      when(authConnector.authorise(any(), any[Retrieval[Option[AffinityGroup] ~ Option[CBCEnrolment]]]())(any(), any()))
        .thenReturn(Future.successful(new ~[Option[AffinityGroup], Option[CBCEnrolment]](Some(AffinityGroup.Organisation), Some(newCBCEnrolment))))
      when(fuService.createEnvelope(any(), any())) thenReturn left[EnvelopeId]("server error")
      val result = partiallyMockedController.chooseXMLFile(fakeRequestChooseXMLFile)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
      TestSessionCache.succeed = true
    }
    "return a 200 and call the DeEnrolReEnrolService if the user has a PrivateBeta cbcId in their bearer token" in {
      when(auditC.sendExtendedEvent(any())(any(),any())) thenReturn Future.successful(AuditResult.Success)
      when(deEnrolReEnrolService.deEnrolReEnrol(any())(any())) thenReturn right(CBCId.create(10).getOrElse(fail("bad cbcid")))
      when(authConnector.authorise(any(), any[Retrieval[Option[AffinityGroup] ~ Option[CBCEnrolment]]]())(any(), any()))
        .thenReturn(Future.successful(new ~[Option[AffinityGroup], Option[CBCEnrolment]](Some(AffinityGroup.Organisation), Some(CBCEnrolment(CBCId("XGCBC0000000001").getOrElse(fail("bad cbcId")),Utr("9000000001"))))))
      val result = partiallyMockedController.chooseXMLFile(fakeRequestChooseXMLFile)
      status(result) shouldBe Status.OK

      verify(deEnrolReEnrolService).deEnrolReEnrol(any())(any())

    }
  }

  "GET /unregistered-gg-account" should {
    val fakeRequestUnregisteredGGId = addToken((FakeRequest("GET", "/unregistered-gg-account")))

    "return 200 when the envelope is created successfully" in {
      when(authConnector.authorise[Any](any(),any())(any(), any())) thenReturn Future.successful()
      when(cache.readOrCreate[EnvelopeId](any())) thenReturn OptionT.some[Future,EnvelopeId](EnvelopeId("12345678"))
      val result = partiallyMockedController.unregisteredGGAccount(fakeRequestUnregisteredGGId)
      status(result) shouldBe Status.OK
    }
    "return 500 when the is an error creating the envelope\"" in {
      TestSessionCache.succeed = false
      when(authConnector.authorise[Any](any(),any())(any(), any())) thenReturn Future.successful()
      when(fuService.createEnvelope(any(), any())) thenReturn left[EnvelopeId]("server error")
      val result = partiallyMockedController.unregisteredGGAccount(fakeRequestUnregisteredGGId)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR

      TestSessionCache.succeed = true
    }
  }

  "GET /fileUploadResponse/envelopeId/fileId" should {
    val fakeRequestGetFileUploadResponse  = addToken(FakeRequest("GET", "/fileUploadResponse/envelopeId/fileId"))
    "return 202 when the file is available" in {
      when(authConnector.authorise[Any](any(),any())(any(), any())) thenReturn Future.successful()
      when(fuService.getFileUploadResponse(any())(any(), any())) thenReturn right(Some(FileUploadCallbackResponse("envelopeId", "fileId", "AVAILABLE", None)):Option[FileUploadCallbackResponse])
      val result = partiallyMockedController.fileUploadResponse("envelopeId")(fakeRequestGetFileUploadResponse)
      status(result) shouldBe Status.ACCEPTED
    }
    "return 204" when {
      "the FUS hasn't updated the backend yet" in {
        when(authConnector.authorise[Any](any(),any())(any(), any())) thenReturn Future.successful()
        when(fuService.getFileUploadResponse(any())(any(), any())) thenReturn right[Option[FileUploadCallbackResponse]](None)
        val result = partiallyMockedController.fileUploadResponse("envelopeId")(fakeRequestGetFileUploadResponse).futureValue
        status(result) shouldBe Status.NO_CONTENT
      }
      "file is not yet available" in {
        when(authConnector.authorise[Any](any(),any())(any(), any())) thenReturn Future.successful()
        when(fuService.getFileUploadResponse(any())(any(), any())) thenReturn right[Option[FileUploadCallbackResponse]](Some(FileUploadCallbackResponse("envelopeId", "fileId", "QUARENTEENED",None)): Option[FileUploadCallbackResponse])
        val result = partiallyMockedController.fileUploadResponse("envelopeId")(fakeRequestGetFileUploadResponse).futureValue
        status(result) shouldBe Status.NO_CONTENT
      }
    }
    "return a 200" in {
      when(authConnector.authorise[Any](any(),any())(any(), any())) thenReturn Future.successful()
      val request = addToken(FakeRequest("GET", "fileUploadProgress/envelopeId/fileId"))
      val result = partiallyMockedController.fileUploadProgress("test","test")(request)
      status(result) shouldBe Status.OK
    }
    "return a 500 if the envelopeId doesn't match with the cache" in {
      when(authConnector.authorise[Any](any(),any())(any(), any())) thenReturn Future.successful()
      val request = addToken(FakeRequest("GET", "fileUploadProgress/envelopeId/fileId"))
      val result = partiallyMockedController.fileUploadProgress("test2","test")(request)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }
    "direct to technical-difficulties" when {
      "the call to get the file metadata fails" in{
        val request = addToken(FakeRequest("GET", "fileUploadReady/envelopeId/fileId"))
        val cbcId = CBCId("XLCBC0100000056").getOrElse(fail("booo"))
        val enrole:CBCEnrolment = CBCEnrolment(cbcId,Utr("7000000002"))
        when(fuService.getFile(any(),any())(any(),any())) thenReturn right(validFile)
        when(fuService.getFileMetaData(any(),any())(any(),any())) thenReturn right[Option[FileMetadata]](None)
        when(authConnector.authorise[~[~[Credentials, Option[AffinityGroup]], Option[CBCEnrolment]]](any(),any())(any(),any())) thenReturn Future.successful(new ~[ ~ [Credentials, Option[AffinityGroup]],Option[CBCEnrolment]](new ~(creds, Some(AffinityGroup.Organisation)),Some(enrole)))
        val result = Await.result(controller.fileValidate("test","test")(request), 2.second)
        result.header.headers("Location") should endWith("technical-difficulties")
        status(result) shouldBe Status.SEE_OTHER
        verify(fuService).getFile(any(),any())(any(),any())
        verify(fuService).getFileMetaData(any(),any())(any(),any())
      }
      "the call to get the file fails" in {
        val request = addToken(FakeRequest("GET", "fileUploadReady/envelopeId/fileId"))
        val cbcId = CBCId("XLCBC0100000056").getOrElse(fail("booo"))
        val enrole:CBCEnrolment = CBCEnrolment(cbcId,Utr("7000000002"))
        when(fuService.getFile(any(),any())(any(),any())) thenReturn left[File]("oops")
        when(fuService.getFileMetaData(any(),any())(any(),any())) thenReturn right[Option[FileMetadata]](Some(md))
        when(authConnector.authorise[~[~[Credentials, Option[AffinityGroup]], Option[CBCEnrolment]]](any(),any())(any(),any())) thenReturn Future.successful(new ~[ ~ [Credentials, Option[AffinityGroup]],Option[CBCEnrolment]](new ~(creds, Some(AffinityGroup.Organisation)),Some(enrole)))
        val result = Await.result(controller.fileValidate("test","test")(request), 2.second)
        result.header.headers("Location") should endWith("technical-difficulties")
        status(result) shouldBe Status.SEE_OTHER
        verify(fuService).getFile(any(),any())(any(),any())
        verify(fuService).getFileMetaData(any(),any())(any(),any())
      }
      "the call to cache.save fails" in {
        val request = addToken(FakeRequest("GET", "fileUploadReady/envelopeId/fileId"))
        val cbcId = CBCId("XLCBC0100000056").getOrElse(fail("booo"))
        val enrole:CBCEnrolment = CBCEnrolment(cbcId,Utr("7000000002"))
        when(fuService.getFile(any(),any())(any(),any())) thenReturn right(validFile)
        when(fuService.getFileMetaData(any(),any())(any(),any())) thenReturn right[Option[FileMetadata]](Some(md))
        when(authConnector.authorise[~[~[Credentials, Option[AffinityGroup]], Option[CBCEnrolment]]](any(),any())(any(),any())) thenReturn Future.successful(new ~[ ~ [Credentials, Option[AffinityGroup]],Option[CBCEnrolment]](new ~(creds, Some(AffinityGroup.Organisation)),Some(enrole)))
        when(cache.save(any())(any(),any(),any())) thenReturn Future.failed(new Exception("bad"))
        val result = Await.result(controller.fileValidate("test","test")(request), 2.second)
        result.header.headers("Location") should endWith("technical-difficulties")
        status(result) shouldBe Status.SEE_OTHER
        verify(fuService).getFile(any(),any())(any(),any())
        verify(fuService).getFileMetaData(any(),any())(any(),any())
        verify(cache,atLeastOnce()).save(any())(any(),any(),any())
      }
    }
    "return a 200 when the fileValidate call is successful and all dependant calls return successfully" in {
      val evenMoreValidFile = java.nio.file.Files.copy(testFile.toPath,tempFile.toPath,REPLACE_EXISTING).toFile
      val request = addToken(FakeRequest("GET", "fileUploadReady/envelopeId/fileId"))
      val cbcId = CBCId("XLCBC0100000056").getOrElse(fail("booo"))
      val enrole:CBCEnrolment = CBCEnrolment(cbcId,Utr("7000000002"))
      when(fuService.getFile(any(),any())(any(),any())) thenReturn right(evenMoreValidFile)
      when(fuService.getFileMetaData(any(),any())(any(),any())) thenReturn right[Option[FileMetadata]](Some(md))
      when(schemaValidator.validateSchema(any())) thenReturn new XmlErrorHandler()
      when(cache.save(any())(any(),any(),any())) thenReturn Future.successful(new CacheMap("",Map.empty))
      when(cache.readOption(EQ(AffinityGroup.jsonFormat),any(),any())) thenReturn Future.successful(Option(AffinityGroup.Organisation))
      when(businessRulesValidator.validateBusinessRules(any(),any(),any(),any())(any())) thenReturn Future.successful(Valid(xmlinfo))
      when(businessRulesValidator.recoverReportingEntity(any())(any())) thenReturn Future.successful(Valid(completeXmlInfo))
      when(authConnector.authorise[~[~[Credentials, Option[AffinityGroup]], Option[CBCEnrolment]]](any(),any())(any(),any())) thenReturn Future.successful(new ~[ ~ [Credentials, Option[AffinityGroup]],Option[CBCEnrolment]](new ~(creds, Some(AffinityGroup.Organisation)),Some(enrole)))
      val result = Await.result(controller.fileValidate("test","test")(request), 2.second)
      val returnVal = status(result)
      returnVal shouldBe Status.OK
      verify(fuService).getFile(any(),any())(any(),any())
      verify(fuService).getFileMetaData(any(),any())(any(),any())
      verify(cache,atLeastOnce()).save(any())(any(),any(),any())
      verify(businessRulesValidator).validateBusinessRules(any(),any(),any(),any())(any())
      verify(schemaValidator).validateSchema(any())

    }

    "be redirected to an error page" when {
      "the file extension is invalid" in {
        val request = addToken(FakeRequest("GET", "fileUploadReady/envelopeId/fileId"))
        val cbcId = CBCId("XLCBC0100000056").getOrElse(fail("booo"))
        val enrole:CBCEnrolment = CBCEnrolment(cbcId,Utr("7000000002"))
        when(fuService.getFile(any(),any())(any(),any())) thenReturn right(validFile)
        when(fuService.getFileMetaData(any(),any())(any(),any())) thenReturn right[Option[FileMetadata]](Some(md.copy(name = "bad.zip")))
        when(cache.read[CBCId](EQ(CBCId.cbcIdFormat),any(),any())) thenReturn rightE(CBCId.create(1).getOrElse(fail("baaa")))
        when(cache.save(any())(any(),any(),any())) thenReturn Future.successful(CacheMap("cache",Map.empty))
        when(authConnector.authorise[~[~[Credentials, Option[AffinityGroup]], Option[CBCEnrolment]]](any(),any())(any(),any())) thenReturn Future.successful(new ~[ ~ [Credentials, Option[AffinityGroup]],Option[CBCEnrolment]](new ~(creds, Some(AffinityGroup.Organisation)),Some(enrole)))
        val result = Await.result(controller.fileValidate("test","test")(request), 5.second)
        result.header.headers("Location") should endWith("invalid-file-type")
        status(result) shouldBe Status.SEE_OTHER
      }
    }
  }

  "The file-upload error call back" should {
    "cause a redirect to file-too-large if the response has status-code 413" in {
      val request = addToken(FakeRequest())
      when(authConnector.authorise[Any](any(),any())(any(), any())) thenReturn Future.successful()
      val result = Await.result(partiallyMockedController.handleError(413, "no reason")(request), 5.second)
      result.header.headers("Location") should endWith("file-too-large")
      status(result) shouldBe Status.SEE_OTHER
    }
    "cause a redirect to invalid-file-type if the response has status-code 415" in {
      val request = addToken(FakeRequest())
      when(authConnector.authorise[Any](any(),any())(any(), any())) thenReturn Future.successful()
      val result = Await.result(partiallyMockedController.handleError(415, "no reason")(request), 5.second)
      result.header.headers("Location") should endWith("invalid-file-type")
      status(result) shouldBe Status.SEE_OTHER
    }
  }

}
