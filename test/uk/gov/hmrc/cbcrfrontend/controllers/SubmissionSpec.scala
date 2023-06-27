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
import akka.util.Timeout
import cats.data.{EitherT, OptionT}
import cats.implicits.catsStdInstancesForFuture
import org.mockito.ArgumentMatchers.{any, eq => EQ, _}
import org.mockito.MockitoSugar
import org.mockito.cats.MockitoCats
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.i18n.{Messages, MessagesApi}
import play.api.libs.json.{JsNull, JsValue, Json}
import play.api.mvc.{MessagesControllerComponents, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers.{call, contentAsString, writeableOf_AnyContentAsFormUrlEncoded}
import play.api.{Configuration, Environment}
import uk.gov.hmrc.auth.core.retrieve.{Credentials, Retrieval, ~}
import uk.gov.hmrc.auth.core.{AffinityGroup, AuthConnector}
import uk.gov.hmrc.cbcrfrontend._
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.form.SubmitterInfoForm
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.services._
import uk.gov.hmrc.cbcrfrontend.util.UnitSpec
import uk.gov.hmrc.cbcrfrontend.views.Views
import uk.gov.hmrc.emailaddress.EmailAddress
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.cache.client.CacheMap
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}

import java.io.File
import java.time.{LocalDate, LocalDateTime}
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.language.postfixOps

class SubmissionSpec
    extends UnitSpec with GuiceOneAppPerSuite with CSRFTest with BeforeAndAfterEach with MockitoSugar with MockitoCats {

  private implicit val ec: ExecutionContext = app.injector.instanceOf[ExecutionContext]
  private implicit val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
  private implicit val as: ActorSystem = app.injector.instanceOf[ActorSystem]
  private implicit val env: Environment = app.injector.instanceOf[Environment]
  private implicit val config: Configuration = app.injector.instanceOf[Configuration]
  private implicit val feConfig: FrontendAppConfig = mock[FrontendAppConfig]
  private implicit val timeout: Timeout = Timeout(5 seconds)

  private def getMessages(r: FakeRequest[_]): Messages = messagesApi.preferred(r)

  private val creds: Credentials = Credentials("totally", "legit")

  private val cache = mock[CBCSessionCache]
  private val fus = mock[FileUploadService]
  private val docRefService = mock[DocRefIdService]
  private val messageRefIdService = mock[MessageRefIdService]
  private val auth = mock[AuthConnector]
  private val auditMock = mock[AuditConnector]
  private val mockCBCIdService = mock[CBCIdService]
  private val mockEmailService = mock[EmailService]
  private val reportingEntity = mock[ReportingEntityDataService]
  private val mcc = app.injector.instanceOf[MessagesControllerComponents]
  private val views: Views = app.injector.instanceOf[Views]
  private val fileDetails = FileDetails("env1", "file1")

  private val bpr = BusinessPartnerRecord("safeId", None, EtmpAddress("Line1", None, None, None, None, "GB"))

  private implicit val hc: HeaderCarrier = HeaderCarrier()
  private val controller = new SubmissionController(
    messagesApi,
    fus,
    docRefService,
    reportingEntity,
    messageRefIdService,
    mockCBCIdService,
    auditMock,
    env,
    auth,
    mockEmailService,
    mcc,
    views)(ec, cache, config, feConfig)

  override protected def afterEach(): Unit = {
    reset(cache, fus, docRefService, reportingEntity, mockEmailService, auth, messageRefIdService)
    super.afterEach()
  }

  "POST /submitUltimateParentEntity " should {
    val ultimateParentEntity = UltimateParentEntity("UlitmateParentEntity")
    val fakeRequestSubmit = addToken(
      FakeRequest("POST", "/submitUltimateParentEntity ")
        .withFormUrlEncodedBody("ultimateParentEntity" -> ultimateParentEntity.ultimateParentEntity))
    "return 303 and point to the correct page" when {
      "the reporting role is CBC702" in {
        when(auth.authorise[Option[AffinityGroup]](any(), any())(any(), any())) thenReturn Future.successful(
          Some(AffinityGroup.Organisation))
        whenF(cache.read[CompleteXMLInfo](EQ(CompleteXMLInfo.format), any(), any())) thenReturn keyXMLInfo.copy(
          reportingEntity = keyXMLInfo.reportingEntity.copy(reportingRole = CBC702))
        when(cache.save[UltimateParentEntity](any())(EQ(UltimateParentEntity.format), any(), any())) thenReturn Future
          .successful(CacheMap("cache", Map.empty[String, JsValue]))
        val result = call(controller.submitUltimateParentEntity, fakeRequestSubmit)
        result.header.headers("Location") should endWith("/utr/entry-form")
        status(result) shouldBe Status.SEE_OTHER
      }

      "the reporting role is CBC703" in {
        when(auth.authorise[Option[AffinityGroup]](any(), any())(any(), any())) thenReturn Future.successful(
          Some(AffinityGroup.Organisation))
        when(cache.readOption(EQ(AffinityGroup.jsonFormat), any(), any())) thenReturn Future.successful(
          Some(AffinityGroup.Organisation))
        when(cache.save[UltimateParentEntity](any())(EQ(UltimateParentEntity.format), any(), any())) thenReturn Future
          .successful(CacheMap("cache", Map.empty[String, JsValue]))
        whenF(cache.read[CompleteXMLInfo](EQ(CompleteXMLInfo.format), any(), any())) thenReturn keyXMLInfo.copy(
          reportingEntity = keyXMLInfo.reportingEntity.copy(reportingRole = CBC703))
        when(cache.save[UltimateParentEntity](any())(EQ(UltimateParentEntity.format), any(), any())) thenReturn Future
          .successful(CacheMap("cache", Map.empty[String, JsValue]))
        val result = call(controller.submitUltimateParentEntity, fakeRequestSubmit)
        result.header.headers("Location") should endWith("/submitter-info/entry-form")
        status(result) shouldBe Status.SEE_OTHER
      }
    }

    "return 500 when the reportingrole is CBC701 as this should never happen" in {
      when(auth.authorise[Option[AffinityGroup]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(AffinityGroup.Organisation))
      when(cache.save[UltimateParentEntity](any())(EQ(UltimateParentEntity.format), any(), any())) thenReturn Future
        .successful(CacheMap("cache", Map.empty[String, JsValue]))
      whenF(cache.read(EQ(CompleteXMLInfo.format), any(), any())) thenReturn keyXMLInfo.copy(
        reportingEntity = keyXMLInfo.reportingEntity.copy(reportingRole = CBC701))
      val result = call(controller.submitUltimateParentEntity, fakeRequestSubmit)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }
  }

  "GET /submitter-info" should {
    "return a 200 when SubmitterInfo is populated in cache" in {
      val fakeRequestSubmit = addToken(FakeRequest("GET", "/submitter-info"))
      when(cache.readOption(EQ(SubmitterInfo.format), any(), any())) thenReturn Future.successful(
        Some(SubmitterInfo("A Name", None, "0123456", EmailAddress("email@org.com"), None)))
      when(auth.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful(())
      whenF(cache.read[CompleteXMLInfo](EQ(CompleteXMLInfo.format), any(), any())) thenReturn keyXMLInfo
      when(cache.save[FilingType](any())(EQ(FilingType.format), any(), any())) thenReturn Future.successful(
        CacheMap("cache", Map.empty[String, JsValue]))
      when(cache.save[TIN](any())(EQ(TIN.format), any(), any())) thenReturn Future.successful(
        CacheMap("cache", Map.empty[String, JsValue]))
      when(auth.authorise[Option[AffinityGroup]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(AffinityGroup.Organisation))
      whenF(cache.read[FileDetails](EQ(FileDetails.fileDetailsFormat), any(), any())) thenReturn fileDetails
      status(controller.submitterInfo(None)(fakeRequestSubmit)) shouldBe Status.OK
    }

    "return a 200 when SubmitterInfo is NOT in cache" in {
      val fakeRequestSubmit = addToken(FakeRequest("GET", "/submitter-info"))
      when(cache.readOption(EQ(SubmitterInfo.format), any(), any())) thenReturn Future.successful(None)
      when(auth.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful(())
      whenF(cache.read[CompleteXMLInfo](EQ(CompleteXMLInfo.format), any(), any())) thenReturn keyXMLInfo
      when(cache.save[FilingType](any())(EQ(FilingType.format), any(), any())) thenReturn Future.successful(
        CacheMap("cache", Map.empty[String, JsValue]))
      when(cache.save[TIN](any())(EQ(TIN.format), any(), any())) thenReturn Future.successful(
        CacheMap("cache", Map.empty[String, JsValue]))
      when(auth.authorise[Option[AffinityGroup]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(AffinityGroup.Organisation))
      whenF(cache.read[FileDetails](EQ(FileDetails.fileDetailsFormat), any(), any())) thenReturn fileDetails
      status(controller.submitterInfo()(fakeRequestSubmit)) shouldBe Status.OK
    }

    "use the UPE and Filing type form the xml when the ReportingRole is CBC701 " in {
      val cache = mock[CBCSessionCache]
      val controller = new SubmissionController(
        messagesApi,
        fus,
        docRefService,
        reportingEntity,
        messageRefIdService,
        mockCBCIdService,
        auditMock,
        env,
        auth,
        mockEmailService,
        mcc,
        views)(ec, cache, config, feConfig)
      val fakeRequestSubmit = addToken(FakeRequest("GET", "/submitter-info"))
      when(cache.readOption(EQ(SubmitterInfo.format), any(), any())) thenReturn Future.successful(None)
      when(auth.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful(())
      whenF(cache.read[CompleteXMLInfo](EQ(CompleteXMLInfo.format), any(), any())) thenReturn keyXMLInfo
      when(cache.save[UltimateParentEntity](any())(EQ(UltimateParentEntity.format), any(), any())) thenReturn Future
        .successful(CacheMap("cache", Map.empty[String, JsValue]))
      when(cache.save[FilingType](any())(EQ(FilingType.format), any(), any())) thenReturn Future.successful(
        CacheMap("cache", Map.empty[String, JsValue]))
      when(auth.authorise[Option[AffinityGroup]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(AffinityGroup.Organisation))
      whenF(cache.read[FileDetails](EQ(FileDetails.fileDetailsFormat), any(), any())) thenReturn fileDetails
      status(controller.submitterInfo()(fakeRequestSubmit)) shouldBe Status.OK
      verify(cache).save(any())(EQ(FilingType.format), any(), any())
      verify(cache).save(any())(EQ(UltimateParentEntity.format), any(), any())
    }

    "use the Filing type form the xml when the ReportingRole is CBC702" in {
      val cache = mock[CBCSessionCache]
      val controller = new SubmissionController(
        messagesApi,
        fus,
        docRefService,
        reportingEntity,
        messageRefIdService,
        mockCBCIdService,
        auditMock,
        env,
        auth,
        mockEmailService,
        mcc,
        views)(ec, cache, config, feConfig)
      val fakeRequestSubmit = addToken(FakeRequest("GET", "/submitter-info"))
      when(auth.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful(())
      when(cache.readOption(EQ(SubmitterInfo.format), any(), any())) thenReturn Future.successful(None)
      whenF(cache.read[CompleteXMLInfo](EQ(CompleteXMLInfo.format), any(), any())) thenReturn keyXMLInfo.copy(
        reportingEntity = keyXMLInfo.reportingEntity.copy(reportingRole = CBC702))
      when(cache.save[FilingType](any())(EQ(FilingType.format), any(), any())) thenReturn Future.successful(
        CacheMap("cache", Map.empty[String, JsValue]))
      when(auth.authorise[Option[AffinityGroup]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(AffinityGroup.Organisation))
      whenF(cache.read[FileDetails](EQ(FileDetails.fileDetailsFormat), any(), any())) thenReturn fileDetails
      status(controller.submitterInfo()(fakeRequestSubmit)) shouldBe Status.OK
      verify(cache).save(any())(EQ(FilingType.format), any(), any())
    }

    "use the Filing type form the xml when the ReportingRole is CBC703" in {
      val cache = mock[CBCSessionCache]
      val controller = new SubmissionController(
        messagesApi,
        fus,
        docRefService,
        reportingEntity,
        messageRefIdService,
        mockCBCIdService,
        auditMock,
        env,
        auth,
        mockEmailService,
        mcc,
        views)(ec, cache, config, feConfig)
      val fakeRequestSubmit = addToken(FakeRequest("GET", "/submitter-info"))
      when(auth.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful(())
      when(cache.readOption(EQ(SubmitterInfo.format), any(), any())) thenReturn Future.successful(None)
      whenF(cache.read[CompleteXMLInfo](EQ(CompleteXMLInfo.format), any(), any())) thenReturn keyXMLInfo.copy(
        reportingEntity = keyXMLInfo.reportingEntity.copy(reportingRole = CBC703))
      when(cache.save[FilingType](any())(EQ(FilingType.format), any(), any())) thenReturn Future.successful(
        CacheMap("cache", Map.empty[String, JsValue]))
      when(auth.authorise[Option[AffinityGroup]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(AffinityGroup.Organisation))
      whenF(cache.read[FileDetails](EQ(FileDetails.fileDetailsFormat), any(), any())) thenReturn fileDetails
      status(controller.submitterInfo()(fakeRequestSubmit)) shouldBe Status.OK
      verify(cache).save(any())(EQ(FilingType.format), any(), any())
    }
  }

  "POST /submitSubmitterInfo" should {
    "return 400 when the there is no data at all" in {
      val dataSeq = Seq(
        "fullName"           -> "",
        "agencyBusinessName" -> "",
        "email"              -> "",
        "affinityGroup"      -> "",
      )
      val fakeRequestSubmit = addToken(FakeRequest("POST", "/submitSubmitterInfo").withFormUrlEncodedBody(dataSeq: _*))
      when(auth.authorise[Option[AffinityGroup]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(AffinityGroup.Organisation))
      whenF(cache.read[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenFailWith ExpiredSession("")
      whenF(cache.read[FileDetails](EQ(FileDetails.fileDetailsFormat), any(), any())) thenReturn fileDetails
      val result = call(controller.submitSubmitterInfo, fakeRequestSubmit)
      status(result) shouldBe Status.BAD_REQUEST
    }

    "return 400 when the all data exists but Fullname" in {
      val submitterInfo = SubmitterInfo("", None, "07923456708", EmailAddress("abc@xyz.com"), None)
      val dataSeq = Seq(
        "fullName"           -> submitterInfo.fullName,
        "agencyBusinessName" -> submitterInfo.agencyBusinessName.toString,
        "email"              -> submitterInfo.email.toString,
        "affinityGroup"      -> submitterInfo.affinityGroup.toString,
      )
      val fakeRequestSubmit =
        addToken(FakeRequest("POST", "/submitSubmitterInfo").withFormUrlEncodedBody(dataSeq: _*))
      when(auth.authorise[Option[AffinityGroup]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(AffinityGroup.Organisation))
      whenF(cache.read[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenFailWith ExpiredSession("")
      whenF(cache.read[FileDetails](EQ(FileDetails.fileDetailsFormat), any(), any())) thenReturn fileDetails
      val result = call(controller.submitSubmitterInfo, fakeRequestSubmit)
      status(result) shouldBe Status.BAD_REQUEST
    }

    "return 400 when the all data exists but Contact Phone" in {
      val submitterInfo = SubmitterInfo("Fullname", None, "", EmailAddress("abc@xyz.com"), None)
      val dataSeq = Seq(
        "fullName"           -> submitterInfo.fullName,
        "agencyBusinessName" -> submitterInfo.agencyBusinessName.toString,
        "email"              -> submitterInfo.email.toString,
        "affinityGroup"      -> submitterInfo.affinityGroup.toString,
      )
      when(auth.authorise[Option[AffinityGroup]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(AffinityGroup.Organisation))
      whenF(cache.read[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenFailWith ExpiredSession("")
      val fakeRequestSubmit =
        addToken(FakeRequest("POST", "/submitSubmitterInfo").withFormUrlEncodedBody(dataSeq: _*))
      whenF(cache.read[FileDetails](EQ(FileDetails.fileDetailsFormat), any(), any())) thenReturn fileDetails
      val result = call(controller.submitSubmitterInfo, fakeRequestSubmit)
      status(result) shouldBe Status.BAD_REQUEST
    }

    "return 400 when the all data exists but Email Address" in {
      val submitterInfo = Seq(
        "fullName"     -> "Fullname",
        "contactPhone" -> "07923456708",
        "email"        -> ""
      )
      val fakeRequestSubmit =
        addToken(FakeRequest("POST", "/submitSubmitterInfo").withFormUrlEncodedBody(submitterInfo: _*))
      when(auth.authorise[Option[AffinityGroup]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(AffinityGroup.Organisation))
      whenF(cache.read[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenFailWith ExpiredSession("")
      whenF(cache.read[FileDetails](EQ(FileDetails.fileDetailsFormat), any(), any())) thenReturn fileDetails
      val result = call(controller.submitSubmitterInfo, fakeRequestSubmit)
      status(result) shouldBe Status.BAD_REQUEST
    }

    "return 400 when the all data exists but Email Address is in Invalid format" in {
      val submitterInfo = Seq(
        "fullName"     -> "Fullname",
        "contactPhone" -> "07923456708",
        "email"        -> "abc.xyz"
      )

      val fakeRequestSubmit =
        addToken(FakeRequest("POST", "/submitSubmitterInfo").withFormUrlEncodedBody(submitterInfo: _*))
      when(auth.authorise[Option[AffinityGroup]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(AffinityGroup.Organisation))
      whenF(cache.read[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenFailWith ExpiredSession("")
      when(auth.authorise[Option[AffinityGroup]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(AffinityGroup.Organisation))
      when(auth.authorise[Option[AffinityGroup]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(AffinityGroup.Organisation))
      whenF(cache.read[FileDetails](EQ(FileDetails.fileDetailsFormat), any(), any())) thenReturn fileDetails
      val result = call(controller.submitSubmitterInfo, fakeRequestSubmit)
      status(result) shouldBe Status.BAD_REQUEST
    }
    "return 400 when the empty fields of data exists" in {
      val submitterInfo = Seq(
        "fullName"     -> "",
        "contactPhone" -> "",
        "email"        -> ""
      )
      val fakeRequestSubmit =
        addToken(FakeRequest("POST", "/submitSubmitterInfo").withFormUrlEncodedBody(submitterInfo: _*))
      when(auth.authorise[Option[AffinityGroup]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(AffinityGroup.Organisation))
      whenF(cache.read[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenFailWith ExpiredSession("")
      when(auth.authorise[Option[AffinityGroup]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(AffinityGroup.Organisation))
      whenF(cache.read[FileDetails](EQ(FileDetails.fileDetailsFormat), any(), any())) thenReturn fileDetails
      val result = call(controller.submitSubmitterInfo, fakeRequestSubmit)
      status(result) shouldBe Status.BAD_REQUEST
    }

    "return 303 when all of the data exists & valid" in {
      val submitterInfo = SubmitterInfo("Fullname", None, "07923456708", EmailAddress("abc@xyz.com"), None)
      val dataSeq = SubmitterInfoForm.submitterInfoForm.fill(submitterInfo).data.toSeq
      val fakeRequestSubmit =
        addToken(FakeRequest("POST", "/submitSubmitterInfo").withFormUrlEncodedBody(dataSeq: _*))

      when(auth.authorise[Option[AffinityGroup]](any(), any())(any(), any())) thenReturn Future.successful(
        Some(AffinityGroup.Organisation))
      when(cache.readOption[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn Future.successful(None)
      when(cache.save[SubmitterInfo](any())(EQ(SubmitterInfo.format), any(), any())) thenReturn Future.successful(
        CacheMap("cache", Map.empty[String, JsValue]))
      whenF(cache.read[CompleteXMLInfo](EQ(CompleteXMLInfo.format), any(), any())) thenReturn keyXMLInfo
      when(cache.save[CBCId](any())(EQ(CBCId.cbcIdFormat), any(), any())) thenReturn Future.successful(
        CacheMap("cache", Map.empty[String, JsValue]))
      when(cache.readOption[AgencyBusinessName](EQ(AgencyBusinessName.format), any(), any())) thenReturn Future
        .successful(Some(AgencyBusinessName("Colm Cavanagh ltd")))
      whenF(cache.read[SubmitterInfo](EQ(SubmitterInfo.format), any(), any())) thenReturn submitterInfo
      val result = call(controller.submitSubmitterInfo, fakeRequestSubmit)
      val returnVal = status(result)
      returnVal shouldBe Status.SEE_OTHER

      verify(cache, times(1)).read(EQ(CompleteXMLInfo.format), any(), any())
      verify(cache).save(any())(EQ(SubmitterInfo.format), any(), any())
    }

    "return 303 when Email Address is valid" when {
      "the AffinityGroup is Organisation it" should {
        "redirect to submit-summary if a cbcId exists" in {
          val submitterInfo = SubmitterInfo("Billy Bob", None, "07923456708", EmailAddress("abc@xyz.com"), None)
          val dataSeq = SubmitterInfoForm.submitterInfoForm.fill(submitterInfo).data.toSeq
          val fakeRequestSubmit =
            addToken(FakeRequest("POST", "/submitSubmitterInfo").withFormUrlEncodedBody(dataSeq: _*))
          when(auth.authorise[Option[AffinityGroup]](any(), any())(any(), any())) thenReturn Future.successful(
            Some(AffinityGroup.Organisation))
          whenF(cache.read[SubmitterInfo](EQ(SubmitterInfo.format), any(), any())) thenReturn SubmitterInfo(
            "name",
            None,
            "0123123123",
            EmailAddress("max@max.com"),
            Some(AffinityGroup.Organisation))
          when(cache.readOption[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn Future.successful(
            CBCId.create(100).toOption)
          when(cache.save[SubmitterInfo](any())(any(), any(), any())) thenReturn Future.successful(
            CacheMap("cache", Map.empty[String, JsValue]))
          whenF(cache.read[CompleteXMLInfo](EQ(CompleteXMLInfo.format), any(), any())) thenReturn keyXMLInfo
          when(cache.save[CBCId](any())(EQ(CBCId.cbcIdFormat), any(), any())) thenReturn Future.successful(
            CacheMap("cache", Map.empty[String, JsValue]))
          when(cache.readOption[AgencyBusinessName](EQ(AgencyBusinessName.format), any(), any())) thenReturn Future
            .successful(Some(AgencyBusinessName("Colm Cavanagh ltd")))
          val result = call(controller.submitSubmitterInfo, fakeRequestSubmit)

          result.header.headers("Location") should endWith("/submission/summary")
          status(result) shouldBe Status.SEE_OTHER
        }

        "redirect to enter-cbcId if a cbcid does not exist" in {
          val submitterInfo = SubmitterInfo("Billy Bob", None, "07923456708", EmailAddress("abc@xyz.com"), None)
          val dataSeq = SubmitterInfoForm.submitterInfoForm.fill(submitterInfo).data.toSeq
          val fakeRequestSubmit =
            addToken(FakeRequest("POST", "/submitSubmitterInfo").withFormUrlEncodedBody(dataSeq: _*))
          when(auth.authorise[Option[AffinityGroup]](any(), any())(any(), any())) thenReturn Future.successful(
            Some(AffinityGroup.Organisation))
          whenF(cache.read[SubmitterInfo](EQ(SubmitterInfo.format), any(), any())) thenReturn SubmitterInfo(
            "name",
            None,
            "0123123123",
            EmailAddress("max@max.com"),
            Some(AffinityGroup.Organisation))
          when(cache.readOption[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn Future.successful(None)
          when(cache.save[SubmitterInfo](any())(any(), any(), any())) thenReturn Future.successful(
            CacheMap("cache", Map.empty[String, JsValue]))
          whenF(cache.read[CompleteXMLInfo](EQ(CompleteXMLInfo.format), any(), any())) thenReturn keyXMLInfo
          when(cache.save[CBCId](any())(EQ(CBCId.cbcIdFormat), any(), any())) thenReturn Future.successful(
            CacheMap("cache", Map.empty[String, JsValue]))
          when(cache.readOption[AgencyBusinessName](EQ(AgencyBusinessName.format), any(), any())) thenReturn Future
            .successful(Some(AgencyBusinessName("Colm Cavanagh ltd")))
          val result = call(controller.submitSubmitterInfo, fakeRequestSubmit)

          result.header.headers("Location") should endWith("/cbc-id/entry-form")
          status(result) shouldBe Status.SEE_OTHER
        }
      }

      "the AffinityGroup is Agent it" should {
        "redirect to enter-known-facts if a cbcid does not exist" in {
          val submitterInfo = SubmitterInfo("Billy Bob", None, "07923456708", EmailAddress("abc@xyz.com"), None)
          val dataSeq = SubmitterInfoForm.submitterInfoForm.fill(submitterInfo).data.toSeq
          val fakeRequestSubmit =
            addToken(FakeRequest("POST", "/submitSubmitterInfo").withFormUrlEncodedBody(dataSeq: _*))
          when(auth.authorise[Option[AffinityGroup]](any(), any())(any(), any())) thenReturn Future.successful(
            Some(AffinityGroup.Agent))
          whenF(cache.read[SubmitterInfo](EQ(SubmitterInfo.format), any(), any())) thenReturn SubmitterInfo(
            "name",
            None,
            "0123123123",
            EmailAddress("max@max.com"),
            Some(AffinityGroup.Organisation))
          when(cache.readOption[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn Future.successful(None)
          when(cache.save[SubmitterInfo](any())(any(), any(), any())) thenReturn Future.successful(
            CacheMap("cache", Map.empty[String, JsValue]))
          whenF(cache.read[CompleteXMLInfo](EQ(CompleteXMLInfo.format), any(), any())) thenReturn keyXMLInfo
          when(cache.save[CBCId](any())(EQ(CBCId.cbcIdFormat), any(), any())) thenReturn Future.successful(
            CacheMap("cache", Map.empty[String, JsValue]))
          when(cache.readOption[AgencyBusinessName](EQ(AgencyBusinessName.format), any(), any())) thenReturn Future
            .successful(Some(AgencyBusinessName("Colm Cavanagh ltd")))
          val result = call(controller.submitSubmitterInfo, fakeRequestSubmit)

          result.header.headers("Location") should endWith("/agent/verify-form")
          status(result) shouldBe Status.SEE_OTHER
        }
      }
    }
  }

  "The submission controller" should {
    "provide a method to generate the metadata that" should {
      "return a list of errors for each of the missing cache values" in {
        when(cache.readOption[GGId](EQ(GGId.format), any(), any())) thenReturn Future.successful(
          Some(GGId("ggid", "type")))
        whenF(cache.read[BusinessPartnerRecord]) thenFailWith ExpiredSession("")
        whenF(cache.read[TIN]) thenFailWith ExpiredSession("")
        whenF(cache.read[CBCId]) thenFailWith ExpiredSession("@")
        whenF(cache.read[Hash]) thenFailWith ExpiredSession("")
        whenF(cache.read[FileId]) thenFailWith ExpiredSession("")
        whenF(cache.read[EnvelopeId]) thenFailWith ExpiredSession("")
        whenF(cache.read[SubmitterInfo]) thenFailWith ExpiredSession("")
        whenF(cache.read[FilingType]) thenFailWith ExpiredSession("")
        whenF(cache.read[UltimateParentEntity]) thenFailWith ExpiredSession("")
        whenF(cache.read[FileMetadata]) thenFailWith ExpiredSession("")

        Await
          .result(generateMetadataFile(cache, creds), 10.second)
          .fold(
            errors => errors.toList.size shouldBe 10,
            _ => fail("this should have failed")
          )

        whenF(cache.read[FileId]) thenReturn FileId("fileId")
        Await
          .result(generateMetadataFile(cache, creds), 10.second)
          .fold(
            errors => errors.toList.size shouldBe 9,
            _ => fail("this should have failed")
          )

        whenF(cache.read[EnvelopeId]) thenReturn EnvelopeId("yeah")
        Await
          .result(generateMetadataFile(cache, creds), 10.second)
          .fold(
            errors => errors.toList.size shouldBe 8,
            _ => fail("this should have failed")
          )
      }

      "return a Metadata object if all succeeds" in {
        when(cache.readOption[GGId](EQ(GGId.format), any(), any())) thenReturn Future.successful(
          Some(GGId("ggid", "type")))
        whenF(cache.read[BusinessPartnerRecord]) thenReturn bpr
        whenF(cache.read[TIN]) thenReturn TIN("utr", "")
        whenF(cache.read[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn CBCId.create(1).getOrElse(fail("argh"))
        whenF(cache.read[Hash]) thenReturn Hash("hash")
        whenF(cache.read[FileId]) thenReturn FileId("yeah")
        whenF(cache.read[EnvelopeId]) thenReturn EnvelopeId("id")
        whenF(cache.read[SubmitterInfo]) thenReturn SubmitterInfo(
          "name",
          None,
          "0123123123",
          EmailAddress("max@max.com"),
          Some(AffinityGroup.Organisation))
        whenF(cache.read[FilingType]) thenReturn FilingType(CBC701)
        whenF(cache.read[UltimateParentEntity]) thenReturn UltimateParentEntity("yeah")
        whenF(cache.read[FileMetadata]) thenReturn FileMetadata(
          "asdf",
          "lkjasdf",
          "lkj",
          "lkj",
          10,
          "lkjasdf",
          JsNull,
          "")

        Await
          .result(generateMetadataFile(cache, creds), 10.second)
          .leftMap(
            errors => fail(s"There should be no errors: $errors")
          )
      }
    }

    "provide a 'submitSummary' Action that" should {
      val fakeRequestSubmitSummary = addToken(FakeRequest("GET", "/submitSummary"))
      "return 303 if generating the metadata fails redirecting to session expired page" in {
        when(auth.authorise(any(), any[Retrieval[Option[Credentials] ~ Option[AffinityGroup]]]())(any(), any()))
          .thenReturn(Future.successful(
            new ~[Option[Credentials], Option[AffinityGroup]](Some(creds), Some(AffinityGroup.Organisation))))
        when(cache.readOption[GGId](EQ(GGId.format), any(), any())) thenReturn Future.successful(
          Some(GGId("ggid", "type")))
        whenF(cache.read[CompleteXMLInfo](EQ(CompleteXMLInfo.format), any(), any())) thenReturn keyXMLInfo
        whenF(cache.read[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format), any(), any())) thenReturn bpr
        whenF(cache.read[TIN](EQ(TIN.format), any(), any())) thenReturn TIN("utr", "")
        whenF(cache.read[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn CBCId.create(1).getOrElse(fail("argh"))
        whenF(cache.read[Hash](EQ(Hash.format), any(), any())) thenReturn Hash("hash")
        whenF(cache.read[FileId](EQ(FileId.fileIdFormat), any(), any())) thenReturn FileId("yeah")
        whenF(cache.read[EnvelopeId](EQ(EnvelopeId.format), any(), any())) thenReturn EnvelopeId("id")
        whenF(cache.read[SubmitterInfo](EQ(SubmitterInfo.format), any(), any())) thenReturn SubmitterInfo(
          "name",
          None,
          "0123123123",
          EmailAddress("max@max.com"),
          None)
        whenF(cache.read[FilingType](EQ(FilingType.format), any(), any())) thenReturn FilingType(CBC701)
        whenF(cache.read[UltimateParentEntity](EQ(UltimateParentEntity.format), any(), any())) thenFailWith ExpiredSession(
          "nope")
        whenF(cache.read[FileMetadata](EQ(FileMetadata.fileMetadataFormat), any(), any())) thenReturn FileMetadata(
          "asdf",
          "lkjasdf",
          "lkj",
          "lkj",
          10,
          "lkjasdf",
          JsNull,
          "")
        when(cache.save[SummaryData](any())(any(), any(), any())) thenReturn Future.successful(
          CacheMap("cache", Map.empty[String, JsValue]))
        whenF(cache.read[FileDetails](EQ(FileDetails.fileDetailsFormat), any(), any())) thenReturn fileDetails
        val result = controller.submitSummary(fakeRequestSubmitSummary)
        status(result) shouldBe Status.SEE_OTHER
        result.header.headers("Location") should endWith("/session-expired")
      }

      "return 200 if everything succeeds" in {
        val file = File.createTempFile("test", "test")

        when(auth.authorise(any(), any[Retrieval[Option[Credentials] ~ Option[AffinityGroup]]]())(any(), any()))
          .thenReturn(Future.successful(
            new ~[Option[Credentials], Option[AffinityGroup]](Some(creds), Some(AffinityGroup.Organisation))))
        when(cache.readOption[GGId](EQ(GGId.format), any(), any())) thenReturn Future.successful(
          Some(GGId("ggid", "type")))
        whenF(cache.read[CompleteXMLInfo](EQ(CompleteXMLInfo.format), any(), any())) thenReturn keyXMLInfo
        whenF(cache.read[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format), any(), any())) thenReturn bpr
        whenF(cache.read[TIN](EQ(TIN.format), any(), any())) thenReturn TIN("utr", "")
        whenF(cache.read[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn CBCId.create(1).getOrElse(fail("argh"))
        whenF(cache.read[Hash](EQ(Hash.format), any(), any())) thenReturn Hash("hash")
        whenF(cache.read[FileId](EQ(FileId.fileIdFormat), any(), any())) thenReturn FileId("yeah")
        whenF(cache.read[EnvelopeId](EQ(EnvelopeId.format), any(), any())) thenReturn EnvelopeId("id")
        whenF(cache.read[SubmitterInfo](EQ(SubmitterInfo.format), any(), any())) thenReturn SubmitterInfo(
          "name",
          None,
          "0123123123",
          EmailAddress("max@max.com"),
          None)
        whenF(cache.read[FilingType](EQ(FilingType.format), any(), any())) thenReturn FilingType(CBC701)
        whenF(cache.read[UltimateParentEntity](EQ(UltimateParentEntity.format), any(), any())) thenReturn UltimateParentEntity(
          "upe")
        whenF(cache.read[FileMetadata](EQ(FileMetadata.fileMetadataFormat), any(), any())) thenReturn FileMetadata(
          "asdf",
          "lkjasdf",
          "lkj",
          "lkj",
          10,
          "lkjasdf",
          JsNull,
          "")
        when(fus.getFile(anyString, anyString)(any())) thenReturn EitherT[Future, CBCErrors, File](
          Future.successful(Right(file)))
        when(cache.save[SummaryData](any())(any(), any(), any())) thenReturn Future.successful(
          CacheMap("cache", Map.empty[String, JsValue]))
        whenF(cache.read[FileDetails](EQ(FileDetails.fileDetailsFormat), any(), any())) thenReturn fileDetails
        status(controller.submitSummary(fakeRequestSubmitSummary)) shouldBe Status.OK

        file.deleteOnExit()
      }
    }

    "provide an action '/confirm'" which {
      "returns a 303 when the call to the cache fails and redirect to session expired" in {
        val summaryData = SummaryData(bpr, submissionData, keyXMLInfo, false)
        val fakeRequestSubmitSummary = addToken(FakeRequest("POST", "/confirm ").withJsonBody(Json.toJson("{}")))
        when(auth.authorise(any(), any[Retrieval[Credentials ~ Option[AffinityGroup]]]())(any(), any()))
          .thenReturn(
            Future.successful(new ~[Credentials, Option[AffinityGroup]](creds, Some(AffinityGroup.Organisation))))
        whenF(cache.read[SummaryData](EQ(SummaryData.format), any(), any())) thenReturn summaryData
        whenF(cache.read[CompleteXMLInfo](EQ(CompleteXMLInfo.format), any(), any())) thenFailWith ExpiredSession("")
        val result = controller.confirm(fakeRequestSubmitSummary)
        status(result) shouldBe Status.SEE_OTHER
        result.header.headers("Location") should endWith("/session-expired")
      }

      "returns a 500 when the call to file-upload fails" in {
        val summaryData = SummaryData(bpr, submissionData, keyXMLInfo, false)
        val fakeRequestSubmitSummary = addToken(FakeRequest("POST", "/confirm ").withJsonBody(Json.toJson("{}")))
        when(auth.authorise(any(), any[Retrieval[Credentials ~ Option[AffinityGroup]]]())(any(), any()))
          .thenReturn(
            Future.successful(new ~[Credentials, Option[AffinityGroup]](creds, Some(AffinityGroup.Organisation))))
        whenF(cache.read[SummaryData](EQ(SummaryData.format), any(), any())) thenReturn summaryData
        whenF(cache.read[CompleteXMLInfo](EQ(CompleteXMLInfo.format), any(), any())) thenReturn keyXMLInfo
        whenF(fus.uploadMetadataAndRoute(any())(any(), any())) thenFailWith UnexpectedState("fail")
        val result = Await.result(controller.confirm(fakeRequestSubmitSummary), 50.seconds)
        status(result) shouldBe Status.INTERNAL_SERVER_ERROR
      }

      "returns a 500 when the call to save the docRefIds fail" in {
        val summaryData = SummaryData(bpr, submissionData, keyXMLInfo, doesCreationTimeStampHaveMillis = false)
        val fakeRequestSubmitSummary = addToken(FakeRequest("POST", "/confirm ").withJsonBody(Json.toJson("{}")))
        when(auth.authorise(any(), any[Retrieval[Credentials ~ Option[AffinityGroup]]]())(any(), any()))
          .thenReturn(
            Future.successful(new ~[Credentials, Option[AffinityGroup]](creds, Some(AffinityGroup.Organisation))))
        whenF(cache.read[SummaryData](EQ(SummaryData.format), any(), any())) thenReturn summaryData
        whenF(cache.read[CompleteXMLInfo](EQ(CompleteXMLInfo.format), any(), any())) thenReturn keyXMLInfo
        whenF(fus.uploadMetadataAndRoute(any())(any(), any())) thenReturn "ok"
        whenF(docRefService.saveDocRefId(any())(any())) thenReturn UnexpectedState("fails!")
        whenF(docRefService.saveCorrDocRefID(any(), any())(any())) thenReturn UnexpectedState("fails!")
        status(controller.confirm(fakeRequestSubmitSummary)) shouldBe Status.INTERNAL_SERVER_ERROR
      }

      "returns 303 when the there is data and " should {
        "call saveReportingEntityData when the submissionType is OECD1" in {
          val summaryData = SummaryData(bpr, submissionData, keyXMLInfo, doesCreationTimeStampHaveMillis = false)
          val fakeRequestSubmitSummary = addToken(FakeRequest("POST", "/confirm ").withJsonBody(Json.toJson("{}")))
          when(auth.authorise(any(), any[Retrieval[Option[Credentials] ~ Option[AffinityGroup]]]())(any(), any()))
            .thenReturn(Future.successful(
              new ~[Option[Credentials], Option[AffinityGroup]](Some(creds), Some(AffinityGroup.Organisation))))
          whenF(cache.read[SummaryData](EQ(SummaryData.format), any(), any())) thenReturn summaryData
          when(fus.uploadMetadataAndRoute(any())(any(), any())) thenReturn EitherT[Future, CBCErrors, String](
            Future.successful(Right("routed")))
          whenF(cache.read[CompleteXMLInfo](EQ(CompleteXMLInfo.format), any(), any())) thenReturn keyXMLInfo
          when(cache.save[SubmissionDate](any())(EQ(SubmissionDate.format), any(), any())) thenReturn Future.successful(
            CacheMap("cache", Map.empty[String, JsValue]))
          whenF(fus.uploadMetadataAndRoute(any())(any(), any())) thenReturn "ok"
          whenF(reportingEntity.saveReportingEntityData(any())(any())) thenReturn ()
          when(docRefService.saveCorrDocRefID(any(), any())(any())) thenReturn OptionT.none[Future, UnexpectedState]
          when(docRefService.saveDocRefId(any())(any())) thenReturn OptionT.none[Future, UnexpectedState]
          when(messageRefIdService.saveMessageRefId(any())(any())) thenReturn OptionT.none[Future, UnexpectedState]
          when(cache.readOption[GGId](EQ(GGId.format), any(), any())) thenReturn Future.successful(
            Some(GGId("ggid", "type")))
          when(auditMock.sendExtendedEvent(any())(any(), any())) thenReturn Future.successful(AuditResult.Success)
          status(controller.confirm(fakeRequestSubmitSummary)) shouldBe Status.SEE_OTHER
          verify(reportingEntity).saveReportingEntityData(any())(any())
          verify(messageRefIdService, times(1)).saveMessageRefId(any())(any())
        }
        "call updateReportingEntityData when the submissionType is OECD[023]" in {
          val summaryData = SummaryData(bpr, submissionData, keyXMLInfo, doesCreationTimeStampHaveMillis = false)
          val fakeRequestSubmitSummary = addToken(FakeRequest("POST", "/confirm ").withJsonBody(Json.toJson("{}")))
          lazy val updateXml = keyXMLInfo.copy(
            reportingEntity =
              keyXMLInfo.reportingEntity.copy(docSpec = keyXMLInfo.reportingEntity.docSpec.copy(docType = OECD2)))
          when(auth.authorise(any(), any[Retrieval[Option[Credentials] ~ Option[AffinityGroup]]]())(any(), any()))
            .thenReturn(Future.successful(
              new ~[Option[Credentials], Option[AffinityGroup]](Some(creds), Some(AffinityGroup.Organisation))))
          whenF(cache.read[SummaryData](EQ(SummaryData.format), any(), any())) thenReturn summaryData
          when(fus.uploadMetadataAndRoute(any())(any(), any())) thenReturn EitherT[Future, CBCErrors, String](
            Future.successful(Right("routed")))
          whenF(cache.read[CompleteXMLInfo](EQ(CompleteXMLInfo.format), any(), any())) thenReturn updateXml
          when(cache.save[SubmissionDate](any())(EQ(SubmissionDate.format), any(), any())) thenReturn Future.successful(
            CacheMap("cache", Map.empty[String, JsValue]))
          whenF(fus.uploadMetadataAndRoute(any())(any(), any())) thenReturn "ok"
          whenF(reportingEntity.updateReportingEntityData(any())(any())) thenReturn ()
          when(docRefService.saveCorrDocRefID(any(), any())(any())) thenReturn OptionT.none[Future, UnexpectedState]
          when(docRefService.saveDocRefId(any())(any())) thenReturn OptionT.none[Future, UnexpectedState]
          when(messageRefIdService.saveMessageRefId(any())(any())) thenReturn OptionT.none[Future, UnexpectedState]
          when(cache.readOption[GGId](EQ(GGId.format), any(), any())) thenReturn Future.successful(
            Some(GGId("ggid", "type")))
          when(auditMock.sendEvent(any())(any(), any())) thenReturn Future.successful(AuditResult.Success)
          status(controller.confirm(fakeRequestSubmitSummary)) shouldBe Status.SEE_OTHER
          verify(reportingEntity).updateReportingEntityData(any())(any())
          verify(messageRefIdService, times(1)).saveMessageRefId(any())(any())
        }
        "return 500 if saveMessageRefId fails and does NOT callsaveReportingEntityData " in {
          val summaryData = SummaryData(bpr, submissionData, keyXMLInfo, doesCreationTimeStampHaveMillis = false)
          val fakeRequestSubmitSummary = addToken(FakeRequest("POST", "/confirm ").withJsonBody(Json.toJson("{}")))
          when(auth.authorise(any(), any[Retrieval[Credentials ~ Option[AffinityGroup]]]())(any(), any()))
            .thenReturn(
              Future.successful(new ~[Credentials, Option[AffinityGroup]](creds, Some(AffinityGroup.Organisation))))
          whenF(cache.read[SummaryData](EQ(SummaryData.format), any(), any())) thenReturn summaryData
          when(fus.uploadMetadataAndRoute(any())(any(), any())) thenReturn EitherT[Future, CBCErrors, String](
            Future.successful(Right("routed")))
          whenF(cache.read[CompleteXMLInfo](EQ(CompleteXMLInfo.format), any(), any())) thenReturn keyXMLInfo
          when(cache.save[SubmissionDate](any())(EQ(SubmissionDate.format), any(), any())) thenReturn Future.successful(
            CacheMap("cache", Map.empty[String, JsValue]))
          whenF(fus.uploadMetadataAndRoute(any())(any(), any())) thenReturn "ok"
          whenF(reportingEntity.saveReportingEntityData(any())(any())) thenReturn ()
          when(docRefService.saveCorrDocRefID(any(), any())(any())) thenReturn OptionT.none[Future, UnexpectedState]
          when(docRefService.saveDocRefId(any())(any())) thenReturn OptionT.none[Future, UnexpectedState]
          whenF(messageRefIdService.saveMessageRefId(any())(any())) thenReturn UnexpectedState("fails!")
          when(cache.readOption[GGId](EQ(GGId.format), any(), any())) thenReturn Future.successful(
            Some(GGId("ggid", "type")))
          when(auditMock.sendExtendedEvent(any())(any(), any())) thenReturn Future.successful(AuditResult.Success)
          status(controller.confirm(fakeRequestSubmitSummary)) shouldBe Status.INTERNAL_SERVER_ERROR
          verify(reportingEntity, times(0)).saveReportingEntityData(any())(any())
          verify(messageRefIdService, times(1)).saveMessageRefId(any())(any())
        }
      }
    }

    "provide an action 'submitSuccessReceipt'" which {
      "returns a 303 Redirect if it fails to read from the cache" when {
        "looking for the SummaryData" in {
          when(auth.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful(())
          val fakeRequestSubmitSummary = addToken(FakeRequest("GET", "/submitSuccessReceipt"))

          whenF(cache.read[SummaryData](EQ(SummaryData.format), any(), any())) thenFailWith ExpiredSession("")
          whenF(cache.read[SubmissionDate](EQ(SubmissionDate.format), any(), any())) thenReturn SubmissionDate(
            LocalDateTime.now())
          val result = controller.submitSuccessReceipt("Organisation")(fakeRequestSubmitSummary)
          status(result) shouldBe Status.SEE_OTHER
          result.header.headers("Location") should endWith("/session-expired")
        }

        "looking for the SubmissionDate" in {
          when(auth.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful(())
          val summaryData = SummaryData(bpr, submissionData, keyXMLInfo, doesCreationTimeStampHaveMillis = false)
          val fakeRequestSubmitSummary = addToken(FakeRequest("GET", "/submitSuccessReceipt"))

          whenF(cache.read[SummaryData](EQ(SummaryData.format), any(), any())) thenReturn summaryData
          whenF(cache.read[SubmissionDate](EQ(SubmissionDate.format), any(), any())) thenFailWith ExpiredSession("")
          val result = controller.submitSuccessReceipt("Organisation")(fakeRequestSubmitSummary)
          status(result) shouldBe Status.SEE_OTHER
          result.header.headers("Location") should endWith("/session-expired")
        }

        "looking for the CBCId" in {
          when(auth.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful(())
          val summaryData = SummaryData(bpr, submissionData, keyXMLInfo, doesCreationTimeStampHaveMillis = false)
          val fakeRequestSubmitSummary = addToken(FakeRequest("GET", "/submitSuccessReceipt"))

          whenF(cache.read[SummaryData](EQ(SummaryData.format), any(), any())) thenReturn summaryData
          whenF(cache.read[SubmissionDate](EQ(SubmissionDate.format), any(), any())) thenReturn SubmissionDate(
            LocalDateTime.now())
          whenF(cache.read[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenFailWith ExpiredSession("")
          val result = controller.submitSuccessReceipt("Organisation")(fakeRequestSubmitSummary)
          status(result) shouldBe Status.SEE_OTHER
          result.header.headers("Location") should endWith("/session-expired")
        }
      }

      "sends an email" in {
        val summaryData = SummaryData(bpr, submissionData, keyXMLInfo, doesCreationTimeStampHaveMillis = false)
        val fakeRequestSubmitSummary = addToken(FakeRequest("GET", "/submitSuccessReceipt"))
        when(auth.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful(())
        whenF(cache.read[SummaryData](EQ(SummaryData.format), any(), any())) thenReturn summaryData
        whenF(cache.read[SubmissionDate](EQ(SubmissionDate.format), any(), any())) thenReturn SubmissionDate(
          LocalDateTime.now())
        when(cache.readOption[GGId](EQ(GGId.format), any(), any())) thenReturn Future.successful(
          Some(GGId("ggid", "type")))
        whenF(mockEmailService.sendEmail(any())(any())) thenReturn true
        when(cache.save[ConfirmationEmailSent](any())(
          EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat),
          any(),
          any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String, JsValue]))
        when(cache.readOption[ConfirmationEmailSent](
          EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat),
          any(),
          any())) thenReturn Future.successful(None)
        whenF(cache.read[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn CBCId.create(1).getOrElse(fail("argh"))
        when(cache.clear(any())) thenReturn Future.successful(true)
        val result: Future[Result] = controller.submitSuccessReceipt("Organisation")(fakeRequestSubmitSummary)
        status(result) shouldBe Status.OK
        verify(mockEmailService, times(1)).sendEmail(any())(any())
        verify(cache, times(1)).save(any())(EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat), any(), any())
        val webPageAsString = contentAsString(result)
        webPageAsString should not include (getMessages(fakeRequestSubmitSummary)(
          "submitSuccessReceipt.sendAnotherReport.link"))
      }

      "will still return a 200 if the email fails" in {
        val summaryData = SummaryData(bpr, submissionData, keyXMLInfo, doesCreationTimeStampHaveMillis = false)
        val fakeRequestSubmitSummary = addToken(FakeRequest("GET", "/submitSuccessReceipt"))
        when(auth.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful(())
        when(cache.readOption[GGId](EQ(GGId.format), any(), any())) thenReturn Future.successful(
          Some(GGId("ggid", "type")))
        when(cache.save[ConfirmationEmailSent](any())(
          EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat),
          any(),
          any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String, JsValue]))
        whenF(mockEmailService.sendEmail(any())(any())) thenReturn false
        when(cache.readOption[ConfirmationEmailSent](
          EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat),
          any(),
          any())) thenReturn Future.successful(None)
        whenF(cache.read[SummaryData](EQ(SummaryData.format), any(), any())) thenReturn summaryData
        whenF(cache.read[SubmissionDate](EQ(SubmissionDate.format), any(), any())) thenReturn SubmissionDate(
          LocalDateTime.now())
        whenF(cache.read[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn CBCId.create(1).getOrElse(fail("argh"))
        when(cache.clear(any())) thenReturn Future.successful(true)
        val result: Future[Result] = controller.submitSuccessReceipt("Organisation")(fakeRequestSubmitSummary)
        status(result) shouldBe Status.OK
        verify(mockEmailService, times(1)).sendEmail(any())(any())
        verify(cache, times(0)).save(any())(EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat), any(), any())
        val webPageAsString = contentAsString(result)
        webPageAsString should not include (getMessages(fakeRequestSubmitSummary)(
          "submitSuccessReceipt.sendAnotherReport.link"))
      }

      "will write  a ConfirmationEmailSent to the cache if an email is sent" in {
        val summaryData = SummaryData(bpr, submissionData, keyXMLInfo, doesCreationTimeStampHaveMillis = false)
        val fakeRequestSubmitSummary = addToken(FakeRequest("GET", "/submitSuccessReceipt"))
        when(auth.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful(())
        when(cache.readOption[GGId](EQ(GGId.format), any(), any())) thenReturn Future.successful(
          Some(GGId("ggid", "type")))
        whenF(mockEmailService.sendEmail(any())(any())) thenReturn true
        whenF(cache.read[SummaryData](EQ(SummaryData.format), any(), any())) thenReturn summaryData
        when(cache.save[ConfirmationEmailSent](any())(
          EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat),
          any(),
          any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String, JsValue]))
        when(cache.readOption[ConfirmationEmailSent](
          EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat),
          any(),
          any())) thenReturn Future.successful(None)
        whenF(cache.read[SubmissionDate](EQ(SubmissionDate.format), any(), any())) thenReturn SubmissionDate(
          LocalDateTime.now())
        whenF(cache.read[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn CBCId.create(1).getOrElse(fail("argh"))
        when(cache.clear(any())) thenReturn Future.successful(true)
        val result: Future[Result] = controller.submitSuccessReceipt("Organisation")(fakeRequestSubmitSummary)
        status(result) shouldBe Status.OK
        verify(mockEmailService, times(1)).sendEmail(any())(any())
        verify(cache, times(1)).save(any())(EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat), any(), any())
        val webPageAsString = contentAsString(result)
        webPageAsString should not include (getMessages(fakeRequestSubmitSummary)(
          "submitSuccessReceipt.sendAnotherReport.link"))
      }

      "not send the email if it has already been sent and not save to the cache" in {
        val summaryData = SummaryData(bpr, submissionData, keyXMLInfo, doesCreationTimeStampHaveMillis = false)
        val fakeRequestSubmitSummary = addToken(FakeRequest("GET", "/submitSuccessReceipt:Organisation"))
        when(auth.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful(())
        when(cache.readOption[GGId](EQ(GGId.format), any(), any())) thenReturn Future.successful(
          Some(GGId("ggid", "type")))
        whenF(mockEmailService.sendEmail(any())(any())) thenReturn true
        when(cache.save[ConfirmationEmailSent](any())(
          EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat),
          any(),
          any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String, JsValue]))
        whenF(cache.read[SummaryData](EQ(SummaryData.format), any(), any())) thenReturn summaryData
        when(cache.readOption[ConfirmationEmailSent](
          EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat),
          any(),
          any())) thenReturn Future.successful(Some(ConfirmationEmailSent("yep")))
        whenF(cache.read[SubmissionDate](EQ(SubmissionDate.format), any(), any())) thenReturn SubmissionDate(
          LocalDateTime.now())
        whenF(cache.read[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn CBCId.create(1).getOrElse(fail("argh"))
        when(cache.clear(any())) thenReturn Future.successful(true)
        val result: Future[Result] = controller.submitSuccessReceipt("Organisation")(fakeRequestSubmitSummary)
        status(result) shouldBe Status.OK
        status(controller.submitSuccessReceipt("Organisation")(fakeRequestSubmitSummary)) shouldBe Status.OK
        verify(mockEmailService, times(0)).sendEmail(any())(any())
        verify(cache, times(0)).save(any())(EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat), any(), any())
        val webPageAsString = contentAsString(result)
        webPageAsString should not include (getMessages(fakeRequestSubmitSummary)(
          "submitSuccessReceipt.sendAnotherReport.link"))
      }

      "returns a 200 otherwise" in {
        val summaryData = SummaryData(bpr, submissionData, keyXMLInfo, doesCreationTimeStampHaveMillis = false)
        val fakeRequestSubmitSummary = addToken(FakeRequest("GET", "/submitSuccessReceipt:Organisation"))
        when(auth.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful(())
        when(cache.readOption[GGId](EQ(GGId.format), any(), any())) thenReturn Future.successful(
          Some(GGId("ggid", "type")))
        whenF(mockEmailService.sendEmail(any())(any())) thenReturn true
        when(cache.save[ConfirmationEmailSent](any())(
          EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat),
          any(),
          any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String, JsValue]))
        whenF(cache.read[SummaryData](EQ(SummaryData.format), any(), any())) thenReturn summaryData
        when(cache.readOption[ConfirmationEmailSent](
          EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat),
          any(),
          any())) thenReturn Future.successful(None)
        whenF(cache.read[SubmissionDate](EQ(SubmissionDate.format), any(), any())) thenReturn SubmissionDate(
          LocalDateTime.now())
        whenF(cache.read[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn CBCId.create(1).getOrElse(fail("argh"))
        when(cache.clear(any())) thenReturn Future.successful(true)
        val result: Future[Result] = controller.submitSuccessReceipt("Organisation")(fakeRequestSubmitSummary)
        status(result) shouldBe Status.OK
        verify(cache, times(1)).save(any())(EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat), any(), any())
        val webPageAsString = contentAsString(result)
        webPageAsString should not include (getMessages(fakeRequestSubmitSummary)(
          "submitSuccessReceipt.sendAnotherReport.link"))
      }

      "show show link to submit another report if AffinityGroup is Agent and cache.clear succeeds" in {
        val summaryData = SummaryData(bpr, submissionData, keyXMLInfo, doesCreationTimeStampHaveMillis = false)
        val fakeRequestSubmitSummary = addToken(FakeRequest("GET", "/submitSuccessReceipt:Agent"))
        when(auth.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful(())
        when(cache.readOption[GGId](EQ(GGId.format), any(), any())) thenReturn Future.successful(
          Some(GGId("ggid", "type")))
        whenF(mockEmailService.sendEmail(any())(any())) thenReturn true
        when(cache.save[ConfirmationEmailSent](any())(
          EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat),
          any(),
          any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String, JsValue]))
        whenF(cache.read[SummaryData](EQ(SummaryData.format), any(), any())) thenReturn summaryData
        when(cache.readOption[ConfirmationEmailSent](
          EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat),
          any(),
          any())) thenReturn Future.successful(None)
        whenF(cache.read[SubmissionDate](EQ(SubmissionDate.format), any(), any())) thenReturn SubmissionDate(
          LocalDateTime.now())
        whenF(cache.read[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn CBCId.create(1).getOrElse(fail("argh"))
        when(cache.clear(any())) thenReturn Future.successful(true)
        val result: Future[Result] = controller.submitSuccessReceipt("Agent")(fakeRequestSubmitSummary)
        status(result) shouldBe Status.OK
        val webPageAsString = contentAsString(result)
        webPageAsString should include(
          getMessages(fakeRequestSubmitSummary)("submitSuccessReceipt.sendAnotherReport.link"))
      }

      "show NOT show link to submit another reportf AffinityGroup is Agent but cache.clear fails" in {
        val summaryData = SummaryData(bpr, submissionData, keyXMLInfo, doesCreationTimeStampHaveMillis = false)
        val fakeRequestSubmitSummary = addToken(FakeRequest("GET", "/submitSuccessReceipt"))
        when(auth.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful(())
        when(cache.readOption[GGId](EQ(GGId.format), any(), any())) thenReturn Future.successful(
          Some(GGId("ggid", "type")))
        whenF(mockEmailService.sendEmail(any())(any())) thenReturn true
        when(cache.save[ConfirmationEmailSent](any())(
          EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat),
          any(),
          any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String, JsValue]))
        whenF(cache.read[SummaryData](EQ(SummaryData.format), any(), any())) thenReturn summaryData
        when(cache.readOption[ConfirmationEmailSent](
          EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat),
          any(),
          any())) thenReturn Future.successful(None)
        whenF(cache.read[SubmissionDate](EQ(SubmissionDate.format), any(), any())) thenReturn SubmissionDate(
          LocalDateTime.now())
        whenF(cache.read[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn CBCId.create(1).getOrElse(fail("argh"))
        when(cache.clear(any())) thenReturn Future.successful(false)
        val result: Future[Result] = controller.submitSuccessReceipt("Organisation")(fakeRequestSubmitSummary)
        status(result) shouldBe Status.OK
        val webPageAsString = contentAsString(result)
        webPageAsString should not include (getMessages(fakeRequestSubmitSummary)(
          "submitSuccessReceipt.sendAnotherReport.link"))
      }

      "show NOT show link to submit another report if AffinityGroup is NOT Agent and cache.clear succeeds" in {
        val summaryData = SummaryData(bpr, submissionData, keyXMLInfo, doesCreationTimeStampHaveMillis = false)
        val fakeRequestSubmitSummary = addToken(FakeRequest("GET", "/submitSuccessReceipt"))
        when(auth.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful(())
        when(cache.readOption[GGId](EQ(GGId.format), any(), any())) thenReturn Future.successful(
          Some(GGId("ggid", "type")))
        whenF(mockEmailService.sendEmail(any())(any())) thenReturn true
        when(cache.save[ConfirmationEmailSent](any())(
          EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat),
          any(),
          any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String, JsValue]))
        whenF(cache.read[SummaryData](EQ(SummaryData.format), any(), any())) thenReturn summaryData
        when(cache.readOption[ConfirmationEmailSent](
          EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat),
          any(),
          any())) thenReturn Future.successful(None)
        whenF(cache.read[SubmissionDate](EQ(SubmissionDate.format), any(), any())) thenReturn SubmissionDate(
          LocalDateTime.now())
        whenF(cache.read[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn CBCId.create(1).getOrElse(fail("argh"))
        when(cache.clear(any())) thenReturn Future.successful(true)
        val result: Future[Result] = controller.submitSuccessReceipt("Organisation")(fakeRequestSubmitSummary)
        status(result) shouldBe Status.OK
        val webPageAsString = contentAsString(result)
        webPageAsString should not include (getMessages(fakeRequestSubmitSummary)(
          "submitSuccessReceipt.sendAnotherReport.link"))
      }
    }

    "contain a valid dateformat" in {
      LocalDateTime
        .of(2017, 12, 1, 23, 59, 59)
        .format(controller.dateFormat)
        .replace("AM", "am")
        .replace("PM", "pm") shouldEqual "01 December 2017 at 11:59pm"
    }

    "display the audit information correctly" in {
      val sd = SummaryData(bpr, submissionData, keyXMLInfo, doesCreationTimeStampHaveMillis = false)
      val sdj = Json.toJson(sd)
      (sdj \ "submissionMetaData" \ "submissionInfo" \ "ultimateParentEntity")
        .as[String] shouldEqual "ultimateParentEntity"
      (sdj \ "submissionMetaData" \ "submissionInfo" \ "filingType").as[String] shouldEqual "PRIMARY"
      (sdj \ "submissionMetaData" \ "submitterInfo" \ "affinityGroup").as[String] shouldEqual "Agent"
    }
  }

  private def submissionData = {
    val fileInfo =
      FileInfo(FileId("id"), EnvelopeId("envelopeId"), "status", "name", "contentType", BigDecimal(0.0), "created")
    val submissionInfo = SubmissionInfo(
      "gwGredId",
      CBCId("XVCBC0000000056").get,
      "bpSafeId",
      Hash("hash"),
      "ofdsRegime",
      TIN("tin", "GB"),
      FilingType(CBC701),
      UltimateParentEntity("ultimateParentEntity")
    )
    val submitterInfo = SubmitterInfo(
      "fullName",
      Some(AgencyBusinessName("MyAgency")),
      "contactPhone",
      EmailAddress("abc@abc.com"),
      Some(AffinityGroup.Agent))
    SubmissionMetaData(submissionInfo, submitterInfo, fileInfo)
  }

  private val docRefId = "GB2016RGXVCBC0000000056CBC40120170311T090000X_7000000002OECD1ENTZ"

  private lazy val keyXMLInfo = {
    CompleteXMLInfo(
      MessageSpec(
        MessageRefID("GB2016RGXVCBC0000000056CBC40120170311T090000X").getOrElse(fail("waaaaa")),
        "GB",
        CBCId.create(99).getOrElse(fail("booo")),
        LocalDateTime.now(),
        LocalDate.parse("2017-01-30"),
        None,
        None
      ),
      ReportingEntity(
        CBC701,
        DocSpec(OECD1, DocRefId(docRefId).get, None, None),
        TIN("7000000002", "gb"),
        "name",
        None,
        EntityReportingPeriod(LocalDate.parse("2016-03-31"), LocalDate.parse("2017-03-30"))
      ),
      List(CbcReports(DocSpec(OECD1, DocRefId(docRefId).get, None, None))),
      List(AdditionalInfo(DocSpec(OECD1, DocRefId(docRefId).get, None, None), "Some Other Info")),
      Some(LocalDate.now()),
      List.empty[String],
      List.empty[String]
    )
  }

  "calling an authorised function" should {
    "return 200" when {
      "calling notRegistered" in {
        val request = addToken(FakeRequest())
        when(auth.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful((): Unit)
        val result = controller.notRegistered(request)
        status(result) shouldBe Status.OK
        val webPageAsString = contentAsString(result)
        webPageAsString should include(getMessages(request)("notRegistered.heading"))
      }

      "calling noIndividuals" in {
        val request = addToken(FakeRequest())
        when(auth.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful((): Unit)
        val result = controller.noIndividuals(request)
        status(result) shouldBe Status.OK
        val webPageAsString = contentAsString(result)
        webPageAsString should include(getMessages(request)("notAuthorised.Individual.message.link"))
      }

      "calling noAssistants" in {
        val request = addToken(FakeRequest())
        when(auth.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful((): Unit)
        val result = controller.noAssistants(request)
        status(result) shouldBe Status.OK
        val webPageAsString = contentAsString(result)
        webPageAsString should include(getMessages(request)("notAuthorised.assistant.message"))
      }

      "calling upe" in {
        val request = addToken(FakeRequest())
        when(auth.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful((): Unit)
        val result = controller.upe(request)
        status(result) shouldBe Status.OK
        val webPageAsString = contentAsString(result)
        webPageAsString should include(getMessages(request)("submitInfoUltimateParentEntity.mainHeading"))
      }

      "calling utr" in {
        val request = addToken(FakeRequest())
        when(auth.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful((): Unit)
        val result = controller.utr(request)
        status(result) shouldBe Status.OK
        val webPageAsString = contentAsString(result)
        webPageAsString should include(getMessages(request)("utrCheck.mainHeading"))
      }

      "calling enterCompanyName" in {
        val request = addToken(FakeRequest())
        when(auth.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful((): Unit)
        whenF(cache.read[FileDetails](EQ(FileDetails.fileDetailsFormat), any(), any())) thenReturn fileDetails
        whenF(cache.read[AgencyBusinessName](EQ(AgencyBusinessName.format), any(), any())) thenReturn AgencyBusinessName(
          "Company")
        val result = controller.enterCompanyName(request)
        status(result) shouldBe Status.OK
        val webPageAsString = contentAsString(result)
        webPageAsString should include(getMessages(request)("enterCompanyName.mainHeading"))
      }
    }
  }

  "calling saveCompanyName" should {
    "return 303 if valid company details passed in request" in {
      val data = "companyName" -> "Any Old Co"
      val request = addToken(FakeRequest("POST", "/")).withFormUrlEncodedBody(data)
      when(auth.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful((): Unit)
      when(cache.save(any())(any(), any(), any())) thenReturn Future.successful(
        CacheMap("", Map.empty[String, JsValue]))
      val result = call(controller.saveCompanyName, request)
      status(result) shouldBe Status.SEE_OTHER
    }

    "return 400 if company details in request are invalid" in {
      val data = "sas" -> "Any Old Iron"
      val request = addToken(FakeRequest("POST", "/")).withFormUrlEncodedBody(data)
      when(auth.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful((): Unit)
      when(cache.save(any())(any(), any(), any())) thenReturn Future.successful(
        CacheMap("", Map.empty[String, JsValue]))
      whenF(cache.read[FileDetails](EQ(FileDetails.fileDetailsFormat), any(), any())) thenReturn fileDetails
      val result = call(controller.saveCompanyName, request)
      status(result) shouldBe Status.BAD_REQUEST
    }
  }
}
