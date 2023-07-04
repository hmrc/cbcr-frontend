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
import org.mockito.ArgumentMatchersSugar.{*, any}
import org.mockito.IdiomaticMockito
import org.mockito.cats.IdiomaticMockitoCats.StubbingOpsCats
import org.mockito.cats.MockitoCats
import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.i18n.{Messages, MessagesApi}
import play.api.libs.json.{JsNull, JsValue, Json}
import play.api.mvc.MessagesControllerComponents
import play.api.test.FakeRequest
import play.api.test.Helpers.{call, contentAsString, header, status, writeableOf_AnyContentAsFormUrlEncoded}
import play.api.{Configuration, Environment}
import uk.gov.hmrc.auth.core.retrieve.{Credentials, Retrieval, ~}
import uk.gov.hmrc.auth.core.{AffinityGroup, AuthConnector}
import uk.gov.hmrc.cbcrfrontend._
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.form.SubmitterInfoForm
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.services._
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
    extends AnyWordSpec with Matchers with GuiceOneAppPerSuite with CSRFTest with BeforeAndAfterEach with IdiomaticMockito with MockitoCats {

  private implicit val ec: ExecutionContext = app.injector.instanceOf[ExecutionContext]
  private implicit val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
  private implicit val as: ActorSystem = app.injector.instanceOf[ActorSystem]
  private implicit val env: Environment = app.injector.instanceOf[Environment]
  private implicit val config: Configuration = app.injector.instanceOf[Configuration]
  private implicit val feConfig: FrontendAppConfig = mock[FrontendAppConfig]
  private implicit val timeout: Timeout = Timeout(5 seconds)

  private def getMessages(r: FakeRequest[_]): Messages = messagesApi.preferred(r)

  private val creds = Credentials("totally", "legit")

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
  private val views = app.injector.instanceOf[Views]
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
    val ultimateParentEntity = UltimateParentEntity("UltimateParentEntity")
    val fakeRequestSubmit = addToken(
      FakeRequest("POST", "/submitUltimateParentEntity ")
        .withFormUrlEncodedBody("ultimateParentEntity" -> ultimateParentEntity.ultimateParentEntity))
    "return 303 and point to the correct page" when {
      "the reporting role is CBC702" in {
        auth.authorise[Option[AffinityGroup]](*, *)(*, *) returns Future.successful(
          Some(AffinityGroup.Organisation))
        cache.read[CompleteXMLInfo](CompleteXMLInfo.format, *, *) returnsF keyXMLInfo.copy(
          reportingEntity = keyXMLInfo.reportingEntity.copy(reportingRole = CBC702))
        cache.save[UltimateParentEntity](*)(UltimateParentEntity.format, *, *) returns Future
          .successful(CacheMap("cache", Map.empty[String, JsValue]))
        val result = call(controller.submitUltimateParentEntity, fakeRequestSubmit)
        header("Location", result).get should endWith("/utr/entry-form")
        status(result) shouldBe Status.SEE_OTHER
      }

      "the reporting role is CBC703" in {
        auth.authorise[Option[AffinityGroup]](*, *)(*, *) returns Future.successful(
          Some(AffinityGroup.Organisation))
        cache.readOption(AffinityGroup.jsonFormat, *, *) returns Future.successful(
          Some(AffinityGroup.Organisation))
        cache.save[UltimateParentEntity](*)(UltimateParentEntity.format, *, *) returns Future
          .successful(CacheMap("cache", Map.empty[String, JsValue]))
        cache.read[CompleteXMLInfo](CompleteXMLInfo.format, *, *) returnsF keyXMLInfo.copy(
          reportingEntity = keyXMLInfo.reportingEntity.copy(reportingRole = CBC703))
        cache.save[UltimateParentEntity](*)(UltimateParentEntity.format, *, *) returns Future
          .successful(CacheMap("cache", Map.empty[String, JsValue]))
        val result = call(controller.submitUltimateParentEntity, fakeRequestSubmit)
        header("Location", result).get should endWith("/submitter-info/entry-form")
        status(result) shouldBe Status.SEE_OTHER
      }
    }

    "return 500 when the reportingrole is CBC701 as this should never happen" in {
      auth.authorise[Option[AffinityGroup]](*, *)(*, *) returns Future.successful(
        Some(AffinityGroup.Organisation))
      cache.save[UltimateParentEntity](*)(UltimateParentEntity.format, *, *) returns Future
        .successful(CacheMap("cache", Map.empty[String, JsValue]))
      cache.read(CompleteXMLInfo.format, *, *) returnsF keyXMLInfo.copy(
        reportingEntity = keyXMLInfo.reportingEntity.copy(reportingRole = CBC701))
      val result = call(controller.submitUltimateParentEntity, fakeRequestSubmit)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }
  }

  "GET /submitter-info" should {
    "return a 200 when SubmitterInfo is populated in cache" in {
      val fakeRequestSubmit = addToken(FakeRequest("GET", "/submitter-info"))
      cache.readOption(SubmitterInfo.format, *, *) returns Future.successful(
        Some(SubmitterInfo("A Name", None, "0123456", EmailAddress("email@org.com"), None)))
      auth.authorise[Any](*, *)(*, *) returns Future.successful(())
      cache.read[CompleteXMLInfo](CompleteXMLInfo.format, *, *) returnsF keyXMLInfo
      cache.save[FilingType](*)(FilingType.format, *, *) returns Future.successful(
        CacheMap("cache", Map.empty[String, JsValue]))
      cache.save[TIN](*)(TIN.format, *, *) returns Future.successful(
        CacheMap("cache", Map.empty[String, JsValue]))
      auth.authorise[Option[AffinityGroup]](*, *)(*, *) returns Future.successful(
        Some(AffinityGroup.Organisation))
      cache.read[FileDetails](FileDetails.fileDetailsFormat, *, *) returnsF fileDetails
      status(controller.submitterInfo(None)(fakeRequestSubmit)) shouldBe Status.OK
    }

    "return a 200 when SubmitterInfo is NOT in cache" in {
      val fakeRequestSubmit = addToken(FakeRequest("GET", "/submitter-info"))
      cache.readOption(SubmitterInfo.format, *, *) returns Future.successful(None)
      auth.authorise[Any](*, *)(*, *) returns Future.successful(())
      cache.read[CompleteXMLInfo](CompleteXMLInfo.format, *, *) returnsF keyXMLInfo
      cache.save[FilingType](*)(FilingType.format, *, *) returns Future.successful(
        CacheMap("cache", Map.empty[String, JsValue]))
      cache.save[TIN](*)(TIN.format, *, *) returns Future.successful(
        CacheMap("cache", Map.empty[String, JsValue]))
      auth.authorise[Option[AffinityGroup]](*, *)(*, *) returns Future.successful(
        Some(AffinityGroup.Organisation))
      cache.read[FileDetails](FileDetails.fileDetailsFormat, *, *) returnsF fileDetails
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
      cache.readOption(SubmitterInfo.format, *, *) returns Future.successful(None)
      auth.authorise[Any](*, *)(*, *) returns Future.successful(())
      cache.read[CompleteXMLInfo](CompleteXMLInfo.format, *, *) returnsF keyXMLInfo
      cache.save[UltimateParentEntity](*)(UltimateParentEntity.format, *, *) returns Future
        .successful(CacheMap("cache", Map.empty[String, JsValue]))
      cache.save[FilingType](*)(FilingType.format, *, *) returns Future.successful(
        CacheMap("cache", Map.empty[String, JsValue]))
      auth.authorise[Option[AffinityGroup]](*, *)(*, *) returns Future.successful(
        Some(AffinityGroup.Organisation))
      cache.read[FileDetails](FileDetails.fileDetailsFormat, *, *) returnsF fileDetails
      status(controller.submitterInfo()(fakeRequestSubmit)) shouldBe Status.OK
      cache.save(*)(FilingType.format, *, *) was called
      cache.save(*)(UltimateParentEntity.format, *, *) was called
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
      auth.authorise[Any](*, *)(*, *) returns Future.successful(())
      cache.readOption(SubmitterInfo.format, *, *) returns Future.successful(None)
      cache.read[CompleteXMLInfo](CompleteXMLInfo.format, *, *) returnsF keyXMLInfo.copy(
        reportingEntity = keyXMLInfo.reportingEntity.copy(reportingRole = CBC702))
      cache.save[FilingType](*)(FilingType.format, *, *) returns Future.successful(
        CacheMap("cache", Map.empty[String, JsValue]))
      auth.authorise[Option[AffinityGroup]](*, *)(*, *) returns Future.successful(
        Some(AffinityGroup.Organisation))
      cache.read[FileDetails](FileDetails.fileDetailsFormat, *, *) returnsF fileDetails
      status(controller.submitterInfo()(fakeRequestSubmit)) shouldBe Status.OK
      cache.save(*)(FilingType.format, *, *) was called
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
      auth.authorise[Any](*, *)(*, *) returns Future.successful(())
      cache.readOption(SubmitterInfo.format, *, *) returns Future.successful(None)
      cache.read[CompleteXMLInfo](CompleteXMLInfo.format, *, *) returnsF keyXMLInfo.copy(
        reportingEntity = keyXMLInfo.reportingEntity.copy(reportingRole = CBC703))
      cache.save[FilingType](*)(FilingType.format, *, *) returns Future.successful(
        CacheMap("cache", Map.empty[String, JsValue]))
      auth.authorise[Option[AffinityGroup]](*, *)(*, *) returns Future.successful(
        Some(AffinityGroup.Organisation))
      cache.read[FileDetails](FileDetails.fileDetailsFormat, *, *) returnsF fileDetails
      status(controller.submitterInfo()(fakeRequestSubmit)) shouldBe Status.OK
      cache.save(*)(FilingType.format, *, *) was called
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
      auth.authorise[Option[AffinityGroup]](*, *)(*, *) returns Future.successful(
        Some(AffinityGroup.Organisation))
      cache.read[CBCId](CBCId.cbcIdFormat, *, *) raises ExpiredSession("")
      cache.read[FileDetails](FileDetails.fileDetailsFormat, *, *) returnsF fileDetails
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
      auth.authorise[Option[AffinityGroup]](*, *)(*, *) returns Future.successful(
        Some(AffinityGroup.Organisation))
      cache.read[CBCId](CBCId.cbcIdFormat, *, *) raises ExpiredSession("")
      cache.read[FileDetails](FileDetails.fileDetailsFormat, *, *) returnsF fileDetails
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
      auth.authorise[Option[AffinityGroup]](*, *)(*, *) returns Future.successful(
        Some(AffinityGroup.Organisation))
      cache.read[CBCId](CBCId.cbcIdFormat, *, *) raises ExpiredSession("")
      val fakeRequestSubmit =
        addToken(FakeRequest("POST", "/submitSubmitterInfo").withFormUrlEncodedBody(dataSeq: _*))
      cache.read[FileDetails](FileDetails.fileDetailsFormat, *, *) returnsF fileDetails
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
      auth.authorise[Option[AffinityGroup]](*, *)(*, *) returns Future.successful(
        Some(AffinityGroup.Organisation))
      cache.read[CBCId](CBCId.cbcIdFormat, *, *) raises ExpiredSession("")
      cache.read[FileDetails](FileDetails.fileDetailsFormat, *, *) returnsF fileDetails
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
      auth.authorise[Option[AffinityGroup]](*, *)(*, *) returns Future.successful(
        Some(AffinityGroup.Organisation))
      cache.read[CBCId](CBCId.cbcIdFormat, *, *) raises ExpiredSession("")
      auth.authorise[Option[AffinityGroup]](*, *)(*, *) returns Future.successful(
        Some(AffinityGroup.Organisation))
      auth.authorise[Option[AffinityGroup]](*, *)(*, *) returns Future.successful(
        Some(AffinityGroup.Organisation))
      cache.read[FileDetails](FileDetails.fileDetailsFormat, *, *) returnsF fileDetails
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
      auth.authorise[Option[AffinityGroup]](*, *)(*, *) returns Future.successful(
        Some(AffinityGroup.Organisation))
      cache.read[CBCId](CBCId.cbcIdFormat, *, *) raises ExpiredSession("")
      auth.authorise[Option[AffinityGroup]](*, *)(*, *) returns Future.successful(
        Some(AffinityGroup.Organisation))
      cache.read[FileDetails](FileDetails.fileDetailsFormat, *, *) returnsF fileDetails
      val result = call(controller.submitSubmitterInfo, fakeRequestSubmit)
      status(result) shouldBe Status.BAD_REQUEST
    }

    "return 303 when all of the data exists & valid" in {
      val submitterInfo = SubmitterInfo("Fullname", None, "07923456708", EmailAddress("abc@xyz.com"), None)
      val dataSeq = SubmitterInfoForm.submitterInfoForm.fill(submitterInfo).data.toSeq
      val fakeRequestSubmit =
        addToken(FakeRequest("POST", "/submitSubmitterInfo").withFormUrlEncodedBody(dataSeq: _*))

      auth.authorise[Option[AffinityGroup]](*, *)(*, *) returns Future.successful(
        Some(AffinityGroup.Organisation))
      cache.readOption[CBCId](CBCId.cbcIdFormat, *, *) returns Future.successful(None)
      cache.save[SubmitterInfo](*)(SubmitterInfo.format, *, *) returns Future.successful(
        CacheMap("cache", Map.empty[String, JsValue]))
      cache.read[CompleteXMLInfo](CompleteXMLInfo.format, *, *) returnsF keyXMLInfo
      cache.save[CBCId](*)(CBCId.cbcIdFormat, *, *) returns Future.successful(
        CacheMap("cache", Map.empty[String, JsValue]))
      cache.readOption[AgencyBusinessName](AgencyBusinessName.format, *, *) returns Future
        .successful(Some(AgencyBusinessName("Colm Cavanagh ltd")))
      cache.read[SubmitterInfo](SubmitterInfo.format, *, *) returnsF submitterInfo
      val result = call(controller.submitSubmitterInfo, fakeRequestSubmit)
      val returnVal = status(result)
      returnVal shouldBe Status.SEE_OTHER

      cache.read(CompleteXMLInfo.format, *, *) was called
      cache.save(*)(SubmitterInfo.format, *, *) was called
    }

    "return 303 when Email Address is valid" when {
      "the AffinityGroup is Organisation it" should {
        "redirect to submit-summary if a cbcId exists" in {
          val submitterInfo = SubmitterInfo("Billy Bob", None, "07923456708", EmailAddress("abc@xyz.com"), None)
          val dataSeq = SubmitterInfoForm.submitterInfoForm.fill(submitterInfo).data.toSeq
          val fakeRequestSubmit =
            addToken(FakeRequest("POST", "/submitSubmitterInfo").withFormUrlEncodedBody(dataSeq: _*))
          auth.authorise[Option[AffinityGroup]](*, *)(*, *) returns Future.successful(
            Some(AffinityGroup.Organisation))
          cache.read[SubmitterInfo](SubmitterInfo.format, *, *) returnsF SubmitterInfo(
            "name",
            None,
            "0123123123",
            EmailAddress("max@max.com"),
            Some(AffinityGroup.Organisation))
          cache.readOption[CBCId](CBCId.cbcIdFormat, *, *) returns Future.successful(
            CBCId.create(100).toOption)
          cache.save[SubmitterInfo](*)(*, *, *) returns Future.successful(
            CacheMap("cache", Map.empty[String, JsValue]))
          cache.read[CompleteXMLInfo](CompleteXMLInfo.format, *, *) returnsF keyXMLInfo
          cache.save[CBCId](*)(CBCId.cbcIdFormat, *, *) returns Future.successful(
            CacheMap("cache", Map.empty[String, JsValue]))
          cache.readOption[AgencyBusinessName](AgencyBusinessName.format, *, *) returns Future
            .successful(Some(AgencyBusinessName("Colm Cavanagh ltd")))
          val result = call(controller.submitSubmitterInfo, fakeRequestSubmit)

          header("Location", result).get should endWith("/submission/summary")
          status(result) shouldBe Status.SEE_OTHER
        }

        "redirect to enter-cbcId if a cbcid does not exist" in {
          val submitterInfo = SubmitterInfo("Billy Bob", None, "07923456708", EmailAddress("abc@xyz.com"), None)
          val dataSeq = SubmitterInfoForm.submitterInfoForm.fill(submitterInfo).data.toSeq
          val fakeRequestSubmit =
            addToken(FakeRequest("POST", "/submitSubmitterInfo").withFormUrlEncodedBody(dataSeq: _*))
          auth.authorise[Option[AffinityGroup]](*, *)(*, *) returns Future.successful(
            Some(AffinityGroup.Organisation))
          cache.read[SubmitterInfo](SubmitterInfo.format, *, *) returnsF SubmitterInfo(
            "name",
            None,
            "0123123123",
            EmailAddress("max@max.com"),
            Some(AffinityGroup.Organisation))
          cache.readOption[CBCId](CBCId.cbcIdFormat, *, *) returns Future.successful(None)
          cache.save[SubmitterInfo](*)(*, *, *) returns Future.successful(
            CacheMap("cache", Map.empty[String, JsValue]))
          cache.read[CompleteXMLInfo](CompleteXMLInfo.format, *, *) returnsF keyXMLInfo
          cache.save[CBCId](*)(CBCId.cbcIdFormat, *, *) returns Future.successful(
            CacheMap("cache", Map.empty[String, JsValue]))
          cache.readOption[AgencyBusinessName](AgencyBusinessName.format, *, *) returns Future
            .successful(Some(AgencyBusinessName("Colm Cavanagh ltd")))
          val result = call(controller.submitSubmitterInfo, fakeRequestSubmit)

          header("Location", result).get should endWith("/cbc-id/entry-form")
          status(result) shouldBe Status.SEE_OTHER
        }
      }

      "the AffinityGroup is Agent it" should {
        "redirect to enter-known-facts if a cbcid does not exist" in {
          val submitterInfo = SubmitterInfo("Billy Bob", None, "07923456708", EmailAddress("abc@xyz.com"), None)
          val dataSeq = SubmitterInfoForm.submitterInfoForm.fill(submitterInfo).data.toSeq
          val fakeRequestSubmit =
            addToken(FakeRequest("POST", "/submitSubmitterInfo").withFormUrlEncodedBody(dataSeq: _*))
          auth.authorise[Option[AffinityGroup]](*, *)(*, *) returns Future.successful(
            Some(AffinityGroup.Agent))
          cache.read[SubmitterInfo](SubmitterInfo.format, *, *) returnsF SubmitterInfo(
            "name",
            None,
            "0123123123",
            EmailAddress("max@max.com"),
            Some(AffinityGroup.Organisation))
          cache.readOption[CBCId](CBCId.cbcIdFormat, *, *) returns Future.successful(None)
          cache.save[SubmitterInfo](*)(*, *, *) returns Future.successful(
            CacheMap("cache", Map.empty[String, JsValue]))
          cache.read[CompleteXMLInfo](CompleteXMLInfo.format, *, *) returnsF keyXMLInfo
          cache.save[CBCId](*)(CBCId.cbcIdFormat, *, *) returns Future.successful(
            CacheMap("cache", Map.empty[String, JsValue]))
          cache.readOption[AgencyBusinessName](AgencyBusinessName.format, *, *) returns Future
            .successful(Some(AgencyBusinessName("Colm Cavanagh ltd")))
          val result = call(controller.submitSubmitterInfo, fakeRequestSubmit)

          header("Location", result).get should endWith("/agent/verify-form")
          status(result) shouldBe Status.SEE_OTHER
        }
      }
    }
  }

  "The submission controller" should {
    "provide a method to generate the metadata that" should {
      "return a list of errors for each of the missing cache values" in {
        cache.readOption[GGId](GGId.format, *, *) returns Future.successful(
          Some(GGId("ggid", "type")))
        cache.read[BusinessPartnerRecord] raises ExpiredSession("")
        cache.read[TIN] raises ExpiredSession("")
        cache.read[CBCId] raises ExpiredSession("@")
        cache.read[Hash] raises ExpiredSession("")
        cache.read[FileId] raises ExpiredSession("")
        cache.read[EnvelopeId] raises ExpiredSession("")
        cache.read[SubmitterInfo] raises ExpiredSession("")
        cache.read[FilingType] raises ExpiredSession("")
        cache.read[UltimateParentEntity] raises ExpiredSession("")
        cache.read[FileMetadata] raises ExpiredSession("")

        Await
          .result(generateMetadataFile(cache, creds), 10.second)
          .fold(
            errors => errors.toList.size shouldBe 10,
            _ => fail("this should have failed")
          )

        cache.read[FileId] returnsF FileId("fileId")
        Await
          .result(generateMetadataFile(cache, creds), 10.second)
          .fold(
            errors => errors.toList.size shouldBe 9,
            _ => fail("this should have failed")
          )

        cache.read[EnvelopeId] returnsF EnvelopeId("yeah")
        Await
          .result(generateMetadataFile(cache, creds), 10.second)
          .fold(
            errors => errors.toList.size shouldBe 8,
            _ => fail("this should have failed")
          )
      }

      "return a Metadata object if all succeeds" in {
        cache.readOption[GGId](GGId.format, *, *) returns Future.successful(
          Some(GGId("ggid", "type")))
        cache.read[BusinessPartnerRecord] returnsF bpr
        cache.read[TIN] returnsF TIN("utr", "")
        cache.read[CBCId](CBCId.cbcIdFormat, *, *) returnsF CBCId.create(1).getOrElse(fail("argh"))
        cache.read[Hash] returnsF Hash("hash")
        cache.read[FileId] returnsF FileId("yeah")
        cache.read[EnvelopeId] returnsF EnvelopeId("id")
        cache.read[SubmitterInfo] returnsF SubmitterInfo(
          "name",
          None,
          "0123123123",
          EmailAddress("max@max.com"),
          Some(AffinityGroup.Organisation))
        cache.read[FilingType] returnsF FilingType(CBC701)
        cache.read[UltimateParentEntity] returnsF UltimateParentEntity("yeah")
        cache.read[FileMetadata] returnsF FileMetadata(
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
        auth.authorise(*, any[Retrieval[Option[Credentials] ~ Option[AffinityGroup]]])(*, *) returns Future.successful(
            new ~[Option[Credentials], Option[AffinityGroup]](Some(creds), Some(AffinityGroup.Organisation)))
        cache.readOption[GGId](GGId.format, *, *) returns Future.successful(
          Some(GGId("ggid", "type")))
        cache.read[CompleteXMLInfo](CompleteXMLInfo.format, *, *) returnsF keyXMLInfo
        cache.read[BusinessPartnerRecord](BusinessPartnerRecord.format, *, *) returnsF bpr
        cache.read[TIN](TIN.format, *, *) returnsF TIN("utr", "")
        cache.read[CBCId](CBCId.cbcIdFormat, *, *) returnsF CBCId.create(1).getOrElse(fail("argh"))
        cache.read[Hash](Hash.format, *, *) returnsF Hash("hash")
        cache.read[FileId](FileId.fileIdFormat, *, *) returnsF FileId("yeah")
        cache.read[EnvelopeId](EnvelopeId.format, *, *) returnsF EnvelopeId("id")
        cache.read[SubmitterInfo](SubmitterInfo.format, *, *) returnsF SubmitterInfo(
          "name",
          None,
          "0123123123",
          EmailAddress("max@max.com"),
          None)
        cache.read[FilingType](FilingType.format, *, *) returnsF FilingType(CBC701)
        cache.read[UltimateParentEntity](UltimateParentEntity.format, *, *) raises ExpiredSession(
          "nope")
        cache.read[FileMetadata](FileMetadata.fileMetadataFormat, *, *) returnsF FileMetadata(
          "asdf",
          "lkjasdf",
          "lkj",
          "lkj",
          10,
          "lkjasdf",
          JsNull,
          "")
        cache.save[SummaryData](*)(*, *, *) returns Future.successful(
          CacheMap("cache", Map.empty[String, JsValue]))
        cache.read[FileDetails](FileDetails.fileDetailsFormat, *, *) returnsF fileDetails
        val result = controller.submitSummary(fakeRequestSubmitSummary)
        status(result) shouldBe Status.SEE_OTHER
        header("Location", result).get should endWith("/session-expired")
      }

      "return 200 if everything succeeds" in {
        val file = File.createTempFile("test", "test")

        auth.authorise(*, any[Retrieval[Option[Credentials] ~ Option[AffinityGroup]]])(*, *) returns Future.successful(
            new ~[Option[Credentials], Option[AffinityGroup]](Some(creds), Some(AffinityGroup.Organisation)))
        cache.readOption[GGId](GGId.format, *, *) returns Future.successful(
          Some(GGId("ggid", "type")))
        cache.read[CompleteXMLInfo](CompleteXMLInfo.format, *, *) returnsF keyXMLInfo
        cache.read[BusinessPartnerRecord](BusinessPartnerRecord.format, *, *) returnsF bpr
        cache.read[TIN](TIN.format, *, *) returnsF TIN("utr", "")
        cache.read[CBCId](CBCId.cbcIdFormat, *, *) returnsF CBCId.create(1).getOrElse(fail("argh"))
        cache.read[Hash](Hash.format, *, *) returnsF Hash("hash")
        cache.read[FileId](FileId.fileIdFormat, *, *) returnsF FileId("yeah")
        cache.read[EnvelopeId](EnvelopeId.format, *, *) returnsF EnvelopeId("id")
        cache.read[SubmitterInfo](SubmitterInfo.format, *, *) returnsF SubmitterInfo(
          "name",
          None,
          "0123123123",
          EmailAddress("max@max.com"),
          None)
        cache.read[FilingType](FilingType.format, *, *) returnsF FilingType(CBC701)
        cache.read[UltimateParentEntity](UltimateParentEntity.format, *, *) returnsF UltimateParentEntity(
          "upe")
        cache.read[FileMetadata](FileMetadata.fileMetadataFormat, *, *) returnsF FileMetadata(
          "asdf",
          "lkjasdf",
          "lkj",
          "lkj",
          10,
          "lkjasdf",
          JsNull,
          "")
        fus.getFile(any[String], any[String])(*) returns EitherT[Future, CBCErrors, File](
          Future.successful(Right(file)))
        cache.save[SummaryData](*)(*, *, *) returns Future.successful(
          CacheMap("cache", Map.empty[String, JsValue]))
        cache.read[FileDetails](FileDetails.fileDetailsFormat, *, *) returnsF fileDetails
        status(controller.submitSummary(fakeRequestSubmitSummary)) shouldBe Status.OK

        file.deleteOnExit()
      }
    }

    "provide an action '/confirm'" which {
      "returns a 303 when the call to the cache fails and redirect to session expired" in {
        val summaryData = SummaryData(bpr, submissionData, keyXMLInfo, doesCreationTimeStampHaveMillis = false)
        val fakeRequestSubmitSummary = addToken(FakeRequest("POST", "/confirm ").withJsonBody(Json.toJson("{}")))
        auth.authorise(*, any[Retrieval[Credentials ~ Option[AffinityGroup]]])(*, *) returns Future.successful(new ~[Credentials, Option[AffinityGroup]](creds, Some(AffinityGroup.Organisation)))
        cache.read[SummaryData](SummaryData.format, *, *) returnsF summaryData
        cache.read[CompleteXMLInfo](CompleteXMLInfo.format, *, *) raises ExpiredSession("")
        val result = controller.confirm(fakeRequestSubmitSummary)
        status(result) shouldBe Status.SEE_OTHER
        header("Location", result).get should endWith("/session-expired")
      }

      "returns a 500 when the call to file-upload fails" in {
        val summaryData = SummaryData(bpr, submissionData, keyXMLInfo, doesCreationTimeStampHaveMillis = false)
        val fakeRequestSubmitSummary = addToken(FakeRequest("POST", "/confirm ").withJsonBody(Json.toJson("{}")))
        auth.authorise(*, any[Retrieval[Credentials ~ Option[AffinityGroup]]])(*, *) returns Future.successful(new ~[Credentials, Option[AffinityGroup]](creds, Some(AffinityGroup.Organisation)))
        cache.read[SummaryData](SummaryData.format, *, *) returnsF summaryData
        cache.read[CompleteXMLInfo](CompleteXMLInfo.format, *, *) returnsF keyXMLInfo
        fus.uploadMetadataAndRoute(*)(*, *) raises UnexpectedState("fail")
        val result = Await.result(controller.confirm(fakeRequestSubmitSummary), 50.seconds)
        status(result) shouldBe Status.INTERNAL_SERVER_ERROR
      }

      "returns a 500 when the call to save the docRefIds fail" in {
        val summaryData = SummaryData(bpr, submissionData, keyXMLInfo, doesCreationTimeStampHaveMillis = false)
        val fakeRequestSubmitSummary = addToken(FakeRequest("POST", "/confirm ").withJsonBody(Json.toJson("{}")))
        auth.authorise(*, any[Retrieval[Credentials ~ Option[AffinityGroup]]])(*, *) returns Future.successful(new ~[Credentials, Option[AffinityGroup]](creds, Some(AffinityGroup.Organisation)))
        cache.read[SummaryData](SummaryData.format, *, *) returnsF summaryData
        cache.read[CompleteXMLInfo](CompleteXMLInfo.format, *, *) returnsF keyXMLInfo
        fus.uploadMetadataAndRoute(*)(*, *) returnsF "ok"
        docRefService.saveDocRefId(*)(*) returnsF UnexpectedState("fails!")
        docRefService.saveCorrDocRefID(*, *)(*) returnsF UnexpectedState("fails!")
        status(controller.confirm(fakeRequestSubmitSummary)) shouldBe Status.INTERNAL_SERVER_ERROR
      }

      "returns 303 when the there is data and " should {
        "call saveReportingEntityData when the submissionType is OECD1" in {
          val summaryData = SummaryData(bpr, submissionData, keyXMLInfo, doesCreationTimeStampHaveMillis = false)
          val fakeRequestSubmitSummary = addToken(FakeRequest("POST", "/confirm ").withJsonBody(Json.toJson("{}")))
          auth.authorise(*, any[Retrieval[Option[Credentials] ~ Option[AffinityGroup]]])(*, *) returns Future.successful(
              new ~[Option[Credentials], Option[AffinityGroup]](Some(creds), Some(AffinityGroup.Organisation)))
          cache.read[SummaryData](SummaryData.format, *, *) returnsF summaryData
          fus.uploadMetadataAndRoute(*)(*, *) returns EitherT[Future, CBCErrors, String](
            Future.successful(Right("routed")))
          cache.read[CompleteXMLInfo](CompleteXMLInfo.format, *, *) returnsF keyXMLInfo
          cache.save[SubmissionDate](*)(SubmissionDate.format, *, *) returns Future.successful(
            CacheMap("cache", Map.empty[String, JsValue]))
          fus.uploadMetadataAndRoute(*)(*, *) returnsF "ok"
          reportingEntity.saveReportingEntityData(*)(*) returnsF ()
          docRefService.saveCorrDocRefID(*, *)(*) returns OptionT.none[Future, UnexpectedState]
          docRefService.saveDocRefId(*)(*) returns OptionT.none[Future, UnexpectedState]
          messageRefIdService.saveMessageRefId(*)(*) returns OptionT.none[Future, UnexpectedState]
          cache.readOption[GGId](GGId.format, *, *) returns Future.successful(
            Some(GGId("ggid", "type")))
          auditMock.sendExtendedEvent(*)(*, *) returns Future.successful(AuditResult.Success)
          status(controller.confirm(fakeRequestSubmitSummary)) shouldBe Status.SEE_OTHER
          reportingEntity.saveReportingEntityData(*)(*) was called
          messageRefIdService.saveMessageRefId(*)(*) was called
        }

        "call updateReportingEntityData when the submissionType is OECD[023]" in {
          val summaryData = SummaryData(bpr, submissionData, keyXMLInfo, doesCreationTimeStampHaveMillis = false)
          val fakeRequestSubmitSummary = addToken(FakeRequest("POST", "/confirm ").withJsonBody(Json.toJson("{}")))
          lazy val updateXml = keyXMLInfo.copy(
            reportingEntity =
              keyXMLInfo.reportingEntity.copy(docSpec = keyXMLInfo.reportingEntity.docSpec.copy(docType = OECD2)))
          auth.authorise(*, any[Retrieval[Option[Credentials] ~ Option[AffinityGroup]]])(*, *) returns Future.successful(
              new ~[Option[Credentials], Option[AffinityGroup]](Some(creds), Some(AffinityGroup.Organisation)))
          cache.read[SummaryData](SummaryData.format, *, *) returnsF summaryData
          fus.uploadMetadataAndRoute(*)(*, *) returns EitherT[Future, CBCErrors, String](
            Future.successful(Right("routed")))
          cache.read[CompleteXMLInfo](CompleteXMLInfo.format, *, *) returnsF updateXml
          cache.save[SubmissionDate](*)(SubmissionDate.format, *, *) returns Future.successful(
            CacheMap("cache", Map.empty[String, JsValue]))
          fus.uploadMetadataAndRoute(*)(*, *) returnsF "ok"
          reportingEntity.updateReportingEntityData(*)(*) returnsF ()
          docRefService.saveCorrDocRefID(*, *)(*) returns OptionT.none[Future, UnexpectedState]
          docRefService.saveDocRefId(*)(*) returns OptionT.none[Future, UnexpectedState]
          messageRefIdService.saveMessageRefId(*)(*) returns OptionT.none[Future, UnexpectedState]
          cache.readOption[GGId](GGId.format, *, *) returns Future.successful(
            Some(GGId("ggid", "type")))
          auditMock.sendEvent(*)(*, *) returns Future.successful(AuditResult.Success)
          status(controller.confirm(fakeRequestSubmitSummary)) shouldBe Status.SEE_OTHER
          reportingEntity.updateReportingEntityData(*)(*) was called
          messageRefIdService.saveMessageRefId(*)(*) was called
        }

        "return 500 if saveMessageRefId fails and does NOT call saveReportingEntityData " in {
          val summaryData = SummaryData(bpr, submissionData, keyXMLInfo, doesCreationTimeStampHaveMillis = false)
          val fakeRequestSubmitSummary = addToken(FakeRequest("POST", "/confirm ").withJsonBody(Json.toJson("{}")))
          auth.authorise(*, any[Retrieval[Credentials ~ Option[AffinityGroup]]])(*, *) returns Future.successful(new ~[Credentials, Option[AffinityGroup]](creds, Some(AffinityGroup.Organisation)))
          cache.read[SummaryData](SummaryData.format, *, *) returnsF summaryData
          fus.uploadMetadataAndRoute(*)(*, *) returns EitherT[Future, CBCErrors, String](
            Future.successful(Right("routed")))
          cache.read[CompleteXMLInfo](CompleteXMLInfo.format, *, *) returnsF keyXMLInfo
          cache.save[SubmissionDate](*)(SubmissionDate.format, *, *) returns Future.successful(
            CacheMap("cache", Map.empty[String, JsValue]))
          fus.uploadMetadataAndRoute(*)(*, *) returnsF "ok"
          reportingEntity.saveReportingEntityData(*)(*) returnsF ()
          docRefService.saveCorrDocRefID(*, *)(*) returns OptionT.none[Future, UnexpectedState]
          docRefService.saveDocRefId(*)(*) returns OptionT.none[Future, UnexpectedState]
          messageRefIdService.saveMessageRefId(*)(*) returnsF UnexpectedState("fails!")
          cache.readOption[GGId](GGId.format, *, *) returns Future.successful(
            Some(GGId("ggid", "type")))
          auditMock.sendExtendedEvent(*)(*, *) returns Future.successful(AuditResult.Success)
          status(controller.confirm(fakeRequestSubmitSummary)) shouldBe Status.INTERNAL_SERVER_ERROR
          reportingEntity.saveReportingEntityData(*)(*) wasNever called
          messageRefIdService.saveMessageRefId(*)(*) was called
        }
      }
    }

    "provide an action 'submitSuccessReceipt'" which {
      "returns a 303 Redirect if it fails to read from the cache" when {
        "looking for the SummaryData" in {
          auth.authorise[Any](*, *)(*, *) returns Future.successful(())
          val fakeRequestSubmitSummary = addToken(FakeRequest("GET", "/submitSuccessReceipt"))

          cache.read[SummaryData](SummaryData.format, *, *) raises ExpiredSession("")
          cache.read[SubmissionDate](SubmissionDate.format, *, *) returnsF SubmissionDate(
            LocalDateTime.now())
          val result = controller.submitSuccessReceipt("Organisation")(fakeRequestSubmitSummary)
          status(result) shouldBe Status.SEE_OTHER
          header("Location", result).get should endWith("/session-expired")
        }

        "looking for the SubmissionDate" in {
          auth.authorise[Any](*, *)(*, *) returns Future.successful(())
          val summaryData = SummaryData(bpr, submissionData, keyXMLInfo, doesCreationTimeStampHaveMillis = false)
          val fakeRequestSubmitSummary = addToken(FakeRequest("GET", "/submitSuccessReceipt"))

          cache.read[SummaryData](SummaryData.format, *, *) returnsF summaryData
          cache.read[SubmissionDate](SubmissionDate.format, *, *) raises ExpiredSession("")
          val result = controller.submitSuccessReceipt("Organisation")(fakeRequestSubmitSummary)
          status(result) shouldBe Status.SEE_OTHER
          header("Location", result).get should endWith("/session-expired")
        }

        "looking for the CBCId" in {
          auth.authorise[Any](*, *)(*, *) returns Future.successful(())
          val summaryData = SummaryData(bpr, submissionData, keyXMLInfo, doesCreationTimeStampHaveMillis = false)
          val fakeRequestSubmitSummary = addToken(FakeRequest("GET", "/submitSuccessReceipt"))

          cache.read[SummaryData](SummaryData.format, *, *) returnsF summaryData
          cache.read[SubmissionDate](SubmissionDate.format, *, *) returnsF SubmissionDate(
            LocalDateTime.now())
          cache.read[CBCId](CBCId.cbcIdFormat, *, *) raises ExpiredSession("")
          val result = controller.submitSuccessReceipt("Organisation")(fakeRequestSubmitSummary)
          status(result) shouldBe Status.SEE_OTHER
          header("Location", result).get should endWith("/session-expired")
        }
      }

      "sends an email" in {
        val summaryData = SummaryData(bpr, submissionData, keyXMLInfo, doesCreationTimeStampHaveMillis = false)
        val fakeRequestSubmitSummary = addToken(FakeRequest("GET", "/submitSuccessReceipt"))
        auth.authorise[Any](*, *)(*, *) returns Future.successful(())
        cache.read[SummaryData](SummaryData.format, *, *) returnsF summaryData
        cache.read[SubmissionDate](SubmissionDate.format, *, *) returnsF SubmissionDate(
          LocalDateTime.now())
        cache.readOption[GGId](GGId.format, *, *) returns Future.successful(
          Some(GGId("ggid", "type")))
        mockEmailService.sendEmail(*)(*) returnsF true
        cache.save[ConfirmationEmailSent](*)(
          ConfirmationEmailSent.ConfirmationEmailSentFormat,
          *,
          *) returns Future.successful(CacheMap("cache", Map.empty[String, JsValue]))
        cache.readOption[ConfirmationEmailSent](
          ConfirmationEmailSent.ConfirmationEmailSentFormat,
          *,
          *) returns Future.successful(None)
        cache.read[CBCId](CBCId.cbcIdFormat, *, *) returnsF CBCId.create(1).getOrElse(fail("argh"))
        cache.clear(*) returns Future.successful(true)
        val result = controller.submitSuccessReceipt("Organisation")(fakeRequestSubmitSummary)
        status(result) shouldBe Status.OK
        mockEmailService.sendEmail(*)(*) was called
        cache.save(*)(ConfirmationEmailSent.ConfirmationEmailSentFormat, *, *) was called
        val webPageAsString = contentAsString(result)
        webPageAsString should not include getMessages(fakeRequestSubmitSummary)(
          "submitSuccessReceipt.sendAnotherReport.link")
      }

      "will still return a 200 if the email fails" in {
        val summaryData = SummaryData(bpr, submissionData, keyXMLInfo, doesCreationTimeStampHaveMillis = false)
        val fakeRequestSubmitSummary = addToken(FakeRequest("GET", "/submitSuccessReceipt"))
        auth.authorise[Any](*, *)(*, *) returns Future.successful(())
        cache.readOption[GGId](GGId.format, *, *) returns Future.successful(
          Some(GGId("ggid", "type")))
        cache.save[ConfirmationEmailSent](*)(
          ConfirmationEmailSent.ConfirmationEmailSentFormat,
          *,
          *) returns Future.successful(CacheMap("cache", Map.empty[String, JsValue]))
        mockEmailService.sendEmail(*)(*) returnsF false
        cache.readOption[ConfirmationEmailSent](
          ConfirmationEmailSent.ConfirmationEmailSentFormat,
          *,
          *) returns Future.successful(None)
        cache.read[SummaryData](SummaryData.format, *, *) returnsF summaryData
        cache.read[SubmissionDate](SubmissionDate.format, *, *) returnsF SubmissionDate(
          LocalDateTime.now())
        cache.read[CBCId](CBCId.cbcIdFormat, *, *) returnsF CBCId.create(1).getOrElse(fail("argh"))
        cache.clear(*) returns Future.successful(true)
        val result = controller.submitSuccessReceipt("Organisation")(fakeRequestSubmitSummary)
        status(result) shouldBe Status.OK
        mockEmailService.sendEmail(*)(*) was called
        cache.save(*)(ConfirmationEmailSent.ConfirmationEmailSentFormat, *, *) wasNever called
        val webPageAsString = contentAsString(result)
        webPageAsString should not include getMessages(fakeRequestSubmitSummary)(
          "submitSuccessReceipt.sendAnotherReport.link")
      }

      "will write a ConfirmationEmailSent to the cache if an email is sent" in {
        val summaryData = SummaryData(bpr, submissionData, keyXMLInfo, doesCreationTimeStampHaveMillis = false)
        val fakeRequestSubmitSummary = addToken(FakeRequest("GET", "/submitSuccessReceipt"))
        auth.authorise[Any](*, *)(*, *) returns Future.successful(())
        cache.readOption[GGId](GGId.format, *, *) returns Future.successful(
          Some(GGId("ggid", "type")))
        mockEmailService.sendEmail(*)(*) returnsF true
        cache.read[SummaryData](SummaryData.format, *, *) returnsF summaryData
        cache.save[ConfirmationEmailSent](*)(
          ConfirmationEmailSent.ConfirmationEmailSentFormat,
          *,
          *) returns Future.successful(CacheMap("cache", Map.empty[String, JsValue]))
        cache.readOption[ConfirmationEmailSent](
          ConfirmationEmailSent.ConfirmationEmailSentFormat,
          *,
          *) returns Future.successful(None)
        cache.read[SubmissionDate](SubmissionDate.format, *, *) returnsF SubmissionDate(
          LocalDateTime.now())
        cache.read[CBCId](CBCId.cbcIdFormat, *, *) returnsF CBCId.create(1).getOrElse(fail("argh"))
        cache.clear(*) returns Future.successful(true)
        val result = controller.submitSuccessReceipt("Organisation")(fakeRequestSubmitSummary)
        status(result) shouldBe Status.OK
        mockEmailService.sendEmail(*)(*) was called
        cache.save(*)(ConfirmationEmailSent.ConfirmationEmailSentFormat, *, *) was called
        val webPageAsString = contentAsString(result)
        webPageAsString should not include getMessages(fakeRequestSubmitSummary)(
          "submitSuccessReceipt.sendAnotherReport.link")
      }

      "not send the email if it has already been sent and not save to the cache" in {
        val summaryData = SummaryData(bpr, submissionData, keyXMLInfo, doesCreationTimeStampHaveMillis = false)
        val fakeRequestSubmitSummary = addToken(FakeRequest("GET", "/submitSuccessReceipt:Organisation"))
        auth.authorise[Any](*, *)(*, *) returns Future.successful(())
        cache.readOption[GGId](GGId.format, *, *) returns Future.successful(
          Some(GGId("ggid", "type")))
        mockEmailService.sendEmail(*)(*) returnsF true
        cache.save[ConfirmationEmailSent](*)(
          ConfirmationEmailSent.ConfirmationEmailSentFormat,
          *,
          *) returns Future.successful(CacheMap("cache", Map.empty[String, JsValue]))
        cache.read[SummaryData](SummaryData.format, *, *) returnsF summaryData
        cache.readOption[ConfirmationEmailSent](
          ConfirmationEmailSent.ConfirmationEmailSentFormat,
          *,
          *) returns Future.successful(Some(ConfirmationEmailSent("yep")))
        cache.read[SubmissionDate](SubmissionDate.format, *, *) returnsF SubmissionDate(
          LocalDateTime.now())
        cache.read[CBCId](CBCId.cbcIdFormat, *, *) returnsF CBCId.create(1).getOrElse(fail("argh"))
        cache.clear(*) returns Future.successful(true)
        val result = controller.submitSuccessReceipt("Organisation")(fakeRequestSubmitSummary)
        status(result) shouldBe Status.OK
        status(controller.submitSuccessReceipt("Organisation")(fakeRequestSubmitSummary)) shouldBe Status.OK
        mockEmailService.sendEmail(*)(*) wasNever called
        cache.save(*)(ConfirmationEmailSent.ConfirmationEmailSentFormat, *, *) wasNever called
        val webPageAsString = contentAsString(result)
        webPageAsString should not include getMessages(fakeRequestSubmitSummary)(
          "submitSuccessReceipt.sendAnotherReport.link")
      }

      "returns a 200 otherwise" in {
        val summaryData = SummaryData(bpr, submissionData, keyXMLInfo, doesCreationTimeStampHaveMillis = false)
        val fakeRequestSubmitSummary = addToken(FakeRequest("GET", "/submitSuccessReceipt:Organisation"))
        auth.authorise[Any](*, *)(*, *) returns Future.successful(())
        cache.readOption[GGId](GGId.format, *, *) returns Future.successful(
          Some(GGId("ggid", "type")))
        mockEmailService.sendEmail(*)(*) returnsF true
        cache.save[ConfirmationEmailSent](*)(
          ConfirmationEmailSent.ConfirmationEmailSentFormat,
          *,
          *) returns Future.successful(CacheMap("cache", Map.empty[String, JsValue]))
        cache.read[SummaryData](SummaryData.format, *, *) returnsF summaryData
        cache.readOption[ConfirmationEmailSent](
          ConfirmationEmailSent.ConfirmationEmailSentFormat,
          *,
          *) returns Future.successful(None)
        cache.read[SubmissionDate](SubmissionDate.format, *, *) returnsF SubmissionDate(
          LocalDateTime.now())
        cache.read[CBCId](CBCId.cbcIdFormat, *, *) returnsF CBCId.create(1).getOrElse(fail("argh"))
        cache.clear(*) returns Future.successful(true)
        val result = controller.submitSuccessReceipt("Organisation")(fakeRequestSubmitSummary)
        status(result) shouldBe Status.OK
        cache.save(*)(ConfirmationEmailSent.ConfirmationEmailSentFormat, *, *) was called
        val webPageAsString = contentAsString(result)
        webPageAsString should not include getMessages(fakeRequestSubmitSummary)(
          "submitSuccessReceipt.sendAnotherReport.link")
      }

      "show show link to submit another report if AffinityGroup is Agent and cache.clear succeeds" in {
        val summaryData = SummaryData(bpr, submissionData, keyXMLInfo, doesCreationTimeStampHaveMillis = false)
        val fakeRequestSubmitSummary = addToken(FakeRequest("GET", "/submitSuccessReceipt:Agent"))
        auth.authorise[Any](*, *)(*, *) returns Future.successful(())
        cache.readOption[GGId](GGId.format, *, *) returns Future.successful(
          Some(GGId("ggid", "type")))
        mockEmailService.sendEmail(*)(*) returnsF true
        cache.save[ConfirmationEmailSent](*)(
          ConfirmationEmailSent.ConfirmationEmailSentFormat,
          *,
          *) returns Future.successful(CacheMap("cache", Map.empty[String, JsValue]))
        cache.read[SummaryData](SummaryData.format, *, *) returnsF summaryData
        cache.readOption[ConfirmationEmailSent](
          ConfirmationEmailSent.ConfirmationEmailSentFormat,
          *,
          *) returns Future.successful(None)
        cache.read[SubmissionDate](SubmissionDate.format, *, *) returnsF SubmissionDate(
          LocalDateTime.now())
        cache.read[CBCId](CBCId.cbcIdFormat, *, *) returnsF CBCId.create(1).getOrElse(fail("argh"))
        cache.clear(*) returns Future.successful(true)
        val result = controller.submitSuccessReceipt("Agent")(fakeRequestSubmitSummary)
        status(result) shouldBe Status.OK
        val webPageAsString = contentAsString(result)
        webPageAsString should include(
          getMessages(fakeRequestSubmitSummary)("submitSuccessReceipt.sendAnotherReport.link"))
      }

      "show NOT show link to submit another report if AffinityGroup is Agent but cache.clear fails" in {
        val summaryData = SummaryData(bpr, submissionData, keyXMLInfo, doesCreationTimeStampHaveMillis = false)
        val fakeRequestSubmitSummary = addToken(FakeRequest("GET", "/submitSuccessReceipt"))
        auth.authorise[Any](*, *)(*, *) returns Future.successful(())
        cache.readOption[GGId](GGId.format, *, *) returns Future.successful(
          Some(GGId("ggid", "type")))
        mockEmailService.sendEmail(*)(*) returnsF true
        cache.save[ConfirmationEmailSent](*)(
          ConfirmationEmailSent.ConfirmationEmailSentFormat,
          *,
          *) returns Future.successful(CacheMap("cache", Map.empty[String, JsValue]))
        cache.read[SummaryData](SummaryData.format, *, *) returnsF summaryData
        cache.readOption[ConfirmationEmailSent](
          ConfirmationEmailSent.ConfirmationEmailSentFormat,
          *,
          *) returns Future.successful(None)
        cache.read[SubmissionDate](SubmissionDate.format, *, *) returnsF SubmissionDate(
          LocalDateTime.now())
        cache.read[CBCId](CBCId.cbcIdFormat, *, *) returnsF CBCId.create(1).getOrElse(fail("argh"))
        cache.clear(*) returns Future.successful(false)
        val result = controller.submitSuccessReceipt("Organisation")(fakeRequestSubmitSummary)
        status(result) shouldBe Status.OK
        val webPageAsString = contentAsString(result)
        webPageAsString should not include getMessages(fakeRequestSubmitSummary)(
          "submitSuccessReceipt.sendAnotherReport.link")
      }

      "show NOT show link to submit another report if AffinityGroup is NOT Agent and cache.clear succeeds" in {
        val summaryData = SummaryData(bpr, submissionData, keyXMLInfo, doesCreationTimeStampHaveMillis = false)
        val fakeRequestSubmitSummary = addToken(FakeRequest("GET", "/submitSuccessReceipt"))
        auth.authorise[Any](*, *)(*, *) returns Future.successful(())
        cache.readOption[GGId](GGId.format, *, *) returns Future.successful(
          Some(GGId("ggid", "type")))
        mockEmailService.sendEmail(*)(*) returnsF true
        cache.save[ConfirmationEmailSent](*)(
          ConfirmationEmailSent.ConfirmationEmailSentFormat,
          *,
          *) returns Future.successful(CacheMap("cache", Map.empty[String, JsValue]))
        cache.read[SummaryData](SummaryData.format, *, *) returnsF summaryData
        cache.readOption[ConfirmationEmailSent](
          ConfirmationEmailSent.ConfirmationEmailSentFormat,
          *,
          *) returns Future.successful(None)
        cache.read[SubmissionDate](SubmissionDate.format, *, *) returnsF SubmissionDate(
          LocalDateTime.now())
        cache.read[CBCId](CBCId.cbcIdFormat, *, *) returnsF CBCId.create(1).getOrElse(fail("argh"))
        cache.clear(*) returns Future.successful(true)
        val result = controller.submitSuccessReceipt("Organisation")(fakeRequestSubmitSummary)
        status(result) shouldBe Status.OK
        val webPageAsString = contentAsString(result)
        webPageAsString should not include getMessages(fakeRequestSubmitSummary)(
          "submitSuccessReceipt.sendAnotherReport.link")
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
        auth.authorise[Any](*, *)(*, *) returns Future.successful((): Unit)
        val result = controller.notRegistered(request)
        status(result) shouldBe Status.OK
        val webPageAsString = contentAsString(result)
        webPageAsString should include(getMessages(request)("notRegistered.heading"))
      }

      "calling noIndividuals" in {
        val request = addToken(FakeRequest())
        auth.authorise[Any](*, *)(*, *) returns Future.successful((): Unit)
        val result = controller.noIndividuals(request)
        status(result) shouldBe Status.OK
        val webPageAsString = contentAsString(result)
        webPageAsString should include(getMessages(request)("notAuthorised.Individual.message.link"))
      }

      "calling noAssistants" in {
        val request = addToken(FakeRequest())
        auth.authorise[Any](*, *)(*, *) returns Future.successful((): Unit)
        val result = controller.noAssistants(request)
        status(result) shouldBe Status.OK
        val webPageAsString = contentAsString(result)
        webPageAsString should include(getMessages(request)("notAuthorised.assistant.message"))
      }

      "calling upe" in {
        val request = addToken(FakeRequest())
        auth.authorise[Any](*, *)(*, *) returns Future.successful((): Unit)
        val result = controller.upe(request)
        status(result) shouldBe Status.OK
        val webPageAsString = contentAsString(result)
        webPageAsString should include(getMessages(request)("submitInfoUltimateParentEntity.mainHeading"))
      }

      "calling utr" in {
        val request = addToken(FakeRequest())
        auth.authorise[Any](*, *)(*, *) returns Future.successful((): Unit)
        val result = controller.utr(request)
        status(result) shouldBe Status.OK
        val webPageAsString = contentAsString(result)
        webPageAsString should include(getMessages(request)("utrCheck.mainHeading"))
      }

      "calling enterCompanyName" in {
        val request = addToken(FakeRequest())
        auth.authorise[Any](*, *)(*, *) returns Future.successful((): Unit)
        cache.read[FileDetails](FileDetails.fileDetailsFormat, *, *) returnsF fileDetails
        cache.read[AgencyBusinessName](AgencyBusinessName.format, *, *) returnsF AgencyBusinessName(
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
      auth.authorise[Any](*, *)(*, *) returns Future.successful((): Unit)
      cache.save(*)(*, *, *) returns Future.successful(
        CacheMap("", Map.empty[String, JsValue]))
      val result = call(controller.saveCompanyName, request)
      status(result) shouldBe Status.SEE_OTHER
    }

    "return 400 if company details in request are invalid" in {
      val data = "sas" -> "Any Old Iron"
      val request = addToken(FakeRequest("POST", "/")).withFormUrlEncodedBody(data)
      auth.authorise[Any](*, *)(*, *) returns Future.successful((): Unit)
      cache.save(*)(*, *, *) returns Future.successful(
        CacheMap("", Map.empty[String, JsValue]))
      cache.read[FileDetails](FileDetails.fileDetailsFormat, *, *) returnsF fileDetails
      val result = call(controller.saveCompanyName, request)
      status(result) shouldBe Status.BAD_REQUEST
    }
  }
}
