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
import java.time.{LocalDate, LocalDateTime}

import akka.actor.ActorSystem
import cats.data.{EitherT, OptionT}
import cats.instances.future._
import org.mockito.Matchers.{eq => EQ, _}
import org.mockito.Mockito._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.mock.MockitoSugar
import org.scalatestplus.play.OneAppPerSuite
import play.api.{Configuration, Environment}
import play.api.http.Status
import play.api.i18n.{Messages, MessagesApi}
import play.api.libs.json.{JsNull, JsValue, Json}
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.contentAsString
import uk.gov.hmrc.auth.core.retrieve.{Credentials, EmptyRetrieval, Retrieval, ~}
import uk.gov.hmrc.auth.core.{AffinityGroup, AuthConnector}
import uk.gov.hmrc.cbcrfrontend._
import uk.gov.hmrc.cbcrfrontend.model.{CompleteXMLInfo, FileId, _}
import uk.gov.hmrc.cbcrfrontend.services.{CBCSessionCache, DocRefIdService, FileUploadService, ReportingEntityDataService, _}
import uk.gov.hmrc.cbcrfrontend.typesclasses.{CbcrsUrl, FusFeUrl, FusUrl, ServiceUrl}
import uk.gov.hmrc.emailaddress.EmailAddress
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.cache.client.CacheMap
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import uk.gov.hmrc.auth.core.AffinityGroup.{Agent, Organisation}
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import akka.util.Timeout


class SubmissionSpec  extends UnitSpec with OneAppPerSuite with CSRFTest with MockitoSugar with BeforeAndAfterEach {


  implicit val ec             = app.injector.instanceOf[ExecutionContext]
  implicit val messagesApi    = app.injector.instanceOf[MessagesApi]
  implicit val as             = app.injector.instanceOf[ActorSystem]
  implicit val env            = app.injector.instanceOf[Environment]
  implicit val config         = app.injector.instanceOf[Configuration]
  implicit val feConfig       = mock[FrontendAppConfig]
  implicit val timeout        = Timeout(5 seconds)
  def getMessages(r: FakeRequest[_]): Messages = messagesApi.preferred(r)


  when(feConfig.analyticsHost) thenReturn "host"
  when(feConfig.analyticsToken) thenReturn "token"

  val creds:Credentials       = Credentials("totally", "legit")

  val cache                   = mock[CBCSessionCache]
  val fus                     = mock[FileUploadService]
  val docRefService           = mock[DocRefIdService]
  val auth                    = mock[AuthConnector]
  val auditMock               = mock[AuditConnector]
  val mockCBCIdService        = mock[CBCIdService]
  val mockEmailService        = mock[EmailService]
  val reportingEntity         = mock[ReportingEntityDataService]

  implicit lazy val fusUrl = new ServiceUrl[FusUrl] { val url = "file-upload"}
  implicit lazy val fusFeUrl = new ServiceUrl[FusFeUrl] { val url = "file-upload-frontend"}
  implicit lazy val cbcrsUrl = new ServiceUrl[CbcrsUrl] { val url = "cbcr"}

  val bpr = BusinessPartnerRecord("safeId",None, EtmpAddress("Line1",None,None,None,None,"GB"))

  val cbcId = CBCId.create(99).getOrElse(fail("failed to gen cbcid"))


  implicit val hc = HeaderCarrier()
  val controller = new SubmissionController(messagesApi,fus, docRefService,reportingEntity,mockCBCIdService,auditMock,env,auth,mockEmailService)(ec,cache,config,feConfig)

  override protected def afterEach(): Unit = {
    reset(cache,fus,docRefService,reportingEntity,mockEmailService, auth)
    super.afterEach()
  }

  "POST /submitUltimateParentEntity " should {
    val ultimateParentEntity  = UltimateParentEntity("UlitmateParentEntity")
    val fakeRequestSubmit = addToken(FakeRequest("POST", "/submitUltimateParentEntity ").withJsonBody(Json.obj("ultimateParentEntity" -> ultimateParentEntity.ultimateParentEntity)))
    "return 303 and point to the correct page" when {
      "the reporting role is CBC702" in {
        when(auth.authorise[Option[AffinityGroup]](any(),any())(any(),any())) thenReturn Future.successful(Some(AffinityGroup.Organisation))
        when(cache.read(EQ(CompleteXMLInfo.format),any(),any())) thenReturn rightE(keyXMLInfo.copy(reportingEntity = keyXMLInfo.reportingEntity.copy(reportingRole = CBC702)))
        when(cache.save[UltimateParentEntity](any())(EQ(UltimateParentEntity.format),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
        val result = Await.result(controller.submitUltimateParentEntity(fakeRequestSubmit), 2.second)
        result.header.headers("Location") should endWith("/utr/entry-form")
        status(result) shouldBe Status.SEE_OTHER
      }
      "the reporting role is CBC703" in {
        when(auth.authorise[Option[AffinityGroup]](any(),any())(any(),any())) thenReturn Future.successful(Some(AffinityGroup.Organisation))
        when(cache.readOption(EQ(AffinityGroup.jsonFormat),any(),any())) thenReturn Future.successful(Some(AffinityGroup.Organisation))
        when(cache.save[UltimateParentEntity](any())(EQ(UltimateParentEntity.format),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
        when(cache.read(EQ(CompleteXMLInfo.format),any(),any())) thenReturn rightE(keyXMLInfo.copy(reportingEntity = keyXMLInfo.reportingEntity.copy(reportingRole = CBC703)))
        when(cache.save[UltimateParentEntity](any())(EQ(UltimateParentEntity.format),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
        val result = Await.result(controller.submitUltimateParentEntity(fakeRequestSubmit), 2.second)
        result.header.headers("Location") should endWith("/submitter-info/entry-form")
        status(result) shouldBe Status.SEE_OTHER
      }
    }
    "return 500 when the reportingrole is CBC701 as this should never happen" in {
      when(auth.authorise[Option[AffinityGroup]](any(),any())(any(),any())) thenReturn Future.successful(Some(AffinityGroup.Organisation))
      when(cache.save[UltimateParentEntity](any())(EQ(UltimateParentEntity.format),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      when(cache.read(EQ(CompleteXMLInfo.format),any(),any())) thenReturn rightE(keyXMLInfo.copy(reportingEntity = keyXMLInfo.reportingEntity.copy(reportingRole = CBC701)))
      val result = Await.result(controller.submitUltimateParentEntity(fakeRequestSubmit), 2.second)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }
  }

  "GET /submitter-info" should {
    "return a 200 when SubmitterInfo is populated in cache" in {
      val fakeRequestSubmit = addToken(FakeRequest("GET", "/submitter-info"))
      when(cache.readOption(EQ(SubmitterInfo.format),any(),any())) thenReturn Future.successful(Some(SubmitterInfo("A Name", None,"0123456",EmailAddress("email@org.com"),None)))
      when(auth.authorise[Any](any(),any())(any(),any())) thenReturn Future.successful(())
      when(cache.read[CompleteXMLInfo](EQ(CompleteXMLInfo.format),any(),any())) thenReturn rightE(keyXMLInfo)
      when(cache.save[FilingType](any())(EQ(FilingType.format),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      when(cache.save[TIN](any())(EQ(TIN.format),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      status(controller.submitterInfo(fakeRequestSubmit)) shouldBe Status.OK
    }
    "return a 200 when SubmitterInfo is NOT in cache" in {
      val fakeRequestSubmit = addToken(FakeRequest("GET", "/submitter-info"))
      when(cache.readOption(EQ(SubmitterInfo.format),any(),any())) thenReturn Future.successful(None)
      when(auth.authorise[Any](any(),any())(any(),any())) thenReturn Future.successful(())
      when(cache.read[CompleteXMLInfo](EQ(CompleteXMLInfo.format),any(),any())) thenReturn rightE(keyXMLInfo)
      when(cache.save[FilingType](any())(EQ(FilingType.format),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      when(cache.save[TIN](any())(EQ(TIN.format),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      status(controller.submitterInfo(fakeRequestSubmit)) shouldBe Status.OK
    }
    "use the UPE and Filing type form the xml when the ReportingRole is CBC701 " in {
      val cache = mock[CBCSessionCache]
      val controller = new SubmissionController(messagesApi,fus, docRefService,reportingEntity,mockCBCIdService,auditMock,env,auth,mockEmailService)(ec,cache,config,feConfig)
      val fakeRequestSubmit = addToken(FakeRequest("GET", "/submitter-info"))
      when(cache.readOption(EQ(SubmitterInfo.format),any(),any())) thenReturn Future.successful(None)
      when(auth.authorise[Any](any(),any())(any(),any())) thenReturn Future.successful(())
      when(cache.read[CompleteXMLInfo](EQ(CompleteXMLInfo.format),any(),any())) thenReturn rightE(keyXMLInfo)
      when(cache.save[UltimateParentEntity](any())(EQ(UltimateParentEntity.format),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      when(cache.save[FilingType](any())(EQ(FilingType.format),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      status(controller.submitterInfo(fakeRequestSubmit)) shouldBe Status.OK
      verify(cache).save(any())(EQ(FilingType.format),any(),any())
      verify(cache).save(any())(EQ(UltimateParentEntity.format),any(),any())
    }
    "use the Filing type form the xml when the ReportingRole is CBC702" in {
      val cache = mock[CBCSessionCache]
      val controller = new SubmissionController(messagesApi,fus, docRefService,reportingEntity,mockCBCIdService,auditMock,env,auth,mockEmailService)(ec,cache,config,feConfig)
      val fakeRequestSubmit = addToken(FakeRequest("GET", "/submitter-info"))
      when(auth.authorise[Any](any(),any())(any(),any())) thenReturn Future.successful(())
      when(cache.readOption(EQ(SubmitterInfo.format),any(),any())) thenReturn Future.successful(None)
      when(cache.read[CompleteXMLInfo](EQ(CompleteXMLInfo.format),any(),any())) thenReturn rightE(keyXMLInfo.copy(reportingEntity = keyXMLInfo.reportingEntity.copy(reportingRole = CBC702)))
      when(cache.save[FilingType](any())(EQ(FilingType.format),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      status(controller.submitterInfo(fakeRequestSubmit)) shouldBe Status.OK
      verify(cache).save(any())(EQ(FilingType.format),any(),any())
    }
    "use the Filing type form the xml when the ReportingRole is CBC703" in {
      val cache = mock[CBCSessionCache]
      val controller = new SubmissionController(messagesApi,fus, docRefService,reportingEntity,mockCBCIdService,auditMock,env,auth,mockEmailService)(ec,cache,config,feConfig)
      val fakeRequestSubmit = addToken(FakeRequest("GET", "/submitter-info"))
      when(auth.authorise[Any](any(),any())(any(),any())) thenReturn Future.successful(())
      when(cache.readOption(EQ(SubmitterInfo.format),any(),any())) thenReturn Future.successful(None)
      when(cache.read[CompleteXMLInfo](EQ(CompleteXMLInfo.format),any(),any())) thenReturn rightE(keyXMLInfo.copy(reportingEntity = keyXMLInfo.reportingEntity.copy(reportingRole = CBC703)))
      when(cache.save[FilingType](any())(EQ(FilingType.format),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      status(controller.submitterInfo(fakeRequestSubmit)) shouldBe Status.OK
      verify(cache).save(any())(EQ(FilingType.format),any(),any())
    }
  }

  "POST /submitSubmitterInfo" should {
    "return 400 when the there is no data at all" in {
      val fakeRequestSubmit = addToken(FakeRequest("POST", "/submitSubmitterInfo"))
      when(auth.authorise[Option[AffinityGroup]](any(),any())(any(),any())) thenReturn Future.successful(Some(AffinityGroup.Organisation))
      when(cache.read[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn leftE[CBCId](ExpiredSession(""))

      val returnVal = status(controller.submitSubmitterInfo(fakeRequestSubmit))
      returnVal shouldBe Status.BAD_REQUEST
    }
    "return 400 when the all data exists but Fullname" in {
      val submitterInfo = SubmitterInfo("", None,  "07923456708", EmailAddress("abc@xyz.com"),None)
      val fakeRequestSubmit = addToken(FakeRequest("POST", "/submitSubmitterInfo").withJsonBody(Json.toJson(submitterInfo)))
      when(auth.authorise[Option[AffinityGroup]](any(),any())(any(),any())) thenReturn Future.successful(Some(AffinityGroup.Organisation))
      //      when(cache.readOption(EQ(AffinityGroup.jsonFormat),any(),any())) thenReturn Future.successful(Some(AffinityGroup.Organisation))
      when(cache.read[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn leftE[CBCId](ExpiredSession(""))
      val returnVal = status(controller.submitSubmitterInfo(fakeRequestSubmit))
      returnVal shouldBe Status.BAD_REQUEST
    }
    "return 400 when the all data exists but Contact Phone" in {
      val submitterInfo = SubmitterInfo("Fullname", None, "", EmailAddress("abc@xyz.com"),None)
      when(auth.authorise[Option[AffinityGroup]](any(),any())(any(),any())) thenReturn Future.successful(Some(AffinityGroup.Organisation))
      //      when(cache.readOption(EQ(AffinityGroup.jsonFormat),any(),any())) thenReturn Future.successful(Some(AffinityGroup.Organisation))
      when(cache.read[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn leftE[CBCId](ExpiredSession(""))
      val fakeRequestSubmit = addToken(FakeRequest("POST", "/submitSubmitterInfo").withJsonBody(Json.toJson(submitterInfo)))
      status(controller.submitSubmitterInfo(fakeRequestSubmit)) shouldBe Status.BAD_REQUEST
    }
    "return 400 when the all data exists but Email Address" in {

      val submitterInfo = Json.obj(
        "fullName" ->"Fullname",
        "contactPhone" -> "07923456708",
        "email" -> ""
      )
      val fakeRequestSubmit = addToken(FakeRequest("POST", "/submitSubmitterInfo").withJsonBody(Json.toJson(submitterInfo)))
      when(auth.authorise[Option[AffinityGroup]](any(),any())(any(),any())) thenReturn Future.successful(Some(AffinityGroup.Organisation))
      //      when(cache.readOption(EQ(AffinityGroup.jsonFormat),any(),any())) thenReturn Future.successful(Some(AffinityGroup.Organisation))
      when(cache.read[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn leftE[CBCId](ExpiredSession(""))
      status(controller.submitSubmitterInfo(fakeRequestSubmit)) shouldBe Status.BAD_REQUEST
    }
    "return 400 when the all data exists but Email Address is in Invalid format" in {
      val submitterInfo = Json.obj(
        "fullName" ->"Fullname",
        "contactPhone" -> "07923456708",
        "email" -> "abc.xyz"
      )

      val fakeRequestSubmit = addToken(FakeRequest("POST", "/submitSubmitterInfo").withJsonBody(Json.toJson(submitterInfo)))
      when(auth.authorise[Option[AffinityGroup]](any(),any())(any(),any())) thenReturn Future.successful(Some(AffinityGroup.Organisation))
      //      when(cache.readOption(EQ(AffinityGroup.jsonFormat),any(),any())) thenReturn Future.successful(Some(AffinityGroup.Organisation))
      when(cache.read[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn leftE[CBCId](ExpiredSession(""))
      status(controller.submitSubmitterInfo(fakeRequestSubmit)) shouldBe Status.BAD_REQUEST
    }
    "return 400 when the empty fields of data exists" in {
      val submitterInfo = Json.obj(
        "fullName" ->"",
        "contactPhone" -> "",
        "email" -> ""
      )
      val fakeRequestSubmit = addToken(FakeRequest("POST", "/submitSubmitterInfo").withJsonBody(Json.toJson(submitterInfo)))
      when(auth.authorise[Option[AffinityGroup]](any(),any())(any(),any())) thenReturn Future.successful(Some(AffinityGroup.Organisation))
      //      when(cache.readOption(EQ(AffinityGroup.jsonFormat),any(),any())) thenReturn Future.successful(Some(AffinityGroup.Organisation))
      when(cache.read[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn leftE[CBCId](ExpiredSession(""))
      status(controller.submitSubmitterInfo(fakeRequestSubmit)) shouldBe Status.BAD_REQUEST
    }
    "return 303 when all of the data exists & valid" in {
      val submitterInfo = SubmitterInfo("Fullname", None, "07923456708", EmailAddress("abc@xyz.com"),None)
      val fakeRequestSubmit = addToken(FakeRequest("POST", "/submitSubmitterInfo").withJsonBody(Json.toJson(submitterInfo)))

      when(auth.authorise[Option[AffinityGroup]](any(),any())(any(),any())) thenReturn Future.successful(Some(AffinityGroup.Organisation))
      when(cache.readOption[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn Future.successful(None)
      when(cache.save[SubmitterInfo](any())(EQ(SubmitterInfo.format),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      when(cache.read[CompleteXMLInfo](EQ(CompleteXMLInfo.format),any(),any())) thenReturn rightE(keyXMLInfo)
      when(cache.save[CBCId](any())(EQ(CBCId.cbcIdFormat),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      when(cache.readOption[AgencyBusinessName](EQ(AgencyBusinessName.format),any(),any())) thenReturn Future.successful(Some(AgencyBusinessName("Colm Cavanagh ltd")))
      when(cache.read[SubmitterInfo](EQ(SubmitterInfo.format),any(),any())) thenReturn rightE(submitterInfo)
      val returnVal = status(controller.submitSubmitterInfo(fakeRequestSubmit))
      returnVal shouldBe Status.SEE_OTHER


      verify(cache, times(1)).read(EQ(CompleteXMLInfo.format),any(),any())
      verify(cache).save(any())(EQ(SubmitterInfo.format),any(),any())

    }

    "return 303 when Email Address is valid" when{
      "the AffinityGroup is Organisation it" should {

        "redirect to submit-summary if a cbcId exists" in {

          val submitterInfo = SubmitterInfo("Billy Bob", None,  "07923456708", EmailAddress("abc@xyz.com"),None)
          val fakeRequestSubmit = addToken(FakeRequest("POST", "/submitSubmitterInfo").withJsonBody(Json.toJson(submitterInfo)))
          when(auth.authorise[Option[AffinityGroup]](any(),any())(any(),any())) thenReturn Future.successful(Some(AffinityGroup.Organisation))
          when(cache.read[SubmitterInfo](EQ(SubmitterInfo.format), any(), any())) thenReturn rightE(SubmitterInfo("name", None, "0123123123", EmailAddress("max@max.com"), Some(AffinityGroup.Organisation)))
          when(cache.readOption[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn Future.successful(CBCId.create(100).toOption)
          when(cache.save[SubmitterInfo](any())(any(), any(), any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String, JsValue]))
          when(cache.read[CompleteXMLInfo](EQ(CompleteXMLInfo.format),any(),any())) thenReturn rightE(keyXMLInfo)
          when(cache.save[CBCId](any())(EQ(CBCId.cbcIdFormat),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
          when(cache.readOption[AgencyBusinessName](EQ(AgencyBusinessName.format),any(),any())) thenReturn Future.successful(Some(AgencyBusinessName("Colm Cavanagh ltd")))
          val result = Await.result(controller.submitSubmitterInfo(fakeRequestSubmit), 2.seconds)

          result.header.headers("Location") should endWith("/submission/summary")
          status(result) shouldBe Status.SEE_OTHER
        }
        "redirect to enter-cbcId if a cbcid does not exist" in {
          val submitterInfo = SubmitterInfo("Billy Bob", None,  "07923456708", EmailAddress("abc@xyz.com"),None)
          val fakeRequestSubmit = addToken(FakeRequest("POST", "/submitSubmitterInfo").withJsonBody(Json.toJson(submitterInfo)))
          when(auth.authorise[Option[AffinityGroup]](any(),any())(any(),any())) thenReturn Future.successful(Some(AffinityGroup.Organisation))
          when(cache.read[SubmitterInfo](EQ(SubmitterInfo.format), any(), any())) thenReturn rightE(SubmitterInfo("name", None, "0123123123", EmailAddress("max@max.com"), Some(AffinityGroup.Organisation)))
          when(cache.readOption[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn Future.successful(None)
          when(cache.save[SubmitterInfo](any())(any(), any(), any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String, JsValue]))
          when(cache.read[CompleteXMLInfo](EQ(CompleteXMLInfo.format),any(),any())) thenReturn rightE(keyXMLInfo)
          when(cache.save[CBCId](any())(EQ(CBCId.cbcIdFormat),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
          when(cache.readOption[AgencyBusinessName](EQ(AgencyBusinessName.format),any(),any())) thenReturn Future.successful(Some(AgencyBusinessName("Colm Cavanagh ltd")))
          val result = Await.result(controller.submitSubmitterInfo(fakeRequestSubmit), 2.seconds)

          result.header.headers("Location") should endWith("/cbc-id/entry-form")
          status(result) shouldBe Status.SEE_OTHER
        }
      }
      "the AffinityGroup is Agent it" should {
        "redirect to enter-known-facts if a cbcid does not exist" in {
          val submitterInfo = SubmitterInfo("Billy Bob", None,  "07923456708", EmailAddress("abc@xyz.com"),None)
          val fakeRequestSubmit = addToken(FakeRequest("POST", "/submitSubmitterInfo").withJsonBody(Json.toJson(submitterInfo)))
          when(auth.authorise[Option[AffinityGroup]](any(),any())(any(),any())) thenReturn Future.successful(Some(AffinityGroup.Agent))
          when(cache.read[SubmitterInfo](EQ(SubmitterInfo.format), any(), any())) thenReturn rightE(SubmitterInfo("name", None, "0123123123", EmailAddress("max@max.com"), Some(AffinityGroup.Organisation)))
          when(cache.readOption[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn Future.successful(None)
          when(cache.save[SubmitterInfo](any())(any(), any(), any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String, JsValue]))
          when(cache.read[CompleteXMLInfo](EQ(CompleteXMLInfo.format),any(),any())) thenReturn rightE(keyXMLInfo)
          when(cache.save[CBCId](any())(EQ(CBCId.cbcIdFormat),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
          when(cache.readOption[AgencyBusinessName](EQ(AgencyBusinessName.format),any(),any())) thenReturn Future.successful(Some(AgencyBusinessName("Colm Cavanagh ltd")))
          val result = Await.result(controller.submitSubmitterInfo(fakeRequestSubmit), 2.seconds)

          result.header.headers("Location") should endWith("/agent/verify-form")
          status(result) shouldBe Status.SEE_OTHER
        }

      }
    }
  }


  "The submission controller" should {
    "provide a method to generate the metadata that"  should {
      "return a list of errors for each of the missing cache values" in {
        when(cache.readOption[GGId](EQ(GGId.format),any(),any())) thenReturn Future.successful(Some(GGId("ggid","type")))
        when(cache.read[BusinessPartnerRecord]) thenReturn leftE[BusinessPartnerRecord](ExpiredSession(""))
        when(cache.read[TIN]) thenReturn leftE[TIN](ExpiredSession(""))
        when(cache.read[CBCId]) thenReturn leftE[CBCId](ExpiredSession("@"))
        when(cache.read[Hash]) thenReturn leftE[Hash](ExpiredSession(""))
        when(cache.read[FileId]) thenReturn leftE[FileId](ExpiredSession(""))
        when(cache.read[EnvelopeId]) thenReturn leftE[EnvelopeId](ExpiredSession(""))
        when(cache.read[SubmitterInfo]) thenReturn leftE[SubmitterInfo](ExpiredSession(""))
        when(cache.read[FilingType]) thenReturn leftE[FilingType](ExpiredSession(""))
        when(cache.read[UltimateParentEntity]) thenReturn leftE[UltimateParentEntity](ExpiredSession(""))
        when(cache.read[FileMetadata]) thenReturn leftE[FileMetadata](ExpiredSession(""))


        Await.result(generateMetadataFile(cache,creds),10.second).fold(
          errors => errors.toList.size shouldBe 10,
          _ => fail("this should have failed")
        )

        when(cache.read[FileId]) thenReturn rightE(FileId("fileId"))
        Await.result(generateMetadataFile(cache,creds),10.second).fold(
          errors => errors.toList.size shouldBe 9,
          _ => fail("this should have failed")
        )

        when(cache.read[EnvelopeId]) thenReturn rightE(EnvelopeId("yeah"))
        Await.result(generateMetadataFile(cache,creds),10.second).fold(
          errors => errors.toList.size shouldBe 8,
          _ => fail("this should have failed")
        )

      }
      "return a Metadata object if all succeeds" in {
        when(cache.readOption[GGId](EQ(GGId.format),any(),any())) thenReturn Future.successful(Some(GGId("ggid","type")))
        when(cache.read[BusinessPartnerRecord]) thenReturn rightE(bpr)
        when(cache.read[TIN]) thenReturn rightE(TIN("utr",""))
        when(cache.read[CBCId](EQ(CBCId.cbcIdFormat),any(),any())) thenReturn  rightE(CBCId.create(1).getOrElse(fail("argh")))
        when(cache.read[Hash]) thenReturn rightE(Hash("hash"))
        when(cache.read[FileId]) thenReturn rightE(FileId("yeah"))
        when(cache.read[EnvelopeId]) thenReturn rightE(EnvelopeId("id"))
        when(cache.read[SubmitterInfo]) thenReturn rightE(SubmitterInfo("name",None,"0123123123",EmailAddress("max@max.com"), Some(AffinityGroup.Organisation)))
        when(cache.read[FilingType]) thenReturn rightE(FilingType(CBC701))
        when(cache.read[UltimateParentEntity]) thenReturn rightE(UltimateParentEntity("yeah"))
        when(cache.read[FileMetadata]) thenReturn rightE(FileMetadata("asdf","lkjasdf","lkj","lkj",10,"lkjasdf",JsNull,""))

        Await.result(generateMetadataFile(cache,creds),10.second).leftMap(
          errors => fail(s"There should be no errors: $errors")
        )
      }
    }
    "provide a 'submitSummary' Action that" should {
      val fakeRequestSubmitSummary = addToken(FakeRequest("GET", "/submitSummary"))
      "return 303 if generating the metadata fails redirecting to session expired page" in {
        when(auth.authorise[Credentials](any(),any())(any(),any())) thenReturn Future.successful(creds)
        when(cache.readOption[GGId](EQ(GGId.format),any(),any())) thenReturn Future.successful(Some(GGId("ggid","type")))
        when(cache.read[CompleteXMLInfo](EQ(CompleteXMLInfo.format),any(),any())) thenReturn rightE(keyXMLInfo)
        when(cache.read[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format),any(),any())) thenReturn rightE(bpr)
        when(cache.read[TIN](EQ(TIN.format),any(),any())) thenReturn rightE(TIN("utr",""))
        when(cache.read[CBCId](EQ(CBCId.cbcIdFormat),any(),any())) thenReturn  rightE(CBCId.create(1).getOrElse(fail("argh")))
        when(cache.read[Hash](EQ(Hash.format),any(),any())) thenReturn rightE(Hash("hash"))
        when(cache.read[FileId](EQ(FileId.fileIdFormat),any(),any())) thenReturn rightE(FileId("yeah"))
        when(cache.read[EnvelopeId](EQ(EnvelopeId.format),any(),any())) thenReturn rightE(EnvelopeId("id"))
        when(cache.read[SubmitterInfo](EQ(SubmitterInfo.format),any(),any())) thenReturn rightE(SubmitterInfo("name",None,"0123123123",EmailAddress("max@max.com"),None))
        when(cache.read[FilingType](EQ(FilingType.format),any(),any())) thenReturn rightE(FilingType(CBC701))
        when(cache.read[UltimateParentEntity](EQ(UltimateParentEntity.format),any(),any())) thenReturn leftE[UltimateParentEntity](ExpiredSession("nope"))
        when(cache.read[FileMetadata](EQ(FileMetadata.fileMetadataFormat),any(),any())) thenReturn rightE(FileMetadata("asdf","lkjasdf","lkj","lkj",10,"lkjasdf",JsNull,""))
        when(cache.save[SummaryData](any())(any(),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
        val result = controller.submitSummary(fakeRequestSubmitSummary)
        status(result) shouldBe Status.SEE_OTHER
        result.header.headers("Location") should endWith("/session-expired")

      }


      "return 200 if everything succeeds" in {

        val file = File.createTempFile("test","test")

        when(auth.authorise[Credentials](any(),any())(any(),any())) thenReturn Future.successful(creds)
        when(cache.readOption[GGId](EQ(GGId.format),any(),any())) thenReturn Future.successful(Some(GGId("ggid","type")))
        when(cache.read[CompleteXMLInfo](EQ(CompleteXMLInfo.format),any(),any())) thenReturn rightE(keyXMLInfo)
        when(cache.read[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format),any(),any())) thenReturn rightE(bpr)
        when(cache.read[TIN](EQ(TIN.format),any(),any())) thenReturn rightE(TIN("utr",""))
        when(cache.read[CBCId](EQ(CBCId.cbcIdFormat),any(),any())) thenReturn  rightE(CBCId.create(1).getOrElse(fail("argh")))
        when(cache.read[Hash](EQ(Hash.format),any(),any())) thenReturn rightE(Hash("hash"))
        when(cache.read[FileId](EQ(FileId.fileIdFormat),any(),any())) thenReturn rightE(FileId("yeah"))
        when(cache.read[EnvelopeId](EQ(EnvelopeId.format),any(),any())) thenReturn rightE(EnvelopeId("id"))
        when(cache.read[SubmitterInfo](EQ(SubmitterInfo.format),any(),any())) thenReturn rightE(SubmitterInfo("name",None,"0123123123",EmailAddress("max@max.com"),None))
        when(cache.read[FilingType](EQ(FilingType.format),any(),any())) thenReturn rightE(FilingType(CBC701))
        when(cache.read[UltimateParentEntity](EQ(UltimateParentEntity.format),any(),any())) thenReturn rightE(UltimateParentEntity("upe"))
        when(cache.read[FileMetadata](EQ(FileMetadata.fileMetadataFormat),any(),any())) thenReturn rightE(FileMetadata("asdf","lkjasdf","lkj","lkj",10,"lkjasdf",JsNull,""))
        when(fus.getFile(anyString, anyString)(any(),any())) thenReturn EitherT[Future, CBCErrors,File](Future.successful(Right(file)))
        when(cache.save[SummaryData](any())(any(),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))

        status(controller.submitSummary(fakeRequestSubmitSummary)) shouldBe Status.OK

        file.deleteOnExit()

      }
    }

    "provide an action '/confirm'" which {
      "returns a 303 when the call to the cache fails and redirect to session expired" in {
        val summaryData = SummaryData(bpr, submissionData, keyXMLInfo)
        val fakeRequestSubmitSummary = addToken(FakeRequest("POST", "/confirm ").withJsonBody(Json.toJson("{}")))
        when(auth.authorise(any(), any[Retrieval[Credentials ~ Option[AffinityGroup]]]())(any(), any()))
          .thenReturn(Future.successful(new ~[Credentials, Option[AffinityGroup]](creds, Some(AffinityGroup.Organisation))))
        when(cache.read[SummaryData](EQ(SummaryData.format),any(),any())) thenReturn rightE(summaryData)
        when(cache.read[CompleteXMLInfo](EQ(CompleteXMLInfo.format),any(),any())) thenReturn leftE[CompleteXMLInfo](ExpiredSession(""))
        val result = controller.confirm(fakeRequestSubmitSummary)
        status(result) shouldBe Status.SEE_OTHER
        result.header.headers("Location") should endWith("/session-expired")
      }
      "returns a 500 when the call to file-upload fails" in {
        val summaryData = SummaryData(bpr, submissionData, keyXMLInfo)
        val fakeRequestSubmitSummary = addToken(FakeRequest("POST", "/confirm ").withJsonBody(Json.toJson("{}")))
//        when(auth.authorise[Option[AffinityGroup]](any(),any[Retrieval[Option[AffinityGroup]]]())(any(),any())) thenReturn Future.successful(Some(AffinityGroup.Organisation))
//        when(auth.authorise[Credentials](any(),any[Retrieval[Credentials]]())(any(),any())) thenReturn Future.successful(creds)
        when(auth.authorise(any(), any[Retrieval[Credentials ~ Option[AffinityGroup]]]())(any(), any()))
          .thenReturn(Future.successful(new ~[Credentials, Option[AffinityGroup]](creds, Some(AffinityGroup.Organisation))))
        when(cache.read[SummaryData](EQ(SummaryData.format),any(),any())) thenReturn rightE(summaryData)
        when(cache.read[CompleteXMLInfo](EQ(CompleteXMLInfo.format),any(),any())) thenReturn rightE(keyXMLInfo)
        when(fus.uploadMetadataAndRoute(any())(any(),any())) thenReturn EitherT.left[Future,CBCErrors,String](UnexpectedState("fail"))
        val result = Await.result(controller.confirm(fakeRequestSubmitSummary), 50.seconds)
        status(result) shouldBe Status.INTERNAL_SERVER_ERROR

      }
      "returns a 500 when the call to save the docRefIds fail" in {
        val summaryData = SummaryData(bpr, submissionData, keyXMLInfo)
        val fakeRequestSubmitSummary = addToken(FakeRequest("POST", "/confirm ").withJsonBody(Json.toJson("{}")))
//        when(auth.authorise[Credentials](any(),any())(any(),any())) thenReturn Future.successful(creds)
        when(auth.authorise(any(), any[Retrieval[Credentials ~ Option[AffinityGroup]]]())(any(), any()))
          .thenReturn(Future.successful(new ~[Credentials, Option[AffinityGroup]](creds, Some(AffinityGroup.Organisation))))
        when(cache.read[SummaryData](EQ(SummaryData.format),any(),any())) thenReturn rightE(summaryData)
        when(cache.read[CompleteXMLInfo](EQ(CompleteXMLInfo.format),any(),any())) thenReturn rightE(keyXMLInfo)
        when(fus.uploadMetadataAndRoute(any())(any(),any())) thenReturn EitherT.pure[Future,CBCErrors,String]("ok")
        when(docRefService.saveDocRefId(any())(any())) thenReturn OptionT.some[Future,UnexpectedState](UnexpectedState("fails!"))
        when(docRefService.saveCorrDocRefID(any(),any())(any())) thenReturn OptionT.some[Future,UnexpectedState](UnexpectedState("fails!"))
        status(controller.confirm(fakeRequestSubmitSummary)) shouldBe Status.INTERNAL_SERVER_ERROR
      }

      "returns 303 when the there is data and " should {
        "call saveReportingEntityData when the submissionType is OECD1" in {
          val summaryData = SummaryData(bpr, submissionData, keyXMLInfo)
          val fakeRequestSubmitSummary = addToken(FakeRequest("POST", "/confirm ").withJsonBody(Json.toJson("{}")))
//          when(auth.authorise[Credentials](any(),any())(any(),any())) thenReturn Future.successful(creds)
          when(auth.authorise(any(), any[Retrieval[Credentials ~ Option[AffinityGroup]]]())(any(), any()))
            .thenReturn(Future.successful(new ~[Credentials, Option[AffinityGroup]](creds, Some(AffinityGroup.Organisation))))
          when(cache.read[SummaryData](EQ(SummaryData.format),any(),any())) thenReturn rightE(summaryData)
          when(fus.uploadMetadataAndRoute(any())(any(),any())) thenReturn EitherT[Future,CBCErrors,String](Future.successful(Right("routed")))
          when(cache.read[CompleteXMLInfo](EQ(CompleteXMLInfo.format),any(),any())) thenReturn rightE(keyXMLInfo)
          when(cache.save[SubmissionDate](any())(EQ(SubmissionDate.format),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
          when(fus.uploadMetadataAndRoute(any())(any(),any())) thenReturn EitherT.pure[Future,CBCErrors,String]("ok")
          when(reportingEntity.saveReportingEntityData(any())(any())) thenReturn EitherT.pure[Future,CBCErrors,Unit](())
          when(docRefService.saveCorrDocRefID(any(),any())(any())) thenReturn OptionT.none[Future,UnexpectedState]
          when(docRefService.saveDocRefId(any())(any())) thenReturn OptionT.none[Future,UnexpectedState]
          when(cache.readOption[GGId](EQ(GGId.format),any(),any())) thenReturn Future.successful(Some(GGId("ggid","type")))
          when(auditMock.sendExtendedEvent(any())(any(),any())) thenReturn Future.successful(AuditResult.Success)
          status(controller.confirm(fakeRequestSubmitSummary)) shouldBe Status.SEE_OTHER
          verify(reportingEntity).saveReportingEntityData(any())(any())
        }
        "call updateReportingEntityData when the submissionType is OECD[023]" in {
          val summaryData = SummaryData(bpr, submissionData, keyXMLInfo)
          val fakeRequestSubmitSummary = addToken(FakeRequest("POST", "/confirm ").withJsonBody(Json.toJson("{}")))
          lazy val updateXml = keyXMLInfo.copy(reportingEntity = keyXMLInfo.reportingEntity.copy(docSpec = keyXMLInfo.reportingEntity.docSpec.copy(docType = OECD2)))
//          when(auth.authorise[Credentials](any(),any())(any(),any())) thenReturn Future.successful(creds)
          when(auth.authorise(any(), any[Retrieval[Credentials ~ Option[AffinityGroup]]]())(any(), any()))
            .thenReturn(Future.successful(new ~[Credentials, Option[AffinityGroup]](creds, Some(AffinityGroup.Organisation))))
          when(cache.read[SummaryData](EQ(SummaryData.format),any(),any())) thenReturn rightE(summaryData)
          when(fus.uploadMetadataAndRoute(any())(any(),any())) thenReturn EitherT[Future,CBCErrors,String](Future.successful(Right("routed")))
          when(cache.read[CompleteXMLInfo](EQ(CompleteXMLInfo.format),any(),any())) thenReturn rightE(updateXml)
          when(cache.save[SubmissionDate](any())(EQ(SubmissionDate.format),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
          when(fus.uploadMetadataAndRoute(any())(any(),any())) thenReturn EitherT.pure[Future,CBCErrors,String]("ok")
          when(reportingEntity.updateReportingEntityData(any())(any())) thenReturn EitherT.pure[Future,CBCErrors,Unit](())
          when(docRefService.saveCorrDocRefID(any(),any())(any())) thenReturn OptionT.none[Future,UnexpectedState]
          when(docRefService.saveDocRefId(any())(any())) thenReturn OptionT.none[Future,UnexpectedState]
          when(cache.readOption[GGId](EQ(GGId.format),any(),any())) thenReturn Future.successful(Some(GGId("ggid","type")))
          when(auditMock.sendEvent(any())(any(),any())) thenReturn Future.successful(AuditResult.Success)
          status(controller.confirm(fakeRequestSubmitSummary)) shouldBe Status.SEE_OTHER
          verify(reportingEntity).updateReportingEntityData(any())(any())

        }
      }

    }

    "provide an action 'submitSuccessReceipt'" which {
      "returns a 303 Redirect if it fails to read from the cache" when {
        "looking for the SummaryData" in {

          when(auth.authorise[Any](any(),any())(any(),any())) thenReturn Future.successful(())
          val fakeRequestSubmitSummary = addToken(FakeRequest("GET", "/submitSuccessReceipt"))

          when(cache.read[SummaryData](EQ(SummaryData.format), any(), any())) thenReturn leftE[SummaryData](ExpiredSession(""))
          when(cache.read[SubmissionDate](EQ(SubmissionDate.format), any(), any())) thenReturn rightE(SubmissionDate(LocalDateTime.now()))
          val result = controller.submitSuccessReceipt("Organisation")(fakeRequestSubmitSummary)
          status(result) shouldBe Status.SEE_OTHER
          result.header.headers("Location") should endWith("/session-expired")

        }
        "looking for the SubmissionDate" in {
          when(auth.authorise[Any](any(),any())(any(),any())) thenReturn Future.successful(())
          val summaryData = SummaryData(bpr, submissionData, keyXMLInfo)
          val fakeRequestSubmitSummary = addToken(FakeRequest("GET", "/submitSuccessReceipt"))

          when(cache.read[SummaryData](EQ(SummaryData.format), any(), any())) thenReturn rightE(summaryData)
          when(cache.read[SubmissionDate](EQ(SubmissionDate.format), any(), any())) thenReturn leftE[SubmissionDate](ExpiredSession(""))
          val result = controller.submitSuccessReceipt("Organisation")(fakeRequestSubmitSummary)
          status(result) shouldBe Status.SEE_OTHER
          result.header.headers("Location") should endWith("/session-expired")

        }
        "looking for the CBCId" in {
          when(auth.authorise[Any](any(),any())(any(),any())) thenReturn Future.successful(())
          val summaryData = SummaryData(bpr, submissionData, keyXMLInfo)
          val fakeRequestSubmitSummary = addToken(FakeRequest("GET", "/submitSuccessReceipt"))

          when(cache.read[SummaryData](EQ(SummaryData.format), any(), any())) thenReturn rightE(summaryData)
          when(cache.read[SubmissionDate](EQ(SubmissionDate.format), any(), any())) thenReturn rightE(SubmissionDate(LocalDateTime.now()))
          when(cache.read[CBCId](EQ(CBCId.cbcIdFormat),any(),any())) thenReturn  leftE[CBCId](ExpiredSession(""))
          val result = controller.submitSuccessReceipt("Organisation")(fakeRequestSubmitSummary)
          status(result) shouldBe Status.SEE_OTHER
          result.header.headers("Location") should endWith("/session-expired")

        }
      }
      "sends an email" in {
        val summaryData = SummaryData(bpr, submissionData, keyXMLInfo)
        val fakeRequestSubmitSummary = addToken(FakeRequest("GET", "/submitSuccessReceipt"))
        when(auth.authorise[Any](any(),any())(any(),any())) thenReturn Future.successful(())
        when(cache.read[SummaryData](EQ(SummaryData.format), any(), any())) thenReturn rightE(summaryData)
        when(cache.read[SubmissionDate](EQ(SubmissionDate.format), any(), any())) thenReturn rightE(SubmissionDate(LocalDateTime.now()))
        when(cache.readOption[GGId](EQ(GGId.format),any(),any())) thenReturn Future.successful(Some(GGId("ggid","type")))
        when(mockEmailService.sendEmail(any())(any())) thenReturn  OptionT.pure[Future,Boolean](true)
        when(cache.save[ConfirmationEmailSent](any())(EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
        when(cache.readOption[ConfirmationEmailSent](EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat), any(), any())) thenReturn Future.successful(None)
        when(cache.read[CBCId](EQ(CBCId.cbcIdFormat),any(),any())) thenReturn  rightE(CBCId.create(1).getOrElse(fail("argh")))
//        when(auth.authorise[Option[AffinityGroup]](any(),any())(any(),any())) thenReturn Future.successful(Some(AffinityGroup.Organisation))
        when(cache.clear(any())) thenReturn Future.successful(true)
        val result: Future[Result] = controller.submitSuccessReceipt("Organisation")(fakeRequestSubmitSummary)
        status(result) shouldBe Status.OK
        verify(mockEmailService,times(1)).sendEmail(any())(any())
        verify(cache,times(1)).save(any())(EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat),any(),any())
        val webPageAsString =   contentAsString(result)
        webPageAsString should not include(getMessages(fakeRequestSubmitSummary)("submitSuccessReceipt.sendAnotherReport.link"))
      }
      "will still return a 200 if the email fails" in {
        val summaryData = SummaryData(bpr, submissionData, keyXMLInfo)
        val fakeRequestSubmitSummary = addToken(FakeRequest("GET", "/submitSuccessReceipt"))
        when(auth.authorise[Any](any(),any())(any(),any())) thenReturn Future.successful(())
        when(cache.readOption[GGId](EQ(GGId.format),any(),any())) thenReturn Future.successful(Some(GGId("ggid","type")))
        when(cache.save[ConfirmationEmailSent](any())(EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
        when(mockEmailService.sendEmail(any())(any())) thenReturn  OptionT.pure[Future,Boolean](false)
        when(cache.readOption[ConfirmationEmailSent](EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat), any(), any())) thenReturn Future.successful(None)
        when(cache.read[SummaryData](EQ(SummaryData.format), any(), any())) thenReturn rightE(summaryData)
        when(cache.read[SubmissionDate](EQ(SubmissionDate.format), any(), any())) thenReturn rightE(SubmissionDate(LocalDateTime.now()))
        when(cache.read[CBCId](EQ(CBCId.cbcIdFormat),any(),any())) thenReturn  rightE(CBCId.create(1).getOrElse(fail("argh")))
//        when(auth.authorise[Option[AffinityGroup]](any(),any())(any(),any())) thenReturn Future.successful(Some(AffinityGroup.Organisation))
        when(cache.clear(any())) thenReturn Future.successful(true)
        val result: Future[Result] = controller.submitSuccessReceipt("Organisation")(fakeRequestSubmitSummary)
        status(result) shouldBe Status.OK
        verify(mockEmailService,times(1)).sendEmail(any())(any())
        verify(cache,times(0)).save(any())(EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat),any(),any())
        val webPageAsString =   contentAsString(result)
        webPageAsString should not include(getMessages(fakeRequestSubmitSummary)("submitSuccessReceipt.sendAnotherReport.link"))
      }
      "will write  a ConfirmationEmailSent to the cache if an email is sent" in {
        val summaryData = SummaryData(bpr, submissionData, keyXMLInfo)
        val fakeRequestSubmitSummary = addToken(FakeRequest("GET", "/submitSuccessReceipt"))
        when(auth.authorise[Any](any(),any())(any(),any())) thenReturn Future.successful(())
        when(cache.readOption[GGId](EQ(GGId.format),any(),any())) thenReturn Future.successful(Some(GGId("ggid","type")))
        when(mockEmailService.sendEmail(any())(any())) thenReturn  OptionT.pure[Future,Boolean](true)
        when(cache.read[SummaryData](EQ(SummaryData.format), any(), any())) thenReturn rightE(summaryData)
        when(cache.save[ConfirmationEmailSent](any())(EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
        when(cache.readOption[ConfirmationEmailSent](EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat), any(), any())) thenReturn Future.successful(None)
        when(cache.read[SubmissionDate](EQ(SubmissionDate.format), any(), any())) thenReturn rightE(SubmissionDate(LocalDateTime.now()))
        when(cache.read[CBCId](EQ(CBCId.cbcIdFormat),any(),any())) thenReturn  rightE(CBCId.create(1).getOrElse(fail("argh")))
//        when(auth.authorise[Option[AffinityGroup]](any(),any())(any(),any())) thenReturn Future.successful(Some(AffinityGroup.Organisation))
        when(cache.clear(any())) thenReturn Future.successful(true)
        val result: Future[Result] = controller.submitSuccessReceipt("Organisation")(fakeRequestSubmitSummary)
        status(result) shouldBe Status.OK
        verify(mockEmailService,times(1)).sendEmail(any())(any())
        verify(cache,times(1)).save(any())(EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat),any(),any())
        val webPageAsString =   contentAsString(result)
        webPageAsString should not include(getMessages(fakeRequestSubmitSummary)("submitSuccessReceipt.sendAnotherReport.link"))
      }
      "not send the email if it has already been sent and not save to the cache" in {
        val summaryData = SummaryData(bpr, submissionData, keyXMLInfo)
        val fakeRequestSubmitSummary = addToken(FakeRequest("GET", "/submitSuccessReceipt:Organisation"))
        when(auth.authorise[Any](any(),any())(any(),any())) thenReturn Future.successful(())
        when(cache.readOption[GGId](EQ(GGId.format),any(),any())) thenReturn Future.successful(Some(GGId("ggid","type")))
        when(mockEmailService.sendEmail(any())(any())) thenReturn  OptionT.pure[Future,Boolean](true)
        when(cache.save[ConfirmationEmailSent](any())(EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
        when(cache.read[SummaryData](EQ(SummaryData.format), any(), any())) thenReturn rightE(summaryData)
        when(cache.readOption[ConfirmationEmailSent](EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat), any(), any())) thenReturn Future.successful(Some(ConfirmationEmailSent("yep")))
        when(cache.read[SubmissionDate](EQ(SubmissionDate.format), any(), any())) thenReturn rightE(SubmissionDate(LocalDateTime.now()))
        when(cache.read[CBCId](EQ(CBCId.cbcIdFormat),any(),any())) thenReturn  rightE(CBCId.create(1).getOrElse(fail("argh")))
//        when(auth.authorise[Option[AffinityGroup]](any(),any())(any(),any())) thenReturn Future.successful(Some(AffinityGroup.Organisation))
        when(cache.clear(any())) thenReturn Future.successful(true)
        val result: Future[Result] = controller.submitSuccessReceipt("Organisation")(fakeRequestSubmitSummary)
        status(result) shouldBe Status.OK
        status(controller.submitSuccessReceipt("Organisation")(fakeRequestSubmitSummary)) shouldBe Status.OK
        verify(mockEmailService,times(0)).sendEmail(any())(any())
        verify(cache,times(0)).save(any())(EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat),any(),any())
        val webPageAsString =   contentAsString(result)
        webPageAsString should not include(getMessages(fakeRequestSubmitSummary)("submitSuccessReceipt.sendAnotherReport.link"))
      }
      "returns a 200 otherwise" in {
        val summaryData = SummaryData(bpr, submissionData, keyXMLInfo)
        val fakeRequestSubmitSummary = addToken(FakeRequest("GET", "/submitSuccessReceipt:Organisation"))
        when(auth.authorise[Any](any(),any())(any(),any())) thenReturn Future.successful(())
        when(cache.readOption[GGId](EQ(GGId.format),any(),any())) thenReturn Future.successful(Some(GGId("ggid","type")))
        when(mockEmailService.sendEmail(any())(any())) thenReturn  OptionT.pure[Future,Boolean](true)
        when(cache.save[ConfirmationEmailSent](any())(EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
        when(cache.read[SummaryData](EQ(SummaryData.format), any(), any())) thenReturn rightE(summaryData)
        when(cache.readOption[ConfirmationEmailSent](EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat), any(), any())) thenReturn Future.successful(None)
        when(cache.read[SubmissionDate](EQ(SubmissionDate.format), any(), any())) thenReturn rightE(SubmissionDate(LocalDateTime.now()))
        when(cache.read[CBCId](EQ(CBCId.cbcIdFormat),any(),any())) thenReturn  rightE(CBCId.create(1).getOrElse(fail("argh")))
//        when(auth.authorise[Option[AffinityGroup]](any(),any())(any(),any())) thenReturn Future.successful(Some(AffinityGroup.Organisation))
        when(cache.clear(any())) thenReturn Future.successful(true)
        val result: Future[Result] = controller.submitSuccessReceipt("Organisation")(fakeRequestSubmitSummary)
        status(result) shouldBe Status.OK
        verify(cache,times(1)).save(any())(EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat),any(),any())
        val webPageAsString =   contentAsString(result)
        webPageAsString should not include(getMessages(fakeRequestSubmitSummary)("submitSuccessReceipt.sendAnotherReport.link"))
      }
      "show show link to submit another report if AffinityGroup is Agent and cache.clear succeeds" in {
        val summaryData = SummaryData(bpr, submissionData, keyXMLInfo)
        val fakeRequestSubmitSummary = addToken(FakeRequest("GET", "/submitSuccessReceipt:Agent"))
        when(auth.authorise[Any](any(),any())(any(),any())) thenReturn Future.successful(())
        when(cache.readOption[GGId](EQ(GGId.format),any(),any())) thenReturn Future.successful(Some(GGId("ggid","type")))
        when(mockEmailService.sendEmail(any())(any())) thenReturn  OptionT.pure[Future,Boolean](true)
        when(cache.save[ConfirmationEmailSent](any())(EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
        when(cache.read[SummaryData](EQ(SummaryData.format), any(), any())) thenReturn rightE(summaryData)
        when(cache.readOption[ConfirmationEmailSent](EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat), any(), any())) thenReturn Future.successful(None)
        when(cache.read[SubmissionDate](EQ(SubmissionDate.format), any(), any())) thenReturn rightE(SubmissionDate(LocalDateTime.now()))
        when(cache.read[CBCId](EQ(CBCId.cbcIdFormat),any(),any())) thenReturn  rightE(CBCId.create(1).getOrElse(fail("argh")))
//        when(auth.authorise[Option[AffinityGroup]](any(),any())(any(),any())) thenReturn Future.successful(Some(AffinityGroup.Agent))
        when(cache.clear(any())) thenReturn Future.successful(true)
        val result: Future[Result] = controller.submitSuccessReceipt("Agent")(fakeRequestSubmitSummary)
        status(result) shouldBe Status.OK
        val webPageAsString =   contentAsString(result)
        webPageAsString should include(getMessages(fakeRequestSubmitSummary)("submitSuccessReceipt.sendAnotherReport.link"))
      }
      "show NOT show link to submit another reportf AffinityGroup is Agent but cache.clear fails" in {
        val summaryData = SummaryData(bpr, submissionData, keyXMLInfo)
        val fakeRequestSubmitSummary = addToken(FakeRequest("GET", "/submitSuccessReceipt"))
        when(auth.authorise[Any](any(),any())(any(),any())) thenReturn Future.successful(())
        when(cache.readOption[GGId](EQ(GGId.format),any(),any())) thenReturn Future.successful(Some(GGId("ggid","type")))
        when(mockEmailService.sendEmail(any())(any())) thenReturn  OptionT.pure[Future,Boolean](true)
        when(cache.save[ConfirmationEmailSent](any())(EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
        when(cache.read[SummaryData](EQ(SummaryData.format), any(), any())) thenReturn rightE(summaryData)
        when(cache.readOption[ConfirmationEmailSent](EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat), any(), any())) thenReturn Future.successful(None)
        when(cache.read[SubmissionDate](EQ(SubmissionDate.format), any(), any())) thenReturn rightE(SubmissionDate(LocalDateTime.now()))
        when(cache.read[CBCId](EQ(CBCId.cbcIdFormat),any(),any())) thenReturn  rightE(CBCId.create(1).getOrElse(fail("argh")))
//        when(auth.authorise[Option[AffinityGroup]](any(),any())(any(),any())) thenReturn Future.successful(Some(AffinityGroup.Agent))
        when(cache.clear(any())) thenReturn Future.successful(false)
        val result: Future[Result] = controller.submitSuccessReceipt("Organisation")(fakeRequestSubmitSummary)
        status(result) shouldBe Status.OK
        val webPageAsString =   contentAsString(result)
        webPageAsString should not include(getMessages(fakeRequestSubmitSummary)("submitSuccessReceipt.sendAnotherReport.link"))
      }
      "show NOT show link to submit another report if AffinityGroup is NOT Agent and cache.clear succeeds" in {
        val summaryData = SummaryData(bpr, submissionData, keyXMLInfo)
        val fakeRequestSubmitSummary = addToken(FakeRequest("GET", "/submitSuccessReceipt"))
        when(auth.authorise[Any](any(),any())(any(),any())) thenReturn Future.successful(())
        when(cache.readOption[GGId](EQ(GGId.format),any(),any())) thenReturn Future.successful(Some(GGId("ggid","type")))
        when(mockEmailService.sendEmail(any())(any())) thenReturn  OptionT.pure[Future,Boolean](true)
        when(cache.save[ConfirmationEmailSent](any())(EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
        when(cache.read[SummaryData](EQ(SummaryData.format), any(), any())) thenReturn rightE(summaryData)
        when(cache.readOption[ConfirmationEmailSent](EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat), any(), any())) thenReturn Future.successful(None)
        when(cache.read[SubmissionDate](EQ(SubmissionDate.format), any(), any())) thenReturn rightE(SubmissionDate(LocalDateTime.now()))
        when(cache.read[CBCId](EQ(CBCId.cbcIdFormat),any(),any())) thenReturn  rightE(CBCId.create(1).getOrElse(fail("argh")))
//        when(auth.authorise[Option[AffinityGroup]](any(),any())(any(),any())) thenReturn Future.successful(Some(AffinityGroup.Organisation))
        when(cache.clear(any())) thenReturn Future.successful(true)
        val result: Future[Result] = controller.submitSuccessReceipt("Organisation")(fakeRequestSubmitSummary)
        status(result) shouldBe Status.OK
        val webPageAsString =   contentAsString(result)
        webPageAsString should not include(getMessages(fakeRequestSubmitSummary)("submitSuccessReceipt.sendAnotherReport.link"))
      }
    }
    "contain a valid dateformat" in {

      LocalDateTime.of(2017,12,1,23,59,59).format(controller.dateFormat) shouldEqual "01 December 2017 at 23:59"
    }
    "display the audit information correctly" in {
      val sd = SummaryData(bpr,submissionData,keyXMLInfo)
      val sdj = Json.toJson(sd)
      (sdj \ "submissionMetaData" \ "submissionInfo" \ "ultimateParentEntity").as[String] shouldEqual "ultimateParentEntity"
      (sdj \ "submissionMetaData" \ "submissionInfo" \ "filingType").as[String] shouldEqual "PRIMARY"
      (sdj \ "submissionMetaData" \ "submitterInfo" \ "affinityGroup").as[String] shouldEqual "Agent"
    }
  }

  private def submissionData = {
    val fileInfo = FileInfo( FileId("id") ,
      EnvelopeId("envelopeId"),
      "status",
      "name",
      "contentType",
      BigDecimal(0.0),
      "created"
    )
    val submissionInfo =   SubmissionInfo(
     "gwGredId",
     CBCId("XVCBC0000000056").get,
     "bpSafeId",
     Hash("hash"),
     "ofdsRegime",
     TIN("tin","GB"),
     FilingType(CBC701),
     UltimateParentEntity("ultimateParentEntity")
   )
    val submitterInfo = SubmitterInfo("fullName", Some(AgencyBusinessName("MyAgency")), "contactPhone", EmailAddress("abc@abc.com"), Some(AffinityGroup.Agent))
    SubmissionMetaData(submissionInfo, submitterInfo, fileInfo)

  }

  val docRefId="GB2016RGXVCBC0000000056CBC40120170311T090000X_7000000002OECD1ENTZ"

  private lazy val keyXMLInfo = {
    CompleteXMLInfo(
      MessageSpec(
        MessageRefID("GB2016RGXVCBC0000000056CBC40120170311T090000X").getOrElse(fail("waaaaa")),
        "GB",
        CBCId.create(99).getOrElse(fail("booo")),
        LocalDateTime.now(),
        LocalDate.parse("2017-01-30"),
        None
      ),
      ReportingEntity(CBC701,DocSpec(OECD1,DocRefId(docRefId).get,None),TIN("7000000002", "gb"),"name"),
      List(CbcReports(DocSpec(OECD1,DocRefId(docRefId).get,None))),
      Some(AdditionalInfo(DocSpec(OECD1,DocRefId(docRefId).get,None)))
    )
  }
}
