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
import java.time.{LocalDate, LocalDateTime, Year}

import akka.actor.ActorSystem
import cats.instances.future._
import cats.data.{EitherT, OptionT}
import org.mockito.Matchers.{eq => EQ, _}
import org.mockito.Mockito._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.mock.MockitoSugar
import org.scalatestplus.play.OneAppPerSuite
import play.api.http.Status
import play.api.i18n.MessagesApi
import play.api.libs.json.{JsNull, JsValue, Json}
import play.api.test.FakeRequest
import uk.gov.hmrc.cbcrfrontend._
import uk.gov.hmrc.cbcrfrontend.controllers.auth.{SecuredActionsTest, TestUsers}
import uk.gov.hmrc.cbcrfrontend.model.{FileId, XMLInfo, _}
import uk.gov.hmrc.cbcrfrontend.services._
import uk.gov.hmrc.cbcrfrontend.services.{CBCSessionCache, DocRefIdService, FileUploadService, ReportingEntityDataService}
import uk.gov.hmrc.cbcrfrontend.typesclasses.{CbcrsUrl, FusFeUrl, FusUrl, ServiceUrl}
import uk.gov.hmrc.emailaddress.EmailAddress
import uk.gov.hmrc.http.cache.client.CacheMap
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}
import uk.gov.hmrc.play.frontend.auth.AuthContext
import uk.gov.hmrc.play.frontend.auth.connectors.AuthConnector
import uk.gov.hmrc.play.frontend.auth.connectors.domain.{Accounts, Authority, ConfidenceLevel, CredentialStrength}
import uk.gov.hmrc.play.http.HeaderCarrier
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}


class SubmissionSpec  extends UnitSpec with OneAppPerSuite with CSRFTest with MockitoSugar with FakeAuthConnector with BeforeAndAfterEach {


  implicit val ec = app.injector.instanceOf[ExecutionContext]
  implicit val messagesApi = app.injector.instanceOf[MessagesApi]
  val authCon = authConnector(TestUsers.cbcrUser)
  val securedActions = new SecuredActionsTest(TestUsers.cbcrUser, authCon)
  implicit val as = app.injector.instanceOf[ActorSystem]

  val cache = mock[CBCSessionCache]
  val fus  = mock[FileUploadService]
  val docRefService = mock[DocRefIdService]
  val auth = mock[AuthConnector]
  val auditMock = mock[AuditConnector]
  val mockCBCIdService   = mock[CBCIdService]
  val mockEmailService = mock[EmailService]
  val reportingEntity = mock[ReportingEntityDataService]

  implicit lazy val fusUrl = new ServiceUrl[FusUrl] { val url = "file-upload"}
  implicit lazy val fusFeUrl = new ServiceUrl[FusFeUrl] { val url = "file-upload-frontend"}
  implicit lazy val cbcrsUrl = new ServiceUrl[CbcrsUrl] { val url = "cbcr"}

  val bpr = BusinessPartnerRecord("safeId",None, EtmpAddress("Line1",None,None,None,None,"GB"))

  val cbcId = CBCId.create(99).getOrElse(fail("failed to gen cbcid"))

  val authContext = AuthContext(Authority("",Accounts(),None,None,CredentialStrength.Weak,ConfidenceLevel.L200,None,None,None,"oid"))

  implicit val hc = HeaderCarrier()
  val controller = new SubmissionController(securedActions, fus, docRefService,reportingEntity,mockCBCIdService,mockEmailService)(ec,cache,auth) {
    override lazy val audit = auditMock
  }

  override protected def afterEach(): Unit = {
    reset(cache,fus,docRefService,reportingEntity,mockEmailService)
    super.afterEach()
  }

  "POST /submitUltimateParentEntity " should {
    val ultimateParentEntity  = UltimateParentEntity("UlitmateParentEntity")
    val fakeRequestSubmit = addToken(FakeRequest("POST", "/submitUltimateParentEntity ").withJsonBody(Json.obj("ultimateParentEntity" -> ultimateParentEntity.ultimateParentEntity)))
    "return 303 and point to the correct page" when {
      "the reporting role is CBC702" in {
        when(cache.read(EQ(XMLInfo.format),any(),any())) thenReturn Future.successful(Some(keyXMLInfo.copy(reportingEntity = keyXMLInfo.reportingEntity.copy(reportingRole = CBC702))))
        when(cache.save[UltimateParentEntity](any())(EQ(UltimateParentEntity.format),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
        val result = Await.result(controller.submitUltimateParentEntity(fakeRequestSubmit), 2.second)
        result.header.headers("Location") should endWith("/utr/entry-form")
        status(result) shouldBe Status.SEE_OTHER
      }
      "the reporting role is CBC703" in {
        when(cache.save[UltimateParentEntity](any())(EQ(UltimateParentEntity.format),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
        when(cache.read(EQ(XMLInfo.format),any(),any())) thenReturn Future.successful(Some(keyXMLInfo.copy(reportingEntity = keyXMLInfo.reportingEntity.copy(reportingRole = CBC703))))
        when(cache.save[UltimateParentEntity](any())(EQ(UltimateParentEntity.format),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
        val result = Await.result(controller.submitUltimateParentEntity(fakeRequestSubmit), 2.second)
        result.header.headers("Location") should endWith("/company-name/entry-form")
        status(result) shouldBe Status.SEE_OTHER
      }
    }
    "return 500 when the reportingrole is CBC701 as this should never happen" in {
      when(cache.save[UltimateParentEntity](any())(EQ(UltimateParentEntity.format),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      when(cache.read(EQ(XMLInfo.format),any(),any())) thenReturn Future.successful(Some(keyXMLInfo.copy(reportingEntity = keyXMLInfo.reportingEntity.copy(reportingRole = CBC701))))
      val result = Await.result(controller.submitUltimateParentEntity(fakeRequestSubmit), 2.second)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }
  }

  "GET /submitter-info" should {
    "return a 200" in {
      val fakeRequestSubmit = addToken(FakeRequest("GET", "/submitter-info"))
      when(cache.read[XMLInfo](EQ(XMLInfo.format),any(),any())) thenReturn Future.successful(Some(keyXMLInfo))
      when(cache.save[Utr](any())(EQ(Utr.utrFormat),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      status(controller.submitterInfo(fakeRequestSubmit)) shouldBe Status.OK
    }
    "use the UTR, UPE and Filing type form the xml when the ReportingRole is CBC701 " in {
      val cache = mock[CBCSessionCache]
      val controller = new SubmissionController(securedActions, fus, docRefService,reportingEntity,mockCBCIdService,mockEmailService)(ec,cache,auth)
      val fakeRequestSubmit = addToken(FakeRequest("GET", "/submitter-info"))
      when(cache.read[XMLInfo](EQ(XMLInfo.format),any(),any())) thenReturn Future.successful(Some(keyXMLInfo))
      when(cache.save[UltimateParentEntity](any())(EQ(UltimateParentEntity.format),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      when(cache.save[Utr](any())(EQ(Utr.utrFormat),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      when(cache.save[FilingType](any())(EQ(FilingType.format),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      status(controller.submitterInfo(fakeRequestSubmit)) shouldBe Status.OK
      verify(cache).save(any())(EQ(Utr.utrFormat),any(),any())
      verify(cache).save(any())(EQ(FilingType.format),any(),any())
      verify(cache).save(any())(EQ(UltimateParentEntity.format),any(),any())
    }
    "use the Filing type form the xml when the ReportingRole is CBC702" in {
      val cache = mock[CBCSessionCache]
      val controller = new SubmissionController(securedActions, fus, docRefService,reportingEntity,mockCBCIdService,mockEmailService)(ec,cache,auth)
      val fakeRequestSubmit = addToken(FakeRequest("GET", "/submitter-info"))
      when(cache.read[XMLInfo](EQ(XMLInfo.format),any(),any())) thenReturn Future.successful(Some(keyXMLInfo.copy(reportingEntity = keyXMLInfo.reportingEntity.copy(reportingRole = CBC702))))
      when(cache.save[FilingType](any())(EQ(FilingType.format),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      status(controller.submitterInfo(fakeRequestSubmit)) shouldBe Status.OK
      verify(cache).save(any())(EQ(FilingType.format),any(),any())
    }
    "use the UTR and Filing type form the xml when the ReportingRole is CBC703" in {
      val cache = mock[CBCSessionCache]
      val controller = new SubmissionController(securedActions, fus, docRefService,reportingEntity,mockCBCIdService,mockEmailService)(ec,cache,auth){
        override lazy val audit = auditMock
      }
      val fakeRequestSubmit = addToken(FakeRequest("GET", "/submitter-info"))
      when(cache.read[XMLInfo](EQ(XMLInfo.format),any(),any())) thenReturn Future.successful(Some(keyXMLInfo.copy(reportingEntity = keyXMLInfo.reportingEntity.copy(reportingRole = CBC703))))
      when(cache.save[Utr](any())(EQ(Utr.utrFormat),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      when(cache.save[FilingType](any())(EQ(FilingType.format),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      status(controller.submitterInfo(fakeRequestSubmit)) shouldBe Status.OK
      verify(cache).save(any())(EQ(Utr.utrFormat),any(),any())
      verify(cache).save(any())(EQ(FilingType.format),any(),any())
    }
  }

  "POST /submitSubmitterInfo" should {
    "return 400 when the there is no data at all" in {
      val fakeRequestSubmit = addToken(FakeRequest("POST", "/submitSubmitterInfo"))
      when(cache.read[AffinityGroup](EQ(AffinityGroup.format), any(), any())) thenReturn Future.successful(Some(AffinityGroup("organisation")))
      when(cache.read[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn Future.successful(None)

      val returnVal = status(controller.submitSubmitterInfo(fakeRequestSubmit))
      returnVal shouldBe Status.BAD_REQUEST
    }
    "return 400 when the all data exists but Fullname" in {
      val submitterInfo = SubmitterInfo("", None,  "07923456708", EmailAddress("abc@xyz.com"),None)
      val fakeRequestSubmit = addToken(FakeRequest("POST", "/submitSubmitterInfo").withJsonBody(Json.toJson(submitterInfo)))
      when(cache.read[AffinityGroup](EQ(AffinityGroup.format), any(), any())) thenReturn Future.successful(Some(AffinityGroup("organisation")))
      when(cache.read[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn Future.successful(None)
      val returnVal = status(controller.submitSubmitterInfo(fakeRequestSubmit))
      returnVal shouldBe Status.BAD_REQUEST
    }
    "return 400 when the all data exists but Contact Phone" in {
      val submitterInfo = SubmitterInfo("Fullname", None, "", EmailAddress("abc@xyz.com"),None)
      when(cache.read[AffinityGroup](EQ(AffinityGroup.format), any(), any())) thenReturn Future.successful(Some(AffinityGroup("organisation")))
      when(cache.read[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn Future.successful(None)
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
      when(cache.read[AffinityGroup](EQ(AffinityGroup.format), any(), any())) thenReturn Future.successful(Some(AffinityGroup("organisation")))
      when(cache.read[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn Future.successful(None)
      status(controller.submitSubmitterInfo(fakeRequestSubmit)) shouldBe Status.BAD_REQUEST
    }
    "return 400 when the all data exists but Email Address is in Invalid format" in {
      val submitterInfo = Json.obj(
        "fullName" ->"Fullname",
        "contactPhone" -> "07923456708",
        "email" -> "abc.xyz"
      )

      val fakeRequestSubmit = addToken(FakeRequest("POST", "/submitSubmitterInfo").withJsonBody(Json.toJson(submitterInfo)))
      when(cache.read[AffinityGroup](EQ(AffinityGroup.format), any(), any())) thenReturn Future.successful(Some(AffinityGroup("organisation")))
      when(cache.read[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn Future.successful(None)
      status(controller.submitSubmitterInfo(fakeRequestSubmit)) shouldBe Status.BAD_REQUEST
    }
    "return 400 when the empty fields of data exists" in {
      val submitterInfo = Json.obj(
        "fullName" ->"",
        "contactPhone" -> "",
        "email" -> ""
      )
      val fakeRequestSubmit = addToken(FakeRequest("POST", "/submitSubmitterInfo").withJsonBody(Json.toJson(submitterInfo)))
      when(cache.read[AffinityGroup](EQ(AffinityGroup.format), any(), any())) thenReturn Future.successful(Some(AffinityGroup("organisation")))
      when(cache.read[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn Future.successful(None)
      status(controller.submitSubmitterInfo(fakeRequestSubmit)) shouldBe Status.BAD_REQUEST
    }
    "return 303 when all of the data exists & valid" in {
      val submitterInfo = SubmitterInfo("Fullname", None, "07923456708", EmailAddress("abc@xyz.com"),None)
      val fakeRequestSubmit = addToken(FakeRequest("POST", "/submitSubmitterInfo").withJsonBody(Json.toJson(submitterInfo)))

      when(cache.read[AffinityGroup](EQ(AffinityGroup.format),any(),any())) thenReturn Future.successful(Some(AffinityGroup("Organisation")))
      when(cache.read[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn Future.successful(None)
      when(cache.save[SubmitterInfo](any())(EQ(SubmitterInfo.format),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      when(cache.read[XMLInfo](EQ(XMLInfo.format),any(),any())) thenReturn Future.successful(Some(keyXMLInfo))
      when(cache.save[CBCId](any())(EQ(CBCId.cbcIdFormat),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
      when(cache.read[AgencyBusinessName](EQ(AgencyBusinessName.format),any(),any())) thenReturn Future.successful(Some(AgencyBusinessName("Colm Cavanagh ltd")))
      when(cache.read[SubmitterInfo](EQ(SubmitterInfo.format),any(),any())) thenReturn Future.successful(Some(submitterInfo))
      val returnVal = status(controller.submitSubmitterInfo(fakeRequestSubmit))
      returnVal shouldBe Status.SEE_OTHER


      verify(cache, times(2)).read(EQ(AffinityGroup.format),any(),any())
      verify(cache).read(EQ(XMLInfo.format),any(),any())
      verify(cache,times(2)).save(any())(EQ(SubmitterInfo.format),any(),any())
      verify(cache).save(any())(EQ(CBCId.cbcIdFormat),any(),any())

    }

    "return 303 when Email Address is valid" when{
      "the AffinityGroup is Organisation it" should {

        "redirect to submit-summary if a cbcId exists" in {

          val submitterInfo = SubmitterInfo("Billy Bob", None,  "07923456708", EmailAddress("abc@xyz.com"),None)
          val fakeRequestSubmit = addToken(FakeRequest("POST", "/submitSubmitterInfo").withJsonBody(Json.toJson(submitterInfo)))
          when(cache.read[AffinityGroup](EQ(AffinityGroup.format), any(), any())) thenReturn Future.successful(Some(AffinityGroup("organisation")))
          when(cache.read[SubmitterInfo](EQ(SubmitterInfo.format), any(), any())) thenReturn Future.successful(Some(SubmitterInfo("name", None, "0123123123", EmailAddress("max@max.com"), Some(AffinityGroup("Organisation")))))
          when(cache.read[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn Future.successful(Some(CBCId.create(10).getOrElse(fail("oops"))))
          when(cache.save[SubmitterInfo](any())(any(), any(), any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String, JsValue]))
          when(cache.read[XMLInfo](EQ(XMLInfo.format),any(),any())) thenReturn Future.successful(Some(keyXMLInfo))
          when(cache.save[CBCId](any())(EQ(CBCId.cbcIdFormat),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
          when(cache.read[AgencyBusinessName](EQ(AgencyBusinessName.format),any(),any())) thenReturn Future.successful(Some(AgencyBusinessName("Colm Cavanagh ltd")))
          val result = Await.result(controller.submitSubmitterInfo(fakeRequestSubmit), 2.seconds)

          result.header.headers("Location") should endWith("/submission/summary")
          status(result) shouldBe Status.SEE_OTHER
        }
        "redirect to enter-cbcId if a cbcid does not exist" in {
          val submitterInfo = SubmitterInfo("Billy Bob", None,  "07923456708", EmailAddress("abc@xyz.com"),None)
          val fakeRequestSubmit = addToken(FakeRequest("POST", "/submitSubmitterInfo").withJsonBody(Json.toJson(submitterInfo)))
          when(cache.read[AffinityGroup](EQ(AffinityGroup.format), any(), any())) thenReturn Future.successful(Some(AffinityGroup("organisation")))
          when(cache.read[SubmitterInfo](EQ(SubmitterInfo.format), any(), any())) thenReturn Future.successful(Some(SubmitterInfo("name", None, "0123123123", EmailAddress("max@max.com"), Some(AffinityGroup("Organisation")))))
          when(cache.read[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn Future.successful(None)
          when(cache.save[SubmitterInfo](any())(any(), any(), any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String, JsValue]))
          when(cache.read[XMLInfo](EQ(XMLInfo.format),any(),any())) thenReturn Future.successful(Some(keyXMLInfo))
          when(cache.save[CBCId](any())(EQ(CBCId.cbcIdFormat),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
          when(cache.read[AgencyBusinessName](EQ(AgencyBusinessName.format),any(),any())) thenReturn Future.successful(Some(AgencyBusinessName("Colm Cavanagh ltd")))
          val result = Await.result(controller.submitSubmitterInfo(fakeRequestSubmit), 2.seconds)

          result.header.headers("Location") should endWith("/cbc-id/entry-form")
          status(result) shouldBe Status.SEE_OTHER
        }
      }
      "the AffinityGroup is Agent it" should {
        "redirect to enter-known-facts if a cbcid does not exist" in {
          val submitterInfo = SubmitterInfo("Billy Bob", None,  "07923456708", EmailAddress("abc@xyz.com"),None)
          val fakeRequestSubmit = addToken(FakeRequest("POST", "/submitSubmitterInfo").withJsonBody(Json.toJson(submitterInfo)))
          when(cache.read[AffinityGroup](EQ(AffinityGroup.format), any(), any())) thenReturn Future.successful(Some(AffinityGroup("agent")))
          when(cache.read[SubmitterInfo](EQ(SubmitterInfo.format), any(), any())) thenReturn Future.successful(Some(SubmitterInfo("name", None, "0123123123", EmailAddress("max@max.com"), Some(AffinityGroup("Organisation")))))
          when(cache.read[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn Future.successful(None)
          when(cache.save[SubmitterInfo](any())(any(), any(), any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String, JsValue]))
          when(cache.read[XMLInfo](EQ(XMLInfo.format),any(),any())) thenReturn Future.successful(Some(keyXMLInfo))
          when(cache.save[CBCId](any())(EQ(CBCId.cbcIdFormat),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
          when(cache.read[AgencyBusinessName](EQ(AgencyBusinessName.format),any(),any())) thenReturn Future.successful(Some(AgencyBusinessName("Colm Cavanagh ltd")))
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
        when(cache.read[GGId](EQ(GGId.format),any(),any())) thenReturn Future.successful(Some(GGId("ggid","type")))
        when(cache.read[BusinessPartnerRecord]) thenReturn Future.successful(None)
        when(cache.read[Utr]) thenReturn Future.successful(None)
        when(cache.read[CBCId]) thenReturn Future.successful(None)
        when(cache.read[Hash]) thenReturn Future.successful(None)
        when(cache.read[FileId]) thenReturn Future.successful(None)
        when(cache.read[EnvelopeId]) thenReturn Future.successful(None)
        when(cache.read[SubmitterInfo]) thenReturn Future.successful(None)
        when(cache.read[FilingType]) thenReturn Future.successful(None)
        when(cache.read[UltimateParentEntity]) thenReturn Future.successful(None)
        when(cache.read[FileMetadata]) thenReturn Future.successful(None)

        Await.result(generateMetadataFile(cache,authContext)(hc,ec,auth),10.second).fold(
          errors => errors.toList.size shouldBe 10,
          _ => fail("this should have failed")
        )

        when(cache.read[FileId]) thenReturn Future.successful(Some(FileId("fileId")))
        Await.result(generateMetadataFile(cache,authContext)(hc,ec,auth),10.second).fold(
          errors => errors.toList.size shouldBe 9,
          _ => fail("this should have failed")
        )

        when(cache.read[EnvelopeId]) thenReturn Future.successful(Some(EnvelopeId("yeah")))
        Await.result(generateMetadataFile(cache,authContext)(hc,ec,auth),10.second).fold(
          errors => errors.toList.size shouldBe 8,
          _ => fail("this should have failed")
        )

      }
      "return a failed future if any of the cache calls fails" in {
        when(cache.read[EnvelopeId]) thenReturn Future.failed(new Exception("Cache gone bad"))
        intercept[Exception] {
          Await.result(generateMetadataFile(cache,authContext)(hc,ec,auth),10.second)
        }
      }
      "return a Metadata object if all succeeds" in {
        when(cache.read[GGId](EQ(GGId.format),any(),any())) thenReturn Future.successful(Some(GGId("ggid","type")))
        when(cache.read[BusinessPartnerRecord]) thenReturn Future.successful(Some(bpr))
        when(cache.read[Utr]) thenReturn Future.successful(Some(Utr("utr")))
        when(cache.read[CBCId]) thenReturn Future.successful(CBCId.create(1).toOption)
        when(cache.read[Hash]) thenReturn Future.successful(Some(Hash("hash")))
        when(cache.read[FileId]) thenReturn Future.successful(Some(FileId("yeah")))
        when(cache.read[EnvelopeId]) thenReturn Future.successful(Some(EnvelopeId("id")))
        when(cache.read[SubmitterInfo]) thenReturn Future.successful(Some(SubmitterInfo("name",None,"0123123123",EmailAddress("max@max.com"), Some(AffinityGroup("Organisation")))))
        when(cache.read[FilingType]) thenReturn Future.successful(Some(FilingType(CBC701)))
        when(cache.read[UltimateParentEntity]) thenReturn Future.successful(Some(UltimateParentEntity("yeah")))
        when(cache.read[FileMetadata]) thenReturn Future.successful(Some(FileMetadata("asdf","lkjasdf","lkj","lkj",10,"lkjasdf",JsNull,"")))

        Await.result(generateMetadataFile(cache,authContext)(hc,ec,auth),10.second).leftMap(
          errors => fail(s"There should be no errors: $errors")
        )
      }
    }
    "provide a 'submitSummary' Action that" should {
      val fakeRequestSubmitSummary = addToken(FakeRequest("GET", "/submitSummary"))
      "return 500 if generating the metadata fails" in {
        when(cache.read[GGId](EQ(GGId.format),any(),any())) thenReturn Future.successful(Some(GGId("ggid","type")))
        when(cache.read[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format),any(),any())) thenReturn Future.failed(new Exception("argh"))
        when(auth.getIds[UserIds](any())(any(),any())) thenReturn Future.successful(UserIds("a","b"))
        status(Await.result(controller.submitSummary(fakeRequestSubmitSummary), 10.second)) shouldBe Status.INTERNAL_SERVER_ERROR
        when(cache.read[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format),any(),any())) thenReturn Future.successful(Some(bpr))

        when(cache.read[UltimateParentEntity]) thenReturn Future.successful(None)
        status(controller.submitSummary(fakeRequestSubmitSummary)) shouldBe Status.INTERNAL_SERVER_ERROR
        when(cache.read[UltimateParentEntity]) thenReturn Future.successful(Some(UltimateParentEntity("yeah")))

      }

    "return 500 if the fileUploadService fails" in {
      when(cache.read[GGId](EQ(GGId.format),any(),any())) thenReturn Future.successful(Some(GGId("ggid","type")))
      when(cache.read[XMLInfo](EQ(XMLInfo.format),any(),any())) thenReturn Future.successful(Some(keyXMLInfo))
      when(cache.read[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format),any(),any())) thenReturn Future.successful(Some(bpr))
      when(cache.read[Utr](EQ(Utr.utrRead),any(),any())) thenReturn Future.successful(Some(Utr("utr")))
      when(cache.read[CBCId](EQ(CBCId.cbcIdFormat),any(),any())) thenReturn Future.successful(CBCId.create(1).toOption)
      when(cache.read[Hash](EQ(Hash.format),any(),any())) thenReturn Future.successful(Some(Hash("hash")))
      when(cache.read[FileId](EQ(FileId.fileIdFormat),any(),any())) thenReturn Future.successful(Some(FileId("yeah")))
      when(cache.read[EnvelopeId](EQ(EnvelopeId.format),any(),any())) thenReturn Future.successful(Some(EnvelopeId("id")))
      when(cache.read[SubmitterInfo](EQ(SubmitterInfo.format),any(),any())) thenReturn Future.successful(Some(SubmitterInfo("name",None,"0123123123",EmailAddress("max@max.com"),None)))
      when(cache.read[FilingType](EQ(FilingType.format),any(),any())) thenReturn Future.successful(Some(FilingType(CBC701)))
      when(cache.read[UltimateParentEntity](EQ(UltimateParentEntity.format),any(),any())) thenReturn Future.successful(Some(UltimateParentEntity("yeah")))
      when(cache.read[FileMetadata](EQ(FileMetadata.fileMetadataFormat),any(),any())) thenReturn Future.successful(Some(FileMetadata("asdf","lkjasdf","lkj","lkj",10,"lkjasdf",JsNull,"")))

      when(fus.getFile(anyString, anyString)(any(),any(),any())) thenReturn EitherT[Future, CBCErrors,File](Future.successful(Left(UnexpectedState("Some error"))))
      status(controller.submitSummary(fakeRequestSubmitSummary)) shouldBe Status.INTERNAL_SERVER_ERROR

    }

      "return 200 if everything succeeds" in {

        val file = File.createTempFile("test","test")

        when(cache.read[GGId](EQ(GGId.format),any(),any())) thenReturn Future.successful(Some(GGId("ggid","type")))
        when(cache.read[XMLInfo](EQ(XMLInfo.format),any(),any())) thenReturn Future.successful(Some(keyXMLInfo))
        when(cache.read[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format),any(),any())) thenReturn Future.successful(Some(bpr))
        when(cache.read[Utr](EQ(Utr.utrRead),any(),any())) thenReturn Future.successful(Some(Utr("utr")))
        when(cache.read[CBCId](EQ(CBCId.cbcIdFormat),any(),any())) thenReturn Future.successful(CBCId.create(1).toOption)
        when(cache.read[Hash](EQ(Hash.format),any(),any())) thenReturn Future.successful(Some(Hash("hash")))
        when(cache.read[FileId](EQ(FileId.fileIdFormat),any(),any())) thenReturn Future.successful(Some(FileId("yeah")))
        when(cache.read[EnvelopeId](EQ(EnvelopeId.format),any(),any())) thenReturn Future.successful(Some(EnvelopeId("id")))
        when(cache.read[SubmitterInfo](EQ(SubmitterInfo.format),any(),any())) thenReturn Future.successful(Some(SubmitterInfo("name",None,"0123123123",EmailAddress("max@max.com"),None)))
        when(cache.read[FilingType](EQ(FilingType.format),any(),any())) thenReturn Future.successful(Some(FilingType(CBC701)))
        when(cache.read[UltimateParentEntity](EQ(UltimateParentEntity.format),any(),any())) thenReturn Future.successful(Some(UltimateParentEntity("yeah")))
        when(cache.read[FileMetadata](EQ(FileMetadata.fileMetadataFormat),any(),any())) thenReturn Future.successful(Some(FileMetadata("asdf","lkjasdf","lkj","lkj",10,"lkjasdf",JsNull,"")))
        when(fus.getFile(anyString, anyString)(any(),any(),any())) thenReturn EitherT[Future, CBCErrors,File](Future.successful(Right(file)))
        when(cache.save[SummaryData](any())(any(),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))

        status(controller.submitSummary(fakeRequestSubmitSummary)) shouldBe Status.OK

        file.deleteOnExit()

      }
    }

    "provide an action '/confirm'" which {
      "returns a 500 when the call to the cache fails" in {
        val summaryData = SummaryData(bpr, submissionData, keyXMLInfo)
        val fakeRequestSubmitSummary = addToken(FakeRequest("POST", "/confirm ").withJsonBody(Json.toJson("{}")))
        when(cache.read[SummaryData](EQ(SummaryData.format),any(),any())) thenReturn Future.successful(Some(summaryData))
        when(cache.read[XMLInfo](EQ(XMLInfo.format),any(),any())) thenReturn Future.successful(None)
        status(controller.confirm(fakeRequestSubmitSummary)) shouldBe Status.INTERNAL_SERVER_ERROR
      }
      "returns a 500 when the call to file-upload fails" in {
        val summaryData = SummaryData(bpr, submissionData, keyXMLInfo)
        val fakeRequestSubmitSummary = addToken(FakeRequest("POST", "/confirm ").withJsonBody(Json.toJson("{}")))
        when(cache.read[SummaryData](EQ(SummaryData.format),any(),any())) thenReturn Future.successful(Some(summaryData))
        when(cache.read[XMLInfo](EQ(XMLInfo.format),any(),any())) thenReturn Future.successful(Some(keyXMLInfo))
        when(fus.uploadMetadataAndRoute(any())(any(),any(),any(),any())) thenReturn EitherT.left[Future,CBCErrors,String](UnexpectedState("fail"))
        status(controller.confirm(fakeRequestSubmitSummary)) shouldBe Status.INTERNAL_SERVER_ERROR

      }
      "returns a 500 when the call to save the docRefIds fail" in {
        val summaryData = SummaryData(bpr, submissionData, keyXMLInfo)
        val fakeRequestSubmitSummary = addToken(FakeRequest("POST", "/confirm ").withJsonBody(Json.toJson("{}")))
        when(cache.read[SummaryData](EQ(SummaryData.format),any(),any())) thenReturn Future.successful(Some(summaryData))
        when(cache.read[XMLInfo](EQ(XMLInfo.format),any(),any())) thenReturn Future.successful(Some(keyXMLInfo))
        when(fus.uploadMetadataAndRoute(any())(any(),any(),any(),any())) thenReturn EitherT.pure[Future,CBCErrors,String]("ok")
        when(docRefService.saveDocRefId(any())(any())) thenReturn OptionT.some[Future,UnexpectedState](UnexpectedState("fails!"))
        when(docRefService.saveCorrDocRefID(any(),any())(any())) thenReturn OptionT.some[Future,UnexpectedState](UnexpectedState("fails!"))
        status(controller.confirm(fakeRequestSubmitSummary)) shouldBe Status.INTERNAL_SERVER_ERROR
      }

      "returns 303 when the there is data and " should {
        "call saveReportingEntityData when the submissionType is OECD1" in {
          val summaryData = SummaryData(bpr, submissionData, keyXMLInfo)
          val fakeRequestSubmitSummary = addToken(FakeRequest("POST", "/confirm ").withJsonBody(Json.toJson("{}")))
          when(cache.read[SummaryData](EQ(SummaryData.format),any(),any())) thenReturn Future.successful(Some(summaryData))
          when(fus.uploadMetadataAndRoute(any())(any(),any(),any(),any())) thenReturn EitherT[Future,CBCErrors,String](Future.successful(Right("routed")))
          when(cache.read[XMLInfo](EQ(XMLInfo.format),any(),any())) thenReturn Future.successful(Some(keyXMLInfo))
          when(cache.save[SubmissionDate](any())(EQ(SubmissionDate.format),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
          when(fus.uploadMetadataAndRoute(any())(any(),any(),any(),any())) thenReturn EitherT.pure[Future,CBCErrors,String]("ok")
          when(reportingEntity.saveReportingEntityData(any())(any())) thenReturn EitherT.pure[Future,CBCErrors,Unit](())
          when(docRefService.saveCorrDocRefID(any(),any())(any())) thenReturn OptionT.none[Future,UnexpectedState]
          when(docRefService.saveDocRefId(any())(any())) thenReturn OptionT.none[Future,UnexpectedState]
          status(controller.confirm(fakeRequestSubmitSummary)) shouldBe Status.SEE_OTHER
          verify(reportingEntity).saveReportingEntityData(any())(any())
        }
        "call updateReportingEntityData when the submissionType is OECD[023]" in {
          val summaryData = SummaryData(bpr, submissionData, keyXMLInfo)
          val fakeRequestSubmitSummary = addToken(FakeRequest("POST", "/confirm ").withJsonBody(Json.toJson("{}")))
          lazy val updateXml = keyXMLInfo.copy(reportingEntity = keyXMLInfo.reportingEntity.copy(docSpec = keyXMLInfo.reportingEntity.docSpec.copy(docType = OECD2)))
          when(cache.read[SummaryData](EQ(SummaryData.format),any(),any())) thenReturn Future.successful(Some(summaryData))
          when(fus.uploadMetadataAndRoute(any())(any(),any(),any(),any())) thenReturn EitherT[Future,CBCErrors,String](Future.successful(Right("routed")))
          when(cache.read[XMLInfo](EQ(XMLInfo.format),any(),any())) thenReturn Future.successful(Some(updateXml))
          when(cache.save[SubmissionDate](any())(EQ(SubmissionDate.format),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
          when(fus.uploadMetadataAndRoute(any())(any(),any(),any(),any())) thenReturn EitherT.pure[Future,CBCErrors,String]("ok")
          when(reportingEntity.updateReportingEntityData(any())(any())) thenReturn EitherT.pure[Future,CBCErrors,Unit](())
          when(docRefService.saveCorrDocRefID(any(),any())(any())) thenReturn OptionT.none[Future,UnexpectedState]
          when(docRefService.saveDocRefId(any())(any())) thenReturn OptionT.none[Future,UnexpectedState]
          status(controller.confirm(fakeRequestSubmitSummary)) shouldBe Status.SEE_OTHER
          verify(reportingEntity).updateReportingEntityData(any())(any())

        }
      }

    }

    "provide an action 'submitSuccessReceipt'" which {
      "returns a 500 InternalServerError if it fails to read from the cache" when {
        "looking for the SummaryData" in {

          val fakeRequestSubmitSummary = addToken(FakeRequest("GET", "/submitSuccessReceipt"))

          when(cache.read[SummaryData](EQ(SummaryData.format), any(), any())) thenReturn Future.successful(None)
          when(cache.read[SubmissionDate](EQ(SubmissionDate.format), any(), any())) thenReturn Future.successful(Some(SubmissionDate(LocalDateTime.now())))
          status(controller.submitSuccessReceipt(fakeRequestSubmitSummary)) shouldBe Status.INTERNAL_SERVER_ERROR

        }
        "looking for the SubmissionDate" in {
          val summaryData = SummaryData(bpr, submissionData, keyXMLInfo)
          val fakeRequestSubmitSummary = addToken(FakeRequest("GET", "/submitSuccessReceipt"))

          when(cache.read[SummaryData](EQ(SummaryData.format), any(), any())) thenReturn Future.successful(Some(summaryData))
          when(cache.read[SubmissionDate](EQ(SubmissionDate.format), any(), any())) thenReturn Future.successful(None)
          status(controller.submitSuccessReceipt(fakeRequestSubmitSummary)) shouldBe Status.INTERNAL_SERVER_ERROR

        }
      }
      "sends an email" in {
        val summaryData = SummaryData(bpr, submissionData, keyXMLInfo)
        val fakeRequestSubmitSummary = addToken(FakeRequest("GET", "/submitSuccessReceipt"))

        when(cache.read[GGId](EQ(GGId.format),any(),any())) thenReturn Future.successful(Some(GGId("ggid","type")))
        when(auditMock.sendEvent(any())(any(),any())) thenReturn Future.successful(AuditResult.Success)
        when(mockEmailService.sendEmail(any())(any())) thenReturn  OptionT.pure[Future,Boolean](true)
        when(cache.save[ConfirmationEmailSent](any())(EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
        when(cache.read[SummaryData](EQ(SummaryData.format), any(), any())) thenReturn Future.successful(Some(summaryData))
        when(cache.read[ConfirmationEmailSent](EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat), any(), any())) thenReturn Future.successful(None)
        when(cache.read[SubmissionDate](EQ(SubmissionDate.format), any(), any())) thenReturn Future.successful(Some(SubmissionDate(LocalDateTime.now())))
        status(controller.submitSuccessReceipt(fakeRequestSubmitSummary)) shouldBe Status.OK
        verify(mockEmailService,times(1)).sendEmail(any())(any())
        verify(cache,times(1)).save(any())(EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat),any(),any())
      }
      "will still return a 200 if the email fails" in {
        val summaryData = SummaryData(bpr, submissionData, keyXMLInfo)
        val fakeRequestSubmitSummary = addToken(FakeRequest("GET", "/submitSuccessReceipt"))
        when(cache.read[GGId](EQ(GGId.format),any(),any())) thenReturn Future.successful(Some(GGId("ggid","type")))
        when(cache.save[ConfirmationEmailSent](any())(EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
        when(auditMock.sendEvent(any())(any(),any())) thenReturn Future.successful(AuditResult.Success)
        when(mockEmailService.sendEmail(any())(any())) thenReturn  OptionT.pure[Future,Boolean](false)
        when(cache.read[ConfirmationEmailSent](EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat), any(), any())) thenReturn Future.successful(None)
        when(cache.read[SummaryData](EQ(SummaryData.format), any(), any())) thenReturn Future.successful(Some(summaryData))
        when(cache.read[SubmissionDate](EQ(SubmissionDate.format), any(), any())) thenReturn Future.successful(Some(SubmissionDate(LocalDateTime.now())))
        status(controller.submitSuccessReceipt(fakeRequestSubmitSummary)) shouldBe Status.OK
        verify(mockEmailService,times(1)).sendEmail(any())(any())
        verify(cache,times(0)).save(any())(EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat),any(),any())
      }
      "will write  a ConfirmationEmailSent to the cache if an email is sent" in {

        val summaryData = SummaryData(bpr, submissionData, keyXMLInfo)
        val fakeRequestSubmitSummary = addToken(FakeRequest("GET", "/submitSuccessReceipt"))

        when(cache.read[GGId](EQ(GGId.format),any(),any())) thenReturn Future.successful(Some(GGId("ggid","type")))
        when(auditMock.sendEvent(any())(any(),any())) thenReturn Future.successful(AuditResult.Success)
        when(mockEmailService.sendEmail(any())(any())) thenReturn  OptionT.pure[Future,Boolean](true)
        when(cache.read[SummaryData](EQ(SummaryData.format), any(), any())) thenReturn Future.successful(Some(summaryData))
        when(cache.save[ConfirmationEmailSent](any())(EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
        when(cache.read[ConfirmationEmailSent](EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat), any(), any())) thenReturn Future.successful(None)
        when(cache.read[SubmissionDate](EQ(SubmissionDate.format), any(), any())) thenReturn Future.successful(Some(SubmissionDate(LocalDateTime.now())))
        status(controller.submitSuccessReceipt(fakeRequestSubmitSummary)) shouldBe Status.OK
        verify(mockEmailService,times(1)).sendEmail(any())(any())
        verify(cache,times(1)).save(any())(EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat),any(),any())
      }
      "not send the email if it has already been sent and not save to the cache" in {
        val summaryData = SummaryData(bpr, submissionData, keyXMLInfo)
        val fakeRequestSubmitSummary = addToken(FakeRequest("GET", "/submitSuccessReceipt"))
        when(cache.read[GGId](EQ(GGId.format),any(),any())) thenReturn Future.successful(Some(GGId("ggid","type")))
        when(mockEmailService.sendEmail(any())(any())) thenReturn  OptionT.pure[Future,Boolean](true)
        when(auditMock.sendEvent(any())(any(),any())) thenReturn Future.successful(AuditResult.Success)
        when(cache.save[ConfirmationEmailSent](any())(EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
        when(cache.read[SummaryData](EQ(SummaryData.format), any(), any())) thenReturn Future.successful(Some(summaryData))
        when(cache.read[ConfirmationEmailSent](EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat), any(), any())) thenReturn Future.successful(Some(ConfirmationEmailSent()))
        when(cache.read[SubmissionDate](EQ(SubmissionDate.format), any(), any())) thenReturn Future.successful(Some(SubmissionDate(LocalDateTime.now())))
        status(controller.submitSuccessReceipt(fakeRequestSubmitSummary)) shouldBe Status.OK
        verify(mockEmailService,times(0)).sendEmail(any())(any())
        verify(cache,times(0)).save(any())(EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat),any(),any())
      }
      "returns a 200 otherwise" in {
        val summaryData = SummaryData(bpr, submissionData, keyXMLInfo)
        val fakeRequestSubmitSummary = addToken(FakeRequest("GET", "/submitSuccessReceipt"))
        when(cache.read[GGId](EQ(GGId.format),any(),any())) thenReturn Future.successful(Some(GGId("ggid","type")))
        when(mockEmailService.sendEmail(any())(any())) thenReturn  OptionT.pure[Future,Boolean](true)
        when(auditMock.sendEvent(any())(any(),any())) thenReturn Future.successful(AuditResult.Success)
        when(cache.save[ConfirmationEmailSent](any())(EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat),any(),any())) thenReturn Future.successful(CacheMap("cache", Map.empty[String,JsValue]))
        when(cache.read[SummaryData](EQ(SummaryData.format), any(), any())) thenReturn Future.successful(Some(summaryData))
        when(cache.read[ConfirmationEmailSent](EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat), any(), any())) thenReturn Future.successful(None)
        when(cache.read[SubmissionDate](EQ(SubmissionDate.format), any(), any())) thenReturn Future.successful(Some(SubmissionDate(LocalDateTime.now())))
        status(controller.submitSuccessReceipt(fakeRequestSubmitSummary)) shouldBe Status.OK
        verify(cache,times(1)).save(any())(EQ(ConfirmationEmailSent.ConfirmationEmailSentFormat),any(),any())
      }
    }
    "contain a valid dateformat" in {

      LocalDateTime.of(2017,12,1,23,59,59).format(controller.dateFormat) shouldEqual "01 December 2017 at 23:59"
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
     Utr("utr"),
     FilingType(CBC701),
     UltimateParentEntity("ultimateParentEntity")
   )
    val submitterInfo = SubmitterInfo("fullName", None, "contactPhone", EmailAddress("abc@abc.com"), None)
    SubmissionMetaData(submissionInfo, submitterInfo, fileInfo)

  }

  val docRefId="GB2016RGXVCBC0000000056CBC40120170311T090000X_7000000002OECD1ENTZ"

  private lazy val keyXMLInfo = {
    XMLInfo(
      MessageSpec(
        MessageRefID("GB2016RGXVCBC0000000056CBC40120170311T090000X").getOrElse(fail("waaaaa")),
        "GB",
        CBCId.create(99).getOrElse(fail("booo")),
        LocalDateTime.now(),
        LocalDate.parse("2017-01-30"),
        None
      ),
      ReportingEntity(CBC701,DocSpec(OECD1,DocRefId(docRefId).get,None),Utr("7000000002"),"name"),
      Some(CbcReports(DocSpec(OECD1,DocRefId(docRefId).get,None))),
      Some(AdditionalInfo(DocSpec(OECD1,DocRefId(docRefId).get,None)))
    )
  }
}
