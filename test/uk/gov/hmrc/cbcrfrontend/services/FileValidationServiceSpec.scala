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

package uk.gov.hmrc.cbcrfrontend.services

import akka.actor.ActorSystem
import base.SpecBase
import cats.data.EitherT
import cats.data.Validated.Valid
import cats.instances.future._
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{reset, when}
import org.scalatest.EitherValues
import play.api.Environment
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import uk.gov.hmrc.auth.core.AffinityGroup
import uk.gov.hmrc.auth.core.AffinityGroup.Organisation
import uk.gov.hmrc.cbcrfrontend.connectors.UpscanConnector
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.model.upscan._
import uk.gov.hmrc.cbcrfrontend.util.FakeUpscanConnector

import java.io.File
import java.time.{LocalDate, LocalDateTime}
import scala.concurrent.{ExecutionContext, Future}

class FileValidationServiceSpec extends SpecBase with EitherValues {

  implicit val ec: ExecutionContext = app.injector.instanceOf[ExecutionContext]
  implicit val env = app.injector.instanceOf[Environment]
  implicit val as = app.injector.instanceOf[ActorSystem]

  val mockCBCRXMLValidator: CBCRXMLValidator = mock[CBCRXMLValidator]
  val mockCBCBusinessRuleValidator: CBCBusinessRuleValidator = mock[CBCBusinessRuleValidator]
  val mockXmlInfoExtract: XmlInfoExtract = mock[XmlInfoExtract]
  val mockUpscanConnector = app.injector.instanceOf[FakeUpscanConnector]
  val mockAuditService: AuditService = mock[AuditService]
  val mockFile: File = mock[File]
  val mockFileService = mock[FileService]
  val mockRawXMLInfo = mock[RawXMLInfo]

  override protected def afterEach(): Unit = {
    reset(mockCBCRXMLValidator, mockCBCBusinessRuleValidator, mockXmlInfoExtract, mockAuditService)
    super.afterEach()
  }
  //implicit def liftFuture[A](v: A): Future[A] = Future.successful(v)

  override def guiceApplicationBuilder(): GuiceApplicationBuilder =
    super
      .guiceApplicationBuilder()
      .overrides(
        bind[CBCBusinessRuleValidator].to(mockCBCBusinessRuleValidator),
        bind[CBCRXMLValidator].to(mockCBCRXMLValidator),
        bind[XmlInfoExtract].to(mockXmlInfoExtract),
        bind[UpscanConnector].to(mockUpscanConnector),
        bind[AuditService].to(mockAuditService),
        bind[FileService].to(mockFileService)
      )

  val fileValidationService: FileValidationService = app.injector.instanceOf[FileValidationService]

  def right[A](a: Future[A]): ServiceResponse[A] = EitherT.right[Future, CBCErrors, A](a)
  def left[A](s: String): ServiceResponse[A] = EitherT.left[Future, CBCErrors, A](Future.successful(UnexpectedState(s)))
  def pure[A](a: A): ServiceResponse[A] = EitherT.pure[Future, CBCErrors, A](a)

  val docRefId = "GB2016RGXVCBC0000000056CBC40120170311T090000X_7000000002OECD1"
  val xmlinfo: XMLInfo = XMLInfo(
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
  val completeXmlInfo: CompleteXMLInfo = CompleteXMLInfo(
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

  "FileValidationService" - {
    "validate xml schema and business rules and return FileValidationSuccess" in {
      val expectedResult = FileValidationSuccess(Some(Organisation), Some("afile"), Some(0), None, None, Some(CBC701))
      val uploadDetails = UploadSessionDetails(
        uploadId,
        Reference("123"),
        UploadedSuccessfully("afile", "downloadURL")
      )
      mockUpscanConnector.setDetails(uploadDetails)

      when(mockFileService.getFile(uploadId, "downloadURL")(hc, ec))
        .thenReturn(EitherT.right[Future, CBCErrors, File](Future.successful(mockFile)))
      when(mockFileService.deleteFile(any())).thenReturn(true)
      when(mockCache.readOption[UploadId])
        .thenReturn(Future.successful(Some(uploadId)))
      when(mockCache.save(any())(any(), any(), any()))
        .thenReturn(Future.successful(cacheMap))
      when(mockXmlInfoExtract.extract(any())).thenReturn(mockRawXMLInfo)
      when(mockFileService.sha256Hash(any()))
        .thenReturn("63fe78532fed67be93556c62ec9242fdeb3635dd86b03aa81f631779fe378e18")
      when(mockCBCRXMLValidator.validateSchema(any[File]())) thenReturn new XmlErrorHandler()
      when(mockCBCBusinessRuleValidator.validateBusinessRules(any(), any(), any(), any())(any())) thenReturn Future
        .successful(Valid(xmlinfo))
      when(mockCBCBusinessRuleValidator.recoverReportingEntity(any())(any())) thenReturn Future.successful(
        Valid(completeXmlInfo))

      val result: EitherT[Future, CBCErrors, FileValidationSuccess] =
        fileValidationService.fileValidate(creds, Some(AffinityGroup.Organisation), Some(enrolment))

      val eitherResult: Either[CBCErrors, FileValidationSuccess] =  result.value.futureValue
      eitherResult.right.value mustBe expectedResult

    }
  }
}
