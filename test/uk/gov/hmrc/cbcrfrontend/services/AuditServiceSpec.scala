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

import base.SpecBase
import org.mockito.ArgumentMatchers.{any, eq => EQ}
import org.mockito.Mockito._
import org.scalatest.EitherValues
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.JsNull
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AffinityGroup
import uk.gov.hmrc.cbcrfrontend.controllers.CSRFTest
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}

import scala.concurrent.Future

class AuditServiceSpec extends SpecBase with CSRFTest with EitherValues {

  val auditC: AuditConnector = mock[AuditConnector]
  val md = FileMetadata("", "", "something.xml", "", 1.0, "", JsNull, "")

  override protected def afterEach(): Unit = {
    reset(auditC)
    super.afterEach()
  }
  override def guiceApplicationBuilder(): GuiceApplicationBuilder =
    super
      .guiceApplicationBuilder()
      .overrides(
        bind[AuditConnector].toInstance(auditC)
      )

  val auditService: AuditService = app.injector.instanceOf[AuditService]

  "a call to auditFailedSubmission" - {
    "return success if audit enabled and sendExtendedEvent succeeds" in {
      val cbcId = CBCId("XLCBC0100000056").getOrElse(fail("booo"))
      val enrole: CBCEnrolment = CBCEnrolment(cbcId, Utr("7000000002"))
      val request = addToken(FakeRequest())

      when(auditC.sendExtendedEvent(any())(any(), any())) thenReturn Future.successful(AuditResult.Success)
      when(mockCache.readOption[AllBusinessRuleErrors](EQ(AllBusinessRuleErrors.format), any(), any())) thenReturn Future
        .successful(Some(AllBusinessRuleErrors(List(TestDataError))))
      when(mockCache.readOption[XMLErrors](EQ(XMLErrors.format), any(), any())) thenReturn Future.successful(
        Some(XMLErrors(List("Big xml error"))))
      when(mockCache.readOption[XMLErrors](EQ(XMLErrors.format), any(), any())) thenReturn Future.successful(
        Some(XMLErrors(List("Big xml error"))))
      when(mockCache.readOption[FileMetadata](EQ(FileMetadata.fileMetadataFormat), any(), any())) thenReturn Future
        .successful(Some(md))
      when(mockCache.readOption[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn Future.successful(Some(cbcId))
      when(mockCache.readOption[Utr](EQ(Utr.utrRead), any(), any())) thenReturn Future.successful(
        Some(Utr("1234567890")))

      val result: ServiceResponse[AuditResult.Success.type] = auditService
        .auditFailedSubmission(creds, Some(AffinityGroup.Organisation), Some(enrole), "just because")(
          hc,
          request,
          messages)

      result.value.futureValue.right.value shouldBe AuditResult.Success
    }

    "return success if audit disabled and sendExtendedEvent succeeds" in {
      val cbcId = CBCId("XLCBC0100000056").getOrElse(fail("booo"))
      val request = addToken(FakeRequest())
      when(auditC.sendExtendedEvent(any())(any(), any())) thenReturn Future.successful(AuditResult.Disabled)
      when(mockCache.readOption[AllBusinessRuleErrors](EQ(AllBusinessRuleErrors.format), any(), any())) thenReturn Future
        .successful(Some(AllBusinessRuleErrors(List(TestDataError))))
      when(mockCache.readOption[XMLErrors](EQ(XMLErrors.format), any(), any())) thenReturn Future.successful(
        Some(XMLErrors(List("Big xml error"))))
      when(mockCache.readOption[FileMetadata](EQ(FileMetadata.fileMetadataFormat), any(), any())) thenReturn Future
        .successful(Some(md))
      when(mockCache.readOption[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn Future.successful(Some(cbcId))
      when(mockCache.readOption[Utr](EQ(Utr.utrRead), any(), any())) thenReturn Future.successful(
        Some(Utr("1234567890")))
      val result = auditService
        .auditFailedSubmission(creds, Some(AffinityGroup.Organisation), None, "just because")(hc, request, messages)

      result.value.futureValue.right.value shouldBe AuditResult.Success
    }

    "return error if sendExtendedEvent fails" in {
      val cbcId = CBCId("XLCBC0100000056").getOrElse(fail("booo"))
      val enrole: CBCEnrolment = CBCEnrolment(cbcId, Utr("7000000002"))
      val request = addToken(FakeRequest())
      val failure = AuditResult.Failure("boo hoo")
      when(auditC.sendExtendedEvent(any())(any(), any())) thenReturn Future.successful(failure)
      when(mockCache.readOption[AllBusinessRuleErrors](EQ(AllBusinessRuleErrors.format), any(), any())) thenReturn Future
        .successful(Some(AllBusinessRuleErrors(List(TestDataError))))
      when(mockCache.readOption[XMLErrors](EQ(XMLErrors.format), any(), any())) thenReturn Future.successful(
        Some(XMLErrors(List("Big xml error"))))
      when(mockCache.readOption[FileMetadata](EQ(FileMetadata.fileMetadataFormat), any(), any())) thenReturn Future
        .successful(Some(md))
      when(mockCache.readOption[CBCId](EQ(CBCId.cbcIdFormat), any(), any())) thenReturn Future.successful(Some(cbcId))
      when(mockCache.readOption[Utr](EQ(Utr.utrRead), any(), any())) thenReturn Future.successful(
        Some(Utr("1234567890")))
      val result = auditService
        .auditFailedSubmission(creds, Some(AffinityGroup.Organisation), Some(enrole), "just because")(
          hc,
          request,
          messages)

      result.value.futureValue.left.value shouldBe UnexpectedState("Unable to audit a failed submission: boo hoo", None)
    }
  }
}
