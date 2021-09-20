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
import cats.data.EitherT
import cats.instances.future._
import org.mockito.ArgumentCaptor
import org.mockito.ArgumentMatchers.{any, eq => EQ}
import org.mockito.Mockito.{reset, times, verify, when}
import org.scalatest.{BeforeAndAfterEach, EitherValues}
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import uk.gov.hmrc.cbcrfrontend.connectors.CBCRBackendConnector
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.model.upscan.UploadedSuccessfully
import uk.gov.hmrc.cbcrfrontend.model.{ExpiredSession, UnexpectedState}
import uk.gov.hmrc.cbcrfrontend.util.XmlLoadHelper
import uk.gov.hmrc.http.HttpResponse

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.xml.NodeSeq
import scala.xml.Utility.trim

class SubmissionServiceSpec extends SpecBase with EitherValues with BeforeAndAfterEach {

  val mockConnector: CBCRBackendConnector = mock[CBCRBackendConnector]
  val mockXmlLoadHelper: XmlLoadHelper = mock[XmlLoadHelper]

  val application: GuiceApplicationBuilder = new GuiceApplicationBuilder()
    .overrides(
      bind[CBCRBackendConnector].toInstance(mockConnector),
      bind[CBCSessionCache].toInstance(mockCache),
      bind[XmlLoadHelper].toInstance(mockXmlLoadHelper)
    )

  override def beforeEach: Unit = {
    reset(mockConnector, mockCache, mockXmlLoadHelper)
    super.beforeEach
  }

  val submissionService: SubmissionService = application.injector.instanceOf[SubmissionService]

  "SubmissionService" - {
    "must return OK and submit xml to the backed" in {

      val uploadedData = UploadedSuccessfully("test.xml", "application/xml", "url", None)
      when(mockConnector.submitDocument(any())(any())).thenReturn(Future.successful(HttpResponse(200, "")))
      when(mockCache.read[UploadedSuccessfully](any(), any(), any()))
        .thenReturn(EitherT.right[Future, ExpiredSession, UploadedSuccessfully](Future.successful(uploadedData)))

      when(mockXmlLoadHelper.loadXML(any[String]())).thenReturn(<test>Success</test>)

      val nodeSeqCaptor = ArgumentCaptor.forClass(classOf[NodeSeq])
      val submissionXml: NodeSeq = <submission>
        <fileName>test.xml</fileName>
        <cbcId>XLCBC0100000056</cbcId>
        <file><test>Success</test></file>
      </submission>

      val result: ServiceResponse[Unit] = submissionService.submit(cbcId)
      result.value.futureValue.right.value shouldBe ((): Unit)

      verify(mockConnector, times(1)).submitDocument(nodeSeqCaptor.capture())(EQ(hc))
      val actualXml: NodeSeq = nodeSeqCaptor.getValue
      actualXml.map(trim) shouldBe submissionXml.map(trim)
    }

    "must return BAD_REQUEST and submit xml to the backed" in {

      val uploadedData = UploadedSuccessfully("test.xml", "application/xml", "url", None)
      when(mockConnector.submitDocument(any())(any())).thenReturn(Future.successful(HttpResponse(400, "")))
      when(mockCache.read[UploadedSuccessfully](any(), any(), any()))
        .thenReturn(EitherT.right[Future, ExpiredSession, UploadedSuccessfully](Future.successful(uploadedData)))

      val result: ServiceResponse[Unit] = submissionService.submit(cbcId)
      result.value.futureValue.left.value shouldBe UnexpectedState("Failed to submit the xml document", None)

    }

    "must return CBCError UnexpectedState when failed to submit xml to the backed" in {

      val uploadedData = UploadedSuccessfully("test.xml", "application/xml", "url", None)
      when(mockConnector.submitDocument(any())(any())).thenReturn(Future.failed(new Exception("error")))
      when(mockCache.read[UploadedSuccessfully](any(), any(), any()))
        .thenReturn(EitherT.right[Future, ExpiredSession, UploadedSuccessfully](Future.successful(uploadedData)))

      val result: ServiceResponse[Unit] = submissionService.submit(cbcId)
      result.value.futureValue.left.value shouldBe UnexpectedState("Failed to submit the document: error", None)
    }

    "must return CBCError and when uploadedData not persisted in keystore" in {
      val expectedError = ExpiredSession("error")
      when(mockCache.read[UploadedSuccessfully](any(), any(), any()))
        .thenReturn(EitherT.left[Future, ExpiredSession, UploadedSuccessfully](Future.successful(expectedError)))

      val result: ServiceResponse[Unit] = submissionService.submit(cbcId)
      result.value.futureValue.left.value shouldBe expectedError
    }

    "must construct a submission document from a fileName and document" in {
      val xml =
        <test>
          <value>This should be preserved</value>
        </test>
      when(mockXmlLoadHelper.loadXML(any[String]())).thenReturn(xml)

      val expectedReturn =
        <submission>
          <fileName>test-file.xml</fileName>
          <cbcId>XLCBC0100000056</cbcId>
          <file>
            <test><value>This should be preserved</value></test>
          </file>
        </submission>

      submissionService.constructSubmission("test-file.xml", "url", cbcId).map(trim) shouldBe expectedReturn.map(trim)
    }
  }
}
