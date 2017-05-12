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

import cats.data.EitherT
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mock.MockitoSugar
import org.scalatestplus.play.OneAppPerSuite
import uk.gov.hmrc.cbcrfrontend.controllers.auth.{SecuredActionsTest, TestUsers}
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.services.{CBCSessionCache, FileUploadService}
import uk.gov.hmrc.play.test.UnitSpec
import uk.gov.hmrc.cbcrfrontend.typesclasses.{CbcrsUrl, FusFeUrl, FusUrl, ServiceUrl}
import uk.gov.hmrc.play.http.HeaderCarrier
import org.mockito.Mockito._
import org.mockito.Matchers.{eq => EQ,_}
import play.api.http.Status
import play.api.libs.json.JsNull
import play.api.test.FakeRequest
import uk.gov.hmrc.cbcrfrontend.exceptions.UnexpectedState
import uk.gov.hmrc.emailaddress.EmailAddress

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global


/**
  * Created by max on 12/05/17.
  */
class SubmitControllerSpec extends UnitSpec with ScalaFutures with OneAppPerSuite with CSRFTest with MockitoSugar with FakeAuthConnector {

  val authCon = authConnector(TestUsers.cbcrUser)
  val securedActions = new SecuredActionsTest(TestUsers.cbcrUser, authCon)
  val cache = mock[CBCSessionCache]
  val fus  = mock[FileUploadService]
  val controller = new SubmitController(cache,securedActions,fus)

  implicit val hc = HeaderCarrier()
  implicit lazy val fusUrl = new ServiceUrl[FusUrl] { val url = "file-upload"}
  implicit lazy val fusFeUrl = new ServiceUrl[FusFeUrl] { val url = "file-upload-frontend"}
  implicit lazy val cbcrsUrl = new ServiceUrl[CbcrsUrl] { val url = "cbcr"}

  val bpr = BusinessPartnerRecord("safeId",None, EtmpAddress(None,None,None,None,None,None))

  "The Submitcontroller" should {
    "provide a method to generate the metadata that"  should {
      "return a list of errors for each of the missing cache values" in {
        when(cache.read[BusinessPartnerRecord]) thenReturn Future.successful(None)
        when(cache.read[Utr]) thenReturn Future.successful(None)
        when(cache.read[CBCId]) thenReturn Future.successful(None)
        when(cache.read[Hash]) thenReturn Future.successful(None)
        when(cache.read[FileId]) thenReturn Future.successful(None)
        when(cache.read[EnvelopeId]) thenReturn Future.successful(None)
        when(cache.read[SubmitterInfo]) thenReturn Future.successful(None)
        when(cache.read[FilingType]) thenReturn Future.successful(None)
        when(cache.read[UPE]) thenReturn Future.successful(None)
        when(cache.read[FilingCapacity]) thenReturn Future.successful(None)
        when(cache.read[FileMetadata]) thenReturn Future.successful(None)

        Await.result(controller.generateMetadataFile("gatewayId"),10.second).fold(
          errors => errors.toList.size shouldBe 11,
          _ => fail("this should have failed")
        )

        when(cache.read[FileId]) thenReturn Future.successful(Some(FileId("fileId")))
        Await.result(controller.generateMetadataFile("gatewayId"),10.second).fold(
          errors => errors.toList.size shouldBe 10,
          _ => fail("this should have failed")
        )

        when(cache.read[EnvelopeId]) thenReturn Future.successful(Some(EnvelopeId("yeah")))
        Await.result(controller.generateMetadataFile("gatewayId"),10.second).fold(
          errors => errors.toList.size shouldBe 9,
          _ => fail("this should have failed")
        )

      }
      "return a failed future if any of the cache calls fails" in {
        when(cache.read[EnvelopeId]) thenReturn Future.failed(new Exception("Cache gone bad"))
        intercept[Exception] {
          Await.result(controller.generateMetadataFile("gatewayId"), 10.second)
        }
      }
      "return a Metadata object if all succeeds" in {
        when(cache.read[BusinessPartnerRecord]) thenReturn Future.successful(Some(bpr))
        when(cache.read[Utr]) thenReturn Future.successful(Some(Utr("utr")))
        when(cache.read[CBCId]) thenReturn Future.successful(CBCId.create(1).toOption)
        when(cache.read[Hash]) thenReturn Future.successful(Some(Hash("hash")))
        when(cache.read[FileId]) thenReturn Future.successful(Some(FileId("yeah")))
        when(cache.read[EnvelopeId]) thenReturn Future.successful(Some(EnvelopeId("id")))
        when(cache.read[SubmitterInfo]) thenReturn Future.successful(Some(SubmitterInfo("name","agency","MD","0123123123",EmailAddress("max@max.com"))))
        when(cache.read[FilingType]) thenReturn Future.successful(Some(FilingType("asdf")))
        when(cache.read[UPE]) thenReturn Future.successful(Some(UPE("yeah")))
        when(cache.read[FilingCapacity]) thenReturn Future.successful(Some(FilingCapacity("Filing capacity")))
        when(cache.read[FileMetadata]) thenReturn Future.successful(Some(FileMetadata("asdf","lkjasdf","lkj","lkj",10,"lkjasdf",JsNull,"")))

        Await.result(controller.generateMetadataFile("gatewayId"),10.second).leftMap(
          errors => fail(s"There should be no errors: $errors")
        )
      }
    }
    "provide a 'confirm' Action that" should {
      val fakeRequestConfirm= addToken(FakeRequest("GET", "/confirm"))
      "return 500 if generating the metadata fails" in {
        when(cache.read[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format),any(),any())) thenReturn Future.failed(new Exception("argh"))
        status(Await.result(controller.confirm(fakeRequestConfirm), 10.second)) shouldBe Status.INTERNAL_SERVER_ERROR
        when(cache.read[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format),any(),any())) thenReturn Future.successful(Some(bpr))

        when(cache.read[UPE]) thenReturn Future.successful(None)
        status(controller.confirm(fakeRequestConfirm)) shouldBe Status.INTERNAL_SERVER_ERROR
        when(cache.read[UPE]) thenReturn Future.successful(Some(UPE("yeah")))

      }
      "return 500 if the fileUploadService fails" in {
        when(fus.uploadMetadataAndRoute(any())(any(),any(),any(),any())) thenReturn EitherT[Future,UnexpectedState, String](Future.successful(Left(UnexpectedState("Some error"))))
        status(controller.confirm(fakeRequestConfirm)) shouldBe Status.INTERNAL_SERVER_ERROR

        when(fus.uploadMetadataAndRoute(any())(any(),any(),any(),any())) thenReturn EitherT[Future,UnexpectedState, String](Future.failed(new Exception("Something else went wrong!")))
        status(controller.confirm(fakeRequestConfirm)) shouldBe Status.INTERNAL_SERVER_ERROR

      }
      "return 303 if everything succeeds" in {

        when(cache.read[BusinessPartnerRecord](EQ(BusinessPartnerRecord.format),any(),any())) thenReturn Future.successful(Some(bpr))
        when(cache.read[Utr](EQ(Utr.utrRead),any(),any())) thenReturn Future.successful(Some(Utr("utr")))
        when(cache.read[CBCId](EQ(CBCId.cbcIdFormat),any(),any())) thenReturn Future.successful(CBCId.create(1).toOption)
        when(cache.read[Hash](EQ(Hash.format),any(),any())) thenReturn Future.successful(Some(Hash("hash")))
        when(cache.read[FileId](EQ(FileId.fileIdFormat),any(),any())) thenReturn Future.successful(Some(FileId("yeah")))
        when(cache.read[EnvelopeId](EQ(EnvelopeId.format),any(),any())) thenReturn Future.successful(Some(EnvelopeId("id")))
        when(cache.read[SubmitterInfo](EQ(SubmitterInfo.format),any(),any())) thenReturn Future.successful(Some(SubmitterInfo("name","agency","MD","0123123123",EmailAddress("max@max.com"))))
        when(cache.read[FilingType](EQ(FilingType.format),any(),any())) thenReturn Future.successful(Some(FilingType("asdf")))
        when(cache.read[UPE](EQ(UPE.format),any(),any())) thenReturn Future.successful(Some(UPE("yeah")))
        when(cache.read[FilingCapacity](EQ(FilingCapacity.format),any(),any())) thenReturn Future.successful(Some(FilingCapacity("Filing capacity")))
        when(cache.read[FileMetadata](EQ(FileMetadata.fileMetadataFormat),any(),any())) thenReturn Future.successful(Some(FileMetadata("asdf","lkjasdf","lkj","lkj",10,"lkjasdf",JsNull,"")))

        when(fus.uploadMetadataAndRoute(any())(any(),any(),any(),any())) thenReturn EitherT[Future,UnexpectedState, String](Future.successful(Right("yeah")))
        status(controller.confirm(fakeRequestConfirm)) shouldBe Status.SEE_OTHER

      }
    }
  }

}
