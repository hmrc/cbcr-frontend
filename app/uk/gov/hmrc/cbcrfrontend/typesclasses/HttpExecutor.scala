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

package uk.gov.hmrc.cbcrfrontend.typesclasses

import akka.util.ByteString
import play.api.libs.json.{JsObject, JsValue, Json, Writes}
import uk.gov.hmrc.cbcrfrontend.{FileUploadFrontEndWS, WSHttp}
import uk.gov.hmrc.cbcrfrontend.model.{EnvelopeId, FileId}

import scala.concurrent.ExecutionContext
import uk.gov.hmrc.play.http.{HeaderCarrier, HttpReads, HttpResponse}

import scala.concurrent.Future

trait GetBody[O, T] {
  def apply(obj: O): T
}

object GetBody {
  implicit object createEnvelopBody extends GetBody[CreateEnvelope, JsObject] {
    def apply(obj: CreateEnvelope) = obj.body
  }

  implicit object uploadFileBody extends GetBody[UploadFile, Array[Byte]] {
    def apply(obj: UploadFile) = obj.body
  }

  implicit object FileUploadCallbackResponseBody extends GetBody[FileUploadCallbackResponse, JsObject] {
    def apply(obj: FileUploadCallbackResponse) = obj.body
  }

}

case class CreateEnvelope(body: JsObject)
case class UploadFile(envelopeId: EnvelopeId, fileId: FileId, fileName: String, contentType: String, body: Array[Byte])
case class FileUploadCallbackResponse(body: JsObject)

trait HttpExecutor[U, P, I] {
  def makeCall(
    url: ServiceUrl[U],
    obj: P
  )(
    implicit
    hc: HeaderCarrier,
    wts: Writes[I],
    rds: HttpReads[HttpResponse],
    getBody: GetBody[P, I]
  ): Future[HttpResponse]
}

object HttpExecutor {
  implicit object createEnvelope extends HttpExecutor[FusUrl, CreateEnvelope, JsObject] {
    def makeCall(
      fusUrl: ServiceUrl[FusUrl],
      obj: CreateEnvelope
    )(
      implicit
      hc: HeaderCarrier,
      wts: Writes[JsObject],
      rds: HttpReads[HttpResponse],
      getBody: GetBody[CreateEnvelope, JsObject]
    ): Future[HttpResponse] = {
      WSHttp.POST[JsObject, HttpResponse](s"${fusUrl.url}/file-upload/envelopes", getBody(obj))
    }
  }

  implicit object uploadFile extends HttpExecutor[FusFeUrl, UploadFile, Array[Byte]] {
    def makeCall(
      fusFeUrl: ServiceUrl[FusFeUrl],
      obj: UploadFile
    )(
      implicit
      hc: HeaderCarrier,
      wts: Writes[Array[Byte]],
      rds: HttpReads[HttpResponse],
      getBody: GetBody[UploadFile, Array[Byte]]
    ): Future[HttpResponse] = {
      import obj._
      val url = s"${fusFeUrl.url}/file-upload/upload/envelopes/$envelopeId/files/$fileId"
      FileUploadFrontEndWS.doFormPartPost(url, fileName, contentType, ByteString.fromArray(getBody(obj)), Seq("CSRF-token" -> "nocheck"))
    }
  }


  implicit object fileUploadCallbackResponse extends HttpExecutor[CbcrsUrl, FileUploadCallbackResponse, JsObject] {
    def makeCall(
                  cbcrsUrl: ServiceUrl[CbcrsUrl],
                  obj: FileUploadCallbackResponse
                )(
                  implicit
                  hc: HeaderCarrier,
                  wts: Writes[JsObject],
                  rds: HttpReads[HttpResponse],
                  getBody: GetBody[FileUploadCallbackResponse, JsObject]
                ): Future[HttpResponse] = {
      WSHttp.POST[JsObject, HttpResponse](s"${cbcrsUrl.url}/cbcr/saveFileUploadResponse?cbcId=CBCId1234", getBody(obj))
    }

  }

  def apply[U, P, I](
    url: ServiceUrl[U],
    obj: P
  )(
    implicit
    hc: HeaderCarrier,
    ec: ExecutionContext,
    httpExecutor: HttpExecutor[U, P, I],
    wts: Writes[I],
    getBody: GetBody[P, I]
  ): Future[HttpResponse] = {
    httpExecutor.makeCall(url, obj)
  }
}
