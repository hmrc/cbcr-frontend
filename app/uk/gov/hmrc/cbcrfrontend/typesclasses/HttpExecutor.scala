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

package uk.gov.hmrc.cbcrfrontend.typesclasses

import akka.util.ByteString
import play.api.libs.json.{JsObject, Json, OFormat, Writes}
import uk.gov.hmrc.cbcrfrontend.config.FileUploadFrontEndWS
import uk.gov.hmrc.cbcrfrontend.model.{EnvelopeId, FileId}
import uk.gov.hmrc.http.{HeaderCarrier, HttpPut, HttpResponse}

import scala.concurrent.{ExecutionContext, Future}

trait GetBody[O, T] {
  def apply(obj: O): T
}

object GetBody {
  implicit object createEnvelopBody extends GetBody[CreateEnvelope, JsObject] {
    def apply(obj: CreateEnvelope): JsObject = obj.body
  }

  implicit object uploadFileBody extends GetBody[UploadFile, Array[Byte]] {
    def apply(obj: UploadFile): Array[Byte] = obj.body
  }

  implicit object FileUploadCallbackResponseBody extends GetBody[FUCallbackResponse, JsObject] {
    def apply(obj: FUCallbackResponse): JsObject = obj.body
  }

  implicit object routeEnvelopeBody extends GetBody[RouteEnvelopeRequest, RouteEnvelopeRequest] {
    def apply(obj: RouteEnvelopeRequest): RouteEnvelopeRequest = obj
  }

}

case class CreateEnvelope(body: JsObject)
case class UploadFile(envelopeId: EnvelopeId, fileId: FileId, fileName: String, contentType: String, body: Array[Byte])
case class FUCallbackResponse(body: JsObject)
case class GetFile(envelopeId: String, fileId: String)
case class RouteEnvelopeRequest(envelopeId: EnvelopeId, application: String, destination: String)

object RouteEnvelopeRequest {
  implicit val format: OFormat[RouteEnvelopeRequest] = Json.format[RouteEnvelopeRequest]
}

trait HttpExecutor[U, P, I] {
  def makeCall(
    url: ServiceUrl[U],
    obj: P
  )(
    implicit
    hc: HeaderCarrier,
    wts: Writes[I],
    getBody: GetBody[P, I],
//              TO DO - is http2 required
    http2: HttpPut,
    fileUploadFrontEndWS: FileUploadFrontEndWS,
    ec: ExecutionContext
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
      getBody: GetBody[CreateEnvelope, JsObject],
      //              TO DO - is http2 required
      http2: HttpPut,
      fileUploadFrontEndWS: FileUploadFrontEndWS,
      ec: ExecutionContext
    ): Future[HttpResponse] =
      fileUploadFrontEndWS
        .POST[JsObject, HttpResponse](s"${fusUrl.url}/file-upload/envelopes", getBody(obj))
  }

  implicit object uploadFile extends HttpExecutor[FusFeUrl, UploadFile, Array[Byte]] {
    def makeCall(
      fusFeUrl: ServiceUrl[FusFeUrl],
      obj: UploadFile
    )(
      implicit
      hc: HeaderCarrier,
      wts: Writes[Array[Byte]],
      getBody: GetBody[UploadFile, Array[Byte]],
      //              TO DO - is http2 required
      http2: HttpPut,
      fileUploadFrontEndWS: FileUploadFrontEndWS,
      ec: ExecutionContext
    ): Future[HttpResponse] = {
      import obj._
      val url = s"${fusFeUrl.url}/file-upload/upload/envelopes/$envelopeId/files/$fileId"
      fileUploadFrontEndWS
        .doFormPartPost(url, fileName, contentType, ByteString.fromArray(getBody(obj)), Seq("CSRF-token" -> "nocheck"))
    }
  }

  implicit object fileUploadCallbackResponse extends HttpExecutor[CbcrsUrl, FUCallbackResponse, JsObject] {
    def makeCall(
      cbcrsUrl: ServiceUrl[CbcrsUrl],
      obj: FUCallbackResponse
    )(
      implicit
      hc: HeaderCarrier,
      wts: Writes[JsObject],
      getBody: GetBody[FUCallbackResponse, JsObject],
      //              TO DO - is http2 required
      http2: HttpPut,
      fileUploadFrontEndWS: FileUploadFrontEndWS,
      ec: ExecutionContext
    ): Future[HttpResponse] =
      fileUploadFrontEndWS
        .POST[JsObject, HttpResponse](s"${cbcrsUrl.url}/cbcr/file-upload-response", getBody(obj))

  }

  implicit object routeRequest extends HttpExecutor[FusUrl, RouteEnvelopeRequest, RouteEnvelopeRequest] {
    def makeCall(
      fusUrl: ServiceUrl[FusUrl],
      obj: RouteEnvelopeRequest
    )(
      implicit
      hc: HeaderCarrier,
      wts: Writes[RouteEnvelopeRequest],
      getBody: GetBody[RouteEnvelopeRequest, RouteEnvelopeRequest],
      //              TO DO - is http2 required
      http2: HttpPut,
      fileUploadFrontEndWS: FileUploadFrontEndWS,
      ec: ExecutionContext
    ): Future[HttpResponse] =
      fileUploadFrontEndWS
        .POST[RouteEnvelopeRequest, HttpResponse](s"${fusUrl.url}/file-routing/requests", getBody(obj))
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
    getBody: GetBody[P, I],
    //              TO DO - is http2 required
    http2: HttpPut,
    fileUploadFrontEndWS: FileUploadFrontEndWS
  ): Future[HttpResponse] =
    httpExecutor.makeCall(url, obj)
}
