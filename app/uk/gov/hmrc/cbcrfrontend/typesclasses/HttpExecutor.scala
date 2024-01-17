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

import akka.stream.scaladsl.Source
import akka.util.ByteString
import play.api.libs.json.{JsObject, Json, OFormat}
import play.api.mvc.MultipartFormData.FilePart
import uk.gov.hmrc.cbcrfrontend.model.{EnvelopeId, FileId}
import uk.gov.hmrc.http.HttpReads.Implicits.readRaw
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import java.net.URL
import scala.concurrent.{ExecutionContext, Future}

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
    obj: P,
    httpClient: HttpClientV2
  )(
    implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[HttpResponse]
}

object HttpExecutor {
  implicit object createEnvelope extends HttpExecutor[FusUrl, CreateEnvelope, JsObject] {
    def makeCall(
      fusUrl: ServiceUrl[FusUrl],
      obj: CreateEnvelope,
      httpClient: HttpClientV2
    )(
      implicit
      hc: HeaderCarrier,
      ec: ExecutionContext
    ): Future[HttpResponse] =
      httpClient
        .post(new URL(s"${fusUrl.url}/file-upload/envelopes"))
        .withBody(obj.body)
        .execute
  }

  implicit object uploadFile extends HttpExecutor[FusFeUrl, UploadFile, Array[Byte]] {
    def makeCall(
      fusFeUrl: ServiceUrl[FusFeUrl],
      obj: UploadFile,
      httpClient: HttpClientV2
    )(
      implicit
      hc: HeaderCarrier,
      ec: ExecutionContext
    ): Future[HttpResponse] = {
      import obj._
      httpClient
        .post(new URL(s"${fusFeUrl.url}/file-upload/upload/envelopes/$envelopeId/files/$fileId"))
        .withBody(
          Source(FilePart(fileName, fileName, Some(contentType), Source.single(ByteString.fromArray(obj.body))) :: Nil)
        )
        .setHeader(("CSRF-token", "nocheck"))
        .execute
    }
  }

  implicit object fileUploadCallbackResponse extends HttpExecutor[CbcrsUrl, FUCallbackResponse, JsObject] {
    def makeCall(
      cbcrsUrl: ServiceUrl[CbcrsUrl],
      obj: FUCallbackResponse,
      httpClient: HttpClientV2
    )(
      implicit
      hc: HeaderCarrier,
      ec: ExecutionContext
    ): Future[HttpResponse] =
      httpClient
        .post(new URL(s"${cbcrsUrl.url}/cbcr/file-upload-response"))
        .withBody(obj.body)
        .execute
  }

  implicit object routeRequest extends HttpExecutor[FusUrl, RouteEnvelopeRequest, RouteEnvelopeRequest] {
    def makeCall(
      fusUrl: ServiceUrl[FusUrl],
      obj: RouteEnvelopeRequest,
      httpClient: HttpClientV2
    )(
      implicit
      hc: HeaderCarrier,
      ec: ExecutionContext
    ): Future[HttpResponse] =
      httpClient
        .post(new URL(s"${fusUrl.url}/file-routing/requests"))
        .withBody(Json.toJson(obj))
        .execute
  }

  def apply[U, P, I](
    url: ServiceUrl[U],
    obj: P,
    httpClient: HttpClientV2
  )(
    implicit
    hc: HeaderCarrier,
    ec: ExecutionContext,
    httpExecutor: HttpExecutor[U, P, I]
  ): Future[HttpResponse] =
    httpExecutor.makeCall(url, obj, httpClient)
}
