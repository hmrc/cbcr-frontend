/*
 * Copyright 2024 HM Revenue & Customs
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

package uk.gov.hmrc.cbcrfrontend.connectors

import org.apache.pekko.stream.scaladsl.Source
import org.apache.pekko.util.ByteString
import play.api.libs.json.Json
import play.api.mvc.MultipartFormData.FilePart
import uk.gov.hmrc.cbcrfrontend.model.{CreateEnvelope, RouteEnvelopeRequest, UploadFile}
import uk.gov.hmrc.http.HttpReads.Implicits.readRaw
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse, StringContextOps}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import play.api.libs.ws.JsonBodyWritables.writeableOf_JsValue
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}
import uk.gov.hmrc.http.client.readStreamHttpResponse
import play.api.libs.ws.WSBodyWritables.bodyWritableOf_Multipart

@Singleton
class FileUploadServiceConnector @Inject() (httpClient: HttpClientV2, servicesConfig: ServicesConfig)(implicit
  ec: ExecutionContext
) {
  private lazy val fusUrl = servicesConfig.baseUrl("file-upload")
  private lazy val fusFeUrl = servicesConfig.baseUrl("file-upload-frontend")

  def createEnvelope(body: CreateEnvelope)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    httpClient
      .post(url"$fusUrl/file-upload/envelopes")
      .withBody(body.body)
      .execute

  def getFile(envelopeId: String, fileId: String)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    httpClient
      .get(url"$fusUrl/file-upload/envelopes/$envelopeId/files/$fileId/content")
      .stream[HttpResponse]

  def getFileMetaData(envelopeId: String, fileId: String)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    httpClient
      .get(url"$fusUrl/file-upload/envelopes/$envelopeId/files/$fileId/metadata")
      .execute[HttpResponse]

  def uploadFile(body: UploadFile)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    httpClient
      .post(url"$fusFeUrl/file-upload/upload/envelopes/${body.envelopeId}/files/${body.fileId}")
      .withBody(
        Source(
          FilePart(
            body.fileName,
            body.fileName,
            Some(body.contentType),
            Source.single(
              ByteString(Json.toJson(body.metadata).toString().getBytes)
            )
          ) :: Nil
        )
      )
      .setHeader(("CSRF-token", "nocheck"))
      .execute

  def routeEnvelopeRequest(body: RouteEnvelopeRequest)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    httpClient
      .post(url"$fusUrl/file-routing/requests")
      .withBody(Json.toJson(body))
      .execute
}
