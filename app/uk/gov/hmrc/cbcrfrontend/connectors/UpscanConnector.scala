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

package uk.gov.hmrc.cbcrfrontend.connectors

import org.slf4j.LoggerFactory
import play.api.http.HeaderNames
import play.api.http.Status.OK
import play.api.libs.json.{JsError, JsSuccess}
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.controllers.routes
import uk.gov.hmrc.cbcrfrontend.model.upscan._
import uk.gov.hmrc.http.HttpReads.Implicits._
import uk.gov.hmrc.http.{HeaderCarrier, HttpClient, HttpResponse}

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class UpscanConnector @Inject()(configuration: FrontendAppConfig, httpClient: HttpClient)(
  implicit ec: ExecutionContext) {

  private val logger = LoggerFactory.getLogger(getClass)

  private val headers = Map(
    HeaderNames.CONTENT_TYPE -> "application/json"
  )

  def getUpscanFormData(implicit hc: HeaderCarrier): Future[UpscanInitiateResponse] = {
    val callbackUrl = s"$backendUrl/callback"
    //TODO Upscan successRedirectUrl
    val successRedirectUrl = configuration.upscanRedirectBase + routes.FileUploadController
      .fileUploadResponse("test")
      .url
    val errorRedirectUrl = configuration.upscanRedirectBase + "/disclose-cross-border-arrangements/upload/error"
    val body = UpscanInitiateRequest(
      callbackUrl,
      successRedirectUrl,
      errorRedirectUrl,
      None,
      Some(upscanMaxSize * 1048576),
      Some("text/xml"))
    httpClient.POST[UpscanInitiateRequest, PreparedUpload](upscanInitiateUrl, body, headers.toSeq).map {
      _.toUpscanInitiateResponse
    }
  }

  def requestUpload(fileReference: Reference)(implicit hc: HeaderCarrier): Future[UploadId] = {
    val uploadId: UploadId = UploadId.generate
    val uploadUrl = s"$backendUrl/upscan/upload"
    httpClient.POST[UpscanIdentifiers, HttpResponse](uploadUrl, UpscanIdentifiers(uploadId, fileReference)).map { _ =>
      uploadId
    }
  }

  def getUploadDetails(uploadId: UploadId)(implicit hc: HeaderCarrier): Future[Option[UploadSessionDetails]] = {
    val detailsUrl = s"$backendUrl/upscan/details/${uploadId.value}"
    httpClient.GET[HttpResponse](detailsUrl).map { response =>
      response.status match {
        case OK =>
          response.json.validate[UploadSessionDetails] match {
            case JsSuccess(details, _) => Some(details)
            case JsError(_)            => None
          }
        case _ => None
      }
    }
  }

  def getUploadStatus(uploadId: UploadId)(implicit hc: HeaderCarrier): Future[Option[UploadStatus]] = {
    val statusUrl = s"$backendUrl/upscan/status/${uploadId.value}"
    httpClient.GET[HttpResponse](statusUrl).map { response =>
      logger.debug(s"Status uploaded: $response")
      response.status match {
        case OK =>
          response.json.validate[UploadStatus] match {
            case JsSuccess(status, _) =>
              Some(status)
            case JsError(_) =>
              None
          }
        case _ => None
      }
    }
  }

  private[connectors] val upscanInitiatePath: String = "/upscan/v2/initiate"
  //TODO replace with actual backend end point
  private val backendUrl = s"${configuration.crossBorderArrangementsUrl}/disclose-cross-border-arrangements"
  private val upscanInitiateUrl = s"${configuration.upscanInitiateHost}$upscanInitiatePath"
  private val upscanMaxSize = configuration.upscanMaxFileSize
}
