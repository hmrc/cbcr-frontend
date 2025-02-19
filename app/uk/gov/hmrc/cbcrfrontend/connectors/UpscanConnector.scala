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

package uk.gov.hmrc.cbcrfrontend.connectors

import play.api.libs.json.Json
import play.api.libs.json.OFormat.oFormatFromReadsAndOWrites
import play.mvc.Http.HeaderNames
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.model.upscan.{PreparedUpload, UpscanFileReference, UpscanInitiateRequest, UpscanInitiateResponse}
import uk.gov.hmrc.http.HttpReadsInstances.readFromJson
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.{HeaderCarrier, StringContextOps}

import javax.inject.{Inject, Named}
import scala.concurrent.{ExecutionContext, Future}

class UpscanConnector @Inject() (
  configuration: FrontendAppConfig,
  httpClient: HttpClientV2,
  @Named("appName") appName: String
)(implicit ec: ExecutionContext) {

  private val upscanInitiateHost: String = configuration.upscanInitiateHost
  private[connectors] val upscanInitiatePath: String = "/upscan/v2/initiate"
  private val upscanInitiateUrl: String = upscanInitiateHost + upscanInitiatePath
  private val headers = Map(
    HeaderNames.CONTENT_TYPE -> "application/json"
  )
  def initiateToUpscan(
    redirectOnSuccess: Option[String],
    redirectOnError: Option[String]
  )(implicit
    hc: HeaderCarrier
  ): Future[UpscanInitiateResponse] = {
    val payload = UpscanInitiateRequest(
      callbackUrl = configuration.callbackEndpointTarget,
      successRedirect = redirectOnSuccess,
      errorRedirect = redirectOnError
    )
    for {
      response <- httpClient
                    .post(url"$upscanInitiateUrl")
                    .withBody(Json.toJson(payload))
                    .setHeader(headers.toSeq: _*)
                    .execute[PreparedUpload]
      fileReference = UpscanFileReference(response.reference.value)
      postTarget = response.uploadRequest.href
      formFields = response.uploadRequest.fields
    } yield UpscanInitiateResponse(fileReference, postTarget, formFields)
  }
}
