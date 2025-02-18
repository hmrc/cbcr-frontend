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

package uk.gov.hmrc.cbcrfrontend.services

import play.api.mvc.Request
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.connectors.UpscanConnector
import uk.gov.hmrc.cbcrfrontend.model.upscan.{UpscanInitiateRequest, UpscanInitiateResponse}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future
import javax.inject.Inject

class UpscanService @Inject() (
  applicationConfig: FrontendAppConfig,
  upscanConnector: UpscanConnector
) {

  lazy val redirectUrlBase: String = applicationConfig.upscanRedirectBase

  def getUpscanFormData()(implicit hc: HeaderCarrier, request: Request[_]): Future[UpscanInitiateResponse] = {

    val upscanInitiateRequest = UpscanInitiateRequest(
      callbackUrl = redirectUrlBase + "/upload/callback",
      successRedirect = redirectUrlBase + "/cbcr/upload-csv/success",
      errorRedirect = redirectUrlBase + "/cbcr/upload-csv/failure",
      minimumFileSize = Some(0),
      maximumFileSize = Some(100)
    )
    upscanConnector.initiateToUpscan(upscanInitiateRequest)
  }

}
