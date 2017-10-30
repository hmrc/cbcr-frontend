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

package uk.gov.hmrc.cbcrfrontend.connectors.test

import play.api.libs.json.JsValue
import uk.gov.hmrc.cbcrfrontend.WSHttp
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.http._

import scala.concurrent.Future

trait TestRegistrationConnector {
  def insertSubscriptionData(jsonData: JsValue)(implicit hc: HeaderCarrier) : Future[HttpResponse]

  def deleteSubscription(utr: String)(implicit hc: HeaderCarrier) : Future[HttpResponse]

  def deleteSingleDocRefId(docRefId: String)(implicit hc: HeaderCarrier) : Future[HttpResponse]
}


object TestCBCRConnector extends TestRegistrationConnector with ServicesConfig{

  val cbcrUrl = baseUrl("cbcr")
  val http = WSHttp

  def insertSubscriptionData(jsonData: JsValue)(implicit hc: HeaderCarrier) : Future[HttpResponse] = {
    http.POST[JsValue, HttpResponse](s"$cbcrUrl/cbcr/test-only/insertSubscriptionData", jsonData)
  }

  def deleteSubscription(utr: String)(implicit hc: HeaderCarrier) : Future[HttpResponse] = {
    http.DELETE[HttpResponse](s"$cbcrUrl/cbcr/test-only/deleteSubscription/$utr")
  }

  def deleteSingleDocRefId(docRefId: String)(implicit hc: HeaderCarrier) : Future[HttpResponse] = {
    http.DELETE[HttpResponse](s"$cbcrUrl/cbcr/test-only/deleteDocRefId/$docRefId")
  }
}