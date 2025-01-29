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

package uk.gov.hmrc.cbcrfrontend.connectors.test

import play.api.libs.json.{JsNull, JsValue}
import uk.gov.hmrc.http.HttpReads.Implicits.readRaw
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse, StringContextOps}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import java.net.URL
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class TestCBCRConnector @Inject() (http: HttpClientV2, servicesConfig: ServicesConfig)(implicit ec: ExecutionContext) {
  private val url = s"${servicesConfig.baseUrl("cbcr")}/cbcr"

  def insertSubscriptionData(jsonData: JsValue)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.post(url"$url/test-only/insertSubscriptionData").withBody(jsonData).execute[HttpResponse]

  def deleteSubscription(utr: String)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.delete(new URL(s"$url/test-only/deleteSubscription/$utr")).execute[HttpResponse]

  def deleteSingleDocRefId(docRefId: String)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.delete(new URL(s"$url/test-only/deleteDocRefId/$docRefId")).execute[HttpResponse]

  def deleteReportingEntityData(docRefId: String)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.delete(new URL(s"$url/test-only/reportingEntityData/$docRefId")).execute[HttpResponse]

  def deleteSingleMessageRefId(messageRefId: String)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.delete(new URL(s"$url/test-only/deleteMessageRefId/$messageRefId")).execute[HttpResponse]

  def dropReportingEntityDataCollection()(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.delete(url"$url/test-only/reportingEntityData").execute[HttpResponse]

  def dropSubscriptionData()(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.delete(url"$url/test-only/deleteSubscription").execute[HttpResponse]

  def updateReportingEntityCreationDate(createDate: String, docRefId: String)(implicit
    hc: HeaderCarrier
  ): Future[HttpResponse] =
    http
      .put(new URL(s"$url/test-only/updateReportingEntityCreationDate/$createDate/$docRefId"))
      .withBody(JsNull)
      .execute[HttpResponse]

  def updateReportingEntityReportingPeriod(docRefId: String)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http
      .put(new URL(s"$url/test-only/deleteReportingPeriodByRepEntDocRefId/$docRefId"))
      .withBody(JsNull)
      .execute[HttpResponse]

  def deleteReportingEntityCreationDate(docRefId: String)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.delete(new URL(s"$url/test-only/deleteReportingEntityCreationDate/$docRefId")).execute[HttpResponse]

  def confirmReportingEntityCreationDate(createDate: String, docRefId: String)(implicit
    hc: HeaderCarrier
  ): Future[HttpResponse] =
    http.put(new URL(s"$url/test-only/confirmReportingEntityCreationDate/$createDate/$docRefId")).execute[HttpResponse]

  def deleteReportingEntityReportingPeriod(docRefId: String)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.delete(new URL(s"$url/test-only/deleteReportingEntityReportingPeriod/$docRefId")).execute[HttpResponse]

  def updateReportingEntityAdditionalInfoDRI(docRefId: String)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http
      .put(new URL(s"$url/test-only/updateReportingEntityAdditionalInfoDRI/$docRefId"))
      .withBody(JsNull)
      .execute[HttpResponse]

  def checkNumberOfCbcIdForUtr(utr: String)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.get(new URL(s"$url/test-only/validateNumberOfCbcIdForUtr/$utr")).execute[HttpResponse]
}
