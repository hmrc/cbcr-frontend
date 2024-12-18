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
import uk.gov.hmrc.http.{HeaderCarrier, HttpClient, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class TestCBCRConnector @Inject() (http: HttpClient, servicesConfig: ServicesConfig)(implicit ec: ExecutionContext) {
  private val url = s"${servicesConfig.baseUrl("cbcr")}/cbcr"

  def insertSubscriptionData(jsonData: JsValue)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.POST[JsValue, HttpResponse](s"$url/test-only/insertSubscriptionData", jsonData)

  def deleteSubscription(utr: String)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.DELETE[HttpResponse](s"$url/test-only/deleteSubscription/$utr")

  def deleteSingleDocRefId(docRefId: String)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.DELETE[HttpResponse](s"$url/test-only/deleteDocRefId/$docRefId")

  def deleteReportingEntityData(docRefId: String)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.DELETE(s"$url/test-only/reportingEntityData/$docRefId")

  def deleteSingleMessageRefId(messageRefId: String)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.DELETE[HttpResponse](s"$url/test-only/deleteMessageRefId/$messageRefId")

  def dropReportingEntityDataCollection()(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.DELETE(s"$url/test-only/reportingEntityData")

  def dropSubscriptionData()(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.DELETE(s"$url/test-only/deleteSubscription")

  def updateReportingEntityCreationDate(createDate: String, docRefId: String)(implicit
    hc: HeaderCarrier
  ): Future[HttpResponse] =
    http.PUT(s"$url/test-only/updateReportingEntityCreationDate/$createDate/$docRefId", JsNull)

  def updateReportingEntityReportingPeriod(docRefId: String)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.PUT(s"$url/test-only/deleteReportingPeriodByRepEntDocRefId/$docRefId", JsNull)

  def deleteReportingEntityCreationDate(docRefId: String)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.DELETE(s"$url/test-only/deleteReportingEntityCreationDate/$docRefId")

  def confirmReportingEntityCreationDate(createDate: String, docRefId: String)(implicit
    hc: HeaderCarrier
  ): Future[HttpResponse] =
    http.PUT(s"$url/test-only/confirmReportingEntityCreationDate/$createDate/$docRefId", JsNull)

  def deleteReportingEntityReportingPeriod(docRefId: String)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.DELETE(s"$url/test-only/deleteReportingEntityReportingPeriod/$docRefId")

  def updateReportingEntityAdditionalInfoDRI(docRefId: String)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.PUT(s"$url/test-only/updateReportingEntityAdditionalInfoDRI/$docRefId", JsNull)

  def checkNumberOfCbcIdForUtr(utr: String)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.GET[HttpResponse](s"$url/test-only/validateNumberOfCbcIdForUtr/$utr")
}
