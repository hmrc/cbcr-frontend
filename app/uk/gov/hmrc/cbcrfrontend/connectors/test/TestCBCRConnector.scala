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

package uk.gov.hmrc.cbcrfrontend.connectors.test

import javax.inject.{Inject, Singleton}
import com.typesafe.config.Config
import configs.syntax._
import play.api.Configuration
import play.api.libs.json.{JsNull, JsValue}

import scala.concurrent.{ExecutionContext, Future}
import uk.gov.hmrc.http.{HeaderCarrier, HttpClient, HttpResponse}
@Singleton
class TestCBCRConnector @Inject()(http: HttpClient, config: Configuration)(implicit ec: ExecutionContext) {

  val conf = config.underlying.get[Config]("microservice.services.cbcr").value

  val url: String = (for {
    proto <- conf.get[String]("protocol")
    host  <- conf.get[String]("host")
    port  <- conf.get[Int]("port")
  } yield s"$proto://$host:$port/cbcr").value

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

  def updateReportingEntityCreationDate(createDate: String, docRefId: String)(
    implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.PUT(s"$url/test-only/updateReportingEntityCreationDate/$createDate/$docRefId", JsNull)

  def updateReportingEntityReportingPeriod(docRefId: String)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.PUT(s"$url/test-only/deleteReportingPeriodByRepEntDocRefId/$docRefId", JsNull)

  def deleteReportingEntityCreationDate(docRefId: String)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.DELETE(s"$url/test-only/deleteReportingEntityCreationDate/$docRefId")

  def confirmReportingEntityCreationDate(createDate: String, docRefId: String)(
    implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.PUT(s"$url/test-only/confirmReportingEntityCreationDate/$createDate/$docRefId", JsNull)

  def deleteReportingEntityReportingPeriod(docRefId: String)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.DELETE(s"$url/test-only/deleteReportingEntityReportingPeriod/$docRefId")

  def updateReportingEntityAdditionalInfoDRI(docRefId: String)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.PUT(s"$url/test-only/updateReportingEntityAdditionalInfoDRI/$docRefId", JsNull)

  def checkNumberOfCbcIdForUtr(utr: String)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.GET[HttpResponse](s"$url/test-only/validateNumberOfCbcIdForUtr/$utr")

}
