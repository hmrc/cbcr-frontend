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

import cats.syntax.show._
import com.typesafe.config.Config
import configs.syntax._
import play.api.Configuration
import play.api.libs.json.{JsNull, JsString, JsValue}
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.http.HttpReads.Implicits.readRaw
import uk.gov.hmrc.http.{HeaderCarrier, HttpClient, HttpResponse, StringContextOps}

import java.time.LocalDate
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class CBCRBackendConnector @Inject()(http: HttpClient, config: Configuration)(implicit ec: ExecutionContext) {
  private val conf = config.underlying.get[Config]("microservice.services.cbcr").value

  private val url = (for {
    proto <- conf.get[String]("protocol")
    host  <- conf.get[String]("host")
    port  <- conf.get[Int]("port")
  } yield s"$proto://$host:$port/cbcr").value

  def getFileUploadResponse(envelopeId: String)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.GET[HttpResponse](url"$url/file-upload-response/$envelopeId")

  def subscribe(s: SubscriptionDetails)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.POST[SubscriptionDetails, HttpResponse](url + "/subscription", s)

  def sendEmail(email: Email)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.POST[Email, HttpResponse](url + s"/email", email)

  def getETMPSubscriptionData(safeId: String)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.GET[HttpResponse](url + s"/subscription/$safeId")

  def updateETMPSubscriptionData(safeId: String, correspondenceDetails: CorrespondenceDetails)(
    implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.PUT[CorrespondenceDetails, HttpResponse](url + s"/subscription/$safeId", correspondenceDetails)

  def messageRefIdExists(id: String)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.GET[HttpResponse](url + s"/message-ref-id/$id")

  def saveMessageRefId(id: String)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.PUT[JsValue, HttpResponse](url + s"/message-ref-id/$id", JsNull)

  def docRefIdQuery(d: DocRefId)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.GET[HttpResponse](url + s"/doc-ref-id/${d.show}")

  def docRefIdSave(d: DocRefId)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.PUT[JsValue, HttpResponse](url + s"/doc-ref-id/${d.show}", JsNull)

  def corrDocRefIdSave(c: CorrDocRefId, d: DocRefId)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.PUT[JsValue, HttpResponse](url + s"/corr-doc-ref-id/${c.cid.show}", JsString(d.show))

  def reportingEntityDataSave(r: ReportingEntityData)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.POST[ReportingEntityData, HttpResponse](url + "/reporting-entity", r)

  def reportingEntityDataUpdate(r: PartialReportingEntityData)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.PUT[PartialReportingEntityData, HttpResponse](url + "/reporting-entity", r)

  def reportingEntityDataQuery(d: DocRefId)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.GET[HttpResponse](url + s"/reporting-entity/query/${d.show}")

  def reportingEntityDataModelQuery(d: DocRefId)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.GET[HttpResponse](url + s"/reporting-entity/model/${d.show}")

  def reportingEntityDocRefId(d: DocRefId)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.GET[HttpResponse](url + s"/reporting-entity/doc-ref-id/${d.show}")

  def reportingEntityCBCIdAndReportingPeriod(cbcId: CBCId, reportingPeriod: LocalDate)(
    implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.GET[HttpResponse](url + s"/reporting-entity/query-cbc-id/${cbcId.toString}/${reportingPeriod.toString}")

  def reportingEntityDataQueryTin(tin: String, reportingPeriod: String)(
    implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.GET[HttpResponse](url + s"/reporting-entity/query-tin/$tin/$reportingPeriod")

  def overlapQuery(tin: String, entityReportingPeriod: EntityReportingPeriod)(
    implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.GET[HttpResponse](
      url + s"/reporting-entity/query-dates/$tin/start-date/${entityReportingPeriod.startDate.toString}/end-date/${entityReportingPeriod.endDate.toString}")

  def adminReportingEntityDataQuery(d: String)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.GET[HttpResponse](url + s"/admin/reporting-entity/doc-ref-id/$d")

  def adminReportingEntityCBCIdAndReportingPeriod(cbcId: String, reportingPeriod: LocalDate)(
    implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.GET[HttpResponse](url + s"/admin/reporting-entity/query-cbc-id/$cbcId/${reportingPeriod.toString}")

  def adminReportingEntityDataQueryTin(tin: String, reportingPeriod: String)(
    implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.GET[HttpResponse](url + s"/admin/reporting-entity/query-tin/$tin/$reportingPeriod")

  def adminEditDocRefId(docRefId: String)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.PUT[JsValue, HttpResponse](url + s"/admin/updateDocRefId/$docRefId", JsNull)
}
