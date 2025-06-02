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

import cats.syntax.show._
import play.api.libs.json.{JsNull, JsString, Json}
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.http.HttpReads.Implicits.readRaw
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse, StringContextOps}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import java.time.LocalDate
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}
import play.api.libs.ws.WSBodyWritables.writeableOf_JsValue

@Singleton
class CBCRBackendConnector @Inject() (http: HttpClientV2, servicesConfig: ServicesConfig)(implicit
  ec: ExecutionContext
) {
  private val url = s"${servicesConfig.baseUrl("cbcr")}/cbcr"

  def getFileUploadResponse(envelopeId: String)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.get(url"$url/file-upload-response/$envelopeId").execute[HttpResponse]

  def subscribe(s: SubscriptionDetails)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.post(url"$url/subscription").withBody(Json.toJson(s)).execute[HttpResponse]
  def sendEmail(email: Email)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.post(url"$url/email").withBody(Json.toJson(email)).execute[HttpResponse]
  def getETMPSubscriptionData(safeId: String)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.get(url"$url/subscription/$safeId").execute[HttpResponse]
  def updateETMPSubscriptionData(safeId: String, correspondenceDetails: CorrespondenceDetails)(implicit
    hc: HeaderCarrier
  ): Future[HttpResponse] =
    http.put(url"$url/subscription/$safeId").withBody(Json.toJson(correspondenceDetails)).execute[HttpResponse]

  def messageRefIdExists(id: String)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.get(url"$url/message-ref-id/$id").execute[HttpResponse]

  def saveMessageRefId(id: String)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.put(url"$url/message-ref-id/$id").withBody(Json.toJson(JsNull)).execute[HttpResponse]
  def docRefIdQuery(d: DocRefId)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.get(url"$url/doc-ref-id/${d.show}").execute[HttpResponse]
  def docRefIdSave(d: DocRefId)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.put(url"$url/doc-ref-id/${d.show}").withBody(Json.toJson(JsNull)).execute[HttpResponse]
  def corrDocRefIdSave(c: CorrDocRefId, d: DocRefId)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.put(url"$url/corr-doc-ref-id/${c.cid.show}").withBody(Json.toJson(JsString(d.show))).execute[HttpResponse]
  def reportingEntityDataSave(r: ReportingEntityData)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.post(url"$url/reporting-entity").withBody(Json.toJson(r)).execute[HttpResponse]
  def reportingEntityDataUpdate(r: PartialReportingEntityData)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.put(url"$url/reporting-entity").withBody(Json.toJson(r)).execute[HttpResponse]
  def reportingEntityDataQuery(d: DocRefId)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.get(url"$url/reporting-entity/query/${d.show}").execute[HttpResponse]
  def reportingEntityDataModelQuery(d: DocRefId)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.get(url"$url/reporting-entity/model/${d.show}").execute[HttpResponse]
  def reportingEntityDocRefId(d: DocRefId)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.get(url"$url/reporting-entity/doc-ref-id/${d.show}").execute[HttpResponse]
  def reportingEntityCBCIdAndReportingPeriod(cbcId: CBCId, reportingPeriod: LocalDate)(implicit
    hc: HeaderCarrier
  ): Future[HttpResponse] =
    http
      .get(url"$url/reporting-entity/query-cbc-id/${cbcId.toString}/${reportingPeriod.toString}")
      .execute[HttpResponse]
  def reportingEntityDataQueryTin(tin: String, reportingPeriod: String)(implicit
    hc: HeaderCarrier
  ): Future[HttpResponse] =
    http.get(url"$url/reporting-entity/query-tin/$tin/$reportingPeriod").execute[HttpResponse]
  def overlapQuery(tin: String, entityReportingPeriod: EntityReportingPeriod)(implicit
    hc: HeaderCarrier
  ): Future[HttpResponse] =
    http
      .get(
        url"$url/reporting-entity/query-dates/$tin/start-date/${entityReportingPeriod.startDate.toString}/end-date/${entityReportingPeriod.endDate.toString}"
      )
      .execute[HttpResponse]
}
