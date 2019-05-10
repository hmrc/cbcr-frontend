/*
 * Copyright 2019 HM Revenue & Customs
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

import java.time.LocalDate

import javax.inject.{Inject, Singleton}
import cats.syntax.show._
import com.typesafe.config.Config
import configs.syntax._
import play.api.Configuration
import play.api.libs.json.JsNull
import uk.gov.hmrc.cbcrfrontend.controllers.{AdminDocRefIdRecord, ListDocRefIdRecord}
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.model.Email

import scala.concurrent.{ExecutionContext, Future}
import uk.gov.hmrc.http._
import uk.gov.hmrc.play.bootstrap.http.HttpClient

@Singleton
class CBCRBackendConnector @Inject()(http: HttpClient, config: Configuration)(implicit ec:ExecutionContext) {

  val conf = config.underlying.get[Config]("microservice.services.cbcr").value

  val url: String = (for {
    proto <- conf.get[String]("protocol")
    host <- conf.get[String]("host")
    port <- conf.get[Int]("port")
  } yield s"$proto://$host:$port/cbcr").value

  def subscribe(s: SubscriptionDetails)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.POST(url + "/subscription", s)

  def sendEmail(email: Email)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.POST(url + s"/email", email)

  def getETMPSubscriptionData(safeId: String)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.GET(url + s"/subscription/$safeId")

  def updateETMPSubscriptionData(safeId: String, correspondenceDetails: CorrespondenceDetails)(implicit hc: HeaderCarrier): Future[HttpResponse] = {
    implicit val emailFormat = ContactDetails.emailFormat
    http.PUT(url + s"/subscription/$safeId", correspondenceDetails)
  }

  def messageRefIdExists(id: String)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.GET(url + s"/message-ref-id/$id")

  def saveMessageRefId(id: String)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.PUT(url + s"/message-ref-id/$id", JsNull)

  def docRefIdQuery(d: DocRefId)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.GET(url + s"/doc-ref-id/${d.show}")

  def docRefIdSave(d: DocRefId)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.PUT(url + s"/doc-ref-id/${d.show}", JsNull)

  def corrDocRefIdSave(c: CorrDocRefId, d: DocRefId)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.PUT(url + s"/corr-doc-ref-id/${c.cid.show}/${d.show}", JsNull)

  def reportingEntityDataSave(r:ReportingEntityData)(implicit hc:HeaderCarrier) : Future[HttpResponse] =
    http.POST(url+ "/reporting-entity",r)

  def reportingEntityDataUpdate(r:PartialReportingEntityData)(implicit hc:HeaderCarrier) : Future[HttpResponse] =
    http.PUT[PartialReportingEntityData,HttpResponse](url+ "/reporting-entity",r)

  def reportingEntityDataQuery(d:DocRefId)(implicit hc:HeaderCarrier) : Future[HttpResponse] =
    http.GET(url + s"/reporting-entity/${d.show}")

  def reportingEntityDataModelQuery(d:DocRefId)(implicit hc:HeaderCarrier) : Future[HttpResponse] =
    http.GET(url + s"/reporting-entity/model/${d.show}")

  def reportingEntityDocRefId(d: DocRefId)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.GET(url + s"/reporting-entity/doc-ref-id/${d.show}")

  def reportingEntityCBCIdAndReportingPeriod(cbcId: CBCId, reportingPeriod: LocalDate)(implicit hc: HeaderCarrier) : Future[HttpResponse] = {
    http.GET(url + s"/reporting-entity/query-cbc-id/${cbcId.toString}/${reportingPeriod.toString}")
  }

  def reportingEntityDataQueryTin(tin: String)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http.GET(url + s"/reporting-entity/query-tin/$tin")

  def getDocRefIdOver200(implicit hc: HeaderCarrier) = {

    http.GET[ListDocRefIdRecord](url + s"/getDocsRefId")
  }


}
