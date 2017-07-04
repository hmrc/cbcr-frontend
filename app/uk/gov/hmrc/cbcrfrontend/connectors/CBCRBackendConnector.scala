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

package uk.gov.hmrc.cbcrfrontend.connectors

import javax.inject.Singleton
import javax.inject.Inject

import com.typesafe.config.Config
import play.api.Configuration
import uk.gov.hmrc.play.http.{HeaderCarrier, HttpGet, HttpPut, HttpResponse}
import configs.syntax._
import play.api.libs.json.JsNull
import uk.gov.hmrc.cbcrfrontend.model.{CorrDocRefId, DocRefId}
import uk.gov.hmrc.play.http._

import scala.concurrent.Future

@Singleton
class CBCRBackendConnector @Inject()(http:HttpGet with HttpPut, config:Configuration) {

  val conf = config.underlying.get[Config]("microservice.services.cbcr").value

  val url: String = (for {
    proto <- conf.get[String]("protocol")
    host  <- conf.get[String]("host")
    port  <- conf.get[Int]("port")
  } yield s"$proto://$host:$port").value

  def getId()(implicit hc:HeaderCarrier) : Future[HttpResponse] = http.GET(url+ "/cbcr/getCBCId")

  def messageRefIdExists(id:String)(implicit hc:HeaderCarrier) : Future[HttpResponse] =
    http.GET(url + s"/cbcr/message-ref-id/$id")

  def saveMessageRefId(id:String)(implicit hc:HeaderCarrier) : Future[HttpResponse] =
    http.PUT(url + s"/cbcr/message-ref-id/$id",JsNull)

  def docRefIdQuery(d:DocRefId)(implicit hc:HeaderCarrier) : Future[HttpResponse] =
    http.GET(url + s"/cbcr/doc-ref-id/${d.id}")

  def docRefIdSave(d:DocRefId)(implicit hc:HeaderCarrier) : Future[HttpResponse] =
    http.PUT(url + s"/cbcr/doc-ref-id/${d.id}",JsNull)

  def corrDocRefIdSave(c:CorrDocRefId, d:DocRefId)(implicit hc:HeaderCarrier) : Future[HttpResponse] =
    http.PUT(url + s"/cbcr/corr-doc-ref-id/${c.cid.id}/${d.id}",JsNull)



}
