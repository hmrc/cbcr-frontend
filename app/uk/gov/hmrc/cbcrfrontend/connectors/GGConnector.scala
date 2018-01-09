/*
 * Copyright 2018 HM Revenue & Customs
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

import javax.inject.{Inject, Singleton}

import com.typesafe.config.Config
import play.api.Configuration
import uk.gov.hmrc.cbcrfrontend.model.CBCKnownFacts
import uk.gov.hmrc.play.http.{HeaderCarrier, HttpPost, HttpResponse}
import configs.syntax._
import play.api.http.Status
import play.api.libs.json.Json

import scala.concurrent.Future

@Singleton @deprecated("Use the TaxEnrolmentsConnector and the EnrolmentsService","release/25.0")
class GGConnector @Inject() (http:HttpPost,config:Configuration){

  val conf = config.underlying.get[Config]("microservice.services.gg").value

  val ggAdminUrl: String = (for {
    proto   <- conf.get[String]("admin.protocol")
    host    <- conf.get[String]("admin.host")
    port    <- conf.get[Int]("admin.port")
    service <- conf.get[String]("admin.url")
  } yield s"$proto://$host:$port/$service").value

  val ggUrl: String = (for {
    proto   <- conf.get[String]("protocol")
    host    <- conf.get[String]("host")
    port    <- conf.get[Int]("port")
    service <- conf.get[String]("url")
  } yield s"$proto://$host:$port/$service").value

  val portalId: String    = conf.get[String]("enrol.portalId").value
  val serviceId: String   = conf.get[String]("enrol.serviceId").value
  val serviceName: String = conf.get[String]("enrol.serviceName").value

  private def createAddFactsBody(kf:CBCKnownFacts) = Json.obj(
    "facts" -> Json.arr(
      Json.obj(
        "type" -> "cbcId",
        "value" -> kf.cBCId.value
      ),
      Json.obj(
        "type" -> "UTR",
        "value" -> kf.utr.utr
      )
    )
  )

  private def createEnrolBody(kf:CBCKnownFacts) = Json.obj(
    "portalId" -> portalId,
    "serviceName" -> serviceId,
    "friendlyName" -> serviceName,
    "knownFacts" -> Json.arr(kf.cBCId.value,kf.utr.utr)
  )

  def addKnownFacts(kf:CBCKnownFacts)(implicit hc:HeaderCarrier): Future[HttpResponse] =
    http.POST(ggAdminUrl,createAddFactsBody(kf))

  def enrolCBC(kf:CBCKnownFacts)(implicit hc:HeaderCarrier) : Future[HttpResponse] =
    http.POST(ggUrl,createEnrolBody(kf))

}
