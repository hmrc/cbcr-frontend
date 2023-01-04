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

import javax.inject.{Inject, Singleton}
import com.typesafe.config.Config
import play.api.Configuration

import scala.concurrent.{ExecutionContext, Future}
import configs.syntax._
import play.api.libs.json.{JsArray, JsObject, Json}
import uk.gov.hmrc.cbcrfrontend.model.{CBCId, Utr}
import uk.gov.hmrc.http._
import uk.gov.hmrc.http.HttpReads.Implicits.readRaw

/**
  * Created by max on 23/05/17.
  */
@Singleton
class TaxEnrolmentsConnector @Inject()(http: HttpClient, config: Configuration)(implicit ec: ExecutionContext) {

  val conf = config.underlying.get[Config]("microservice.services.tax-enrolments").value

  val url: String = (for {
    host     <- conf.get[String]("host")
    port     <- conf.get[Int]("port")
    service  <- conf.get[String]("url")
    protocol <- conf.get[String]("protocol")
  } yield s"$protocol://$host:$port/$service").value

  def deEnrol(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http
      .POST[JsObject, HttpResponse](url + "/de-enrol/HMRC-CBC-ORG", Json.obj("keepAgentAllocations" -> false))

  def enrol(cBCId: CBCId, utr: Utr)(implicit hc: HeaderCarrier): Future[HttpResponse] =
    http
      .PUT(
        url + "/service/HMRC-CBC-ORG/enrolment",
        Json.obj(
          "identifiers" -> JsArray(
            List(
              Json.obj("key" -> "cbcId", "value" -> cBCId.value),
              Json.obj("key" -> "UTR", "value"   -> utr.value)
            )),
          "verifiers" -> JsArray()
        )
      )
      .map { response =>
        response
      }

}
