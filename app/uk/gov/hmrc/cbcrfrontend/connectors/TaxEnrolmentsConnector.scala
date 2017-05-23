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

import javax.inject.{Inject, Singleton}

import com.typesafe.config.Config
import play.api.Configuration
import uk.gov.hmrc.play.http.{HeaderCarrier, HttpPost, HttpResponse}

import scala.concurrent.{ExecutionContext, Future}
import configs.syntax._
import play.api.libs.json.Json

/**
  * Created by max on 23/05/17.
  */
@Singleton
class TaxEnrolmentsConnector @Inject() (httpPost: HttpPost, config:Configuration)(implicit ec:ExecutionContext) {

  val conf = config.underlying.get[Config]("microservice.services.tax-enrolments").value

  val url: String = (for {
    host    <- conf.get[String]("host")
    port    <- conf.get[Int]("port")
    service <- conf.get[String]("url")
  } yield s"http://$host:$port/$service").value


  def deEnrol(implicit hc:HeaderCarrier): Future[HttpResponse] =
    httpPost.POST(url + "/de-enrol/HMRC-CBC-ORG",Json.obj("keepAgentAllocations" -> false))

}
