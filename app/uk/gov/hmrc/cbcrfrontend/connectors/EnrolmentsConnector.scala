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

import javax.inject.Inject

import com.typesafe.config.Config
import play.api.Configuration
import play.api.libs.json.JsValue
import uk.gov.hmrc.play.http.{HeaderCarrier, HttpGet}
import configs.syntax._
import uk.gov.hmrc.cbcrfrontend.model.Enrolment

import scala.concurrent.{ExecutionContext, Future}


class EnrolmentsConnector @Inject() (httpGet: HttpGet, config:Configuration)(implicit ec:ExecutionContext) {

  val conf = config.underlying.get[Config]("microservice.services.auth").value

  val url: String = (for {
    host  <- conf.get[String]("host")
    port  <- conf.get[Int]("port")
  } yield s"http://$host:$port").value

  def getEnrolments(implicit hc:HeaderCarrier): Future[List[Enrolment]] = for {
    authRecord    <- httpGet.GET[JsValue](url + "/auth/authority")
    enrolmentsUri <- Future{(authRecord \ "enrolments").get}
    enrolments    <- httpGet.GET[List[Enrolment]](url + enrolmentsUri.toString().replaceAll("\"",""))
  } yield enrolments




}
