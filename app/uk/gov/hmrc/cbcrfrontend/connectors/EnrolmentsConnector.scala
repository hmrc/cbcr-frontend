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

import cats.data.{EitherT, OptionT}
import cats.syntax.all._
import cats.instances.all._
import com.typesafe.config.Config
import play.api.{Configuration, Logger}
import play.api.libs.json.JsValue
import uk.gov.hmrc.play.http.{HeaderCarrier, HttpGet}
import configs.syntax._
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.model._

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

  def alreadyEnrolled(implicit hc:HeaderCarrier): Future[Boolean] =
    getEnrolments.map(_.exists(_.key == "HMRC-CBC-ORG"))

  def getUtr(implicit hc: HeaderCarrier): OptionT[Future,Utr] =
    OptionT(
      getEnrolments.map{ enrolments =>
        for {
          value <- enrolments.find(_.key == "HMRC-CBC-ORG")
          id    <- value.identifiers.find(_.key == "UTR")
        } yield Utr(id.value)
      }
      )

  def getCbcId(implicit hc:HeaderCarrier): OptionT[Future,CBCId] =
    OptionT(
      getEnrolments.map{ enrolments =>
        for {
          value      <- enrolments.find(_.key == "HMRC-CBC-ORG")
          identifier <- value.identifiers.find(_.key == "cbcId")
          cbcId      <- CBCId(identifier.value)
        } yield cbcId
      }
    )

  def getCBCEnrolment(implicit hc:HeaderCarrier): OptionT[Future,CBCEnrolment] =
    OptionT(
      getEnrolments.map{ enrolments =>
        for {
          value      <- enrolments.find(_.key == "HMRC-CBC-ORG")
          id         <- value.identifiers.find(_.key == "cbcId")
          utr        <- value.identifiers.find(_.key == "UTR")
          cbcId      <- CBCId(id.value)
        } yield CBCEnrolment(cbcId,Utr(utr.value))
      }
    )

}
