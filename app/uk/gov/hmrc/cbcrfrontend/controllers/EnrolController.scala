/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.cbcrfrontend.controllers

import com.typesafe.config.Config
import configs.syntax._
import javax.inject.{Inject, Singleton}
import play.api.libs.json.Json
import play.api.mvc.MessagesControllerComponents
import play.api.{Configuration, Environment}
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.cbcrfrontend.connectors.TaxEnrolmentsConnector
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class EnrolController @Inject()(
  val config: Configuration,
  val enrolConnector: TaxEnrolmentsConnector,
  val authConnector: AuthConnector,
  val env: Environment,
  messagesControllerComponents: MessagesControllerComponents)(implicit ec: ExecutionContext)
    extends FrontendController(messagesControllerComponents) with AuthorisedFunctions {

  implicit val format = uk.gov.hmrc.cbcrfrontend.controllers.enrolmentsFormat
  val conf = config.underlying.get[Config]("microservice.services.gg-proxy").value

  val url: String = (for {
    host     <- conf.get[String]("host")
    port     <- conf.get[Int]("port")
    service  <- conf.get[String]("url")
    protocol <- conf.get[String]("protocol")
  } yield s"$protocol://$host:$port/$service").value

  def deEnrol() = Action.async { implicit request =>
    authorised(AffinityGroup.Organisation and (User or Admin)) {
      enrolConnector.deEnrol.map(r => Ok(r.body))
    }
  }

  def getEnrolments = Action.async { implicit request =>
    authorised().retrieve(Retrievals.allEnrolments) { e =>
      Future.successful(Ok(Json.toJson(e)))
    }
  }

}
