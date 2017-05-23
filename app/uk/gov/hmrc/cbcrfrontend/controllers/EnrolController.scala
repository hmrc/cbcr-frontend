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

package uk.gov.hmrc.cbcrfrontend.controllers

import javax.inject.{Inject, Singleton}

import com.typesafe.config.Config
import configs.syntax._
import play.api.Configuration
import play.api.libs.ws.WSClient
import uk.gov.hmrc.cbcrfrontend.auth.SecuredActions
import uk.gov.hmrc.cbcrfrontend.connectors.{AuthConnector, TaxEnrolmentsConnector}
import uk.gov.hmrc.cbcrfrontend.model.Organisation
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.util.control.NonFatal
/**
  * Created by max on 10/05/17.
  */
@Singleton
class EnrolController @Inject()(val sec: SecuredActions, val config:Configuration, ws:WSClient, auth:AuthConnector, enrolConnector: TaxEnrolmentsConnector) extends FrontendController {

  val conf = config.underlying.get[Config]("microservice.services.gg-proxy").value

  val url: String = (for {
    host    <- conf.get[String]("host")
    port    <- conf.get[Int]("port")
    service <- conf.get[String]("url")
  } yield s"http://$host:$port/$service").value



  def deEnrol() = sec.AsyncAuthenticatedAction(Some(Organisation)) { authContext => implicit request =>
    enrolConnector.deEnrol.map(r => Ok(r.body)).recover{
      case NonFatal(e) => InternalServerError(e.getMessage)
    }
  }

  def getEnrolments = sec.AsyncAuthenticatedAction(){ authContext => implicit request =>
    auth.getEnrolments.map(Ok(_))
  }

}
