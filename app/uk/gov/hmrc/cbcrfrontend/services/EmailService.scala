/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.cbcrfrontend.services

import javax.inject.{Inject, Singleton}

import cats.data.OptionT
import play.api.Logger
import play.api.http.Status
import uk.gov.hmrc.cbcrfrontend.connectors.CBCRBackendConnector
import uk.gov.hmrc.cbcrfrontend.model.Email

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
import uk.gov.hmrc.http.HeaderCarrier

@Singleton
class EmailService @Inject()(connector: CBCRBackendConnector)(implicit ec: ExecutionContext) {

  lazy val logger: Logger = Logger(this.getClass)

  def sendEmail(email: Email)(implicit hc: HeaderCarrier): OptionT[Future, Boolean] =
    OptionT(
      connector
        .sendEmail(email)
        .map { response =>
          response.status match {
            case Status.ACCEPTED => Some(true)
          }
        }
        .recover {
          case NonFatal(e) =>
            logger.error("The email has failed to send :( " + email + " exception " + e)
            Some(false)
        })
}
