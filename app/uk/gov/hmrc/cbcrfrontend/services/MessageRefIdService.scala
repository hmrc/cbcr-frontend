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

package uk.gov.hmrc.cbcrfrontend.services

import javax.inject.{Inject, Singleton}

import cats.data.OptionT
import uk.gov.hmrc.cbcrfrontend.connectors.CBCRBackendConnector
import uk.gov.hmrc.cbcrfrontend.exceptions.UnexpectedState
import uk.gov.hmrc.play.http.{HeaderCarrier, HttpException, NotFoundException}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

@Singleton
class MessageRefIdService @Inject() (connector:CBCRBackendConnector)(implicit ec:ExecutionContext) {

  def saveMessageRefId(m:String)(implicit hc:HeaderCarrier): OptionT[Future,UnexpectedState] =
    OptionT(connector.saveMessageRefId(m).map(_ => None).recover{
      case e:HttpException => Some(UnexpectedState(s"Response Code: ${e.responseCode}\n\n" + e.message))
      case NonFatal(e)     => Some(UnexpectedState(e.getMessage))
    })

  def messageRefIdExists(m:String)(implicit hc:HeaderCarrier) : Future[Boolean] =
    connector.messageRefIdExists(m).map(_ => true).recover{
      case _:NotFoundException => false
    }

}
