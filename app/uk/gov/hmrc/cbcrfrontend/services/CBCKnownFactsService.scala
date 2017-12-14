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

import cats.data.EitherT
import cats.instances.future._
import play.api.http.Status
import uk.gov.hmrc.cbcrfrontend.connectors.GGConnector
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.model.{CBCErrors, CBCKnownFacts, UnexpectedState}
import uk.gov.hmrc.play.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal

@Singleton @deprecated("Use the EnrolmentsService instead","release/25.0")
class CBCKnownFactsService @Inject() (connector:GGConnector)(implicit ec:ExecutionContext) {

  private def httpResponseToEither(res:HttpResponse) : Either[CBCErrors,String] =
    Either.cond(res.status == Status.OK, res.body, UnexpectedState(res.body))

  private def addKnownFacts(kf:CBCKnownFacts)(implicit hc:HeaderCarrier) : ServiceResponse[String] =
    EitherT(connector.addKnownFacts(kf).map(httpResponseToEither).recover{
      case NonFatal(e) => Left(UnexpectedState(e.getMessage))
    })

  private def enrol(kf:CBCKnownFacts)(implicit hc:HeaderCarrier) : ServiceResponse[String] =
    EitherT(connector.enrolCBC(kf).map(httpResponseToEither).recover{
      case NonFatal(e) => Left(UnexpectedState(e.getMessage))
    })

  def addKnownFactsToGG(kf:CBCKnownFacts)(implicit hc:HeaderCarrier) : ServiceResponse[Unit] =
    for {
      _ <- addKnownFacts(kf)
      _ <- enrol(kf)
    } yield ()

}
