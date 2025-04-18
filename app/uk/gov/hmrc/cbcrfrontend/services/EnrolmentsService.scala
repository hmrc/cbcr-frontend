/*
 * Copyright 2024 HM Revenue & Customs
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

import cats.data.EitherT
import uk.gov.hmrc.cbcrfrontend.connectors.TaxEnrolmentsConnector
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.model.{CBCKnownFacts, UnexpectedState}
import uk.gov.hmrc.http.HeaderCarrier

import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal

@Singleton
class EnrolmentsService @Inject() (tec: TaxEnrolmentsConnector)(implicit ec: ExecutionContext) {

  def enrol(cbcKnownFacts: CBCKnownFacts)(implicit hc: HeaderCarrier): ServiceResponse[Unit] =
    EitherT(tec.enrol(cbcKnownFacts.cBCId, cbcKnownFacts.utr).map(_ => Right(())).recover { case NonFatal(t) =>
      Left(UnexpectedState(s"Failed to call enrol: ${t.getMessage}"))
    })
}
