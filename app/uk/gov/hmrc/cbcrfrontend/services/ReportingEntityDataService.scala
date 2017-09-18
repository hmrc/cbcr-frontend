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
import uk.gov.hmrc.cbcrfrontend.connectors.CBCRBackendConnector
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.model.{DocRefId, ReportingEntityData, UnexpectedState}
import uk.gov.hmrc.play.http.{HeaderCarrier, NotFoundException}

import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal

@Singleton
class ReportingEntityDataService @Inject() (connector:CBCRBackendConnector)(implicit ec:ExecutionContext) {

  def saveReportingEntityData(data:ReportingEntityData)(implicit hc:HeaderCarrier) : ServiceResponse[Unit] =
    EitherT(connector.reportingEntityDataSave(data).map(_ => Right(())).recover{
      case NonFatal(t) => Left(UnexpectedState(s"Attempt to save reporting entity data failed: ${t.getMessage}"))
    })

  def queryReportingEntityData(d:DocRefId)(implicit hc:HeaderCarrier) : ServiceResponse[Option[ReportingEntityData]] =
    EitherT(
      connector.reportingEntityDataQuery(d).map { response =>
        response.json.validate[ReportingEntityData].fold(
          failed => Left(UnexpectedState(s"Unable to serialise response as ReportingEntityData: ${failed.mkString}")),
          data   => Right(Some(data))
        )
      }.recover{
        case _:NotFoundException => Right(None)
        case NonFatal(e)         => Left(UnexpectedState(s"Call to QueryReportingEntity failed: ${e.getMessage}"))
      }
    )


}