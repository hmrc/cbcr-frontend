/*
 * Copyright 2021 HM Revenue & Customs
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

import java.time.LocalDate
import javax.inject.{Inject, Singleton}
import cats.data.EitherT
import play.api.Logger
import uk.gov.hmrc.cbcrfrontend.connectors.CBCRBackendConnector
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.model._

import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal
import uk.gov.hmrc.http.HeaderCarrier

@Singleton
class ReportingEntityDataService @Inject()(connector: CBCRBackendConnector)(implicit ec: ExecutionContext) {

  lazy val logger: Logger = Logger(this.getClass)

  def updateReportingEntityData(data: PartialReportingEntityData)(implicit hc: HeaderCarrier): ServiceResponse[Unit] =
    EitherT(connector.reportingEntityDataUpdate(data).map(_ => Right(())).recover {
      case NonFatal(t) => Left(UnexpectedState(s"Attempt to update reporting entity data failed: ${t.getMessage}"))
    })

  def saveReportingEntityData(data: ReportingEntityData)(implicit hc: HeaderCarrier): ServiceResponse[Unit] =
    EitherT(connector.reportingEntityDataSave(data).map(_ => Right(())).recover {
      case NonFatal(t) => Left(UnexpectedState(s"Attempt to save reporting entity data failed: ${t.getMessage}"))
    })

  def queryReportingEntityData(d: DocRefId)(implicit hc: HeaderCarrier): ServiceResponse[Option[ReportingEntityData]] =
    EitherT(
      connector
        .reportingEntityDataQuery(d)
        .map { response =>
          response.status match {
            case 200 =>
              response.json
                .validate[ReportingEntityData]
                .fold(
                  failed =>
                    Left(UnexpectedState(s"Unable to serialise response as ReportingEntityData: ${failed.mkString}")),
                  data => Right(Some(data))
                )
            case 404 =>
              logger.error("Got a NotFoundException - backend returned 404")
              Right(None)
          }
        }
        .recover {
          case NonFatal(e) => Left(UnexpectedState(s"Call to QueryReportingEntity failed: ${e.getMessage}"))
        }
    )

  def queryReportingEntityDataModel(d: DocRefId)(
    implicit hc: HeaderCarrier): ServiceResponse[Option[ReportingEntityDataModel]] =
    EitherT(
      connector
        .reportingEntityDataModelQuery(d)
        .map { response =>
          response.status match {
            case 200 =>
              response.json
                .validate[ReportingEntityDataModel]
                .fold(
                  failed =>
                    Left(UnexpectedState(s"Unable to serialise response as ReportingEntityData: ${failed.mkString}")),
                  data => Right(Some(data))
                )
            case 404 =>
              logger.error("Got a NotFoundException - backend returned 404")
              Right(None)
          }
        }
        .recover {
          case NonFatal(e) => Left(UnexpectedState(s"Call to QueryReportingEntity failed: ${e.getMessage}"))
        }
    )

  def queryReportingEntityDataByCbcId(cbcId: CBCId, reportingPeriod: LocalDate)(
    implicit hc: HeaderCarrier): ServiceResponse[Option[ReportingEntityData]] =
    EitherT(
      connector
        .reportingEntityCBCIdAndReportingPeriod(cbcId, reportingPeriod)
        .map { response =>
          response.status match {
            case 200 =>
              response.json
                .validate[ReportingEntityData]
                .fold(
                  failed =>
                    Left(UnexpectedState(s"Unable to serialise response as ReportingEntityData: ${failed.mkString}")),
                  data => Right(Some(data))
                )
            case 404 => Right(None)
          }

        }
        .recover {
          case NonFatal(e) => Left(UnexpectedState(s"Call to QueryReportingEntity failed: ${e.getMessage}"))
        })

  def queryReportingEntityDataDocRefId(d: DocRefId)(
    implicit hc: HeaderCarrier): ServiceResponse[Option[ReportingEntityData]] =
    EitherT(
      connector
        .reportingEntityDocRefId(d)
        .map { response =>
          response.status match {
            case 200 =>
              response.json
                .validate[ReportingEntityData]
                .fold(
                  failed =>
                    Left(UnexpectedState(s"Unable to serialise response as ReportingEntityData: ${failed.mkString}")),
                  data => Right(Some(data))
                )
            case 404 => Right(None)
          }
        }
        .recover {
          case NonFatal(e) => Left(UnexpectedState(s"Call to QueryReportingEntity failed: ${e.getMessage}"))
        })

  def queryReportingEntityDataTin(tin: String, reportingPeriod: String)(
    implicit hc: HeaderCarrier): ServiceResponse[Option[ReportingEntityData]] =
    EitherT(
      connector
        .reportingEntityDataQueryTin(tin, reportingPeriod)
        .map { response =>
          response.status match {
            case 200 =>
              response.json
                .validate[ReportingEntityData]
                .fold(
                  failed =>
                    Left(UnexpectedState(s"Unable to serialise response as ReportingEntityData: ${failed.mkString}")),
                  data => Right(Some(data)))
            case 404 => Right(None)
          }
        }
        .recover {
          case NonFatal(e) => Left(UnexpectedState(s"Call to QueryReportingEntity failed: ${e.getMessage}"))
        })

  def queryReportingEntityDatesOverlaping(tin: String, entityReportingPeriod: EntityReportingPeriod)(
    implicit hc: HeaderCarrier): ServiceResponse[Option[DatesOverlap]] =
    EitherT(
      connector
        .overlapQuery(tin, entityReportingPeriod)
        .map(
          response =>
            response.status match {
              case 200 =>
                response.json
                  .validate[DatesOverlap]
                  .fold(
                    failed =>
                      Left(UnexpectedState(s"Unable to serialise response as DatesOverlap: ${failed.mkString}")),
                    data => Right(Some(data)))
              case 404 => Right(None)
          }
        )
        .recover {
          case NonFatal(e) => Left(UnexpectedState(s"Call to QueryDatesOverlap failed: ${e.getMessage}"))
        })

}
