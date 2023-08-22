/*
 * Copyright 2023 HM Revenue & Customs
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

import cats.instances.all._
import play.api.Configuration
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.http.HeaderCarrier

import java.time.{LocalDate, Period}
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

object xmlStatusEnum extends Enumeration {
  type xmlStatus = Value
  val dateOld, dateCorrect, dateMissing, dateError = Value
}

@Singleton
class CreationDateService @Inject()(
  configuration: Configuration,
  runMode: RunMode,
  reportingEntityDataService: ReportingEntityDataService)(implicit ec: ExecutionContext) {

  val env: String = runMode.env

  private val creationDay = configuration
    .getOptional[Int](s"$env.default-creation-date.day")
    .getOrElse(
      throw new Exception(s"Missing configuration key: $env.default-creation-date.day")
    )

  private val creationMonth = configuration
    .getOptional[Int](s"$env.default-creation-date.month")
    .getOrElse(
      throw new Exception(s"Missing configuration key: $env.default-creation-date.month")
    )

  private val creationYear = configuration
    .getOptional[Int](s"$env.default-creation-date.year")
    .getOrElse(
      throw new Exception(s"Missing configuration key: $env.default-creation-date.year")
    )

  def isDateValid(in: XMLInfo)(implicit hc: HeaderCarrier): Future[xmlStatusEnum.xmlStatus] = {

    val id = in.cbcReport
      .find(_.docSpec.corrDocRefId.isDefined)
      .flatMap(_.docSpec.corrDocRefId)
      .orElse(in.additionalInfo.headOption.flatMap(_.docSpec.corrDocRefId))
      .orElse(in.reportingEntity.flatMap(_.docSpec.corrDocRefId))
    id.map { drid =>
        reportingEntityDataService
          .queryReportingEntityData(drid.cid)
          .value
          .map {
            case Left(cbcErrors) =>
              UnexpectedState(s"Error communicating with backend: $cbcErrors")
              xmlStatusEnum.dateError
            case Right(Some(red)) =>
              val cd: LocalDate = red.creationDate.getOrElse(LocalDate.of(creationYear, creationMonth, creationDay))
              val lcd: LocalDate = in.creationDate.getOrElse(LocalDate.now())
              if (Period.between(cd, lcd).getYears < 3) xmlStatusEnum.dateCorrect
              else xmlStatusEnum.dateOld
            case Right(None) => xmlStatusEnum.dateMissing
          }
      }
      .getOrElse {

        /************************************************
          *                                               *
          *    If we reach this then the submitted file   *
          *    is an addition and NOT a correction        *
          *                                               *
      ************************************************/
        Future.successful(xmlStatusEnum.dateMissing)
      }
  }
}
