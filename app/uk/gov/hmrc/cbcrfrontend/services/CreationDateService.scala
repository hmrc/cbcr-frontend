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

import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.http.HeaderCarrier

import java.time.{LocalDate, Period}
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

sealed abstract class XmlStatusEnum(val xmlStatus: String)

case object DateOld extends XmlStatusEnum("dateOld")

case object DateCorrect extends XmlStatusEnum("dateCorrect")

case object DateMissing extends XmlStatusEnum("dateMissing")

case object DateError extends XmlStatusEnum("dateError")

@Singleton
class CreationDateService @Inject() (config: FrontendAppConfig, reportingEntityDataService: ReportingEntityDataService)(
  implicit ec: ExecutionContext
) {

  def isDateValid(in: XMLInfo)(implicit hc: HeaderCarrier): Future[XmlStatusEnum] = {

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
            DateError
          case Right(Some(red)) =>
            val cd: LocalDate = red.creationDate.getOrElse(config.defaultCreationDate)
            val lcd: LocalDate = in.creationDate.getOrElse(LocalDate.now())
            if (Period.between(cd, lcd).getYears < 3) DateCorrect
            else DateOld
          case Right(None) => DateMissing
        }
    }.getOrElse {

      /** ********************************************** * If we reach this then the submitted file * is an addition and
        * NOT a correction * *
        */
      Future.successful(DateMissing)
    }
  }
}
