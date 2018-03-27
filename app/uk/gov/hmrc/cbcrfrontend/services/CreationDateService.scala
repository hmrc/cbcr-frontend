/*
 * Copyright 2018 HM Revenue & Customs
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

import cats.data.OptionT
import cats.syntax.show._
import javax.inject.{Inject, Singleton}
import play.api.Logger
import play.api.http.Status
import uk.gov.hmrc.cbcrfrontend.connectors.CBCRBackendConnector
import uk.gov.hmrc.cbcrfrontend.model.DocRefIdResponses.{DocRefIdQueryResponse, DoesNotExist, Invalid, Valid}
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.play.http.{HeaderCarrier, HttpException, NotFoundException, Upstream4xxResponse}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
import java.time.{LocalDate, LocalDateTime, Period}

import org.joda.time.Days

@Singleton
class CreationDateService @Inject()(connector:CBCRBackendConnector,
                                    reportingEntityDataService: ReportingEntityDataService)(implicit ec:ExecutionContext) {

  def checkDate(in:XMLInfo)(implicit hc:HeaderCarrier) : Future[Boolean] = {
  val id = in.cbcReport.find(_.docSpec.corrDocRefId.isDefined).flatMap(_.docSpec.corrDocRefId).orElse(in.additionalInfo.flatMap(_.docSpec.corrDocRefId))
    id.map {drid =>
    reportingEntityDataService.queryReportingEntityData(drid.cid).leftMap{
      cbcErrors => {
        Logger.error(s"Got error back: $cbcErrors")
        throw new Exception(s"Error communicating with backend: $cbcErrors")
      }
    }.subflatMap{
      case Some(red) => {
        val result:Boolean = ((new Period(red.creationDate.getOrElse(LocalDate.of(2017,2,1)), in.creationDate)).getYears < 4)
        Right(result)
      }
      case None      => Left(Future.successful(false))
    }.getOrElse(Future.successful(false))
  }
}


//  def checkDate(in:XMLInfo)(implicit hc:HeaderCarrier) : Future[Boolean] = {
//    val id = in.cbcReport.find(_.docSpec.corrDocRefId.isDefined).flatMap(_.docSpec.corrDocRefId).orElse(in.additionalInfo.flatMap(_.docSpec.corrDocRefId))
//    val d = id.map(b => b.cid).get
//    val result = for {
//      red <- reportingEntityDataService.queryReportingEntityData(d)
//    } yield red
//
//    result.leftMap {
//      cbcErrors => {
//        Logger.error(s"Got error back: $cbcErrors")
//        Future.successful(false)
//      }
//    }
//
//    val result2: LocalDate = for {
//      itsTrue <- result.isRight
//      rel <- itsTrue
//      bob <- result.collectRight
//      c = bob.get.creationDate.getOrElse(LocalDate.of(2016,2,1))
//      } yield c
//    val diff = result2.
//    }
//
//  }

}
