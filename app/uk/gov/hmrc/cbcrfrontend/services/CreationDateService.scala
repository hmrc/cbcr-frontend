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



import javax.inject.{Inject, Singleton}
import uk.gov.hmrc.cbcrfrontend.connectors.CBCRBackendConnector
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.http.HeaderCarrier
import scala.concurrent.{ExecutionContext, Future}
import java.time.{LocalDate, Period}
import cats.instances.all._
import scala.concurrent.ExecutionContext.Implicits.global



@Singleton
class CreationDateService @Inject()(connector:CBCRBackendConnector,
                                    reportingEntityDataService: ReportingEntityDataService)(implicit ec:ExecutionContext) {

  def checkDate(in:XMLInfo)(implicit hc:HeaderCarrier) : Future[Boolean] = {
    val id = in.cbcReport.find(_.docSpec.corrDocRefId.isDefined).flatMap(_.docSpec.corrDocRefId).orElse(in.additionalInfo.flatMap(_.docSpec.corrDocRefId))
    id.map { drid =>
      reportingEntityDataService.queryReportingEntityData(drid.cid).leftMap {
        {
            cbcErrors => UnexpectedState(s"Error communicating with backend: $cbcErrors")
            false
        }
      }.subflatMap{
          case Some(red) => {
            val cd: LocalDate = red.creationDate.getOrElse(LocalDate.of(2017, 2, 1))
            val lcd: LocalDate = in.creationDate.getOrElse(LocalDate.now())
            val result:Boolean = Period.between(cd, lcd).getYears < 4
            Right(result)
          }
          case None      => Left(false)
        }.merge
    }.getOrElse{Future.successful(false)}
  }
}
