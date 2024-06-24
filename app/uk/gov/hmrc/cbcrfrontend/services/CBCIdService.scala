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

import cats.data.OptionT
import cats.instances.future._
import play.api.Logger
import play.api.http.Status
import play.api.libs.json.Json
import uk.gov.hmrc.cbcrfrontend.connectors.CBCRBackendConnector
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.http.HeaderCarrier

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

@Singleton
class CBCIdService @Inject() (connector: CBCRBackendConnector)(implicit ec: ExecutionContext) {

  lazy val logger: Logger = Logger(this.getClass)

  def subscribe(s: SubscriptionDetails)(implicit hc: HeaderCarrier): OptionT[Future, CBCId] =
    OptionT(
      connector
        .subscribe(s)
        .map { response =>
          response.status match {
            case Status.OK => CBCId((response.json \ "cbc-id").as[String])
            case _         => None
          }
        }
        .recover { case NonFatal(t) =>
          logger.error("Failed to call subscribe", t)
          None
        }
    )

  def getETMPSubscriptionData(safeId: String)(implicit hc: HeaderCarrier): OptionT[Future, ETMPSubscription] =
    OptionT(
      connector
        .getETMPSubscriptionData(safeId)
        .map { response =>
          response.status match {
            case Status.OK =>
              Option(response.body)
                .filter(_.nonEmpty)
                .map(Json.parse)
                .flatMap(_.validate[ETMPSubscription].asOpt)
            case _ =>
              None
          }
        }
    )

  def updateETMPSubscriptionData(safeId: String, correspondenceDetails: CorrespondenceDetails)(implicit
    hc: HeaderCarrier
  ): ServiceResponse[UpdateResponse] =
    OptionT(connector.updateETMPSubscriptionData(safeId, correspondenceDetails).map { response =>
      Option(response.json).flatMap(_.validate[UpdateResponse].asOpt)
    }).toRight(UnexpectedState("Failed to update ETMP subscription data"): CBCErrors)
}
