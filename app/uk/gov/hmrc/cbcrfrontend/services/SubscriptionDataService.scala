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
import cats.instances.future._
import play.api.Logger
import play.api.http.Status
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.typesclasses.{CbcrsUrl, ServiceUrl}
import uk.gov.hmrc.http.HttpReads.Implicits.readRaw
import uk.gov.hmrc.http.{HeaderCarrier, HttpClient, HttpResponse, UpstreamErrorResponse}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

@Singleton
class SubscriptionDataService @Inject() (http: HttpClient, servicesConfig: ServicesConfig)(implicit
  ec: ExecutionContext
) {

  private lazy val logger = Logger(this.getClass)

  private implicit lazy val url: ServiceUrl[CbcrsUrl] = new ServiceUrl[CbcrsUrl] {
    val url: String = servicesConfig.baseUrl("cbcr")
  }

  def retrieveSubscriptionData(
    id: Either[Utr, CBCId]
  )(implicit hc: HeaderCarrier): ServiceResponse[Option[SubscriptionDetails]] = {
    val fullUrl = id.fold(
      utr => s"${url.url}/cbcr/subscription-data/utr/${utr.utr}",
      id => s"${url.url}/cbcr/subscription-data/cbc-id/$id"
    )
    EitherT(
      http
        .GET[HttpResponse](fullUrl)
        .map { response =>
          response.status match {
            case Status.OK =>
              response.json
                .validate[SubscriptionDetails]
                .fold(
                  errors => Left[CBCErrors, Option[SubscriptionDetails]](UnexpectedState(errors.mkString)),
                  details => Right[CBCErrors, Option[SubscriptionDetails]](Some(details))
                )
            case Status.NOT_FOUND => Right(None)
            case s =>
              val message = s"Call to RetrieveSubscription failed - backend returned $s"
              logger.warn(message)
              Left(UnexpectedState(message))
          }
        }
        .recover { case NonFatal(t) =>
          logger.error("GET future failed", t)
          Left[CBCErrors, Option[SubscriptionDetails]](UnexpectedState(t.getMessage))
        }
    )
  }

  def updateSubscriptionData(cbcId: CBCId, data: SubscriberContact)(implicit
    hc: HeaderCarrier
  ): ServiceResponse[String] = {
    val fullUrl = s"${url.url}/cbcr/subscription-data/$cbcId"
    EitherT(
      http
        .PUT[SubscriberContact, HttpResponse](fullUrl, data)
        .map { response =>
          response.status match {
            case Status.OK => Right[CBCErrors, String](response.body)
            case _         => Left[CBCErrors, String](UnexpectedState(response.body))
          }
        }
        .recover { case NonFatal(t) =>
          Left[CBCErrors, String](UnexpectedState(t.getMessage))
        }
    )
  }

  def saveSubscriptionData(data: SubscriptionDetails)(implicit hc: HeaderCarrier): ServiceResponse[String] = {
    val fullUrl = s"${url.url}/cbcr/subscription-data"
    EitherT(
      http
        .POST[SubscriptionDetails, HttpResponse](fullUrl, data)
        .map { response =>
          response.status match {
            case Status.OK => Right[CBCErrors, String](response.body)
            case _         => Left[CBCErrors, String](UnexpectedState(response.body))
          }
        }
        .recover { case NonFatal(t) =>
          Left[CBCErrors, String](UnexpectedState(t.getMessage))
        }
    )
  }

  def clearSubscriptionData(id: Either[Utr, CBCId])(implicit hc: HeaderCarrier): ServiceResponse[Option[String]] = {

    val fullUrl = (cbcId: CBCId) => s"${url.url}/cbcr/subscription-data/$cbcId"

    for {
      cbc <- id.fold(
               utr => retrieveSubscriptionData(Left(utr)).map(_.flatMap(_.cbcId)),
               id => EitherT.rightT(Some(id))
             )
      result <- cbc.fold(EitherT.rightT[Future, CBCErrors](Option.empty[String])) { id =>
                  EitherT(
                    http
                      .DELETE[HttpResponse](fullUrl(id))
                      .map { response =>
                        Right(Some(response.body))
                      }
                      .recover {
                        case UpstreamErrorResponse.Upstream4xxResponse(_) => Right(None)
                        case NonFatal(t)                                  => Left(UnexpectedState(t.getMessage))
                      }
                  )
                }
    } yield result
  }
}
