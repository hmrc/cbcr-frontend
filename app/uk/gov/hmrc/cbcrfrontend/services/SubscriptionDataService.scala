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

import cats.data.EitherT
import play.api.http.Status
import uk.gov.hmrc.cbcrfrontend.WSHttp
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.typesclasses.{CbcrsUrl, ServiceUrl}
import uk.gov.hmrc.play.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.play.http.NotFoundException

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
import javax.inject.Singleton

import play.api.Logger
import uk.gov.hmrc.play.config.ServicesConfig
import cats.instances.future._
@Singleton
class SubscriptionDataService extends ServicesConfig{

  implicit lazy val url = new ServiceUrl[CbcrsUrl] { val url = baseUrl("cbcr")}

  def alreadySubscribed(utr:Utr)(implicit hc: HeaderCarrier, ec: ExecutionContext):ServiceResponse[Boolean] =
    retrieveSubscriptionData(Left(utr)).map(_.isDefined)

  def retrieveSubscriptionData(id:Either[Utr,CBCId])(implicit hc: HeaderCarrier, ec: ExecutionContext):ServiceResponse[Option[SubscriptionDetails]] = {
    val fullUrl = id.fold(
      utr => url.url + s"/cbcr/subscription-data/utr/${utr.utr}",
      id  => url.url + s"/cbcr/subscription-data/cbc-id/$id"
    )
    EitherT[Future,CBCErrors, Option[SubscriptionDetails]](
      WSHttp.GET[HttpResponse](fullUrl).map { response =>
        response.json.validate[SubscriptionDetails].fold(
          errors  => Left[CBCErrors,Option[SubscriptionDetails]](UnexpectedState(errors.mkString)),
          details => Right[CBCErrors,Option[SubscriptionDetails]](Some(details))
        )
      }.recover{
        case _:NotFoundException => Right[CBCErrors,Option[SubscriptionDetails]](None)
        case NonFatal(t) =>
          Logger.error("GET future failed", t)
          Left[CBCErrors,Option[SubscriptionDetails]](UnexpectedState(t.getMessage))
      }
    )

  }
  def saveSubscriptionData(data:SubscriptionDetails)(implicit hc: HeaderCarrier, ec: ExecutionContext): ServiceResponse[String] = {
    val fullUrl = url.url + s"/cbcr/subscription-data"
    EitherT[Future,CBCErrors, String](
      WSHttp.POST[SubscriptionDetails,HttpResponse](fullUrl,data).map { response =>
        response.status match {
          case Status.OK => Right[CBCErrors,String](response.body)
          case _         => Left[CBCErrors,String](UnexpectedState(response.body))
        }
      }.recover{
        case NonFatal(t) => Left[CBCErrors,String](UnexpectedState(t.getMessage))
      }
    )
  }

  def clearSubscriptionData(id: Either[Utr,CBCId])(implicit hc: HeaderCarrier, ec: ExecutionContext): ServiceResponse[Option[String]] = {

    val fullUrl = (cbcId:CBCId) => url.url + s"/cbcr/subscription-data/$cbcId"

    for {
      cbc    <- id.fold(
        utr => retrieveSubscriptionData(Left(utr)).map(_.map(_.cbcId)),
        id  => EitherT.pure[Future,CBCErrors,Option[CBCId]](Some(id))
      )
      result <- cbc.fold(EitherT.pure[Future,CBCErrors,Option[String]](None))(id =>
        EitherT[Future, CBCErrors, Option[String]](
          WSHttp.DELETE[HttpResponse](fullUrl(id)).map { response =>
            Right[CBCErrors, Option[String]](Some(response.body))
          }.recover {
            case _:NotFoundException => Right[CBCErrors, Option[String]](None)
            case NonFatal(t)         => Left[CBCErrors, Option[String]](UnexpectedState(t.getMessage))
          }
        )
      )
    } yield result

  }
}
