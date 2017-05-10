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
import uk.gov.hmrc.cbcrfrontend.exceptions.UnexpectedState
import uk.gov.hmrc.cbcrfrontend.model.{CBCId, SubscriberContact, SubscriptionDetails}
import uk.gov.hmrc.cbcrfrontend.typesclasses.{CbcrsUrl, ServiceUrl}
import uk.gov.hmrc.play.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
import javax.inject.Singleton

import uk.gov.hmrc.play.config.ServicesConfig
@Singleton
class SubscriptionDataService extends ServicesConfig{

  implicit lazy val url = new ServiceUrl[CbcrsUrl] { val url = baseUrl("cbcr")}

  def retrieveSubscriptionData(id:CBCId)(implicit hc: HeaderCarrier, ec: ExecutionContext):ServiceResponse[Option[SubscriptionDetails]] = {
    val fullUrl = url.url + s"/cbcr/retrieveSubscriptionData/$id"
    EitherT[Future,UnexpectedState, Option[SubscriptionDetails]](
      WSHttp.GET[HttpResponse](fullUrl).map { response =>
        response.status match {
          case Status.NOT_FOUND => Right[UnexpectedState,Option[SubscriptionDetails]](None)
          case Status.OK =>
            response.json.validate[SubscriptionDetails].fold(
              errors  => Left[UnexpectedState,Option[SubscriptionDetails]](UnexpectedState(errors.mkString)),
              details => Right[UnexpectedState,Option[SubscriptionDetails]](Some(details))
            )
          case _         => Left[UnexpectedState,Option[SubscriptionDetails]](UnexpectedState(response.body))
        }
      }.recover{
        case NonFatal(t) => Left[UnexpectedState,Option[SubscriptionDetails]](UnexpectedState(t.getMessage))
      }
    )

  }
  def saveSubscriptionData(data:SubscriptionDetails)(implicit hc: HeaderCarrier, ec: ExecutionContext): ServiceResponse[String] = {
    val fullUrl = url.url + s"/cbcr/saveSubscriptionData"
    EitherT[Future,UnexpectedState, String](
      WSHttp.POST[SubscriptionDetails,HttpResponse](fullUrl,data).map { response =>
        response.status match {
          case Status.OK => Right[UnexpectedState,String](response.body)
          case _         => Left[UnexpectedState,String](UnexpectedState(response.body))
        }
      }.recover{
        case NonFatal(t) => Left[UnexpectedState,String](UnexpectedState(t.getMessage))
      }
    )
  }

  def clearSubscriptionData(id: CBCId)(implicit hc: HeaderCarrier, ec: ExecutionContext): ServiceResponse[Option[String]] = {
    val fullUrl = url.url + s"/cbcr/clearSubscriptionData"
    EitherT[Future,UnexpectedState, Option[String]](
      WSHttp.POST[CBCId,HttpResponse](fullUrl,id).map { response =>
        response.status match {
          case Status.OK        => Right[UnexpectedState,Option[String]](Some(response.body))
          case Status.NOT_FOUND => Right[UnexpectedState,Option[String]](None)
          case _                => Left[UnexpectedState,Option[String]](UnexpectedState(response.body))
        }
      }.recover{
        case NonFatal(t) => Left[UnexpectedState,Option[String]](UnexpectedState(t.getMessage))
      }
    )

  }
}
