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
import uk.gov.hmrc.cbcrfrontend.model.SubscriptionData
import uk.gov.hmrc.cbcrfrontend.typesclasses.{CbcrsUrl, ServiceUrl}
import uk.gov.hmrc.play.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
import javax.inject.Singleton

import uk.gov.hmrc.play.config.ServicesConfig

trait SubscriptionDataService {
  implicit def url:ServiceUrl[CbcrsUrl]
  def saveSubscriptionData(data: SubscriptionData)(implicit hc: HeaderCarrier, ec: ExecutionContext): ServiceResponse[String]
}

@Singleton
class SubscriptionDataServiceImpl extends SubscriptionDataService with ServicesConfig{

  implicit lazy val url = new ServiceUrl[CbcrsUrl] { val url = baseUrl("cbcr")}

  def saveSubscriptionData(data:SubscriptionData)(implicit hc: HeaderCarrier, ec: ExecutionContext): ServiceResponse[String] = {
    val fullUrl = url.url + s"/cbcr/saveSubscriptionData"
    EitherT[Future,UnexpectedState, String](
      WSHttp.POST[SubscriptionData,HttpResponse](fullUrl,data).map { response =>
        response.status match {
          case Status.OK => Right[UnexpectedState,String](response.body)
          case _         => Left[UnexpectedState,String](UnexpectedState(response.body))
        }
      }.recover{
        case NonFatal(t) => Left[UnexpectedState,String](UnexpectedState(t.getMessage))
      }
    )
  }

}
