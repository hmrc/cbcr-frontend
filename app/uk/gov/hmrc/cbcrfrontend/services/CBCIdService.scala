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

import javax.inject.{Inject, Singleton}

import play.api.http.Status
import uk.gov.hmrc.cbcrfrontend.connectors.CBCRBackendConnector
import uk.gov.hmrc.cbcrfrontend.model.CBCId
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class CBCIdService @Inject()(connector:CBCRBackendConnector)(implicit ec:ExecutionContext){

  def getCbcId(implicit hc:HeaderCarrier) : Future[Option[CBCId]] =
    connector.getId().map{ response =>
      response.status match {
        case Status.OK => CBCId(response.body)
        case _         => None
      }
    }

}
