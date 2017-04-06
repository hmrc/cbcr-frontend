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

import java.util.UUID

import cats.data.EitherT
import cats.syntax.either._
import play.api.libs.json.Json
import uk.gov.hmrc.cbcrfrontend.connectors.ETMPConnector
import uk.gov.hmrc.cbcrfrontend.exceptions.InvalidState
import uk.gov.hmrc.cbcrfrontend.model.{FindBusinessData, FindBusinessDataResponse}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


class ETMPService(val connector: ETMPConnector) {

  /**
    * Lookup the provided UTR and return business information
    */
  def lookup(utr: String): EitherT[Future,InvalidState,FindBusinessDataResponse] =  {
    val query = FindBusinessData(UUID.randomUUID().toString,utr,false,false,None,None)
    EitherT(connector.lookup(query,utr).map{ response =>
      Json.parse(response.body).validate[FindBusinessDataResponse].asEither.leftMap(_ => InvalidState(response.body))
    })
  }

}
