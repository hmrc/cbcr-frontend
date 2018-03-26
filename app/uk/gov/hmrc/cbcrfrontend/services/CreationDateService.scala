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
import play.api.http.Status
import uk.gov.hmrc.cbcrfrontend.connectors.CBCRBackendConnector
import uk.gov.hmrc.cbcrfrontend.model.DocRefIdResponses.{DocRefIdQueryResponse, DoesNotExist, Invalid, Valid}
import uk.gov.hmrc.cbcrfrontend.model.{CorrDocRefId, DocRefId, MessageRefID, UnexpectedState}
import uk.gov.hmrc.play.http.{HeaderCarrier, HttpException, NotFoundException, Upstream4xxResponse}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

@Singleton
class CreationDateService @Inject()(connector:CBCRBackendConnector)(implicit ec:ExecutionContext) {

  def queryCorrDocRefId(c:CorrDocRefId)(implicit hc:HeaderCarrier) : Future[DocRefIdQueryResponse] =
    connector.docRefIdQuery(c.cid).map(_ => Valid).recover {
      case _: NotFoundException => DoesNotExist
      case e: Upstream4xxResponse if e.upstreamResponseCode == Status.CONFLICT => Invalid
    }

}
