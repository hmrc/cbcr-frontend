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
import play.api.http.Status
import uk.gov.hmrc.cbcrfrontend.connectors.CBCRBackendConnector
import uk.gov.hmrc.cbcrfrontend.model.DocRefIdResponses._
import uk.gov.hmrc.cbcrfrontend.model.{CorrDocRefId, DocRefId, UnexpectedState}
import uk.gov.hmrc.http.{HeaderCarrier, HttpException}

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

@Singleton
class DocRefIdService @Inject()(connector: CBCRBackendConnector)(implicit ec: ExecutionContext) {

  def saveDocRefId(d: DocRefId)(implicit hc: HeaderCarrier): OptionT[Future, UnexpectedState] =
    OptionT(connector.docRefIdSave(d).map(_ => None).recover {
      case e: HttpException => Some(UnexpectedState(s"Response Code: ${e.responseCode}\n\n" + e.message))
      case NonFatal(e)      => Some(UnexpectedState(e.getMessage))
    })

  def saveCorrDocRefID(c: CorrDocRefId, d: DocRefId)(implicit hc: HeaderCarrier): OptionT[Future, UnexpectedState] =
    OptionT(connector.corrDocRefIdSave(c, d).map(_ => None).recover {
      case e: HttpException => Some(UnexpectedState(s"Response Code: ${e.responseCode}\n\n" + e.message))
      case NonFatal(e)      => Some(UnexpectedState(e.getMessage))
    })

  def queryDocRefId(d: DocRefId)(implicit hc: HeaderCarrier): Future[DocRefIdQueryResponse] =
    connector
      .docRefIdQuery(d)
      .map { response =>
        response.status match {
          case Status.NOT_FOUND => DoesNotExist
          case Status.CONFLICT  => Invalid
          case Status.OK        => Valid
          case _                => Invalid
        }
      }
      .recover {
        case NonFatal(e) => Invalid
      }
}
