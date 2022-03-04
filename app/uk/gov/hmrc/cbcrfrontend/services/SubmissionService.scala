/*
 * Copyright 2022 HM Revenue & Customs
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
import cats.instances.all._
import play.api.http.Status.OK
import uk.gov.hmrc.cbcrfrontend.connectors.CBCRBackendConnector
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.model.{CBCId, SubmissionMetaData, UnexpectedState}
import uk.gov.hmrc.cbcrfrontend.model.upscan.UploadedSuccessfully
import uk.gov.hmrc.cbcrfrontend.util.XmlLoadHelper
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.HttpReads.is2xx

import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.xml.{Elem, NodeSeq}

class SubmissionService @Inject()(connector: CBCRBackendConnector, xmlLoadHelper: XmlLoadHelper)(
  implicit ec: ExecutionContext,
  cache: CBCSessionCache) {

  def submit(cbcId: CBCId)(implicit hc: HeaderCarrier): ServiceResponse[Unit] =
    for {
      uploadedData <- cache.read[UploadedSuccessfully]
      response     <- submitDocument(uploadedData, cbcId)
    } yield response

  private def submitDocument(uploadedData: UploadedSuccessfully, cbcId: CBCId)(
    implicit hc: HeaderCarrier): ServiceResponse[Unit] = {
    val xml = constructSubmission(uploadedData.name, uploadedData.downloadUrl, cbcId)

    EitherT(
      connector
        .submitDocument(xml)
        .map {
          case response if is2xx(response.status) => Right(())
          case _                                  => Left(UnexpectedState(s"Failed to submit the xml document"))
        }
        .recover {
          case e: Exception => Left(UnexpectedState(s"Failed to submit the document: ${e.getMessage}"))
        })
  }

  private[services] def constructSubmission(fileName: String, downloadUrl: String, cbcId: CBCId): NodeSeq = {
    val uploadedXml: Elem = xmlLoadHelper.loadXML(downloadUrl)
    <submission>
      <fileName>{fileName}</fileName>
      <cbcId>XLCBC0100000056</cbcId>
      <file>{uploadedXml}</file>
    </submission>
  }
}
