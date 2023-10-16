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

package uk.gov.hmrc.cbcrfrontend.connectors

import play.api.Logger
import play.api.http.HeaderNames.LOCATION
import play.api.http.Status
import play.api.libs.json._
import uk.gov.hmrc.cbcrfrontend.core.CBCErrorOr
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.http.HttpResponse

import javax.inject.Singleton

@Singleton
class FileUploadServiceConnector() {

  private val envelopeIdExtractor = "envelopes/([\\w\\d-]+)$".r.unanchored

  lazy val logger: Logger = Logger(this.getClass)

  def envelopeRequest(cbcrsUrl: String, expiryDate: String): JsObject = {

    //@todo refactor the hardcode of the /cbcr/file-upload-response
    val jsObject = Json
      .toJson(EnvelopeRequest(s"$cbcrsUrl/cbcr/file-upload-response", expiryDate, MetaData(), Constraints()))
      .as[JsObject]
    logger.info(s"Envelope Request built as $jsObject")
    jsObject
  }

  def extractEnvelopId(resp: HttpResponse): CBCErrorOr[EnvelopeId] =
    resp.header(LOCATION) match {
      case Some(location) =>
        location match {
          case envelopeIdExtractor(envelopeId) => Right(EnvelopeId(envelopeId))
          case _                               => Left(UnexpectedState(s"EnvelopeId in $LOCATION header: $location not found"))
        }
      case None => Left(UnexpectedState(s"Header $LOCATION not found"))
    }

  def extractFileMetadata(resp: HttpResponse): CBCErrorOr[Option[FileMetadata]] =
    resp.status match {
      case Status.OK =>
        logger.debug("FileMetaData: " + resp.json)
        Right(resp.json.asOpt[FileMetadata])
      case _ => Left(UnexpectedState("Problems getting File Metadata"))
    }
}
