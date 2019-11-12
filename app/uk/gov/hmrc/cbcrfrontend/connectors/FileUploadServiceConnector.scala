/*
 * Copyright 2019 HM Revenue & Customs
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

import javax.inject.Singleton
import play.api.Logger
import play.api.http.HeaderNames.LOCATION
import play.api.libs.json._
import uk.gov.hmrc.cbcrfrontend.core.CBCErrorOr
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.http.HttpResponse

@Singleton
class FileUploadServiceConnector() {

  val EnvelopeIdExtractor = "envelopes/([\\w\\d-]+)$".r.unanchored

  def envelopeRequest(cbcrsUrl: String, expiryDate: Option[String]): JsObject = {

    //@todo refactor the hardcode of the /cbcr/file-upload-response
    val jsObject = Json
      .toJson(EnvelopeRequest(s"$cbcrsUrl/cbcr/file-upload-response", expiryDate, MetaData(), Constraints()))
      .as[JsObject]
    Logger.info(s"Envelope Request built as $jsObject")
    jsObject
  }

  def extractEnvelopId(resp: HttpResponse): CBCErrorOr[EnvelopeId] =
    resp.header(LOCATION) match {
      case Some(location) =>
        location match {
          case EnvelopeIdExtractor(envelopeId) => Right(EnvelopeId(envelopeId))
          case _                               => Left(UnexpectedState(s"EnvelopeId in $LOCATION header: $location not found"))
        }
      case None => Left(UnexpectedState(s"Header $LOCATION not found"))
    }

  def extractFileUploadMessage(resp: HttpResponse): CBCErrorOr[String] =
    resp.status match {
      case 200 => Right(resp.body)
      case _   => Left(UnexpectedState("Problems uploading the file"))
    }

  def extractEnvelopeDeleteMessage(resp: HttpResponse): CBCErrorOr[String] =
    resp.status match {
      case 200 => Right(resp.body)
      case _   => Left(UnexpectedState("Problems deleting the envelope"))
    }

  def extractFileMetadata(resp: HttpResponse): CBCErrorOr[Option[FileMetadata]] =
    resp.status match {
      case 200 => {
        Logger.debug("FileMetaData: " + resp.json)
        Right(resp.json.asOpt[FileMetadata])
      }
      case _ => Left(UnexpectedState("Problems getting File Metadata"))
    }
}
