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

package uk.gov.hmrc.cbcrfrontend.connectors

import java.io.{File, FileInputStream}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import play.api.Logger
import play.api.http.HeaderNames.LOCATION
import play.api.libs.json._
import uk.gov.hmrc.cbcrfrontend.core.CBCErrorOr
import uk.gov.hmrc.cbcrfrontend.exceptions.UnexpectedState
import uk.gov.hmrc.cbcrfrontend.model.{EnvelopeId, FileMetadata, FileUploadCallbackResponse}
import uk.gov.hmrc.play.http.HttpResponse

import scala.xml.Source

class FileUploadServiceConnector() {

  val EnvelopeIdExtractor = "envelopes/([\\w\\d-]+)$".r.unanchored
  val formatter = DateTimeFormatter.ofPattern("YYYY-MM-dd'T'HH:mm:ss'Z'")


  def envelopeRequest(formTypeRef: String, cbcrsUrl: String): JsObject = {

    Logger.debug("CBCR URL: "+cbcrsUrl)

    def envelopeExpiryDate(numberOfDays: Int) = LocalDateTime.now.plusDays(numberOfDays).format(formatter)

    Json.obj(
      "callbackUrl" -> s"$cbcrsUrl/cbcr/saveFileUploadResponse",
      "expiryDate" -> s"${envelopeExpiryDate(7)}",
      "metadata" -> Json.obj(
        "application" -> "Country By Country Reporting Service",
        "formTypeRef" -> s"$formTypeRef"
      )
    )
  }

  def extractEnvelopId(resp: HttpResponse): CBCErrorOr[EnvelopeId] = {
    resp.header(LOCATION) match {
      case Some(location) => location match {
        case EnvelopeIdExtractor(envelopeId) => Right(EnvelopeId(envelopeId))
        case _                               => Left(UnexpectedState(s"EnvelopeId in $LOCATION header: $location not found"))
      }
      case None => Left(UnexpectedState(s"Header $LOCATION not found"))
    }
  }

  def extractFileUploadMessage(resp: HttpResponse): CBCErrorOr[String] = {
      resp.status match {
        case 200 => Right(resp.body)
        case _ => Left(UnexpectedState("Problems uploading the file"))
      }
  }

  def extractFile(resp: HttpResponse): CBCErrorOr[File] = {
    resp.status match {
      case 200 =>

        val inputStream = Source.fromString(resp.body).getCharacterStream
        val xmlFile = File.createTempFile("xml", "xml")
        val fos = new java.io.FileOutputStream(xmlFile)

        fos.write(
          Stream.continually(inputStream.read).takeWhile(-1 !=).map(_.toByte).toArray
        )
        fos.close()
        Right(xmlFile)

      case _ => Left(UnexpectedState("Problems getting the File "))
    }
  }

  def extractEnvelopeDeleteMessage(resp: HttpResponse): CBCErrorOr[String] = {
    resp.status match {
      case 200 => Right(resp.body)
      case _ => Left(UnexpectedState("Problems deleting the envelope"))
    }
  }

  def extractFileMetadata(resp: HttpResponse): CBCErrorOr[Option[FileMetadata]] = {
    resp.status match {
      case 200 => {
        Logger.debug("FileMetaData: "+resp.json)
        Right(resp.json.asOpt[FileMetadata])
      }
      case _ => Left(UnexpectedState("Problems getting File Metadata"))
    }
  }
}
