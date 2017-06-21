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
import uk.gov.hmrc.cbcrfrontend.core.ResponseExtract
import uk.gov.hmrc.cbcrfrontend.exceptions.UnexpectedState
import uk.gov.hmrc.cbcrfrontend.model.{EnvelopeId, FileMetadata, FileUploadCallbackResponse}
import uk.gov.hmrc.play.http.HttpResponse

import scala.xml.Source

class FileUploadServiceConnector() {

  val EnvelopeIdExtractor = "envelopes/([\\w\\d-]+)$".r.unanchored


  def envelopeRequest(cbcrsUrl: String, expiryDate: String): JsObject = {

    Logger.debug("CBCR URL: "+cbcrsUrl)

    Json.obj(
      "callbackUrl" -> s"$cbcrsUrl/cbcr/saveFileUploadResponse",
      "expiryDate" -> s"$expiryDate",
      "metadata" -> Json.obj(
        "application" -> "Country By Country Reporting Service"
      ),
      "constraints" -> 	Json.obj("maxSize"-> "50MB")
    )
  }

  def extractEnvelopId(resp: HttpResponse): ResponseExtract[EnvelopeId] = {
    resp.header(LOCATION) match {
      case Some(location) => location match {
        case EnvelopeIdExtractor(envelopeId) => Right(EnvelopeId(envelopeId))
        case _                               => Left(UnexpectedState(s"EnvelopeId in $LOCATION header: $location not found"))
      }
      case None => Left(UnexpectedState(s"Header $LOCATION not found"))
    }
  }

  def extractFileUploadMessage(resp: HttpResponse): ResponseExtract[String] = {
      resp.status match {
        case 200 => Right(resp.body)
        case _ => Left(UnexpectedState("Problems uploading the file"))
      }
  }

  def extractFileUploadResponseMessage(resp: HttpResponse): ResponseExtract[Option[FileUploadCallbackResponse]] = {
    resp.status match {
      case 200 => {
        val callbackResponse =  resp.json.validate[FileUploadCallbackResponse]
        callbackResponse match {
          case s: JsSuccess[FileUploadCallbackResponse] => Right(Some(s.get))
          case e: JsError => Left(UnexpectedState("Problems extracting File Upload response message "+e.errors))
        }
      }
      case 204 => Right(None)
      case _ => Left(UnexpectedState("Problems getting File Upload response message"))
    }
  }

  def extractFile(resp: HttpResponse): ResponseExtract[File] = {
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

  def extractEnvelopeDeleteMessage(resp: HttpResponse): ResponseExtract[String] = {
    resp.status match {
      case 200 => Right(resp.body)
      case _ => Left(UnexpectedState("Problems deleting the envelope"))
    }
  }

  def extractFileMetadata(resp: HttpResponse): ResponseExtract[Option[FileMetadata]] = {
    resp.status match {
      case 200 => {
        Logger.debug("FileMetaData: "+resp.json)
        Right(resp.json.asOpt[FileMetadata])
      }
      case _ => Left(UnexpectedState("Problems getting File Metadata"))
    }
  }
}
