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

import java.io.{BufferedInputStream, File, FileInputStream}
import java.time.LocalDateTime
import java.util.UUID
import javax.inject.{Inject, Singleton}

import cats.data.EitherT
import cats.implicits._
import play.Play
import play.api.Logger
import play.api.libs.json._
import uk.gov.hmrc.cbcrfrontend.WSHttp
import uk.gov.hmrc.cbcrfrontend.connectors.FileUploadServiceConnector
import uk.gov.hmrc.cbcrfrontend.core.{ServiceResponse, _}
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.typesclasses.{HttpExecutor, _}
import uk.gov.hmrc.play.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.ExecutionContext


@Singleton
class FileUploadService @Inject() (fusConnector: FileUploadServiceConnector) {

  def createEnvelope(implicit hc: HeaderCarrier, ec: ExecutionContext, fusUrl: ServiceUrl[FusUrl], cbcrsUrl: ServiceUrl[CbcrsUrl] ): ServiceResponse[EnvelopeId] = {
    Logger.debug("Country by Country: Creating an envelope for file upload")
    EitherT(HttpExecutor(fusUrl, CreateEnvelope(fusConnector.envelopeRequest(cbcrsUrl.url))).map(fusConnector.extractEnvelopId))
  }

  def uploadFile(xmlFile: java.io.File, envelopeId: String, fileId: String)(
                      implicit
                      hc: HeaderCarrier,
                      ec: ExecutionContext,
                      fusFeUrl: ServiceUrl[FusFeUrl]
                ): ServiceResponse[String] = {

    val fileNamePrefix = s"oecd-${LocalDateTime.now}"
    val bis = new BufferedInputStream(new FileInputStream(xmlFile))
    val xmlByteArray: Array[Byte] = Stream.continually(bis.read).takeWhile(-1 !=).map(_.toByte).toArray

    Logger.debug("Country by Country: FileUpload service: Uploading the file to the envelope")

    fromFutureOptA(HttpExecutor(fusFeUrl,
      UploadFile(EnvelopeId(envelopeId),
        FileId(fileId), s"$fileNamePrefix-cbcr.xml ", "application/xml;charset=UTF-8", xmlByteArray)).map(fusConnector.extractFileUploadMessage))

  }


  def getFileUploadResponse(envelopeId: String, fileId: String)(
                             implicit
                             hc: HeaderCarrier,
                             ec: ExecutionContext,
                             cbcrsUrl: ServiceUrl[CbcrsUrl]
                           ): ServiceResponse[Option[FileUploadCallbackResponse]] = {
    Logger.debug("Country by Country: Get file upload response")
   fromFutureOptA(WSHttp.GET[HttpResponse](s"${cbcrsUrl.url}/cbcr/retrieveFileUploadResponse/$envelopeId")
     .map(fusConnector.extractFileUploadResponseMessage))
  }

  def getFile(envelopeId: String, fileId: String)(
                   implicit
                   hc: HeaderCarrier,
                   ec: ExecutionContext,
                   fusUrl: ServiceUrl[FusUrl]
                   ): ServiceResponse[File] = {

      fromFutureOptA(WSHttp.GET[HttpResponse](s"${fusUrl.url}/file-upload/envelopes/$envelopeId/files/$fileId/content").map(fusConnector.extractFile))
  }


  def deleteEnvelope(envelopeId: String)(
    implicit
    hc: HeaderCarrier,
    ec: ExecutionContext,
    fusUrl: ServiceUrl[FusUrl]
  ): ServiceResponse[String] = {

    fromFutureOptA(WSHttp.DELETE[HttpResponse](s"${fusUrl.url}/file-upload/envelopes/$envelopeId").map(fusConnector.extractEnvelopeDeleteMessage))
  }


  def getFileMetaData(envelopeId: String, fileId: String)(
    implicit
    hc: HeaderCarrier,
    ec: ExecutionContext,
    fusUrl: ServiceUrl[FusUrl]
  ): ServiceResponse[Option[FileMetadata]] = {

    fromFutureOptA(WSHttp.GET[HttpResponse](s"${fusUrl.url}/file-upload/envelopes/$envelopeId/files/$fileId/metadata").map(fusConnector.extractFileMetadata))
  }


  def uploadMetadataAndRoute(metaData:SubmissionMetaData)( implicit hc: HeaderCarrier,
                                                           ec: ExecutionContext,
                                                           fusUrl: ServiceUrl[FusUrl],
                                                           fusFeUrl: ServiceUrl[FusFeUrl] ): ServiceResponse[String] = {

    val metadataFileId = UUID.randomUUID.toString
    val envelopeId = metaData.fileInfo.envelopeId

    for {
      _ <- EitherT.right(HttpExecutor(fusFeUrl, UploadFile(envelopeId,
        FileId(s"json-$metadataFileId"), "metadata.json ", " application/json; charset=UTF-8", Json.toJson(metaData).toString().getBytes)
      ))
      resourceUrl <- EitherT.right(HttpExecutor(fusUrl, RouteEnvelopeRequest(envelopeId, "cbcr", "OFDS")))
    } yield resourceUrl.body
  }

}