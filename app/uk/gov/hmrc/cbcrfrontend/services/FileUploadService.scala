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

import java.io.{File, FileInputStream}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.UUID
import javax.inject.{Inject, Singleton}

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Sink
import akka.util.ByteString
import cats.data.EitherT
import cats.implicits._
import play.api.{Configuration, Logger}
import play.api.http.Status
import play.api.libs.json._
import play.api.libs.ws.WSClient
import uk.gov.hmrc.cbcrfrontend.WSHttp
import uk.gov.hmrc.cbcrfrontend.connectors.FileUploadServiceConnector
import uk.gov.hmrc.cbcrfrontend.core.{ServiceResponse, _}
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.typesclasses.{HttpExecutor, _}
import uk.gov.hmrc.play.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.{ExecutionContext, Future}


@Singleton
class FileUploadService @Inject() (fusConnector: FileUploadServiceConnector,ws:WSClient, configuration: Configuration)(implicit ac:ActorSystem) {

  implicit val materializer = ActorMaterializer()
  val envelopeExpiryDays = configuration.getInt("envelope-expire-days").getOrElse(throw new Exception("Missing configuration key: envelope-expire-days"))

  def createEnvelope(implicit hc: HeaderCarrier, ec: ExecutionContext, fusUrl: ServiceUrl[FusUrl], cbcrsUrl: ServiceUrl[CbcrsUrl] ): ServiceResponse[EnvelopeId] = {
    Logger.debug("Country by Country: Creating an envelope for file upload")
    val formatter = DateTimeFormatter.ofPattern("YYYY-MM-dd'T'HH:mm:ss'Z'")
    def envelopeExpiryDate(numberOfDays: Int) = LocalDateTime.now.plusDays(numberOfDays).format(formatter)

    EitherT(HttpExecutor(fusUrl, CreateEnvelope(fusConnector.envelopeRequest(cbcrsUrl.url, envelopeExpiryDate(envelopeExpiryDays)))).map(fusConnector.extractEnvelopId))
  }

  def uploadFile(xmlFile: java.io.File, envelopeId: String, fileId: String)(
                      implicit
                      hc: HeaderCarrier,
                      ec: ExecutionContext,
                      fusFeUrl: ServiceUrl[FusFeUrl]
                ): ServiceResponse[String] = {

    val fileNamePrefix = s"oecd-${LocalDateTime.now}"
    val xmlByteArray: Array[Byte] = org.apache.commons.io.IOUtils.toByteArray(new FileInputStream(xmlFile))

    Logger.debug("Country by Country: FileUpload service: Uploading the file to the envelope")
    fromFutureOptA(HttpExecutor(fusFeUrl,
      UploadFile(EnvelopeId(envelopeId),
        FileId(fileId), s"$fileNamePrefix-cbcr.xml ", "application/xml;charset=UTF-8", xmlByteArray)).map(fusConnector.extractFileUploadMessage))

  }


  def getFileUploadResponse(envelopeId: String, fileId: String)( implicit hc: HeaderCarrier, ec: ExecutionContext, cbcrsUrl: ServiceUrl[CbcrsUrl] ): ServiceResponse[Option[FileUploadCallbackResponse]] =
    EitherT(
      WSHttp.GET[HttpResponse](s"${cbcrsUrl.url}/cbcr/file-upload-response/$envelopeId")
        .map(resp => resp.status match {
          case 200 => resp.json.validate[FileUploadCallbackResponse].fold(
            invalid   => Left(UnexpectedState("Problems extracting File Upload response message "+invalid)),
            response  => Right(Some(response))
          )
          case 204 => Right(None)
          case _   => Left(UnexpectedState("Problems getting File Upload response message"))
        })
    )


  def getFile(envelopeId: String, fileId: String)(implicit hc: HeaderCarrier,
                                                  ec: ExecutionContext,
                                                  fusUrl: ServiceUrl[FusUrl]): ServiceResponse[File] =
    EitherT(
      ws.url(s"${fusUrl.url}/file-upload/envelopes/$envelopeId/files/$fileId/content").withMethod("GET") .stream().flatMap{
        res => res.headers.status match {
          case Status.OK =>
            val file = java.nio.file.Files.createTempFile(envelopeId,"xml")
            val outputStream = java.nio.file.Files.newOutputStream(file)

            val sink = Sink.foreach[ByteString] { bytes => outputStream.write(bytes.toArray) }

            res.body.runWith(sink).andThen {
              case result =>
                outputStream.close()
                result.get
            }.map(_ => Right(file.toFile))
          case otherStatus => Future.successful(Left(
            UnexpectedState(s"Failed to retrieve file $fileId from envelope $envelopeId - received $otherStatus response")
          ))
        }
      }
    )


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