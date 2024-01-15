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

import akka.actor.ActorSystem
import akka.stream.scaladsl.Sink
import akka.util.ByteString
import cats.data.EitherT
import cats.implicits._
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import play.api.Logging
import play.api.http.HeaderNames.LOCATION
import play.api.http.Status
import play.api.i18n.Messages
import play.api.libs.Files.SingletonTemporaryFileCreator
import play.api.libs.json._
import play.api.libs.ws.WSClient
import uk.gov.hmrc.cbcrfrontend.config.{FileUploadFrontEndWS, FrontendAppConfig}
import uk.gov.hmrc.cbcrfrontend.core._
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.typesclasses._
import uk.gov.hmrc.http.HttpReads.Implicits._
import uk.gov.hmrc.http._
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import java.io.{File, PrintWriter}
import java.util.UUID
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class FileUploadService @Inject()(ws: WSClient, configuration: FrontendAppConfig, servicesConfig: ServicesConfig)(
  implicit http: HttpClient,
  ac: ActorSystem,
  fileUploadFrontEndWS: FileUploadFrontEndWS,
  ec: ExecutionContext)
    extends Logging {

  private lazy val fusUrl = new ServiceUrl[FusUrl] {
    val url: String = servicesConfig.baseUrl("file-upload")
  }

  private lazy val fusFeUrl = new ServiceUrl[FusFeUrl] {
    val url: String = servicesConfig.baseUrl("file-upload-frontend")
  }

  private lazy val cbcrsUrl = servicesConfig.baseUrl("cbcr")

  private val envelopeIdExtractor = "envelopes/([\\w\\d-]+)$".r.unanchored

  def createEnvelope(implicit hc: HeaderCarrier): ServiceResponse[EnvelopeId] = {
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

    val envelopeExpiryDate = {
      val formatter = DateTimeFormat.forPattern("YYYY-MM-dd'T'HH:mm:ss'Z'")
      formatter.print(new DateTime().plusDays(configuration.envelopeExpiryDays))
    }

    EitherT(
      HttpExecutor(fusUrl, CreateEnvelope(envelopeRequest(cbcrsUrl, envelopeExpiryDate)))
        .map(extractEnvelopId))
  }

  def getFileUploadResponse(envelopeId: String)(
    implicit hc: HeaderCarrier): ServiceResponse[Option[FileUploadCallbackResponse]] =
    EitherT(
      http
        .GET[HttpResponse](s"$cbcrsUrl/cbcr/file-upload-response/$envelopeId")
        .map(resp =>
          resp.status match {
            case Status.OK =>
              resp.json
                .validate[FileUploadCallbackResponse]
                .fold(
                  invalid => Left(UnexpectedState("Problems extracting File Upload response message " + invalid)),
                  response => Right(Some(response))
                )
            case 204 => Right(None)
            case _   => Left(UnexpectedState("Problems getting File Upload response message"))
        })
    )

  def getFile(envelopeId: String, fileId: String): ServiceResponse[File] =
    EitherT(
      ws.url(s"${fusUrl.url}/file-upload/envelopes/$envelopeId/files/$fileId/content")
        .withMethod("GET")
        .stream()
        .flatMap { res =>
          res.status match {
            case Status.OK =>
              val file = java.nio.file.Files.createTempFile(envelopeId, "xml")
              val outputStream = java.nio.file.Files.newOutputStream(file)

              val sink = Sink.foreach[ByteString] { bytes =>
                outputStream.write(bytes.toArray)
              }

              res.bodyAsSource
                .runWith(sink)
                .andThen {
                  case result =>
                    outputStream.close()
                    result.get
                }
                .map(_ => Right(file.toFile))
            case otherStatus =>
              Future.successful(
                Left(
                  UnexpectedState(
                    s"Failed to retrieve file $fileId from envelope $envelopeId - received $otherStatus response")
                ))
          }
        }
    )

  def getFileMetaData(envelopeId: String, fileId: String)(
    implicit hc: HeaderCarrier): ServiceResponse[Option[FileMetadata]] = {
    def extractFileMetadata(resp: HttpResponse): CBCErrorOr[Option[FileMetadata]] =
      resp.status match {
        case Status.OK =>
          logger.debug("FileMetaData: " + resp.json)
          Right(resp.json.asOpt[FileMetadata])
        case _ => Left(UnexpectedState("Problems getting File Metadata"))
      }

    fromFutureOptA(
      http
        .GET[HttpResponse](s"${fusUrl.url}/file-upload/envelopes/$envelopeId/files/$fileId/metadata")
        .map(extractFileMetadata))
  }

  def uploadMetadataAndRoute(metaData: SubmissionMetaData)(implicit hc: HeaderCarrier): ServiceResponse[String] = {
    val metadataFileId = UUID.randomUUID.toString
    val envelopeId = metaData.fileInfo.envelopeId

    EitherT(for {
      _ <- HttpExecutor(
            fusFeUrl,
            UploadFile(
              envelopeId,
              FileId(s"json-$metadataFileId"),
              "metadata.json",
              "application/json; charset=UTF-8",
              Json.toJson(metaData).toString().getBytes)
          )
      response <- HttpExecutor(fusUrl, RouteEnvelopeRequest(envelopeId, "cbcr", "OFDS"))
    } yield
      response match {
        case HttpResponse(201, body, _) => Right(body)
        case error =>
          Left(
            UnexpectedState(
              s"[FileUploadService][uploadMetadataAndRoute] Failed to create route request, received ${error.status}"))
      })
  }

  private def errorsToList(e: List[ValidationErrors])(implicit messages: Messages): List[String] =
    e.map(_.show.split(" ").map(messages(_)).mkString(" "))

  def errorsToMap(e: List[ValidationErrors])(implicit messages: Messages): Map[String, String] =
    errorsToList(e).foldLeft(Map[String, String]()) { (m, t) =>
      m + ("error_" + (m.size + 1).toString -> t)
    }

  def errorsToString(e: List[ValidationErrors])(implicit messages: Messages): String =
    errorsToList(e).mkString("\r\n")

  def errorsToFile(e: List[ValidationErrors], name: String)(implicit messages: Messages): File = {
    val b = SingletonTemporaryFileCreator.create(name, ".txt")
    val writer = new PrintWriter(b.path.toFile)
    writer.write(errorsToString(e))
    writer.flush()
    writer.close()
    b.path.toFile
  }
}
