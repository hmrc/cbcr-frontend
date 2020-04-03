/*
 * Copyright 2020 HM Revenue & Customs
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

import java.io.{File, FileInputStream, PrintWriter}
import java.time.LocalDateTime
import java.util.UUID

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Sink
import akka.util.ByteString
import cats.data.EitherT
import cats.implicits._
import javax.inject.{Inject, Singleton}
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import play.api.http.Status
import play.api.i18n.{I18nSupport, Lang, Messages, MessagesApi}
import play.api.libs.Files.SingletonTemporaryFileCreator
import play.api.libs.json._
import play.api.libs.ws.WSClient
import play.api.{Configuration, Environment, Logger}
import uk.gov.hmrc.cbcrfrontend.FileUploadFrontEndWS
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.connectors.FileUploadServiceConnector
import uk.gov.hmrc.cbcrfrontend.core.{ServiceResponse, _}
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.typesclasses._
import uk.gov.hmrc.http._
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.play.bootstrap.http.HttpClient

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class FileUploadService @Inject()(
  fusConnector: FileUploadServiceConnector,
  ws: WSClient,
  configuration: Configuration,
  val messagesApi: MessagesApi,
  servicesConfig: ServicesConfig)(
  implicit http: HttpClient,
  ac: ActorSystem,
  environment: Environment,
  fileUploadFrontEndWS: FileUploadFrontEndWS,
  feConfig: FrontendAppConfig)
    extends I18nSupport {

  implicit val materializer = ActorMaterializer()

  implicit lazy val fusUrl = new ServiceUrl[FusUrl] { val url = servicesConfig.baseUrl("file-upload") }
  implicit lazy val fusFeUrl = new ServiceUrl[FusFeUrl] { val url = servicesConfig.baseUrl("file-upload-frontend") }
  implicit lazy val cbcrsUrl = new ServiceUrl[CbcrsUrl] { val url = servicesConfig.baseUrl("cbcr") }
  implicit lazy val cbcrsStubUrl = new ServiceUrl[FusUrl] { val url = servicesConfig.baseUrl("cbcr-stub") }

  lazy val fileUploadUrl = if (feConfig.fileUploadProtocol == "http") cbcrsStubUrl else fusUrl

  lazy val stubbedFusFeUrl = new ServiceUrl[FusFeUrl] {
    val url = s"${servicesConfig.baseUrl("file-upload-frontend")}/stub"
  }

  def createEnvelope(implicit hc: HeaderCarrier, ec: ExecutionContext): ServiceResponse[EnvelopeId] = {

    val formatter = DateTimeFormat.forPattern("YYYY-MM-dd'T'HH:mm:ss'Z'")

    val envelopeExpiryDays: Option[Int] = configuration.getInt("envelope-expire-days")

    def envelopeExpiryDate(numberOfDays: Option[Int]) = numberOfDays match {
      case Some(n) => Some(formatter.print(new DateTime().plusDays(n)))
      case _       => None
    }

    EitherT(
      HttpExecutor(
        fusUrl,
        CreateEnvelope(fusConnector.envelopeRequest(cbcrsUrl.url, envelopeExpiryDate(envelopeExpiryDays))))
        .map(fusConnector.extractEnvelopId))
  }

  def uploadFile(xmlFile: java.io.File, envelopeId: String, fileId: String)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): ServiceResponse[String] = {

    //@todo replace this wit the Joda Equivalent
    val fileNamePrefix = s"oecd-${LocalDateTime.now}"
    val xmlByteArray: Array[Byte] = org.apache.commons.io.IOUtils.toByteArray(new FileInputStream(xmlFile))

    Logger.debug(s"Country by Country: FileUpload service: Uploading the file to the envelope - fileId = $fileId")
    fromFutureOptA(
      HttpExecutor(
        fusFeUrl,
        UploadFile(
          EnvelopeId(envelopeId),
          FileId(fileId),
          s"$fileNamePrefix-cbcr.xml ",
          "application/xml;charset=UTF-8",
          xmlByteArray)).map(fusConnector.extractFileUploadMessage))

  }

  def getFileUploadResponse(envelopeId: String)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): ServiceResponse[Option[FileUploadCallbackResponse]] =
    EitherT(
      http
        .GET[HttpResponse](s"${cbcrsUrl.url}/cbcr/file-upload-response/$envelopeId")
        .map(resp =>
          resp.status match {
            case 200 =>
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

  def getFile(envelopeId: String, fileId: String)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): ServiceResponse[File] =
    EitherT(
      ws.url(s"${fileUploadUrl.url}/file-upload/envelopes/$envelopeId/files/$fileId/content")
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

  def deleteEnvelope(envelopeId: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): ServiceResponse[String] =
    fromFutureOptA(
      http
        .DELETE[HttpResponse](s"${fusUrl.url}/file-upload/envelopes/$envelopeId")
        .map(fusConnector.extractEnvelopeDeleteMessage))

  def getFileMetaData(envelopeId: String, fileId: String)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): ServiceResponse[Option[FileMetadata]] =
    fromFutureOptA(
      http
        .GET[HttpResponse](s"${fileUploadUrl.url}/file-upload/envelopes/$envelopeId/files/$fileId/metadata")
        .map(fusConnector.extractFileMetadata))

  def uploadMetadataAndRoute(
    metaData: SubmissionMetaData)(implicit hc: HeaderCarrier, ec: ExecutionContext): ServiceResponse[String] = {

    val metadataFileId = UUID.randomUUID.toString
    val envelopeId = metaData.fileInfo.envelopeId
    val fileUploadFrontendUrl = if (feConfig.fileUploadProtocol == "http") stubbedFusFeUrl else fusFeUrl
    for {
      _ <- EitherT.right(
            HttpExecutor(
              fileUploadFrontendUrl,
              UploadFile(
                envelopeId,
                FileId(s"json-$metadataFileId"),
                "metadata.json ",
                " application/json; charset=UTF-8",
                Json.toJson(metaData).toString().getBytes)
            ))
      resourceUrl <- EitherT.right(HttpExecutor(fileUploadUrl, RouteEnvelopeRequest(envelopeId, "cbcr", "OFDS")))
    } yield resourceUrl.body
  }

  def errorsToList(e: List[ValidationErrors])(implicit messages: Messages): List[String] =
    e.map(x => x.show.split(" ").map(x => messages(x)).map(_.toString).mkString(" "))

  def errorsToMap(e: List[ValidationErrors])(implicit messages: Messages): Map[String, String] =
    errorsToList(e).foldLeft(Map[String, String]()) { (m, t) =>
      m + ("error_" + (m.size + 1).toString -> t)
    }

  def errorsToString(e: List[ValidationErrors])(implicit messages: Messages): String =
    errorsToList(e).map(_.toString).mkString("\r\n")

  def errorsToFile(e: List[ValidationErrors], name: String)(implicit messages: Messages): File = {
    val b = SingletonTemporaryFileCreator.create(name, ".txt")
    val writer = new PrintWriter(b.file)
    writer.write(errorsToString(e))
    writer.flush()
    writer.close()
    b.file
  }

}
