/*
 * Copyright 2021 HM Revenue & Customs
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

import akka.stream.Materializer
import akka.stream.scaladsl.Sink
import akka.util.ByteString
import cats.data.EitherT
import play.api.http.Status
import play.api.libs.ws.WSClient
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.model.UnexpectedState
import uk.gov.hmrc.cbcrfrontend.model.upscan.UploadId
import uk.gov.hmrc.http.HeaderCarrier

import java.io.{File, FileInputStream}
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class FileService @Inject()(ws: WSClient)(implicit materializer: Materializer) {

  def getFile(uploadId: UploadId, url: String)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): ServiceResponse[File] =
    EitherT(
      ws.url(s"$url")
        .withMethod("GET")
        .stream()
        .flatMap { res =>
          res.status match {
            case Status.OK =>
              val file = java.nio.file.Files.createTempFile(uploadId.value, "xml")
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
                    s"Failed to retrieve a file with uploadId: ${uploadId.value} from upscan - received $otherStatus response")
                ))
          }
        }
    )

  def deleteFile(file: File): Boolean = java.nio.file.Files.deleteIfExists(file.toPath)

  def sha256Hash(file: File): String = {
     String.format(
      "%064x",
      new java.math.BigInteger(
        1,
        java.security.MessageDigest
          .getInstance("SHA-256")
          .digest(
            org.apache.commons.io.IOUtils.toByteArray(new FileInputStream(file))
          ))
    )
  }
}
