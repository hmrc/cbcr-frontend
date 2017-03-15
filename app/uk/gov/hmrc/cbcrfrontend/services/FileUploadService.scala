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

import java.io.{BufferedInputStream, FileInputStream}
import java.time.LocalDateTime
import java.util.UUID

import cats.implicits._
import play.api.libs.functional.syntax._
import play.api.libs.json._
import uk.gov.hmrc.cbcrfrontend.WSHttp
import uk.gov.hmrc.cbcrfrontend.connectors.FileUploadServiceConnector
import uk.gov.hmrc.cbcrfrontend.core.{ServiceResponse, _}
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.typesclasses._
import uk.gov.hmrc.play.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.ExecutionContext


class FileUploadService(fusConnector: FileUploadServiceConnector) {

  implicit val xmlUploadCallbackFormat: Format[XMLUploadCallback] = (
    (JsPath \ "envelopeId").format[String] and
    (JsPath \ "fileId").format[String] and
    (JsPath \ "status").format[String]

  )(XMLUploadCallback.apply, unlift(XMLUploadCallback.unapply))


  def createEnvelope(
                      implicit
                      hc: HeaderCarrier,
                      ec: ExecutionContext,
                      fusUrl: ServiceUrl[FusUrl],
                      fusFeUrl: ServiceUrl[FusFeUrl],
                      xmlFile: java.io.File
                    ): ServiceResponse[String] = {

    val date = LocalDateTime.now
    val fileNamePrefix = s"oecd-$date"

    val fileId = UUID.randomUUID.toString
    val bis = new BufferedInputStream(new FileInputStream(xmlFile))
    val xmlByteArray: Array[Byte] = Stream.continually(bis.read).takeWhile(-1 !=).map(_.toByte).toArray

    for {
      envelopeId <- fromFutureOptA(HttpExecutor(fusUrl, CreateEnvelope(fusConnector.envelopeRequest("formTypeRef"))).map(fusConnector.extractEnvelopId))
      uploaded <- fromFutureA(HttpExecutor(fusFeUrl,
        UploadFile(envelopeId, FileId(s"xml-$fileId"), s"$fileNamePrefix-metadata.xml ", " application/xml; charset=UTF-8", xmlByteArray)))
    } yield envelopeId.value
  }


  def getFileUploadResponse(
                      implicit
                      hc: HeaderCarrier,
                      ec: ExecutionContext,
                      cbcrsUrl: ServiceUrl[CbcrsUrl],
                      envelopeId: String
                    ): ServiceResponse[String] = {

    for {
//      fileUploadResponse <- fromFutureA(HttpExecutor(cbcrsUrl, GetFileUploadResponse(envelopeId)))

      fileUploadResponse <- fromFutureA(WSHttp.GET[HttpResponse](s"${cbcrsUrl.url}/cbcr/retrieveFileUploadResponse/"+envelopeId+"?cbcId=CBCId1234"))


    } yield fileUploadResponse.body
  }

}


