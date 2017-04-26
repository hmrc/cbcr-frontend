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
import play.Play
import play.api.Logger
import play.api.libs.json._
import uk.gov.hmrc.cbcrfrontend.WSHttp
import uk.gov.hmrc.cbcrfrontend.connectors.FileUploadServiceConnector
import uk.gov.hmrc.cbcrfrontend.core.{ServiceResponse, _}
import uk.gov.hmrc.cbcrfrontend.typesclasses._
import uk.gov.hmrc.play.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.ExecutionContext


class FileUploadService(fusConnector: FileUploadServiceConnector) {



  def createEnvelopeAndUpload(
                      implicit
                      hc: HeaderCarrier,
                      ec: ExecutionContext,
                      fusUrl: ServiceUrl[FusUrl],
                      fusFeUrl: ServiceUrl[FusFeUrl],
                      cbcrsUrl: ServiceUrl[CbcrsUrl]
                    ): ServiceResponse[String] = {

//    val date = LocalDateTime.now
//    val fileNamePrefix = s"oecd-$date"
//
//    val fileId = UUID.randomUUID.toString
//    val bis = new BufferedInputStream(new FileInputStream(xmlFile))
//    val xmlByteArray: Array[Byte] = Stream.continually(bis.read).takeWhile(-1 !=).map(_.toByte).toArray
//    val metadataFileId = UUID.randomUUID.toString


    Logger.debug("Country by Country: Creating an envelope for file upload")

    for {
      envelopeId <- fromFutureOptA(HttpExecutor(fusUrl, CreateEnvelope(fusConnector.envelopeRequest("formTypeRef", cbcrsUrl.url))).map(fusConnector.extractEnvelopId))
//      uploaded <- fromFutureA(HttpExecutor(fusFeUrl,
//        UploadFile(envelopeId, FileId(s"xml-$fileId"), s"$fileNamePrefix-cbcr.xml ", " application/xml; charset=UTF-8", xmlByteArray)))
//      uploaded <- fromFutureA(HttpExecutor(fusFeUrl,
//        UploadFile(envelopeId, FileId(s"json-$metadataFileId"), "metadata.json ", " application/json; charset=UTF-8", mockedMetadata)))

      _          <- fromFutureA    (HttpExecutor(fusUrl, RouteEnvelopeRequest(envelopeId, "dfs", "DMS")))

    } yield envelopeId.value
  }


  def getFileUploadResponse(
                             implicit
                             hc: HeaderCarrier,
                             ec: ExecutionContext,
                             cbcrsUrl: ServiceUrl[CbcrsUrl],
                             envelopeId: String
                           ): ServiceResponse[String] = {


    Logger.debug("Country by Country: Get file upload response")


    for {
      fileUploadResponse <- fromFutureA(WSHttp.GET[HttpResponse](s"${cbcrsUrl.url}/cbcr/retrieveFileUploadResponse/" + envelopeId + "?cbcId=CBCId1234"))
    } yield fileUploadResponse.body
  }


  def saveFileUploadCallbackResponse(
                                      implicit
                                      hc: HeaderCarrier,
                                      callbackResponse: JsObject,
                                      ec: ExecutionContext,
                                      cbcrsUrl: ServiceUrl[CbcrsUrl]
                                    ): ServiceResponse[String] = {

    Logger.debug("Country by Country: Save file upload response")

    for {
      saveResponse <- fromFutureA(HttpExecutor(cbcrsUrl, FileUploadCallbackResponse(callbackResponse)))
    } yield saveResponse.body

  }


  def getFile(
                   implicit
                   hc: HeaderCarrier,
                   ec: ExecutionContext,
                   fusUrl: ServiceUrl[FusUrl],
                   envelopeId: String,
                   fileId: String
                 ): ServiceResponse[String] = {

    for {
      file <- fromFutureA(WSHttp.GET[HttpResponse](s"${fusUrl.url}/file-upload/envelopes/${envelopeId}/files/${fileId}/content"))
    } yield file.body

  }




  def mockedMetadata = {
    val bis = Play.application.resourceAsStream("docs/metadata.json")
    //val bis = new BufferedInputStream(new FileInputStream("docs/metadata.json"))
    Stream.continually(bis.read).takeWhile(-1 !=).map(_.toByte).toArray

  }

}