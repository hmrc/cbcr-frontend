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

package uk.gov.hmrc.cbcrfrontend.controllers

import play.api.mvc._
import uk.gov.hmrc.cbcrfrontend.services.FileUploadService
import uk.gov.hmrc.cbcrfrontend.typesclasses.{CbcrsUrl, FusFeUrl, FusUrl, ServiceUrl}
import uk.gov.hmrc.play.frontend.controller.FrontendController
import uk.gov.hmrc.play.config.ServicesConfig
import play.api.libs.json.{JsObject, JsValue}
import uk.gov.hmrc.cbcrfrontend.connectors.FileUploadServiceConnector
import uk.gov.hmrc.cbcrfrontend.xmlvalidator.CBCRXMLValidator

import scala.concurrent.ExecutionContext.Implicits.global
import cats.implicits._
import play.api.Logger

import scala.concurrent.Future


object  FileUpload extends FileUpload

trait FileUpload  extends FrontendController with ServicesConfig {
  lazy val fusConnector = new FileUploadServiceConnector()
  lazy val fileUploadService = new FileUploadService(fusConnector)
  implicit val fusUrl = new ServiceUrl[FusUrl] { val url = baseUrl("file-upload")}
  implicit val fusFeUrl = new ServiceUrl[FusFeUrl] { val url = baseUrl("file-upload-frontend")}
  implicit val cbcrsUrl = new ServiceUrl[CbcrsUrl] { val url = baseUrl("cbcr")}





  val chooseXMLFile = Action.async { implicit request =>
    Future.successful(Ok(uk.gov.hmrc.cbcrfrontend.views.html.fileupload.chooseFile()))
  }

  val upload =  Action.async(parse.multipartFormData)  { implicit request =>

    import java.io._
    import cats.syntax.either._
    Logger.debug("Country by Country file upload started...")
    val validatedData = Either.fromOption(
      request.body.file("oecdcbcxml").map { _.ref.file },
      new FileNotFoundException("which file")
    ).flatMap{CBCRXMLValidator(_).toEither}

    validatedData.fold (
      exception => {
        Logger.debug("Exception details: "+exception.getLocalizedMessage)
        Future(Redirect(routes.FileUpload.errorFileUpload).flashing("error" -> exception.getLocalizedMessage))
      },
      file => {

        Logger.debug("Country by Countryfile validated OK...")

        implicit val xml:File = file
        fileUploadService.createEnvelope.fold(
                    error => Redirect(routes.FileUpload.errorFileUpload).flashing("error" -> "error uploading file"),
          envelopeId => {
            Redirect(routes.FileUpload.fileUploadProgress).flashing("ENVELOPEID" -> envelopeId)
          }
        )
      }
    )
  }




  val fileUploadCallback = Action.async(parse.json) { implicit request =>
    implicit val callbackResponse: JsObject = request.body.as[JsObject]

    Logger.debug("Callback json: " + callbackResponse)
    fileUploadService.saveFileUploadCallbackResponse.fold(error => InternalServerError("Something went wrong"),  response => Ok(response))
  }

  val fileUploadProgress = Action.async { implicit request =>
    val envelopeId = request.flash.get("ENVELOPEID")
    Logger.debug("Headers :"+envelopeId)

    Future.successful(Ok(uk.gov.hmrc.cbcrfrontend.views.html.fileupload.fileUploadProgress(envelopeId.getOrElse("notfound"))))
  }

  def getFileUploadResponse(eId: String) = Action.async { implicit request =>
    implicit val envelopeId = eId
    fileUploadService.getFileUploadResponse.fold(error => InternalServerError("Something went wrong"),  response => Ok(response))
  }
  val errorFileUpload = Action.async { implicit request =>
    Future.successful(Ok(uk.gov.hmrc.cbcrfrontend.views.html.errors.fileUploadError()))
  }

  val successFileUpload = Action.async { implicit request =>
    Future.successful(Ok(uk.gov.hmrc.cbcrfrontend.views.html.fileupload.fileUploadSuccess()))
  }


/*  val helloFileUpload = Action.async { implicit request =>

  fileUploadService.createEnvelope.fold(
    error => error.toResult,
    response => Ok(response)
  )
}
*/
}
