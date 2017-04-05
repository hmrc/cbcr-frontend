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

import javax.inject.Inject

import play.api.mvc._
import uk.gov.hmrc.cbcrfrontend.services.FileUploadService
import uk.gov.hmrc.cbcrfrontend.typesclasses.{CbcrsUrl, FusFeUrl, FusUrl, ServiceUrl}
import uk.gov.hmrc.play.frontend.controller.FrontendController
import uk.gov.hmrc.play.config.ServicesConfig
import play.api.libs.json.JsObject
import play.api.libs.json._
import uk.gov.hmrc.cbcrfrontend.connectors.FileUploadServiceConnector
import uk.gov.hmrc.cbcrfrontend.xmlvalidator.CBCRXMLValidator

import scala.concurrent.ExecutionContext.Implicits.global
import cats.implicits._
import play.api.Logger
import uk.gov.hmrc.cbcrfrontend.views.html._
import play.api.Play.current
import play.api.i18n.Messages.Implicits._
import uk.gov.hmrc.cbcrfrontend.auth.SecuredActions
import uk.gov.hmrc.cbcrfrontend.{AppConfig, FrontendAppConfig}
import javax.inject.{Inject, Singleton}

import play.api.libs.Files.TemporaryFile

import scala.concurrent.{ExecutionContext, Future}


@Singleton
class FileUpload @Inject()(val sec: SecuredActions)(implicit ec: ExecutionContext) extends FrontendController with ServicesConfig {
  lazy val fusConnector = new FileUploadServiceConnector()
  lazy val fileUploadService = new FileUploadService(fusConnector)
  implicit val fusUrl = new ServiceUrl[FusUrl] { val url = baseUrl("file-upload")}
  implicit val fusFeUrl = new ServiceUrl[FusFeUrl] { val url = baseUrl("file-upload-frontend")}
  implicit val cbcrsUrl = new ServiceUrl[CbcrsUrl] { val url = baseUrl("cbcr")}



  val chooseXMLFile = sec.AsyncAuthenticatedAction { authContext => implicit request =>
    Future.successful(Ok(uk.gov.hmrc.cbcrfrontend.views.html.fileupload.chooseFile(
      includes.asideBusiness(), includes.phaseBannerBeta()
    )))
  }

  val upload =  sec.AsyncAuthenticatedAction { authContext => implicit request =>

    request.body.asMultipartFormData match {

      case Some(body) => {

        val protocol = if (request.secure) "https" else "http"
        val hostName = request.host
        implicit val protocolHostName = s"$protocol://$hostName"

        import java.io._
        import cats.syntax.either._
        Logger.debug("Country by Country file upload started...")
        val validatedData = Either.fromOption(
          body.file("oecdcbcxml").map {
            _.ref.file
          },
          new FileNotFoundException("which file")
        ).flatMap {
          CBCRXMLValidator(_).toEither
        }

        validatedData.fold(
          exception => {
            Logger.debug("Exception details: " + exception.getLocalizedMessage)
            Future(Redirect(routes.FileUpload.errorFileUpload).flashing("error" -> exception.getLocalizedMessage))
          },
          file => {

            Logger.debug("Country by Country file validated OK...")

            implicit val xml: File = file
            fileUploadService.createEnvelopeAndUpload.fold(
              error      => Redirect(routes.FileUpload.errorFileUpload).flashing("error" -> "error uploading file"),
              envelopeId => Redirect(routes.FileUpload.fileUploadProgress).flashing("ENVELOPEID" -> envelopeId)
            )
          }
        )
      }
      case _ => Future(Redirect(routes.FileUpload.errorFileUpload).flashing("error" -> "Input received is not Multipart Upload"))
    }
  }

  val fileUploadCallback = Action.async {  implicit request =>

    Logger.debug("fileUploadCallback called:")
    request.body.asJson match {
      case Some(body) => {
        Logger.debug("Callback json: " + body)
        implicit val callbackResponse = body.as[JsObject]
        fileUploadService.saveFileUploadCallbackResponse.fold(error => InternalServerError("Something went wrong"),  response => Ok(response))
      }
      case _ => Future.successful(Ok("Invalid response received from FileUpload service"))
    }


  }

  val fileUploadProgress = Action.async { implicit request =>
    val envelopeId = request.flash.get("ENVELOPEID")
    Logger.debug("Headers :"+envelopeId)
    val protocol = if(request.secure) "https" else "http"
    val hostName = request.host
    val protocolHostName = s"$protocol://$hostName"
    val assetsLocationPrefix = FrontendAppConfig.assetsPrefix

    Future.successful(Ok(uk.gov.hmrc.cbcrfrontend.views.html.fileupload.fileUploadProgress(
      includes.asideBusiness(), includes.phaseBannerBeta(),
      envelopeId.getOrElse("notfound"), protocolHostName,assetsLocationPrefix)))
  }

  def getFileUploadResponse(eId: String) = Action.async { implicit request =>
    implicit val envelopeId = eId
    fileUploadService.getFileUploadResponse.fold(error => InternalServerError("Something went wrong"),  response => Ok(response))
  }
  val errorFileUpload = Action.async { implicit request =>
    Future.successful(Ok(uk.gov.hmrc.cbcrfrontend.views.html.errors.fileUploadError()))
  }

  val successFileUpload = Action.async { implicit request =>
    Future.successful(Ok(uk.gov.hmrc.cbcrfrontend.views.html.fileupload.fileUploadSuccess(
      includes.asideBusiness(), includes.phaseBannerBeta()
    )))
  }


  val contactInfoSubmitter = Action.async { implicit request =>
    Future.successful(Ok(uk.gov.hmrc.cbcrfrontend.views.html.forms.contactInfoSubmitter(
      includes.asideBusiness(), includes.phaseBannerBeta()
    )))
  }

  val submitSummary = Action.async { implicit request =>
    Future.successful(Ok(uk.gov.hmrc.cbcrfrontend.views.html.forms.submitSummary(
      includes.phaseBannerBeta()
    )))
  }

  val submitSuccessReceipt = Action.async { implicit request =>
    Future.successful(Ok(uk.gov.hmrc.cbcrfrontend.views.html.forms.submitSuccessReceipt(
      includes.asideBusiness(), includes.phaseBannerBeta()
    )))
  }

  val filingHistory = Action.async { implicit request =>
    Future.successful(Ok(uk.gov.hmrc.cbcrfrontend.views.html.forms.filingHistory(
      includes.phaseBannerBeta()
    )))
  }

}
