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

import java.io.File
import java.util.UUID
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

import cats.data.EitherT
import uk.gov.hmrc.cbcrfrontend.views.html._

import scala.concurrent.{ExecutionContext, Future}


@Singleton
class FileUpload @Inject()(val sec: SecuredActions, val fusConnector: FileUploadServiceConnector, val schemaValidator: CBCRXMLValidator)(implicit ec: ExecutionContext) extends FrontendController with ServicesConfig {

  lazy val fileUploadService = new FileUploadService(fusConnector)
  implicit val fusUrl = new ServiceUrl[FusUrl] { val url = baseUrl("file-upload")}
  implicit val fusFeUrl = new ServiceUrl[FusFeUrl] { val url = baseUrl("file-upload-frontend")}
  implicit val cbcrsUrl = new ServiceUrl[CbcrsUrl] { val url = baseUrl("cbcr")}

  val chooseXMLFile = sec.AsyncAuthenticatedAction { authContext => implicit request =>

    fileUploadService.createEnvelope.fold(
      error => InternalServerError("Envelope creation failed: "+error.errorMsg),
      envelopeId => {
      val fileId = UUID.randomUUID.toString
      val fileUploadCreateEnvelopeUrl = "/country-by-country-reporting/file-upload/"+envelopeId.value+"/"+fileId

      Ok(fileupload.chooseFile(fileUploadCreateEnvelopeUrl, includes.asideBusiness(), includes.phaseBannerBeta()))
      })
  }

  def upload(envelopeId: String, fileId: String) =  sec.AsyncAuthenticatedAction { authContext => implicit request =>

    request.body.asMultipartFormData match {

      case Some(body) => {

        import java.io._
        import cats.syntax.either._
        Logger.debug("Country by Country file upload started...")

        val xmlFile = Either.fromOption(
          body.file("oecdcbcxml").map {
            _.ref.file
          },
          new FileNotFoundException("File to be uploaded not found")
        )

        xmlFile.fold(
          error => Future(InternalServerError),
          file => {
            val hostName = FrontendAppConfig.cbcrFrontendHost
            val assetsLocationPrefix = FrontendAppConfig.assetsPrefix
            fileUploadService.uploadFile(file, envelopeId, fileId).fold(
              error => InternalServerError,
              success => Ok(fileupload.fileUploadProgress(
                includes.asideBusiness(), includes.phaseBannerBeta(),
                envelopeId, fileId, hostName,assetsLocationPrefix))
            )
          }
        )
      }
      case _ => Future(InternalServerError)
    }
  }


  def getFileUploadResponse(envelopeId: String, fileId: String) = Action.async { implicit request =>

    val result: EitherT[Future, Result, Result] = for {
      response <- fileUploadService.getFileUploadResponse(envelopeId,fileId).leftMap(_ => NoContent)
      _        <- if(response.exists(r => r.envelopeId == envelopeId && r.status == "AVAILABLE")) {
        EitherT.pure[Future,Result,Unit]((  ))
      } else {
        EitherT.left[Future,Result,Unit](Future.successful(NoContent))
      }
      file   <- fileUploadService.getFile(envelopeId,fileId).leftMap(_ => InternalServerError:Result)
      _ <- EitherT.fromEither[Future](schemaValidator.validate(file).toEither).leftMap {
        e => {
          fileUploadService.deleteEnvelope(envelopeId).leftMap(_ => InternalServerError:Result)
          NotAcceptable(e.getLocalizedMessage):Result
        }
      }
      fileMetadata <- fileUploadService.getFileMetaData(envelopeId,fileId).leftMap(_ => InternalServerError:Result)

    } yield Accepted(
      if (fileMetadata.isDefined) {
        val fileSize = (fileMetadata.get.length/1000).setScale(2, BigDecimal.RoundingMode.HALF_UP)

        Json.obj(("fileName", fileMetadata.get.name), ("size", fileSize))
      } else Json.obj(("fileName", "Not Found"), ("size", "Not Found"))

    )

    result.merge

  }

  def errorFileUpload(errorMessage: String) = Action.async { implicit request =>
    Future.successful(Ok(uk.gov.hmrc.cbcrfrontend.views.html.errors.fileUploadError(includes.asideBusiness(), errorMessage)))
  }

  def successFileUpload(fileName: String, fileSize: String) = Action.async { implicit request =>
    Future.successful(Ok(uk.gov.hmrc.cbcrfrontend.views.html.fileupload.fileUploadSuccess(
      fileName, fileSize, includes.asideBusiness(), includes.phaseBannerBeta()
    )))
  }


  val submitInfoFilingType = Action.async { implicit request =>
    Future.successful(Ok(uk.gov.hmrc.cbcrfrontend.views.html.forms.submitInfoFilingType(
      includes.asideBusiness(), includes.phaseBannerBeta()
    )))
  }

  val submitInfoUltimateParentEntity = Action.async { implicit request =>
    Future.successful(Ok(uk.gov.hmrc.cbcrfrontend.views.html.forms.submitInfoUltimateParentEntity(
      includes.asideBusiness(), includes.phaseBannerBeta()
    )))
  }

  val submitInfoFilingCapacity = Action.async { implicit request =>
    Future.successful(Ok(uk.gov.hmrc.cbcrfrontend.views.html.forms.submitInfoFilingCapacity(
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
