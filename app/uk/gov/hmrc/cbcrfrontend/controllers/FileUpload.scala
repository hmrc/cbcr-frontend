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

import java.io.{File, PrintWriter}
import java.time.LocalDateTime
import java.util.UUID
import javax.inject.{Inject, Singleton}

import cats.data._
import cats.instances.all._
import cats.syntax.all._
import play.api.Logger
import play.api.Play.current
import play.api.i18n.Messages.Implicits._
import play.api.libs.Files
import play.api.mvc._
import uk.gov.hmrc.cbcrfrontend.auth.SecuredActions
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.exceptions.{CBCErrors, InvalidFileType, UnexpectedState}
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.services.{CBCBusinessRuleValidator, CBCRXMLValidator, CBCSessionCache, FileUploadService}
import uk.gov.hmrc.cbcrfrontend.typesclasses.{CbcrsUrl, FusFeUrl, FusUrl, ServiceUrl}
import uk.gov.hmrc.cbcrfrontend.views.html._
import uk.gov.hmrc.cbcrfrontend.{FrontendAppConfig, FrontendGlobal, sha256Hash}
import uk.gov.hmrc.http.cache.client.CacheMap
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal


@Singleton
class FileUpload @Inject()(val sec: SecuredActions,
                           val schemaValidator: CBCRXMLValidator,
                           val businessRuleValidator: CBCBusinessRuleValidator,
                           val cache:CBCSessionCache,
                           val fileUploadService:FileUploadService)(implicit ec: ExecutionContext) extends FrontendController with ServicesConfig {

  implicit lazy val fusUrl = new ServiceUrl[FusUrl] { val url = baseUrl("file-upload") }
  implicit lazy val fusFeUrl = new ServiceUrl[FusFeUrl] { val url = baseUrl("file-upload-frontend") }
  implicit lazy val cbcrsUrl = new ServiceUrl[CbcrsUrl] { val url = baseUrl("cbcr") }

  lazy val hostName = FrontendAppConfig.cbcrFrontendHost
  lazy val fileUploadErrorRedirectUrl = s"$hostName/country-by-country-reporting/failed-callback"

  val chooseXMLFile = sec.AsyncAuthenticatedAction() { authContext => implicit request =>

    val result = for {
      envelopeId     <- fileUploadService.createEnvelope
      _              <- EitherT.right[Future,CBCErrors,CacheMap](cache.save(envelopeId))
      fileId          = UUID.randomUUID.toString
      _              <- EitherT.right[Future,CBCErrors,CacheMap](cache.save(FileId(fileId)))
      successRedirect = s"$hostName/country-by-country-reporting/file-upload-progress/$envelopeId/$fileId"
      fileUploadUrl   = s"${FrontendAppConfig.fileUploadFrontendHost}/file-upload/upload/envelopes/$envelopeId/files/$fileId?" +
        s"redirect-success-url=$successRedirect&" +
        s"redirect-error-url=$fileUploadErrorRedirectUrl"
      fileName        = s"oecd-${LocalDateTime.now}-cbcr.xml"
    } yield Ok(fileupload.chooseFile(fileUploadUrl, fileName, includes.asideBusiness(), includes.phaseBannerBeta()))


    result.leftMap { error =>
      Logger.error(error.show)
      InternalServerError(FrontendGlobal.internalServerErrorTemplate)
    }.merge

  }

  def fileUploadProgress(envelopeId: String, fileId: String) = Action.async { implicit request =>
    val hostName = FrontendAppConfig.cbcrFrontendHost
    val assetsLocationPrefix = FrontendAppConfig.assetsPrefix
    Future.successful(Ok(fileupload.fileUploadProgress(includes.asideBusiness(), includes.phaseBannerBeta(),
      envelopeId, fileId, hostName, assetsLocationPrefix)))
  }


  def fileUploadResponseToResult(optResponse: Option[FileUploadCallbackResponse]): Result =
    optResponse.map(response => response.status match {
      case "AVAILABLE" => Accepted
      case "ERROR"     => response.reason match {
        case Some("VirusDetected") => Conflict
        case _                     => InternalServerError
      }
      case _           => NoContent
    }).getOrElse(NoContent)


  def fileValidate(envelopeId: String, fileId: String) = sec.AsyncAuthenticatedAction(){ authContext => implicit request =>

    val result: ServiceResponse[(Option[NonEmptyList[BusinessRuleErrors]], Option[NonEmptyList[XMLErrors]], FileMetadata, Option[KeyXMLFileInfo])] = for {
      metadata    <- fileUploadService.getFileMetaData(envelopeId, fileId).subflatMap(_.toRight(UnexpectedState("MetaData File not found")))
      _           <- EitherT.cond[Future](metadata.name.endsWith(".xml"),(),InvalidFileType(metadata.name))
      _           <- EitherT.right[Future,CBCErrors,CacheMap](cache.save(metadata))
      f           <- fileUploadService.getFile(envelopeId, fileId)
      schemaVal    = schemaValidator.validateSchema(f).leftMap(XMLErrors.errorHandlerToXmlErrors).toValidatedNel
      cbcId       <- OptionT(cache.read[CBCId]).toRight(UnexpectedState("Unable to find CBCId in cache"))
      businessVal  = businessRuleValidator.validateBusinessRules(f,cbcId,metadata.name)
      xml         <- EitherT.right(businessVal.flatMap(_.map(xmlInfo => cache.save(xmlInfo).map(_ => xmlInfo)).sequence))
      errors      <- EitherT.right(businessVal.map(_.swap.toOption -> schemaVal.swap.toOption))
      _           <- EitherT.right[Future,CBCErrors,CacheMap](cache.save(Hash(sha256Hash(f))))
    } yield (errors._1, errors._2, metadata, xml.toOption)

    result.fold({
      case UnexpectedState(errorMsg, _) => fileUploadService.deleteEnvelope(envelopeId).fold(
        _ => InternalServerError(FrontendGlobal.internalServerErrorTemplate),
        _ => InternalServerError(FrontendGlobal.internalServerErrorTemplate)
      ).map(s => {
        Logger.error(errorMsg)
        s
      })
      case InvalidFileType(_) => Future.successful(Redirect(routes.FileUpload.fileInvalid()))

    },
      validationErrors => {
        val (bErrors, sErrors, md, xml) = validationErrors
        val length: BigDecimal          = (md.length/1000).setScale(2, BigDecimal.RoundingMode.HALF_UP)

        for {
          _ <- bErrors.map(e => cache.save(AllBusinessRuleErrors(e.toList))).sequence[Future,CacheMap]
          _ <- sErrors.map(e => cache.save(e.head)).sequence[Future,CacheMap]
        } yield Ok(fileupload.fileUploadResult(md.name, length, sErrors.isDefined, bErrors.isDefined, includes.asideBusiness(), includes.phaseBannerBeta(), xml.map(_.reportingRole)))

      }
    ).flatten.recover{
      case NonFatal(e) =>
        Logger.error(e.getMessage,e)
        InternalServerError(FrontendGlobal.internalServerErrorTemplate)
    }

 }

  private def errorsToFile(e:List[ValidationErrors], name:String) : File = {
    val b = Files.TemporaryFile(name, ".txt")
    val writer = new PrintWriter(b.file)
    writer.write(e.map(_.show).mkString("\n"))
    writer.flush()
    writer.close()
    b.file
  }

  def getBusinessRuleErrors = sec.AsyncAuthenticatedAction() { authContext => implicit request =>
    OptionT(cache.read[AllBusinessRuleErrors]).map(x => errorsToFile(x.errors,"BusinessRuleErrors")
    ).fold(
      NoContent
    )((file: File) =>
      Ok.sendFile(content = file,inline = false, onClose = () => file.delete())
    )
  }

  def getXmlSchemaErrors = sec.AsyncAuthenticatedAction() { authContext => implicit request =>
    OptionT(cache.read[XMLErrors]).map(x => errorsToFile(List(x),"XMLSchemaErrors")
    ).fold(
      NoContent
    )((file: File) =>
      Ok.sendFile(content = file,inline = false, onClose = () => file.delete())
    )
  }

  def fileInvalid       = fileUploadError(FileNotXml)

  def fileTooLarge      = fileUploadError(FileTooLarge)

  def fileContainsVirus = fileUploadError(FileContainsVirus)

  private def fileUploadError(errorType:FileUploadErrorType) = sec.AsyncAuthenticatedAction() { authContext => implicit request =>
    Future.successful(Ok(fileupload.fileUploadError(
      includes.asideBusiness(),
      includes.phaseBannerBeta(),
      errorType
    )))
  }

  def handleError(errorCode:Int, reason:String) = Action.async{ implicit request =>
    Logger.error(s"Error response received from FileUpload callback - ErrorCode: $errorCode - Reason $reason")
    Future.successful(
      errorCode match {
        case REQUEST_ENTITY_TOO_LARGE => Redirect(routes.FileUpload.fileTooLarge())
        case UNSUPPORTED_MEDIA_TYPE   => Redirect(routes.FileUpload.fileInvalid())
        case _                        => Redirect(routes.CBCController.technicalDifficulties())
      }
    )
  }

  def fileUploadResponse(envelopeId: String, fileId: String) = sec.AsyncAuthenticatedAction() { authContext => implicit request =>
    fileUploadService.getFileUploadResponse(envelopeId,fileId).fold(
      _        => NoContent,
      response => fileUploadResponseToResult(response)
    )
  }

}
