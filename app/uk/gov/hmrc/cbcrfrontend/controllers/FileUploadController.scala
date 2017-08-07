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

import java.io
import java.io.{File, InputStream, PrintWriter}
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
import play.api.libs.json.Json
import play.api.mvc._
import uk.gov.hmrc.cbcrfrontend.auth.SecuredActions
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.services._
import uk.gov.hmrc.cbcrfrontend.typesclasses.{CbcrsUrl, FusFeUrl, FusUrl, ServiceUrl}
import uk.gov.hmrc.cbcrfrontend.views.html._
import uk.gov.hmrc.cbcrfrontend.{FrontendAppConfig, FrontendGlobal, sha256Hash}
import uk.gov.hmrc.http.cache.client.CacheMap
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.frontend.controller.FrontendController
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
import uk.gov.hmrc.cbcrfrontend._
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}
import uk.gov.hmrc.play.audit.model.{DataEvent, ExtendedDataEvent}
import uk.gov.hmrc.play.audit.AuditExtensions._
import uk.gov.hmrc.play.frontend.auth.AuthContext


@Singleton
class FileUploadController @Inject()(val sec: SecuredActions,
                                     val schemaValidator: CBCRXMLValidator,
                                     val businessRuleValidator: CBCBusinessRuleValidator,
                                     val cache:CBCSessionCache,
                                     val fileUploadService:FileUploadService,
                                     val xmlExtractor:XmlInfoExtract,
                                     val validator:CBCRXMLValidator
                                    )(implicit ec: ExecutionContext) extends FrontendController with ServicesConfig {

  implicit lazy val fusUrl = new ServiceUrl[FusUrl] { val url = baseUrl("file-upload") }
  implicit lazy val fusFeUrl = new ServiceUrl[FusFeUrl] { val url = baseUrl("file-upload-frontend") }
  implicit lazy val cbcrsUrl = new ServiceUrl[CbcrsUrl] { val url = baseUrl("cbcr") }

  lazy val hostName = FrontendAppConfig.cbcrFrontendHost
  lazy val audit = FrontendAuditConnector
  lazy val fileUploadErrorRedirectUrl = s"$hostName/country-by-country-reporting/failed-callback"

  val chooseXMLFile = sec.AsyncAuthenticatedAction() { authContext => implicit request =>

    val result = for {
      envelopeId     <- cache.readOrCreate[EnvelopeId](fileUploadService.createEnvelope.toOption).toRight(UnexpectedState("Unable to get envelopeId"))
      fileId         <- cache.readOrCreate[FileId](OptionT.liftF(Future.successful(FileId(UUID.randomUUID.toString)))).toRight(UnexpectedState("Unable to get FileId"))
      successRedirect = s"$hostName/country-by-country-reporting/file-upload-progress/$envelopeId/$fileId"
      fileUploadUrl   = s"${FrontendAppConfig.fileUploadFrontendHost}/file-upload/upload/envelopes/$envelopeId/files/$fileId?" +
        s"redirect-success-url=$successRedirect&" +
        s"redirect-error-url=$fileUploadErrorRedirectUrl"
      fileName        = s"oecd-${LocalDateTime.now}-cbcr.xml"
    } yield Ok(submission.fileupload.chooseFile(fileUploadUrl, fileName, includes.asideBusiness(), includes.phaseBannerBeta()))

    result.leftMap(errorRedirect).merge

  }

  def fileUploadProgress(envelopeId: String, fileId: String) = Action.async { implicit request =>
    val hostName = FrontendAppConfig.cbcrFrontendHost
    val assetsLocationPrefix = FrontendAppConfig.assetsPrefix
    Ok(submission.fileupload.fileUploadProgress(includes.asideBusiness(), includes.phaseBannerBeta(),
      envelopeId, fileId, hostName, assetsLocationPrefix))
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


  def getMetaData(envelopeId: String, fileId: String)(implicit hc:HeaderCarrier): ServiceResponse[FileMetadata] = for {
    metadata <- fileUploadService.getFileMetaData(envelopeId, fileId).subflatMap(_.toRight(UnexpectedState("MetaData File not found")))
    _        <- EitherT.cond[Future](metadata.name.endsWith(".xml"), (), InvalidFileType(metadata.name))
    _        <- EitherT.right[Future, CBCErrors, CacheMap](cache.save(metadata))
  } yield metadata

  def validateBusinessRules(file:InputStream, metadata: FileMetadata)(implicit hc:HeaderCarrier): ServiceResponse[(Option[XMLInfo],List[BusinessRuleErrors])] = {
    val rawXmlInfo  = xmlExtractor.extract(file)
    val xmlInfo     = businessRuleValidator.validateBusinessRules(rawXmlInfo, metadata.name)
    EitherT.right(xmlInfo.fold(
      errors => cache.save(AllBusinessRuleErrors(errors.toList)).map(_ => None -> errors.toList),
      info   => cache.save(info).flatMap(_ => cache.save(Hash(sha256Hash(file))).map(_ => Some(info) -> List.empty))
    ).flatten)
  }

  def fileValidate(envelopeId: String, fileId: String) = sec.AsyncAuthenticatedAction(){ authContext => implicit request =>

    val result = for {
      file_metadata       <- (fileUploadService.getFile(envelopeId, fileId)  |@| getMetaData(envelopeId,fileId)).tupled
      _                   <- EitherT.cond[Future](file_metadata._2.name endsWith ".xml",(),InvalidFileType(file_metadata._2.name))
      schemaErrors        =  validator.validateSchema(file_metadata._1)
      _                   <- EitherT.right[Future,CBCErrors,CacheMap](cache.save(XMLErrors.errorHandlerToXmlErrors(schemaErrors)))
      _                   <- if(!schemaErrors.hasFatalErrors) EitherT.pure[Future,CBCErrors,Unit](())
                             else auditFailedSubmission(authContext, "schema validation errors").flatMap(_ =>
                                  EitherT.left[Future,CBCErrors,Unit](Future.successful(FatalSchemaErrors))
                             )
      xml_bizErrors       <- validateBusinessRules(file_metadata._1,file_metadata._2)
      length              = (file_metadata._2.length/1000).setScale(2, BigDecimal.RoundingMode.HALF_UP)
      _                   <- if(schemaErrors.hasErrors) auditFailedSubmission(authContext,"schema validation errors")
                             else if(xml_bizErrors._2.nonEmpty) auditFailedSubmission(authContext,"business rules errors")
                             else EitherT.pure[Future,CBCErrors,Unit](())
    } yield Ok(submission.fileupload.fileUploadResult(Some(file_metadata._2.name), Some(length), schemaErrors.hasErrors, xml_bizErrors._2.nonEmpty, includes.asideBusiness(), includes.phaseBannerBeta(),xml_bizErrors._1.map(_.reportingEntity.reportingRole)))

    result.leftMap{
      case FatalSchemaErrors      => BadRequest(submission.fileupload.fileUploadResult(None, None, true, false, includes.asideBusiness(), includes.phaseBannerBeta(),None))
      case InvalidFileType(_)     => Redirect(routes.FileUploadController.fileInvalid())
      case e:CBCErrors            =>
        Logger.error(e.toString)
        Redirect(routes.SharedController.technicalDifficulties())
    }.merge.recover{
      case NonFatal(e) =>
        Logger.error(e.getMessage,e)
        Redirect(routes.SharedController.technicalDifficulties())
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
    auditFailedSubmission(authContext,errorType.toString).map(_ => Ok(submission.fileupload.fileUploadError(
      includes.asideBusiness(),
      includes.phaseBannerBeta(),
      errorType
    ))).leftMap(errorRedirect).merge
  }

  def handleError(errorCode:Int, reason:String) = Action.async{ implicit request =>
    Logger.error(s"Error response received from FileUpload callback - ErrorCode: $errorCode - Reason $reason")
    Future.successful(
      errorCode match {
        case REQUEST_ENTITY_TOO_LARGE => Redirect(routes.FileUploadController.fileTooLarge())
        case UNSUPPORTED_MEDIA_TYPE   => Redirect(routes.FileUploadController.fileInvalid())
        case _                        => Redirect(routes.SharedController.technicalDifficulties())
      }
    )
  }

  def fileUploadResponse(envelopeId: String, fileId: String) = sec.AsyncAuthenticatedAction() { authContext => implicit request =>
    fileUploadService.getFileUploadResponse(envelopeId,fileId).fold(
      _        => NoContent,
      response => fileUploadResponseToResult(response)
    )
  }

  def auditFailedSubmission(authContext: AuthContext, reason:String) (implicit hc:HeaderCarrier, request:Request[_]): ServiceResponse[AuditResult.Success.type] = {
    EitherT(audit.sendEvent(DataEvent("Country-By-Country", "FailedSubmission",
      tags = hc.toAuditTags("FailedSubmission", "N/A") ++ Map("reason" -> reason)
    )).map {
      case AuditResult.Success         => Right(AuditResult.Success)
      case AuditResult.Failure(msg, _) => Left(UnexpectedState(s"Unable to audit a failed submission: $msg"))
      case AuditResult.Disabled        => Right(AuditResult.Success)
    })
  }

}
