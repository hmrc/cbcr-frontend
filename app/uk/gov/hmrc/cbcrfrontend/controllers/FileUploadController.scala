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

import java.io._
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
import uk.gov.hmrc.cbcrfrontend.connectors.EnrolmentsConnector
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.controllers._
import uk.gov.hmrc.cbcrfrontend.services._
import uk.gov.hmrc.cbcrfrontend.typesclasses.{CbcrsUrl, FusFeUrl, FusUrl, ServiceUrl}
import uk.gov.hmrc.cbcrfrontend.views.html._
import uk.gov.hmrc.cbcrfrontend.{FrontendAppConfig, getUserType, sha256Hash, _}
import uk.gov.hmrc.http.cache.client.CacheMap
import uk.gov.hmrc.play.audit.AuditExtensions._
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}
import uk.gov.hmrc.play.audit.model.{DataEvent, ExtendedDataEvent}
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.frontend.auth.AuthContext
import uk.gov.hmrc.play.frontend.auth.connectors.AuthConnector
import uk.gov.hmrc.play.frontend.controller.FrontendController
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal


@Singleton
class FileUploadController @Inject()(val sec: SecuredActions,
                                     val schemaValidator: CBCRXMLValidator,
                                     val businessRuleValidator: CBCBusinessRuleValidator,
                                     val enrol:EnrolmentsConnector,
                                     val fileUploadService:FileUploadService,
                                     val xmlExtractor:XmlInfoExtract,
                                     val rrService: DeEnrolReEnrolService)
                                    (implicit ec: ExecutionContext, cache:CBCSessionCache, auth:AuthConnector) extends FrontendController with ServicesConfig {


  implicit lazy val fusUrl = new ServiceUrl[FusUrl] { val url = baseUrl("file-upload") }
  implicit lazy val fusFeUrl = new ServiceUrl[FusFeUrl] { val url = baseUrl("file-upload-frontend") }
  implicit lazy val cbcrsUrl = new ServiceUrl[CbcrsUrl] { val url = baseUrl("cbcr") }

  lazy val hostName = FrontendAppConfig.cbcrFrontendHost
  lazy val audit: AuditConnector = FrontendAuditConnector
  lazy val fileUploadErrorRedirectUrl = s"$hostName${routes.FileUploadController.handleError().url}"

  private def allowedToSubmit(authContext: AuthContext,userType: UserType, enrolled:Boolean)(implicit hc: HeaderCarrier) = userType match {
    case Organisation(_) => if(enrolled) { Future.successful(true) } else { cache.readOption[CBCId].map(_.isDefined) }
    case Agent()        => Future.successful(true)
    case Individual()   => Future.successful(false)
  }

  def auditDeEnrolReEnrolEvent(enrolment: CBCEnrolment,result:ServiceResponse[CBCId])(implicit request:Request[AnyContent]) : ServiceResponse[CBCId] = {
    EitherT(result.value.flatMap { e =>
      audit.sendEvent(ExtendedDataEvent("Country-By-Country-Frontend", "CBCR-DeEnrolReEnrol",
        tags = hc.toAuditTags("CBCR-DeEnrolReEnrol", "N/A") + (
          "path"     -> request.uri,
          "newCBCId" -> e.map(_.value).getOrElse("Failed to get new CBCId"),
          "oldCBCId" -> enrolment.cbcId.value,
          "utr"      -> enrolment.utr.utr)
      )).map {
        case AuditResult.Success         => e
        case AuditResult.Failure(msg, _) => Left(UnexpectedState(s"Unable to audit a successful submission: $msg"))
        case AuditResult.Disabled        => e
      }
    })
  }

  val chooseXMLFile = sec.AsyncAuthenticatedAction() { authContext => implicit request =>
    val result:ServiceResponse[Result] = for {
      userType  <- getUserType(authContext)
      enrolment <- right[Option[CBCEnrolment]](enrol.getCBCEnrolment.value)
      canSubmit <- right[Boolean](allowedToSubmit(authContext, userType, enrolment.isDefined))
      result    <- (userType, enrolment) match {
        case (Organisation(_), Some(e)) if CBCId.isPrivateBetaCBCId(e.cbcId) =>
          auditDeEnrolReEnrolEvent(e,rrService.deEnrolReEnrol(e)).map(
            (id: CBCId) => Ok(shared.regenerate(includes.asideCbc(), includes.phaseBannerBeta(), id))
          )
        case (_, _) if canSubmit =>
          for {
            envelopeId      <- cache.readOrCreate[EnvelopeId](fileUploadService.createEnvelope.toOption).toRight(UnexpectedState("Unable to get envelopeId"))
            fileId          <- cache.readOrCreate[FileId](OptionT.liftF(Future.successful(FileId(UUID.randomUUID.toString)))).toRight(UnexpectedState("Unable to get FileId"): CBCErrors)
            successRedirect = s"$hostName${routes.FileUploadController.fileUploadProgress(envelopeId.value, fileId.value).url}"
            fileUploadUrl   = s"${FrontendAppConfig.fileUploadFrontendHost}/file-upload/upload/envelopes/$envelopeId/files/$fileId?" +
              s"redirect-success-url=$successRedirect&" +
              s"redirect-error-url=$fileUploadErrorRedirectUrl"
            fileName        = s"oecd-${LocalDateTime.now}-cbcr.xml"
          } yield Ok(submission.fileupload.chooseFile(fileUploadUrl, fileName, includes.asideBusiness(), includes.phaseBannerBeta()))
        case _ =>
          pure(Redirect(routes.SubmissionController.notRegistered()))
      }
    } yield result


    result.leftMap(errorRedirect).merge

  }

  def fileUploadProgress(envelopeId: String, fileId: String) = sec.AuthenticatedAction{ _ => implicit request =>
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

  def validateBusinessRules(file_metadata:(File,FileMetadata))(implicit hc:HeaderCarrier): ServiceResponse[Either[NonEmptyList[BusinessRuleErrors],CompleteXMLInfo]] = {
    val rawXmlInfo  = xmlExtractor.extract(file_metadata._1)

    val result = for {
      xmlInfo         <- EitherT(businessRuleValidator.validateBusinessRules(rawXmlInfo, file_metadata._2.name).map(_.toEither))
      completeXI      <- EitherT(businessRuleValidator.recoverReportingEntity(xmlInfo).map(_.toEither))
    } yield completeXI

    EitherT.right[Future,CBCErrors,Either[NonEmptyList[BusinessRuleErrors],CompleteXMLInfo]](result.fold(
      errors => cache.save(AllBusinessRuleErrors(errors.toList)).map(_ => Left(errors)),
      info   => cache.save(info).flatMap(_ => cache.save(Hash(sha256Hash(file_metadata._1))).map(_ => Right(info)))
    ).flatten)
  }

  def calculateFileSize(md:FileMetadata) :  BigDecimal =
    (md.length/1000).setScale(2, BigDecimal.RoundingMode.HALF_UP)


  def fileValidate(envelopeId: String, fileId: String) = sec.AsyncAuthenticatedAction(){ authContext => implicit request =>

    val result = for {
      file_metadata       <- (fileUploadService.getFile(envelopeId, fileId)  |@| getMetaData(envelopeId,fileId)).tupled
      _                   <- right(cache.save(file_metadata._2))
      _                   <- EitherT.cond[Future](file_metadata._2.name endsWith ".xml",(),InvalidFileType(file_metadata._2.name))
      schemaErrors        =  schemaValidator.validateSchema(file_metadata._1)
      xmlErrors           = XMLErrors.errorHandlerToXmlErrors(schemaErrors)
      schemaSize          = if(xmlErrors.errors.nonEmpty){ Some(getErrorFileSize(List(xmlErrors))) } else { None }
      _                   <- EitherT.right[Future,CBCErrors,CacheMap](cache.save(XMLErrors.errorHandlerToXmlErrors(schemaErrors)))
      _                   <- if(!schemaErrors.hasFatalErrors) EitherT.pure[Future,CBCErrors,Unit](())
                             else auditFailedSubmission(authContext, "schema validation errors").flatMap(_ =>
                               EitherT.left[Future,CBCErrors,Unit](Future.successful(FatalSchemaErrors(schemaSize)))
                             )
      result              <- validateBusinessRules(file_metadata)
      businessSize        =  result.fold(e => Some(getErrorFileSize(e.toList)),_ => None)
      length              =  calculateFileSize(file_metadata._2)
      _                   <- if(schemaErrors.hasErrors) auditFailedSubmission(authContext,"schema validation errors")
                             else if(result.isLeft)     auditFailedSubmission(authContext,"business rules errors")
                             else                       EitherT.pure[Future,CBCErrors,Unit](())
      _                   = java.nio.file.Files.deleteIfExists(file_metadata._1.toPath)
      userType            <- getUserType(authContext)
    } yield Ok(submission.fileupload.fileUploadResult(Some(userType), Some(file_metadata._2.name), Some(length), schemaSize, businessSize, includes.asideBusiness(), includes.phaseBannerBeta(),result.map(_.reportingEntity.reportingRole).toOption))

    result.leftMap{
      case FatalSchemaErrors(size)=>
        Ok(submission.fileupload.fileUploadResult(None, None, None, size, None, includes.asideBusiness(), includes.phaseBannerBeta(),None))
      case InvalidFileType(_)     =>
        Redirect(routes.FileUploadController.fileInvalid())
      case e:CBCErrors            =>
        Logger.error(e.toString)
        Redirect(routes.SharedController.technicalDifficulties())
    }.merge.recover{
      case NonFatal(e) =>
        Logger.error(e.getMessage,e)
        Redirect(routes.SharedController.technicalDifficulties())
    }

 }

  private def getErrorFileSize(e:List[ValidationErrors]) : Int = {
    val f = errorsToFile(e,"")
    val kb = f.length() * 0.001
    f.delete()
    Math.incrementExact(kb.toInt)
  }

  private def errorsToFile(e:List[ValidationErrors], name:String) : File = {
    val b = Files.TemporaryFile(name, ".txt")
    val writer = new PrintWriter(b.file)
    writer.write(e.map(_.show).mkString("\r\n"))
    writer.flush()
    writer.close()
    b.file
  }

  def getBusinessRuleErrors = sec.AsyncAuthenticatedAction() { authContext => implicit request =>
    OptionT(cache.readOption[AllBusinessRuleErrors]).map(x => errorsToFile(x.errors,"BusinessRuleErrors")
    ).fold(
      NoContent
    )((file: File) =>
      Ok.sendFile(content = file,inline = false, onClose = () => file.delete())
    )
  }

  def getXmlSchemaErrors = sec.AsyncAuthenticatedAction() { authContext => implicit request =>
    OptionT(cache.readOption[XMLErrors]).map(x => errorsToFile(List(x),"XMLSchemaErrors")
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

  def handleError(errorCode:Int, reason:String) = sec.AuthenticatedAction{ _ => implicit request =>
    Logger.error(s"Error response received from FileUpload callback - ErrorCode: $errorCode - Reason $reason")
    errorCode match {
      case REQUEST_ENTITY_TOO_LARGE => Redirect(routes.FileUploadController.fileTooLarge())
      case UNSUPPORTED_MEDIA_TYPE   => Redirect(routes.FileUploadController.fileInvalid())
      case _                        => Redirect(routes.SharedController.technicalDifficulties())
    }
  }

  def fileUploadResponse(envelopeId: String, fileId: String) = sec.AsyncAuthenticatedAction() { authContext => implicit request =>
    Logger.info(s"Received a file-upload-response query for $envelopeId")
    fileUploadService.getFileUploadResponse(envelopeId,fileId).fold(
      error    => {
        Logger.info(s"File not ready: $error")
        NoContent
      },
      response => {
        Logger.info(s"Response back was: $response")
        fileUploadResponseToResult(response)
      }
    )
  }

  //Turn a Case class into a map
  private[controllers] def getCCParams(cc: AnyRef): Map[String, String] =
    (Map[String, String]() /: cc.getClass.getDeclaredFields) {(acc, field) =>
      field.setAccessible(true)
      acc + (field.getName -> field.get(cc).toString)
    }

  def auditFailedSubmission(authContext: AuthContext, reason:String) (implicit hc:HeaderCarrier, request:Request[_]): ServiceResponse[AuditResult.Success.type] = {
    for {
      ggId   <- right(getUserGGId(authContext))
      md     <- right(cache.readOption[FileMetadata])
      result <- eitherT[AuditResult.Success.type](audit.sendEvent(DataEvent("Country-By-Country-Frontend", "CBCRFilingFailed",
        tags = hc.toAuditTags("CBCRFilingFailed", "N/A") ++ Map("reason" -> reason, "path" -> request.uri, "ggId" -> ggId.authProviderId) ++ md.map(getCCParams).getOrElse(Map.empty[String,String])
      )).map {
        case AuditResult.Success => Right(AuditResult.Success)
        case AuditResult.Failure(msg, _) => Left(UnexpectedState(s"Unable to audit a failed submission: $msg"))
        case AuditResult.Disabled => Right(AuditResult.Success)
      })
    } yield result
  }

}
