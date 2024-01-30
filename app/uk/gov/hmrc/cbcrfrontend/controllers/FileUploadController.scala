/*
 * Copyright 2023 HM Revenue & Customs
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

import cats.data.{EitherT, NonEmptyList, OptionT}
import cats.implicits.{catsStdInstancesForFuture, catsSyntaxEitherId}
import play.api.i18n.Messages
import play.api.libs.json._
import play.api.mvc._
import play.api.{Configuration, Logger}
import uk.gov.hmrc.auth.core.AffinityGroup.{Agent, Individual, Organisation}
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.retrieve._
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.cbcrfrontend._
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.repositories.CBCSessionCache
import uk.gov.hmrc.cbcrfrontend.services._
import uk.gov.hmrc.cbcrfrontend.views.Views
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}
import uk.gov.hmrc.play.audit.model.ExtendedDataEvent
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import java.io._
import java.net.URI
import java.time.{Duration, LocalDateTime}
import java.util.UUID
import javax.inject.{Inject, Singleton}
import scala.concurrent.duration.{Duration => SDuration}
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

@Singleton
class FileUploadController @Inject()(
  val authConnector: AuthConnector,
  schemaValidator: CBCRXMLValidator,
  businessRuleValidator: CBCBusinessRuleValidator,
  fileUploadService: FileUploadService,
  xmlExtractor: XmlInfoExtract,
  audit: AuditConnector,
  messagesControllerComponents: MessagesControllerComponents,
  views: Views,
  cache: CBCSessionCache,
  config: Configuration)(implicit ec: ExecutionContext, feConfig: FrontendAppConfig)
    extends FrontendController(messagesControllerComponents) with AuthorisedFunctions {

  implicit val credentialsFormat: OFormat[Credentials] = uk.gov.hmrc.cbcrfrontend.controllers.credentialsFormat

  private lazy val hostName = config.get[String]("cbcr-frontend.host")
  private lazy val fileUploadErrorRedirectUrl = s"$hostName${routes.FileUploadController.handleError().url}"
  private lazy val fileUploadHost = config.get[String](s"file-upload-public-frontend.host")
  lazy val logger: Logger = Logger(this.getClass)

  private def fileUploadUrl()(implicit hc: HeaderCarrier): EitherT[Future, CBCErrors, String] =
    for {
      envelope <- fileUploadService.createEnvelope
      envelopeId <- cache
                     .create[EnvelopeId](envelope)
                     .toRight(UnexpectedState("Unable to get envelopeId"))
      fileId <- cache
                 .create[FileId](FileId(UUID.randomUUID.toString))
                 .toRight(UnexpectedState("Unable to get FileId"): CBCErrors)
      successRedirect = s"$hostName${routes.FileUploadController.checkFileUploadStatus(envelopeId.value, fileId.value, hasSeen = "false").url}"
      fileUploadUrl = s"$fileUploadHost/file-upload/upload/envelopes/$envelopeId/files/$fileId?" +
        s"redirect-success-url=$successRedirect&" +
        s"redirect-error-url=$fileUploadErrorRedirectUrl"
    } yield fileUploadUrl

  val chooseXMLFile: Action[AnyContent] = Action.async { implicit request =>
    authorised(AffinityGroup.Organisation or AffinityGroup.Agent).retrieve(Retrievals.affinityGroup and cbcEnrolment) {
      case None ~ _ =>
        errorRedirect(
          UnexpectedState("Unable to query AffinityGroup"),
          views.notAuthorisedIndividual,
          views.errorTemplate)
      case Some(Organisation) ~ None if Await.result(cache.readOption[CBCId].map(_.isEmpty), SDuration(5, "seconds")) =>
        Ok(views.unregisteredGGAccount())
      case Some(Individual) ~ _ => Redirect(routes.SubmissionController.noIndividuals)
      case affinityGroup ~ _ =>
        fileUploadUrl()
          .map(fuu => Ok(views.chooseFile(new URI(fuu), s"oecd-${LocalDateTime.now}-cbcr.xml", affinityGroup)))
          .leftMap((error: CBCErrors) => errorRedirect(error, views.notAuthorisedIndividual, views.errorTemplate))
          .merge
    }
  }

  /**
    * Prepare the page to use to embedded the poller js function. This will have been the redirect url passed to
    * FileUpload during the client side POST.
    *
    * @param envelopeId the envelope just uploaded to.
    * @param fileId the Id of the Xml File just uploaded.
    * @return the view to display the poller.
    */
  def fileUploadProgress(envelopeId: String, fileId: String, hasSeen: String): Action[AnyContent] = Action.async {
    implicit request =>
      authorised() {
        cache
          .read[EnvelopeId]
          .subflatMap { e =>
            if (e.value != envelopeId) {
              logger.error("BAD_ENVELOPE_ID")
              cache.clear
              Left(UnexpectedState(
                s"The envelopeId in the cache was: ${e.value} while the progress request was for $envelopeId"))
            } else {
              Right(Ok(views.fileUploadProgress(envelopeId, fileId, hostName, hasSeen)))
            }
          }
          .leftMap((error: CBCErrors) => errorRedirect(error, views.notAuthorisedIndividual, views.errorTemplate))
          .merge
      }
  }

  private def fileUploadResponseToResult(optResponse: Option[FileUploadCallbackResponse]): Result =
    optResponse
      .map(response =>
        response.status match {
          case "AVAILABLE" => Accepted
          case "ERROR" =>
            response.reason match {
              case Some("VirusDetected") => Conflict
              case _                     => InternalServerError
            }
          case _ => NoContent
      })
      .getOrElse(NoContent)

  private def getMetaData(envelopeId: String, fileId: String)(
    implicit hc: HeaderCarrier): ServiceResponse[FileMetadata] =
    for {
      metadata <- fileUploadService
                   .getFileMetaData(envelopeId, fileId)
                   .subflatMap(_.toRight(UnexpectedState("MetaData File not found")))
      _ <- EitherT.cond[Future](metadata.name.toLowerCase.endsWith(".xml"), (), InvalidFileType(metadata.name))
      _ <- EitherT.right(cache.save(metadata))
    } yield metadata

  def validateBusinessRules(
    file_metadata: (File, FileMetadata),
    enrolment: Option[CBCEnrolment],
    affinityGroup: Option[AffinityGroup])(
    implicit hc: HeaderCarrier): ServiceResponse[Either[NonEmptyList[BusinessRuleErrors], CompleteXMLInfo]] = {
    val startValidation = LocalDateTime.now()

    val rawXmlInfo = xmlExtractor.extract(file_metadata._1)

    val result = for {
      xmlInfo <- EitherT(
                  businessRuleValidator
                    .validateBusinessRules(rawXmlInfo, file_metadata._2.name, enrolment, affinityGroup)
                    .map(_.toEither))
      completeXI <- EitherT(businessRuleValidator.recoverReportingEntity(xmlInfo).map(_.toEither))
    } yield completeXI

    result.value.onComplete {
      case Failure(_) =>
        val endValidation = LocalDateTime.now()
        logger.info(
          s"File validation failed for file ${file_metadata._2.id} (${calculateFileSize(file_metadata._2)}kb) in ${Duration.between(startValidation, endValidation).toMillis} milliseconds ")
      case Success(_) =>
        val endValidation = LocalDateTime.now()
        logger.info(
          s"File validation succeeded for file ${file_metadata._2.id} (${calculateFileSize(file_metadata._2)}kb) in ${Duration.between(startValidation, endValidation).toMillis} milliseconds ")
    }

    EitherT.right(
      result
        .fold(
          errors => cache.save(AllBusinessRuleErrors(errors.toList)).map(_ => Left(errors)),
          info => cache.save(info).flatMap(_ => cache.save(Hash(sha256Hash(file_metadata._1))).map(_ => Right(info)))
        )
        .flatten)
  }

  private def calculateFileSize(md: FileMetadata) =
    (md.length / 1000).setScale(2, BigDecimal.RoundingMode.HALF_UP)

  def fileValidate(envelopeId: String, fileId: String): Action[AnyContent] = Action.async { implicit request =>
    authorised().retrieve(Retrievals.credentials and Retrievals.affinityGroup and cbcEnrolment) {
      case Some(creds) ~ affinity ~ enrolment =>
        val result = for {
          file     <- fileUploadService.getFile(envelopeId, fileId)
          metadata <- getMetaData(envelopeId, fileId)
          _        <- EitherT.right[CBCErrors](cache.save(metadata))
          _        <- EitherT.right[CBCErrors](cache.save(FileDetails(envelopeId, fileId)))
          _        <- EitherT.cond[Future](metadata.name.toLowerCase endsWith ".xml", (), InvalidFileType(metadata.name))
          schemaErrors = schemaValidator.validateSchema(file)
          xmlErrors = XMLErrors.errorHandlerToXmlErrors(schemaErrors)
          schemaSize = if (xmlErrors.errors.nonEmpty) Some(getErrorFileSize(List(xmlErrors))) else None
          _ <- EitherT.right[CBCErrors](cache.save(XMLErrors.errorHandlerToXmlErrors(schemaErrors)))
          _ <- if (!schemaErrors.hasFatalErrors) EitherT.fromEither[Future](().asRight[CBCErrors])
              else
                auditFailedSubmission(creds, affinity, enrolment, "schema validation errors").flatMap(_ =>
                  EitherT.left[Result](Future.successful(FatalSchemaErrors(schemaSize).asInstanceOf[CBCErrors])))
          result <- validateBusinessRules((file, metadata), enrolment, affinity)
          businessSize = result.fold(e => Some(getErrorFileSize(e.toList)), _ => None)
          length = calculateFileSize(metadata)
          _ <- if (schemaErrors.hasErrors) auditFailedSubmission(creds, affinity, enrolment, "schema validation errors")
              else if (result.isLeft) auditFailedSubmission(creds, affinity, enrolment, "business rules errors")
              else EitherT.fromEither[Future](().asRight[CBCErrors])
          _ = java.nio.file.Files.deleteIfExists(file.toPath)
        } yield {
          logger.info(s"FileUpload succeeded - envelopeId: $envelopeId")
          Ok(
            views.fileUploadResult(
              affinity,
              Some(metadata.name),
              Some(length),
              schemaSize,
              businessSize,
              result.map(_.reportingEntity.reportingRole).toOption))
        }

        result
          .leftMap {
            case FatalSchemaErrors(size) =>
              Ok(views.fileUploadResult(None, None, None, size, None, None))
            case InvalidFileType(_) =>
              Redirect(routes.FileUploadController.fileInvalid)
            case e: CBCErrors =>
              logger.error(e.toString)
              Redirect(routes.SharedController.technicalDifficulties)
          }
          .merge
          .recover {
            case NonFatal(e) =>
              logger.error(e.getMessage, e)
              Redirect(routes.SharedController.technicalDifficulties)
          }
      case None ~ _ ~ _ => Unauthorized
    }
  }

  private def getErrorFileSize(e: List[ValidationErrors])(implicit messages: Messages) = {
    val f = fileUploadService.errorsToFile(e, "")
    val kb = f.length() * 0.001
    f.delete()
    Math.incrementExact(kb.toInt)
  }

  private def fileUploadName(fname: String)(implicit messages: Messages) =
    messages(fname)

  def getBusinessRuleErrors: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      OptionT(cache.readOption[AllBusinessRuleErrors])
        .map(x => fileUploadService.errorsToFile(x.errors, fileUploadName("fileUpload.BusinessRuleErrors")))
        .fold(
          NoContent
        )((file: File) => Ok.sendFile(content = file, inline = false, onClose = () => file.delete()))
    }
  }

  def getXmlSchemaErrors: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      OptionT(cache.readOption[XMLErrors])
        .map(x => fileUploadService.errorsToFile(List(x), fileUploadName("fileUpload.XMLSchemaErrors")))
        .fold(
          NoContent
        )((file: File) => Ok.sendFile(content = file, inline = false, onClose = () => file.delete()))
    }
  }

  def fileInvalid: Action[AnyContent] = fileUploadError(FileNotXml)
  def fileTooLarge: Action[AnyContent] = fileUploadError(FileTooLarge)
  def fileContainsVirus: Action[AnyContent] = fileUploadError(FileContainsVirus)
  def uploadTimedOut: Action[AnyContent] = fileUploadError(UploadTimedOut)

  private def fileUploadError(errorType: FileUploadErrorType) = Action.async { implicit request =>
    authorised().retrieve(Retrievals.credentials and Retrievals.affinityGroup and cbcEnrolment) {
      case Some(creds) ~ affinity ~ enrolment =>
        auditFailedSubmission(creds, affinity, enrolment, errorType.toString)
          .map(_ => Ok(views.fileUploadError(errorType)))
          .leftMap((error: CBCErrors) => errorRedirect(error, views.notAuthorisedIndividual, views.errorTemplate))
          .merge
      case None ~ _ ~ _ => Unauthorized
    }
  }

  def handleError(errorCode: Int, reason: String): Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      logger.error(s"Error response received from FileUpload callback - ErrorCode: $errorCode - Reason $reason")
      errorCode match {
        case REQUEST_ENTITY_TOO_LARGE => Redirect(routes.FileUploadController.fileTooLarge)
        case UNSUPPORTED_MEDIA_TYPE   => Redirect(routes.FileUploadController.fileInvalid)
        case REQUEST_TIMEOUT          => Redirect(routes.FileUploadController.uploadTimedOut)
        case _                        => Redirect(routes.SharedController.technicalDifficulties)
      }
    }
  }

  // Provided for the poller js function see if FileUpload have called us back and the file is ready for the next stage
  // (validate)
  def fileUploadResponse(envelopeId: String): Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      logger.info(s"Received a file-upload-response query for $envelopeId")
      fileUploadService
        .getFileUploadResponse(envelopeId)
        .fold(
          error => {
            logger.error(s"File not ready: $error")
            NoContent
          },
          response => {
            logger.info(s"Response back was: $response")
            fileUploadResponseToResult(response)
          }
        )
    }
  }

  def checkFileUploadStatus(envelopeId: String, fileId: String, hasSeen: String): Action[AnyContent] = Action.async {
    implicit request =>
      authorised() {
        logger.info(s"Received a file-upload-response query for $envelopeId")
        fileUploadService
          .getFileUploadResponse(envelopeId)
          .fold(
            error => {
              logger.error(s"File not ready: $error")
              Redirect(routes.FileUploadController.fileUploadProgress(envelopeId, fileId, hasSeen))
            },
            optResponse => {
              logger.info(s"Response back was: $optResponse")
              optResponse match {
                case Some(response) =>
                  response.status match {
                    case "AVAILABLE" => Redirect(routes.FileUploadController.fileValidate(envelopeId, fileId))
                    case "ERROR" =>
                      response.reason match {
                        case Some("VirusDetected") => Redirect(routes.FileUploadController.fileContainsVirus)
                        case _                     => Redirect(routes.FileUploadController.handleError())
                      }
                    case _ => Redirect(routes.FileUploadController.fileUploadProgress(envelopeId, fileId, hasSeen))
                  }
                case _ => Redirect(routes.FileUploadController.fileUploadProgress(envelopeId, fileId, hasSeen))
              }
            }
          )
      }
  }

  //Turn a Case class into a map
  private[controllers] def getCCParams(cc: AnyRef): Map[String, String] =
    cc.getClass.getDeclaredFields.foldLeft[Map[String, String]](Map.empty) { (acc, field) =>
      field.setAccessible(true)
      acc + (field.getName -> field.get(cc).toString)
    }

  private def auditDetailAffinity(affinity: AffinityGroup, cbc: Option[CBCId], utr: Option[Utr]): JsObject =
    affinity match {
      case Organisation =>
        Json.obj(
          "affinityGroup" -> Json.toJson(affinity),
          "utr"           -> JsString(utr.map(_.toString).getOrElse("none retrieved")),
          "cbcId"         -> JsString(cbc.map(_.toString).getOrElse("none retrieved"))
        )
      case Agent => Json.obj("affinityGroup" -> Json.toJson(affinity))
      case _     => Json.obj("affinityGroup" -> "none retrieved")
    }

  private def auditDetailErrors(all_errors: (Option[AllBusinessRuleErrors], Option[XMLErrors]))(
    implicit messages: Messages): JsObject =
    (
      all_errors._1.exists(bre => if (bre.errors.isEmpty) false else true),
      all_errors._2.exists(xml => if (xml.errors.isEmpty) false else true)) match {
      case (true, true) =>
        Json.obj(
          "businessRuleErrors" -> Json.toJson(fileUploadService.errorsToMap(all_errors._1.get.errors)),
          "xmlErrors"          -> Json.toJson(fileUploadService.errorsToMap(List(all_errors._2.get)))
        )
      case (true, false) =>
        Json.obj("businessRuleErrors" -> Json.toJson(fileUploadService.errorsToMap(all_errors._1.get.errors)))
      case (false, true) => Json.obj("xmlErrors" -> Json.toJson(fileUploadService.errorsToMap(List(all_errors._2.get))))
      case _             => Json.obj("none"      -> "no business rule or schema errors")
    }

  def auditFailedSubmission(
    creds: Credentials,
    affinity: Option[AffinityGroup],
    enrolment: Option[CBCEnrolment],
    reason: String)(
    implicit hc: HeaderCarrier,
    request: Request[_],
    messages: Messages): ServiceResponse[AuditResult.Success.type] =
    for {
      md             <- EitherT.right[CBCErrors](cache.readOption[FileMetadata])
      businessErrors <- EitherT.right[CBCErrors](cache.readOption[AllBusinessRuleErrors])
      xmlErrors      <- EitherT.right[CBCErrors](cache.readOption[XMLErrors])
      c              <- EitherT.right[CBCErrors](cache.readOption[CBCId])
      cbcId = if (enrolment.isEmpty) c else Option(enrolment.get.cbcId)
      u <- EitherT.right[CBCErrors](cache.readOption[Utr])
      utr = if (enrolment.isEmpty) u else Option(enrolment.get.utr)
      result <- EitherT(
                 audit
                   .sendExtendedEvent(ExtendedDataEvent(
                     "Country-By-Country-Frontend",
                     "CBCRFilingFailed",
                     detail = Json.obj(
                       "reason"        -> JsString(reason),
                       "path"          -> JsString(request.uri),
                       "file metadata" -> Json.toJson(md.map(getCCParams).getOrElse(Map.empty[String, String])),
                       "creds"         -> Json.toJson(creds),
                       "registration"  -> auditDetailAffinity(affinity.get, cbcId, utr),
                       "errorTypes"    -> auditDetailErrors((businessErrors, xmlErrors))
                     )
                   ))
                   .map {
                     case AuditResult.Success => Right(AuditResult.Success)
                     case AuditResult.Failure(msg, _) =>
                       Left(UnexpectedState(s"Unable to audit a failed submission: $msg").asInstanceOf[CBCErrors])
                     case AuditResult.Disabled => Right(AuditResult.Success)
                   })
    } yield result

  val unregisteredGGAccount: Action[AnyContent] = Action.async { implicit request =>
    authorised(AffinityGroup.Organisation and User) {
      fileUploadUrl()
        .map(fuu =>
          Ok(views.chooseFile(new URI(fuu), s"oecd-${LocalDateTime.now}-cbcr.xml", Some(AffinityGroup.Organisation))))
        .leftMap((error: CBCErrors) => errorRedirect(error, views.notAuthorisedIndividual, views.errorTemplate))
        .merge
    }
  }
}
