/*
 * Copyright 2018 HM Revenue & Customs
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
import java.time.{Duration, LocalDateTime}
import java.util.UUID

import javax.inject.{Inject, Singleton}
import cats.data.EitherT
import cats.data._
import cats.instances.all._
import cats.syntax.all._
import org.joda.time.Period
import play.api.i18n.{I18nSupport, Lang, MessagesApi}
import play.api.i18n.Messages.Implicits._
import play.api.libs.Files
import play.api.libs.json._
import play.api.mvc._
import play.api.{Configuration, Environment, Logger}
import uk.gov.hmrc.auth.core.AffinityGroup.{Agent, Individual, Organisation}
import uk.gov.hmrc.auth.core.retrieve.{Retrievals, _}
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.cbcrfrontend._
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.services._
import uk.gov.hmrc.cbcrfrontend.typesclasses.{CbcrsUrl, FusFeUrl, FusUrl, ServiceUrl}
import uk.gov.hmrc.cbcrfrontend.views.html._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.cache.client.CacheMap
import uk.gov.hmrc.play.audit.AuditExtensions._
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}
import uk.gov.hmrc.play.audit.model.ExtendedDataEvent
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import uk.gov.hmrc.play.config.ServicesConfig

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.control.NonFatal
import play.api.libs.json.Json

import scala.concurrent.duration.{Duration => SDuration}
import scala.util.{Failure, Success}


@Singleton
class FileUploadController @Inject()(val messagesApi:MessagesApi,
                                     val authConnector:AuthConnector,
                                     val schemaValidator: CBCRXMLValidator,
                                     val businessRuleValidator: CBCBusinessRuleValidator,
                                     val fileUploadService:FileUploadService,
                                     val xmlExtractor:XmlInfoExtract,
                                     val audit: AuditConnector,
                                     val rrService: DeEnrolReEnrolService,
                                     val env:Environment)
                                    (implicit ec: ExecutionContext, cache:CBCSessionCache, val config:Configuration, feConfig:FrontendAppConfig) extends FrontendController with AuthorisedFunctions with I18nSupport{

  implicit val credentialsFormat = uk.gov.hmrc.cbcrfrontend.controllers.credentialsFormat

  val assetsLocation = (config.getString(s"assets.url") |+| config.getString(s"assets.version")).get
  lazy val hostName = config.getString("cbcr-frontend.host").get
  lazy val fileUploadErrorRedirectUrl = s"$hostName${routes.FileUploadController.handleError().url}"
  lazy val fileUploadHost             = config.getString(s"file-upload-public-frontend.host").get

  private def allowedToSubmit(affinityGroup:AffinityGroup, enrolled:Boolean)(implicit hc: HeaderCarrier) = affinityGroup match {
    case AffinityGroup.Organisation => if(enrolled) { Future.successful(true) } else { cache.readOption[CBCId].map(_.isDefined) }
    case AffinityGroup.Agent        => Future.successful(true)
    case AffinityGroup.Individual   => Future.successful(false)
  }

  def auditDeEnrolReEnrolEvent(enrolment: CBCEnrolment,result:ServiceResponse[CBCId])(implicit request:Request[AnyContent]) : ServiceResponse[CBCId] = {
    EitherT(result.value.flatMap { e =>
      audit.sendExtendedEvent(ExtendedDataEvent("Country-By-Country-Frontend", "CBCR-DeEnrolReEnrol",
        detail = Json.obj(
          "path"     -> JsString(request.uri),
          "newCBCId" -> JsString(e.map(_.value).getOrElse("Failed to get new CBCId")),
          "oldCBCId" -> JsString(enrolment.cbcId.value),
          "utr"      -> JsString(enrolment.utr.utr)
        )
      )).map {
        case AuditResult.Success         => e
        case AuditResult.Failure(msg, _) => Left(UnexpectedState(s"Unable to audit a successful submission: $msg"))
        case AuditResult.Disabled        => e
      }
    })
  }

  private def isEn()(implicit hc: HeaderCarrier):Future[Boolean] = {
    val enrolled = cache.readOption[CBCId].map(_.isDefined)
    enrolled
  }


  val chooseXMLFile = Action.async{ implicit request =>
    authorised(AffinityGroup.Organisation or AffinityGroup.Agent).retrieve(Retrievals.affinityGroup and cbcEnrolment){
      case None               ~ _                    => errorRedirect(UnexpectedState("Unable to query AffinityGroup"))
      case Some(Organisation) ~ None
        if Await.result(cache.readOption[CBCId].map(_.isEmpty
        ), SDuration(5, "seconds"))                   => Redirect(routes.SubmissionController.notRegistered())
      case Some(Organisation) ~ Some(enrolment)
        if CBCId.isPrivateBetaCBCId(enrolment.cbcId) =>
        auditDeEnrolReEnrolEvent(enrolment, rrService.deEnrolReEnrol(enrolment)).map(
          (id: CBCId) => Ok(shared.regenerate(id))
        ).leftMap(errorRedirect).merge
      case Some(Individual) ~ _                      => Redirect(routes.SubmissionController.noIndividuals())
      case _ ~ _                                     => (for {
        envelopeId <- cache.readOrCreate[EnvelopeId](fileUploadService.createEnvelope.toOption).toRight(UnexpectedState("Unable to get envelopeId"))
        fileId     <- cache.readOrCreate[FileId](OptionT.liftF(Future.successful(FileId(UUID.randomUUID.toString)))).toRight(UnexpectedState("Unable to get FileId"): CBCErrors)
        successRedirect = s"$hostName${routes.FileUploadController.fileUploadProgress(envelopeId.value, fileId.value).url}"
        fileUploadUrl   = s"$fileUploadHost/file-upload/upload/envelopes/$envelopeId/files/$fileId?" +
          s"redirect-success-url=$successRedirect&" +
          s"redirect-error-url=$fileUploadErrorRedirectUrl"
        fileName        = s"oecd-${LocalDateTime.now}-cbcr.xml"
      } yield Ok(submission.fileupload.chooseFile(fileUploadUrl, fileName))).leftMap(errorRedirect).merge

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
  def fileUploadProgress(envelopeId: String, fileId: String) = Action.async{ implicit request =>
    authorised() {
      cache.read[EnvelopeId].subflatMap { e =>
        if (e.value != envelopeId) {
          Logger.error("BAD_ENVELOPE_ID")
          cache.remove()
          Left(UnexpectedState(s"The envelopeId in the cache was: ${e.value} while the progress request was for $envelopeId"))
        } else {
          Right(Ok(submission.fileupload.fileUploadProgress(
            envelopeId, fileId, hostName, assetsLocation)))
        }
      }.leftMap(errorRedirect).merge
    }
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

  def validateBusinessRules(file_metadata:(File,FileMetadata), enrolment: Option[CBCEnrolment], affinityGroup: Option[AffinityGroup])(implicit hc:HeaderCarrier): ServiceResponse[Either[NonEmptyList[BusinessRuleErrors],CompleteXMLInfo]] = {
    val startValidation = LocalDateTime.now()

    val rawXmlInfo  = xmlExtractor.extract(file_metadata._1)

    val result = for {
      xmlInfo         <- EitherT(businessRuleValidator.validateBusinessRules(rawXmlInfo, file_metadata._2.name, enrolment, affinityGroup).map(_.toEither))
      completeXI      <- EitherT(businessRuleValidator.recoverReportingEntity(xmlInfo).map(_.toEither))
    } yield completeXI

    result.value.onComplete{
      case Failure(_) =>
        val endValidation = LocalDateTime.now()
        Logger.info(s"File validation failed for file ${file_metadata._2.id} (${calculateFileSize(file_metadata._2)}kb) in ${Duration.between(startValidation,endValidation).toMillis} milliseconds ")
      case Success(_) =>
        val endValidation = LocalDateTime.now()
        Logger.info(s"File validation succeeded for file ${file_metadata._2.id} (${calculateFileSize(file_metadata._2)}kb) in ${Duration.between(startValidation,endValidation).toMillis} milliseconds ")

    }

    EitherT.right[Future,CBCErrors,Either[NonEmptyList[BusinessRuleErrors],CompleteXMLInfo]](result.fold(
      errors => cache.save(AllBusinessRuleErrors(errors.toList)).map(_ => Left(errors)),
      info   => cache.save(info).flatMap(_ => cache.save(Hash(sha256Hash(file_metadata._1))).map(_ => Right(info)))
    ).flatten)
  }

  def calculateFileSize(md:FileMetadata) :  BigDecimal =
    (md.length/1000).setScale(2, BigDecimal.RoundingMode.HALF_UP)


  def fileValidate(envelopeId: String, fileId: String) = Action.async { implicit request =>
    authorised().retrieve(Retrievals.credentials and Retrievals.affinityGroup and cbcEnrolment) { case creds ~ affinity ~ enrolment =>

      val result = for {
        file_metadata <- (fileUploadService.getFile(envelopeId, fileId) |@| getMetaData(envelopeId, fileId)).tupled
        _             <- right(cache.save(file_metadata._2))
        _             <- EitherT.cond[Future](file_metadata._2.name endsWith ".xml", (), InvalidFileType(file_metadata._2.name))
        schemaErrors   = schemaValidator.validateSchema(file_metadata._1)
        xmlErrors      = XMLErrors.errorHandlerToXmlErrors(schemaErrors)
        schemaSize     = if (xmlErrors.errors.nonEmpty) Some(getErrorFileSize(List(xmlErrors))) else None
        _             <- EitherT.right[Future, CBCErrors, CacheMap](cache.save(XMLErrors.errorHandlerToXmlErrors(schemaErrors)))
        _             <- if (!schemaErrors.hasFatalErrors) EitherT.pure[Future, CBCErrors, Unit](())
                         else auditFailedSubmission(creds, affinity, enrolment, "schema validation errors").flatMap(_ =>
                           EitherT.left[Future, CBCErrors, Unit](Future.successful(FatalSchemaErrors(schemaSize)))
                         )
        result        <- validateBusinessRules(file_metadata, enrolment, affinity)
        businessSize   = result.fold(e => Some(getErrorFileSize(e.toList)), _ => None)
        length         = calculateFileSize(file_metadata._2)
        _             <- if (schemaErrors.hasErrors) auditFailedSubmission(creds, affinity, enrolment, "schema validation errors")
                         else if (result.isLeft) auditFailedSubmission(creds, affinity, enrolment, "business rules errors")
                         else EitherT.pure[Future, CBCErrors, Unit](())
        _              = java.nio.file.Files.deleteIfExists(file_metadata._1.toPath)
      } yield Ok(submission.fileupload.fileUploadResult(affinity, Some(file_metadata._2.name), Some(length), schemaSize, businessSize,  result.map(_.reportingEntity.reportingRole).toOption))


      result.leftMap {
        case FatalSchemaErrors(size) =>
          Ok(submission.fileupload.fileUploadResult(None, None, None, size, None,  None))
        case InvalidFileType(_) =>
          Redirect(routes.FileUploadController.fileInvalid())
        case e: CBCErrors =>
          Logger.error(e.toString)
          Redirect(routes.SharedController.technicalDifficulties())
      }.merge.recover {
        case NonFatal(e) =>
          Logger.error(e.getMessage, e)
          Redirect(routes.SharedController.technicalDifficulties())
      }

    }
  }

  private def getErrorFileSize(e:List[ValidationErrors]) : Int = {
    val f = errorsToFile(e,"")
    val kb = f.length() * 0.001
    f.delete()
    Math.incrementExact(kb.toInt)
  }


  private def errorsToList(e:List[ValidationErrors]) : List[String] =
    e.map(x => x.show.split(" ").map(x => messagesApi(x)).map(_.toString).mkString(" "))


  private def errorsToMap(e:List[ValidationErrors]) : Map[String,String] =
    (errorsToList(e) map {x => ("error_"+x.indexOf(x).toString, x)}).toMap


  private def errorsToString(e:List[ValidationErrors]) : String =
    errorsToList(e).map(_.toString).mkString("\r\n")


  private def errorsToFile(e:List[ValidationErrors], name:String)(implicit lang: Lang) : File = {
    val b = Files.TemporaryFile(name, ".txt")
    val writer = new PrintWriter(b.file)
    writer.write(errorsToString(e))
    writer.flush()
    writer.close()
    b.file
  }


  private def fileUploadName(fname: String)(implicit lang: Lang) : String = {
    messagesApi(fname)
  }


  def getBusinessRuleErrors = Action.async{ implicit request =>
    authorised() {
      OptionT(cache.readOption[AllBusinessRuleErrors]).map(x => errorsToFile(x.errors, fileUploadName("fileUpload.BusinessRuleErrors"))
      ).fold(
        NoContent
      )((file: File) =>
        Ok.sendFile(content = file, inline = false, onClose = () => file.delete())
      )
    }
  }

  def getXmlSchemaErrors = Action.async{ implicit request =>
    authorised() {
      OptionT(cache.readOption[XMLErrors]).map(x => errorsToFile(List(x), fileUploadName("fileUpload.XMLSchemaErrors"))
      ).fold(
        NoContent
      )((file: File) =>
        Ok.sendFile(content = file, inline = false, onClose = () => file.delete())
      )
    }
  }

  def fileInvalid       = fileUploadError(FileNotXml)
  def fileTooLarge      = fileUploadError(FileTooLarge)
  def fileContainsVirus = fileUploadError(FileContainsVirus)

  private def fileUploadError(errorType:FileUploadErrorType) = Action.async{ implicit request =>
    authorised().retrieve(Retrievals.credentials and Retrievals.affinityGroup and cbcEnrolment) { case creds ~ affinity  ~ enrolment =>
      auditFailedSubmission(creds, affinity, enrolment, errorType.toString).map(_ =>
        Ok(submission.fileupload.fileUploadError(errorType))
      ).leftMap(errorRedirect).merge
    }
  }

  def handleError(errorCode:Int, reason:String) = Action.async { implicit request =>
    authorised() {
      Logger.error(s"Error response received from FileUpload callback - ErrorCode: $errorCode - Reason $reason")
      errorCode match {
        case REQUEST_ENTITY_TOO_LARGE => Redirect(routes.FileUploadController.fileTooLarge())
        case UNSUPPORTED_MEDIA_TYPE => Redirect(routes.FileUploadController.fileInvalid())
        case _ => Redirect(routes.SharedController.technicalDifficulties())
      }
    }
  }

  /**
    * Provided for the poller js function see if FileUpload have called us back and the file is ready for the next
    * stage (validate)
    * @param envelopeId
    * @return
    */
  def fileUploadResponse(envelopeId: String) = Action.async{ implicit request =>
    authorised() {
      Logger.info(s"Received a file-upload-response query for $envelopeId")
      fileUploadService.getFileUploadResponse(envelopeId).fold(
        error => {
          Logger.error(s"File not ready: $error")
          NoContent
        },
        response => {
          Logger.info(s"Response back was: $response")
          fileUploadResponseToResult(response)
        }
      )
    }
  }

  //Turn a Case class into a map
  private[controllers] def getCCParams(cc: AnyRef): Map[String, String] =
    (Map[String, String]() /: cc.getClass.getDeclaredFields) {(acc, field) =>
      field.setAccessible(true)
      acc + (field.getName -> field.get(cc).toString)
    }


  private def auditDetailAffinity(affinity: AffinityGroup, cbc: Option[CBCId],utr: Option[Utr]) : JsObject = {

    affinity match {
      case Organisation => Json.obj(
        "affinityGroup" -> Json.toJson(affinity),
        "utr"   -> JsString(utr.getOrElse("none retrieved").toString),
        "cbcId" -> JsString(cbc.getOrElse("none retrieved").toString)
      )
      case Agent        => Json.obj("affinityGroup" -> Json.toJson(affinity))
      case _            => Json.obj("affinityGroup" -> "none retrieved")
    }
  }


  private def auditDetailErrors(all_errors: (Option[AllBusinessRuleErrors], Option[XMLErrors]))(implicit hc:HeaderCarrier) : JsObject = {
    (all_errors._1.map(bre => bre.errors.nonEmpty), all_errors._2.map(xml => xml.errors.nonEmpty)) match {
      case (Some(true), Some(true)) => Json.obj(
        "businessRuleErrors" -> Json.toJson(errorsToMap(all_errors._1.get.errors)),
        "xmlErrors" -> Json.toJson(errorsToMap(List(all_errors._2.get)))
      )
      case (Some(true), Some(false)) => Json.obj("businessRuleErrors" -> Json.toJson(errorsToMap(all_errors._1.get.errors)))
      case (Some(false), Some(true)) => Json.obj("xmlErrors" -> Json.toJson(errorsToMap(List(all_errors._2.get))))
      case _                         => Json.obj("none" -> "no business rule or schema errors")

    }
  }


  def auditFailedSubmission(creds: Credentials,
                            affinity: Option[AffinityGroup],
                            enrolment: Option[CBCEnrolment],
                            reason:String)(implicit hc:HeaderCarrier, request:Request[_]): ServiceResponse[AuditResult.Success.type] = {
    for {
      md        <- right(cache.readOption[FileMetadata])
      all_error <- (right(cache.readOption[AllBusinessRuleErrors]) |@| right(cache.readOption[XMLErrors])).tupled
      c         <- right(cache.readOption[CBCId])
      cbcId     =  if(enrolment.isEmpty) c else Option(enrolment.get.cbcId)
      u         <- right(cache.readOption[Utr])
      utr       =  if(enrolment.isEmpty) u else Option(enrolment.get.utr)
      auditDetail = Json.obj(
        "reason"       -> JsString(reason),
        "path"                 -> JsString(request.uri),
        "file metadata"        -> Json.toJson(md.map(getCCParams).getOrElse(Map.empty[String,String])),
        "creds"                -> Json.toJson(creds),
        "registration"         -> auditDetailAffinity(affinity.get, cbcId, utr),
        "errors"               -> auditDetailErrors(all_error)
      )
      result    <- eitherT[AuditResult.Success.type](audit.sendExtendedEvent(ExtendedDataEvent("Country-By-Country-Frontend", "CBCRFilingFailed",
        detail = auditDetail
      )).map {
        case AuditResult.Success => Right(AuditResult.Success)
        case AuditResult.Failure(msg, _) => Left(UnexpectedState(s"Unable to audit a failed submission: $msg"))
        case AuditResult.Disabled => Right(AuditResult.Success)
      })
    } yield result
  }
}
