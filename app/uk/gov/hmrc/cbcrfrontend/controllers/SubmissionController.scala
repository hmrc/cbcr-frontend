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

import java.time.{Duration, LocalDateTime}
import java.time.format.DateTimeFormatter

import cats.data.{EitherT, NonEmptyList, OptionT}
import cats.instances.all._
import cats.syntax.all._
import javax.inject.{Inject, Singleton}
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.libs.json.{JsString, Json}
import play.api.mvc.{AnyContent, MessagesControllerComponents, Request, Result}
import play.api.{Configuration, Environment, Logger}
import uk.gov.hmrc.auth.core.AffinityGroup.{Agent, Organisation}
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.retrieve.Credentials
import uk.gov.hmrc.cbcrfrontend._
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.form.SubmitterInfoForm.submitterInfoForm
import uk.gov.hmrc.cbcrfrontend.model.{ConfirmationEmailSent, SummaryData, _}
import uk.gov.hmrc.cbcrfrontend.services.{CBCSessionCache, DocRefIdService, FileUploadService, ReportingEntityDataService, _}
import uk.gov.hmrc.cbcrfrontend.views.Views
import uk.gov.hmrc.emailaddress.EmailAddress
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.cache.client.CacheMap
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}
import uk.gov.hmrc.play.audit.model.ExtendedDataEvent
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.Exception.nonFatalCatch
import scala.util.control.NonFatal

@Singleton
class SubmissionController @Inject()(
  override val messagesApi: MessagesApi,
  val fus: FileUploadService,
  val docRefIdService: DocRefIdService,
  val reportingEntityDataService: ReportingEntityDataService,
  val messageRefIdService: MessageRefIdService,
  val cbidService: CBCIdService,
  val audit: AuditConnector,
  val env: Environment,
  val authConnector: AuthConnector,
  val emailService: EmailService,
  messagesControllerComponents: MessagesControllerComponents,
  views: Views)(
  implicit ec: ExecutionContext,
  cache: CBCSessionCache,
  val config: Configuration,
  feConfig: FrontendAppConfig)
    extends FrontendController(messagesControllerComponents) with AuthorisedFunctions with I18nSupport {

  implicit val credentialsFormat = uk.gov.hmrc.cbcrfrontend.controllers.credentialsFormat

  val dateFormat = DateTimeFormatter.ofPattern("dd MMMM yyyy 'at' h:mma")

  lazy val logger: Logger = Logger(this.getClass)

  def saveDocRefIds(x: CompleteXMLInfo)(
    implicit hc: HeaderCarrier): EitherT[Future, NonEmptyList[UnexpectedState], Unit] = {
    val cbcReportIds = x.cbcReport.map(reports => reports.docSpec.docRefId           -> reports.docSpec.corrDocRefId)
    val additionalInfoIds = x.additionalInfo.map(addInfo => addInfo.docSpec.docRefId -> addInfo.docSpec.corrDocRefId)

    val allIds = cbcReportIds ++ List(additionalInfoIds).flatten

    // The result of saving these DocRefIds/CorrDocRefIds from the cbcReports
    val result = NonEmptyList
      .fromList(allIds)
      .map(_.map {
        case (doc, corr) =>
          corr.map(docRefIdService.saveCorrDocRefID(_, doc)).getOrElse(docRefIdService.saveDocRefId(doc))
      }.sequence[({ type λ[α] = OptionT[Future, α] })#λ, UnexpectedState])
      .getOrElse(OptionT.none[Future, NonEmptyList[UnexpectedState]])

    x.reportingEntity.docSpec.docType match {
      case OECD0 => result.toLeft(())

      case OECD1 | OECD2 | OECD3 =>
        val reDocRef = x.reportingEntity.docSpec.docRefId
        val reCorr = x.reportingEntity.docSpec.corrDocRefId

        val reResult = reCorr
          .map(c => docRefIdService.saveCorrDocRefID(c, reDocRef))
          .getOrElse(docRefIdService.saveDocRefId(reDocRef))

        EitherT((result.toLeft(()).toValidated |@| reResult.toLeft(()).toValidatedNel).map((a, b) =>
          (a |@| b).map((_, _) => ()).toEither))
    }
  }

  def storeOrUpdateReportingEntityData(xml: CompleteXMLInfo)(implicit hc: HeaderCarrier): ServiceResponse[Unit] =
    xml.reportingEntity.docSpec.docType match {
      // RESENT| NEW
      case OECD1 =>
        ReportingEntityData
          .extract(xml)
          .fold[ServiceResponse[Unit]](
            errors => {
              EitherT.left(Future.successful(
                UnexpectedState(
                  s"Unable to submit partially completed data when docType is ${xml.reportingEntity.docSpec.docType}\n${errors.toList
                    .mkString("\n")}")
              ))
            },
            (data: ReportingEntityData) =>
              EitherT(
                reportingEntityDataService
                  .saveReportingEntityData(data)
                  .value
                  .recover {
                    case e =>
                      logger.error("CBCR_UNVERIFIED_UPLOAD Failed to save reporting entity data. UTR: " + data.tin)
                      Left(UnexpectedState("Failed to save reporting entity data"))
                  })
          )
      case OECD0 | OECD2 | OECD3 =>
        EitherT(
          reportingEntityDataService
            .updateReportingEntityData(PartialReportingEntityData.extract(xml))
            .value
            .recover {
              case e =>
                logger.error("CBCR_UNVERIFIED_UPLOAD Failed to update reporting entity data")
                Left(UnexpectedState("Failed to update reporting entity data"))
            })
    }

  def confirm = Action.async { implicit request =>
    authorised().retrieve(Retrievals.credentials and Retrievals.affinityGroup) { retrieval =>
      (for {
        summaryData <- cache.read[SummaryData]
        xml         <- cache.read[CompleteXMLInfo]
        _           <- fus.uploadMetadataAndRoute(summaryData.submissionMetaData)
        userType <- (for {
                     _ <- saveDocRefIds(xml).leftMap[CBCErrors] { es =>
                           logger.error(s"Errors saving Corr/DocRefIds : ${es.map(_.errorMsg).toList.mkString("\n")}")
                           UnexpectedState("Errors in saving Corr/DocRefIds aborting submission")
                         }
                     _ <- messageRefIdService.saveMessageRefId(xml.messageSpec.messageRefID).toLeft {
                           logger.error(s"Errors saving MessageRefId")
                           UnexpectedState("Errors in saving MessageRefId aborting submission")
                         }
                     _ <- right(cache.save(SubmissionDate(LocalDateTime.now)))
                     _ <- storeOrUpdateReportingEntityData(xml)
                     _ = createSuccessfulSubmissionAuditEvent(retrieval.a.get, summaryData).value.map {
                       case Left(_) =>
                         logger.error("create SuccessfulSubmissionAuditEvent failed.")
                     }
                   } yield
                     retrieval.b match {
                       case Some(Agent) => "Agent"
                       case _           => "Other"
                     })
                     .leftMap(error => {
                       logger.error(
                         "CBCR_UNVERIFIED_UPLOAD Error occurred after uploading a file. State may be inconsistent. UTR: " + summaryData.submissionMetaData.submissionInfo.tin)
                       error
                     })
      } yield Redirect(routes.SubmissionController.submitSuccessReceipt(userType)))
        .leftMap((error: CBCErrors) => errorRedirect(error, views.notAuthorisedIndividual, views.errorTemplate))
        .merge
    }
  }

  def notRegistered = Action.async { implicit request =>
    authorised(AffinityGroup.Organisation and User) {
      Ok(views.notRegistered())
    }
  }

  def noIndividuals = Action.async { implicit request =>
    authorised(AffinityGroup.Individual) {
      Ok(views.notAuthorisedIndividual())
    }
  }
  def noAssistants = Action.async { implicit request =>
    authorised(AffinityGroup.Organisation and Assistant) {
      Ok(views.notAuthorisedAssistant())
    }
  }

  def createSuccessfulSubmissionAuditEvent(creds: Credentials, summaryData: SummaryData)(
    implicit hc: HeaderCarrier,
    request: Request[_]): ServiceResponse[AuditResult.Success.type] = {

    implicit val format = Json.format[ExtendedDataEvent]

    val auditEvent = ExtendedDataEvent(
      "Country-By-Country-Frontend",
      "CBCRFilingSuccessful",
      detail = Json.obj(
        "path"        -> JsString(request.uri),
        "summaryData" -> Json.toJson(summaryData),
        "creds"       -> Json.toJson(creds)
      )
    )

    val auditEventLength: Int = Json.stringify(Json.toJson(auditEvent)).getBytes("UTF-8").length
    // Datastream allows maximum 500kb
    val dataStreamLimit: Int = 500000

    val validAuditEvent =
      if (auditEventLength > dataStreamLimit) {
        val trimmmedSummaryData = summaryData.copy(
          xmlInfo = summaryData.xmlInfo.copy(
            additionalInfo = List.empty
          )
        )
        ExtendedDataEvent(
          "Country-By-Country-Frontend",
          "CBCRFilingSuccessful",
          detail = Json.obj(
            "path"        -> JsString(request.uri),
            "summaryData" -> Json.toJson(trimmmedSummaryData),
            "creds"       -> Json.toJson(creds)
          )
        )
      } else {
        auditEvent
      }

    for {
      result <- eitherT[AuditResult.Success.type](
                 audit
                   .sendExtendedEvent(validAuditEvent)
                   .map {
                     case AuditResult.Success => Right(AuditResult.Success)
                     case AuditResult.Failure(msg, _) =>
                       Left(UnexpectedState(s"Unable to audit a successful submission: $msg"))
                     case AuditResult.Disabled => Right(AuditResult.Success)
                   })
    } yield result
  }

  val utrForm: Form[Utr] = Form(
    mapping(
      "utr" -> nonEmptyText.verifying(s => Utr(s).isValid)
    )(Utr.apply)(Utr.unapply)
  )

  val ultimateParentEntityForm: Form[UltimateParentEntity] = Form(
    mapping("ultimateParentEntity" -> nonEmptyText)(UltimateParentEntity.apply)(UltimateParentEntity.unapply)
  )

  val reconfirmEmailForm: Form[EmailAddress] = Form(
    mapping(
      "reconfirmEmail" -> email.verifying(EmailAddress.isValid(_))
    )(EmailAddress.apply)(EmailAddress.unapply)
  )

  val enterCompanyNameForm: Form[AgencyBusinessName] = Form(
    single(
      "companyName" -> nonEmptyText
    ).transform[AgencyBusinessName](AgencyBusinessName(_), _.name)
  )

  val upe = Action.async { implicit request =>
    authorised() {
      Ok(
        views.submitInfoUltimateParentEntity(
          ultimateParentEntityForm
        ))
    }
  }

  val submitUltimateParentEntity = Action.async(parse.formUrlEncoded) { implicit request =>
    authorised().retrieve(Retrievals.affinityGroup) { userType =>
      (for {
        reportingRole <- cache.read[CompleteXMLInfo].map(_.reportingEntity.reportingRole)
        redirect <- right(
                     ultimateParentEntityForm.bindFromRequest.fold[Future[Result]](
                       formWithErrors => BadRequest(views.submitInfoUltimateParentEntity(formWithErrors)),
                       success =>
                         cache.save(success).map { _ =>
                           (userType, reportingRole) match {
                             case (_, CBC702) => Redirect(routes.SubmissionController.utr)
                             case (Some(Agent), CBC703 | CBC704) =>
                               Redirect(routes.SubmissionController.enterCompanyName)
                             case (Some(Organisation), CBC703 | CBC704) =>
                               Redirect(routes.SubmissionController.submitterInfo(None))
                             case _ =>
                               errorRedirect(
                                 UnexpectedState(
                                   s"Unexpected userType/ReportingRole combination: $userType $reportingRole"),
                                 views.notAuthorisedIndividual,
                                 views.errorTemplate)
                           }
                       }
                     ))
      } yield redirect)
        .leftMap((error: CBCErrors) => errorRedirect(error, views.notAuthorisedIndividual, views.errorTemplate))
        .merge
    }
  }

  val utr = Action.async { implicit request =>
    authorised() {
      Ok(views.utrCheck(utrForm))
    }
  }

  val submitUtr = Action.async { implicit request =>
    authorised().retrieve(Retrievals.affinityGroup) { userType =>
      utrForm.bindFromRequest.fold[Future[Result]](
        errors => BadRequest(views.utrCheck(errors)),
        utr =>
          cache.save(TIN(utr.utr, "")).map { _ =>
            userType match {
              case Some(Organisation) => Redirect(routes.SubmissionController.submitterInfo(None))
              case Some(Agent)        => Redirect(routes.SubmissionController.enterCompanyName)
              case _ =>
                errorRedirect(
                  UnexpectedState(s"Bad affinityGroup: $userType"),
                  views.notAuthorisedIndividual,
                  views.errorTemplate)
            }
        }
      )
    }
  }

  def enterSubmitterInfo(fn: Option[FieldName], userType: Option[AffinityGroup])(
    implicit request: Request[AnyContent]): Future[Result] =
    for {
      form <- cache.readOption[SubmitterInfo].map { osi =>
               (osi.map(_.fullName) |@| osi.map(_.contactPhone) |@| osi.map(_.email))
                 .map { (name, phone, email) =>
                   submitterInfoForm.bind(Map("fullName" -> name, "contactPhone" -> phone, "email" -> email.value))
                 }
                 .getOrElse(submitterInfoForm)
             }
      fileDetails <- cache.read[FileDetails].getOrElse(throw new RuntimeException("Missing file upload details"))

    } yield {
      Ok(views.submitterInfo(form, fn, fileDetails.envelopeId, fileDetails.fileId, userType))
    }

  def submitterInfo(field: Option[String] = None) = Action.async { implicit request =>
    authorised().retrieve(Retrievals.affinityGroup) { userType =>
      cache
        .read[CompleteXMLInfo]
        .map(kXml =>
          kXml.reportingEntity.reportingRole match {

            case CBC701 =>
              (cache.save(FilingType(CBC701)) *>
                cache.save(UltimateParentEntity(kXml.reportingEntity.name))).map(_ => ())

            case CBC702 | CBC703 | CBC704 =>
              cache.save(FilingType(kXml.reportingEntity.reportingRole))

        })
        .semiflatMap(_ => enterSubmitterInfo(FieldName.fromString(field.getOrElse("")), userType))
        .leftMap((error: CBCErrors) => errorRedirect(error, views.notAuthorisedIndividual, views.errorTemplate))
        .merge
    }

  }

  val submitSubmitterInfo = Action.async(parse.formUrlEncoded) { implicit request =>
    authorised().retrieve(Retrievals.affinityGroup) { userType =>
      submitterInfoForm.bindFromRequest.fold(
        formWithErrors => {
          cache
            .read[FileDetails]
            .map { fd =>
              BadRequest(
                views.submitterInfo(
                  formWithErrors,
                  None,
                  fd.envelopeId,
                  fd.fileId,
                  userType
                ))
            }
            .getOrElse(throw new RuntimeException("Missing file upload details"))
        },
        success => {
          val result = for {
            straightThrough <- right[Boolean](cache.readOption[CBCId].map(_.isDefined))
            xml             <- cache.read[CompleteXMLInfo]
            name <- right(
                     OptionT(cache.readOption[AgencyBusinessName])
                       .getOrElse(AgencyBusinessName(xml.reportingEntity.name)))
            _ <- right[CacheMap](cache.save(success.copy(affinityGroup = userType, agencyBusinessName = Some(name))))
            result <- userType match {
                       case Some(Organisation) if straightThrough =>
                         pure(Redirect(routes.SubmissionController.submitSummary))
                       case Some(Organisation) => pure(Redirect(routes.SharedController.enterCBCId))
                       case Some(Agent) =>
                         right(cache.save(xml.messageSpec.sendingEntityIn)).map(_ =>
                           Redirect(routes.SharedController.verifyKnownFactsAgent))
                       case _ => left[Result](UnexpectedState(s"Invalid affinityGroup: $userType"))
                     }
          } yield result

          result
            .leftMap((error: CBCErrors) => errorRedirect(error, views.notAuthorisedIndividual, views.errorTemplate))
            .merge

        }
      )
    }
  }

  val submitSummary = Action.async { implicit request =>
    authorised().retrieve(Retrievals.credentials and Retrievals.affinityGroup) { retrievedInformation =>
      val result = for {
        smd <- retrievedInformation.a match {
                case Some(cred) => EitherT(generateMetadataFile(cache, cred).map(_.toEither)).leftMap(_.head)
                case None       => left(UnexpectedState("Errors in saving MessageRefId aborting submission"))
              }
        sd <- createSummaryData(smd)
      } yield Ok(views.submitSummary(sd, retrievedInformation.b))

      result
        .leftMap(errors => errorRedirect(errors, views.notAuthorisedIndividual, views.errorTemplate))
        .merge
        .recover {
          case NonFatal(e) =>
            logger.error(e.getMessage, e)
            errorRedirect(UnexpectedState(e.getMessage), views.notAuthorisedIndividual, views.errorTemplate)
        }
    }

  }

  def createSummaryData(submissionMetaData: SubmissionMetaData)(
    implicit hc: HeaderCarrier): ServiceResponse[SummaryData] =
    for {
      keyXMLFileInfo <- cache.read[CompleteXMLInfo]
      bpr            <- cache.read[BusinessPartnerRecord]
      summaryData = SummaryData(
        bpr,
        submissionMetaData,
        updateCreationTimeStamp(keyXMLFileInfo),
        doesCreationTimeStampHaveMillis(keyXMLFileInfo))
      _ <- right(cache.save[SummaryData](summaryData))
    } yield summaryData

  def updateCreationTimeStamp(keyXMLFileInfo: CompleteXMLInfo) =
    if (keyXMLFileInfo.messageSpec.messageRefID.uniqueElement.slice(0, 3).forall(_.isDigit)) {
      val millisInfo = keyXMLFileInfo.messageSpec.messageRefID.uniqueElement.slice(0, 3).toLong
      val toModify = keyXMLFileInfo.messageSpec.timestamp.plus(Duration.ofMillis(millisInfo))
      keyXMLFileInfo.copy(messageSpec = keyXMLFileInfo.messageSpec.copy(timestamp = toModify))
    } else {
      keyXMLFileInfo
    }

  def doesCreationTimeStampHaveMillis(keyXMLFileInfo: CompleteXMLInfo) =
    if (keyXMLFileInfo.messageSpec.messageRefID.uniqueElement.slice(0, 3).forall(_.isDigit))
      true
    else
      false

  def enterCompanyName = Action.async { implicit request =>
    authorised() {
      for {
        fileDetails <- cache.read[FileDetails].getOrElse(throw new RuntimeException("Missing file upload details"))
        form <- cache
                 .read[AgencyBusinessName]
                 .map(abn => enterCompanyNameForm.bind(Map("companyName" -> abn.name)))
                 .getOrElse(enterCompanyNameForm)
      } yield {
        Ok(views.enterCompanyName(form, fileDetails.envelopeId, fileDetails.fileId))
      }
    }
  }

  def saveCompanyName = Action.async(parse.formUrlEncoded) { implicit request =>
    authorised() {
      enterCompanyNameForm
        .bindFromRequest()
        .fold(
          errors =>
            cache
              .read[FileDetails]
              .map { fd =>
                BadRequest(views.enterCompanyName(errors, fd.envelopeId, fd.fileId))
              }
              .getOrElse(throw new RuntimeException("Missing file upload details")),
          name => cache.save(name).map(_ => Redirect(routes.SubmissionController.submitterInfo()))
        )
    }
  }

  def submitSuccessReceipt(userType: String) = Action.async { implicit request =>
    authorised() {

      val data: EitherT[Future, CBCErrors, (Hash, String, String, String, Boolean)] =
        for {
          dataTuple <- (cache.read[SummaryData] |@| cache.read[SubmissionDate] |@| cache.read[CBCId]).tupled
          data = dataTuple._1
          date = dataTuple._2
          cbcId = dataTuple._3
          formattedDate <- fromEither(
                            (nonFatalCatch opt date.date.format(dateFormat).replace("AM", "am").replace("PM", "pm"))
                              .toRight(UnexpectedState(s"Unable to format date: ${date.date} to format $dateFormat")))
          emailSentAlready <- right(cache.readOption[ConfirmationEmailSent].map(_.isDefined))
          sentEmail <- if (!emailSentAlready)
                        right(emailService.sendEmail(makeSubmissionSuccessEmail(data, formattedDate, cbcId)).value)
                      else pure(None)
          _ <- if (sentEmail.getOrElse(false)) right(cache.save[ConfirmationEmailSent](ConfirmationEmailSent()))
              else pure(())
          hash = data.submissionMetaData.submissionInfo.hash
          cacheCleared <- right(cache.clear)
        } yield (hash, formattedDate, cbcId.value, userType, cacheCleared)

      data.fold[Result](
        (error: CBCErrors) => errorRedirect(error, views.notAuthorisedIndividual, views.errorTemplate),
        tuple5 => {
          Ok(views.submitSuccessReceipt(tuple5._2, tuple5._1.value, tuple5._3, tuple5._4, tuple5._5))
        }
      )
    }
  }

  private def makeSubmissionSuccessEmail(data: SummaryData, formattedDate: String, cbcId: CBCId): Email = {
    val submittedInfo = data.submissionMetaData.submitterInfo
    Email(
      List(submittedInfo.email.toString()),
      "cbcr_report_confirmation",
      Map(
        "name" → submittedInfo.fullName,
        "received_at" → formattedDate,
        "hash" → data.submissionMetaData.submissionInfo.hash.value,
        "cbcrId" -> cbcId.value
      )
    )
  }

}
