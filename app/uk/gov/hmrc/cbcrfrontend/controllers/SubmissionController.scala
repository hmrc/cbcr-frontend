/*
 * Copyright 2024 HM Revenue & Customs
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
import cats.implicits._
import play.api.Logger
import play.api.data.Form
import play.api.data.Forms._
import play.api.data.validation.{Constraint, Invalid, Valid}
import play.api.i18n.I18nSupport
import play.api.libs.json.{JsString, Json, OFormat}
import play.api.mvc._
import uk.gov.hmrc.auth.core.AffinityGroup.{Agent, Organisation}
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.retrieve.Credentials
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.cbcrfrontend._
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.form.SubmitterInfoForm.submitterInfoForm
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.repositories.CBCSessionCache
import uk.gov.hmrc.cbcrfrontend.services._
import uk.gov.hmrc.cbcrfrontend.util.CBCRMapping.ukPhoneNumberConstraint
import uk.gov.hmrc.cbcrfrontend.views.Views
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.audit.model.ExtendedDataEvent
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import java.time.format.DateTimeFormatter
import java.time.{Duration, LocalDateTime}
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.Exception.nonFatalCatch
import scala.util.control.NonFatal

// scalastyle:off <number.of.methods>

@Singleton
class SubmissionController @Inject() (
  fus: FileUploadService,
  docRefIdService: DocRefIdService,
  reportingEntityDataService: ReportingEntityDataService,
  messageRefIdService: MessageRefIdService,
  audit: AuditConnector,
  val authConnector: AuthConnector,
  emailService: EmailService,
  messagesControllerComponents: MessagesControllerComponents,
  views: Views,
  cache: CBCSessionCache
)(implicit ec: ExecutionContext, feConfig: FrontendAppConfig)
    extends FrontendController(messagesControllerComponents) with AuthorisedFunctions with I18nSupport {

  implicit val credentialsFormat: OFormat[Credentials] = uk.gov.hmrc.cbcrfrontend.controllers.credentialsFormat

  val dateFormat: DateTimeFormatter = DateTimeFormatter.ofPattern("dd MMMM yyyy 'at' h:mma")

  lazy val logger: Logger = Logger(this.getClass)

  private def saveDocRefIds(
    x: CompleteXMLInfo
  )(implicit hc: HeaderCarrier): EitherT[Future, NonEmptyList[UnexpectedState], Unit] = {
    val cbcReportIds = x.cbcReport.map(reports => reports.docSpec.docRefId -> reports.docSpec.corrDocRefId)
    val additionalInfoIds = x.additionalInfo.map(addInfo => addInfo.docSpec.docRefId -> addInfo.docSpec.corrDocRefId)

    val allIds = cbcReportIds ++ additionalInfoIds

    // The result of saving these DocRefIds/CorrDocRefIds from the cbcReports
    val result: OptionT[Future, NonEmptyList[UnexpectedState]] = OptionT(
      Future
        .sequence(allIds.map { case (doc, corr) =>
          corr.map(docRefIdService.saveCorrDocRefID(_, doc)).getOrElse(docRefIdService.saveDocRefId(doc)).value
        })
        .map(f => NonEmptyList.fromList(f.flatten))
    )

    x.reportingEntity.docSpec.docType match {
      case OECD0 => result.toLeft(())

      case OECD1 | OECD2 | OECD3 =>
        val reDocRef = x.reportingEntity.docSpec.docRefId
        val reCorr = x.reportingEntity.docSpec.corrDocRefId

        val reResult = reCorr
          .map(c => docRefIdService.saveCorrDocRefID(c, reDocRef))
          .getOrElse(docRefIdService.saveDocRefId(reDocRef))

        EitherT(
          (result.toLeft(()).toValidated, reResult.toLeft(()).toValidatedNel).mapN((a, b) =>
            (a, b).mapN((_, _) => ()).toEither
          )
        )
    }
  }

  private def storeOrUpdateReportingEntityData(
    xml: CompleteXMLInfo
  )(implicit hc: HeaderCarrier): ServiceResponse[Unit] =
    xml.reportingEntity.docSpec.docType match {
      // RESENT| NEW
      case OECD1 =>
        ReportingEntityData
          .extract(xml)
          .fold[ServiceResponse[Unit]](
            errors =>
              EitherT.left(
                Future.successful(
                  UnexpectedState(
                    s"Unable to submit partially completed data when docType is ${xml.reportingEntity.docSpec.docType}\n${errors.toList
                        .mkString("\n")}"
                  )
                )
              ),
            (data: ReportingEntityData) =>
              EitherT(
                reportingEntityDataService
                  .saveReportingEntityData(data)
                  .value
                  .recover { case _ =>
                    logger.error("CBCR_UNVERIFIED_UPLOAD Failed to save reporting entity data. UTR: " + data.tin)
                    Left(UnexpectedState("Failed to save reporting entity data"))
                  }
              )
          )
      case OECD0 | OECD2 | OECD3 =>
        EitherT(
          reportingEntityDataService
            .updateReportingEntityData(PartialReportingEntityData.extract(xml))
            .value
            .recover { case _ =>
              logger.error("CBCR_UNVERIFIED_UPLOAD Failed to update reporting entity data")
              Left(UnexpectedState("Failed to update reporting entity data"))
            }
        )
    }

  def confirm: Action[AnyContent] = Action.async { implicit request =>
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
                      _ <- EitherT.right[CBCErrors](cache.save(SubmissionDate(LocalDateTime.now)))
                      _ <- storeOrUpdateReportingEntityData(xml)
                      _ = createSuccessfulSubmissionAuditEvent(retrieval.a.get, summaryData)
                      _ = {
                        val file = summaryData.submissionMetaData.fileInfo
                        logger.info(s"Successfully uploaded file: ${file.name}, envelopeId: ${file.envelopeId}")
                      }
                    } yield retrieval.b match {
                      case Some(Agent) => "Agent"
                      case _           => "Other"
                    })
                      .leftMap { error =>
                        logger.error(
                          "CBCR_UNVERIFIED_UPLOAD Error occurred after uploading a file. State may be inconsistent. UTR: " + summaryData.submissionMetaData.submissionInfo.tin
                        )
                        error
                      }
      } yield Redirect(routes.SubmissionController.submitSuccessReceipt(userType)))
        .leftMap((error: CBCErrors) => errorRedirect(error, views.notAuthorisedIndividual, views.errorTemplate))
        .merge
    }
  }

  def notRegistered: Action[AnyContent] = Action.async { implicit request =>
    authorised(AffinityGroup.Organisation and User) {
      Ok(views.notRegistered())
    }
  }

  def noIndividuals: Action[AnyContent] = Action.async { implicit request =>
    authorised(AffinityGroup.Individual) {
      Ok(views.notAuthorisedIndividual())
    }
  }
  def noAssistants: Action[AnyContent] = Action.async { implicit request =>
    authorised(AffinityGroup.Organisation and Assistant) {
      Ok(views.notAuthorisedAssistant())
    }
  }

  private def createSuccessfulSubmissionAuditEvent(creds: Credentials, summaryData: SummaryData)(implicit
    hc: HeaderCarrier,
    request: Request[_]
  ): Unit = {

    val auditEvent = Json.obj(
      "auditSource" -> "Country-By-Country-Frontend",
      "auditType"   -> "CBCRFilingSuccessful",
      "detail" -> Json.obj(
        "path"        -> JsString(request.uri),
        "summaryData" -> Json.toJson(summaryData),
        "creds"       -> Json.toJson(creds)
      )
    )

    val auditEventLength = Json.stringify(auditEvent).getBytes("UTF-8").length
    // Datastream allows maximum 500kb
    val dataStreamLimit = 500000

    val validAuditEvent =
      if (auditEventLength > dataStreamLimit) {
        val trimmmedSummaryData = summaryData.copy(
          xmlInfo = summaryData.xmlInfo.copy(
            additionalInfo = List.empty
          )
        )
        Json.obj(
          "detail" -> Json.obj(
            "path"        -> JsString(request.uri),
            "summaryData" -> Json.toJson(trimmmedSummaryData),
            "creds"       -> Json.toJson(creds)
          )
        )
      } else {
        auditEvent
      }

    audit.sendExtendedEvent(
      ExtendedDataEvent(
        "Country-By-Country-Frontend",
        "CBCRFilingSuccessful",
        detail = validAuditEvent
      )
    )
  }

  private val utrConstraint: Constraint[String] = Constraint { utr =>
    val stripped = Utr(utr).stripUtr
    if (stripped.isEmpty) {
      Invalid("error.required")
    } else if (!stripped.matches("^[0-9]{10}$")) {
      Invalid("error.pattern")
    } else if (Utr(stripped).isValid) {
      Valid
    } else {
      Invalid("error.invalid")
    }
  }

  private val utrForm = Form(
    mapping(
      "utr" -> text.verifying(utrConstraint)
    )(Utr.apply)(Utr.unapply)
  )

  private val ultimateParentEntityForm = Form(
    mapping("ultimateParentEntity" -> nonEmptyText)(UltimateParentEntity.apply)(UltimateParentEntity.unapply)
  )

  private val enterCompanyNameForm = Form(
    single(
      "companyName" -> nonEmptyText
    ).transform[AgencyBusinessName](AgencyBusinessName(_), _.name)
  )

  val upe: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      Ok(
        views.submitInfoUltimateParentEntity(
          ultimateParentEntityForm
        )
      )
    }
  }

  val submitUltimateParentEntity: Action[Map[String, Seq[String]]] = Action.async(parse.formUrlEncoded) {
    implicit request =>
      authorised().retrieve(Retrievals.affinityGroup) { userType =>
        (for {
          reportingRole <- cache.read[CompleteXMLInfo].map(_.reportingEntity.reportingRole)
          redirect <- EitherT.right[CBCErrors](
                        ultimateParentEntityForm
                          .bindFromRequest()
                          .fold[Future[Result]](
                            formWithErrors => BadRequest(views.submitInfoUltimateParentEntity(formWithErrors)),
                            success =>
                              cache.save(success).map { _ =>
                                (userType, reportingRole) match {
                                  case (_, CBC702) => Redirect(routes.SubmissionController.utr)
                                  case (Some(Agent), CBC703 | CBC704) =>
                                    Redirect(routes.SubmissionController.enterCompanyName)
                                  case (Some(Organisation), CBC703 | CBC704) =>
                                    Redirect(routes.SubmissionController.submitterInfo())
                                  case _ =>
                                    errorRedirect(
                                      UnexpectedState(
                                        s"Unexpected userType/ReportingRole combination: $userType $reportingRole"
                                      ),
                                      views.notAuthorisedIndividual,
                                      views.errorTemplate
                                    )
                                }
                              }
                          )
                      )
        } yield redirect)
          .leftMap((error: CBCErrors) => errorRedirect(error, views.notAuthorisedIndividual, views.errorTemplate))
          .merge
      }
  }

  val utr: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      Ok(views.utrCheck(utrForm))
    }
  }

  val submitUtr: Action[AnyContent] = Action.async { implicit request =>
    authorised().retrieve(Retrievals.affinityGroup) { userType =>
      utrForm
        .bindFromRequest()
        .fold[Future[Result]](
          errors => BadRequest(views.utrCheck(errors)),
          utr =>
            cache.save(TIN(utr.utr, "")).map { _ =>
              userType match {
                case Some(Organisation) => Redirect(routes.SubmissionController.submitterInfo())
                case Some(Agent)        => Redirect(routes.SubmissionController.enterCompanyName)
                case _ =>
                  errorRedirect(
                    UnexpectedState(s"Bad affinityGroup: $userType"),
                    views.notAuthorisedIndividual,
                    views.errorTemplate
                  )
              }
            }
        )
    }
  }

  private def enterSubmitterInfo(
    userType: Option[AffinityGroup]
  )(implicit request: Request[AnyContent]): Future[Result] =
    for {
      form <- cache.readOption[SubmitterInfo].map { osi =>
                (osi.map(_.fullName), osi.map(_.contactPhone), osi.map(_.email))
                  .mapN { (name, phone, email) =>
                    submitterInfoForm(
                      feConfig,
                      ukPhoneNumberConstraint
                    ).bind(Map("fullName" -> name, "contactPhone" -> phone, "email" -> email.value))
                  }
                  .getOrElse(
                    submitterInfoForm(
                      feConfig,
                      ukPhoneNumberConstraint
                    )
                  )
              }
      fileDetails <- cache.read[FileDetails].getOrElse(throw new RuntimeException("Missing file upload details"))

    } yield Ok(views.submitterInfo(form, fileDetails.envelopeId, fileDetails.fileId, userType))

  def submitterInfo(): Action[AnyContent] = Action.async { implicit request =>
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

          }
        )
        .semiflatMap(_ => enterSubmitterInfo(userType))
        .leftMap((error: CBCErrors) => errorRedirect(error, views.notAuthorisedIndividual, views.errorTemplate))
        .merge
    }
  }

  val submitSubmitterInfo: Action[Map[String, Seq[String]]] = Action.async(parse.formUrlEncoded) { implicit request =>
    authorised().retrieve(Retrievals.affinityGroup) { userType =>
      submitterInfoForm(feConfig, ukPhoneNumberConstraint)
        .bindFromRequest()
        .fold(
          formWithErrors =>
            cache
              .read[FileDetails]
              .map { fd =>
                BadRequest(
                  views.submitterInfo(
                    formWithErrors,
                    fd.envelopeId,
                    fd.fileId,
                    userType
                  )
                )
              }
              .getOrElse(throw new RuntimeException("Missing file upload details")),
          success => {
            val result = for {
              straightThrough <- EitherT.right[CBCErrors](cache.readOption[CBCId].map(_.isDefined))
              xml             <- cache.read[CompleteXMLInfo]
              name <- EitherT.right[CBCErrors](
                        OptionT(cache.readOption[AgencyBusinessName])
                          .getOrElse(AgencyBusinessName(xml.reportingEntity.name))
                      )
              _ <- EitherT.right[CBCErrors](
                     cache.save(success.copy(affinityGroup = userType, agencyBusinessName = Some(name)))
                   )
              result <- EitherT.fromEither[Future](userType match {
                          case Some(Organisation) if straightThrough =>
                            Right(Redirect(routes.SubmissionController.submitSummary))
                          case Some(Organisation) => Right(Redirect(routes.SharedController.enterCBCId))
                          case Some(Agent) =>
                            Right(cache.save(xml.messageSpec.sendingEntityIn))
                              .map(_ => Redirect(routes.SharedController.verifyKnownFactsAgent))
                          case _ => Left(UnexpectedState(s"Invalid affinityGroup: $userType").asInstanceOf[CBCErrors])
                        })
            } yield result
            result
              .leftMap((error: CBCErrors) => errorRedirect(error, views.notAuthorisedIndividual, views.errorTemplate))
              .merge
          }
        )
    }
  }

  val submitSummary: Action[AnyContent] = Action.async { implicit request =>
    authorised().retrieve(Retrievals.credentials and Retrievals.affinityGroup) { retrievedInformation =>
      val result = for {
        smd <- EitherT(retrievedInformation.a match {
                 case Some(cred) => generateMetadataFile(cache, cred).map(_.toEither.leftMap(_.head))
                 case None =>
                   Future.successful(Left(UnexpectedState("Errors in saving MessageRefId aborting submission")))
               })
        sd <- createSummaryData(smd)
      } yield Ok(views.submitSummary(sd, retrievedInformation.b))

      result
        .leftMap(errors => errorRedirect(errors, views.notAuthorisedIndividual, views.errorTemplate))
        .merge
        .recover { case NonFatal(e) =>
          logger.error(e.getMessage, e)
          errorRedirect(UnexpectedState(e.getMessage), views.notAuthorisedIndividual, views.errorTemplate)
        }
    }
  }

  private def createSummaryData(
    submissionMetaData: SubmissionMetaData
  )(implicit hc: HeaderCarrier): ServiceResponse[SummaryData] =
    for {
      keyXMLFileInfo <- cache.read[CompleteXMLInfo]
      bpr            <- cache.read[BusinessPartnerRecord]
      summaryData = SummaryData(
                      bpr,
                      submissionMetaData,
                      updateCreationTimeStamp(keyXMLFileInfo),
                      doesCreationTimeStampHaveMillis(keyXMLFileInfo)
                    )
      _ <- EitherT.right(cache.save[SummaryData](summaryData))
    } yield summaryData

  private def updateCreationTimeStamp(keyXMLFileInfo: CompleteXMLInfo) =
    if (keyXMLFileInfo.messageSpec.messageRefID.uniqueElement.slice(0, 3).forall(_.isDigit)) {
      val millisInfo = keyXMLFileInfo.messageSpec.messageRefID.uniqueElement.slice(0, 3).toLong
      val toModify = keyXMLFileInfo.messageSpec.timestamp.plus(Duration.ofMillis(millisInfo))
      keyXMLFileInfo.copy(messageSpec = keyXMLFileInfo.messageSpec.copy(timestamp = toModify))
    } else {
      keyXMLFileInfo
    }

  private def doesCreationTimeStampHaveMillis(keyXMLFileInfo: CompleteXMLInfo) =
    if (keyXMLFileInfo.messageSpec.messageRefID.uniqueElement.slice(0, 3).forall(_.isDigit)) {
      true
    } else {
      false
    }

  def enterCompanyName: Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      for {
        fileDetails <- cache.read[FileDetails].getOrElse(throw new RuntimeException("Missing file upload details"))
        form <- cache
                  .read[AgencyBusinessName]
                  .map(abn => enterCompanyNameForm.bind(Map("companyName" -> abn.name)))
                  .getOrElse(enterCompanyNameForm)
      } yield Ok(views.enterCompanyName(form, fileDetails.envelopeId, fileDetails.fileId))
    }
  }

  def saveCompanyName: Action[Map[String, Seq[String]]] = Action.async(parse.formUrlEncoded) { implicit request =>
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

  def submitSuccessReceipt(userType: String): Action[AnyContent] = Action.async { implicit request =>
    authorised() {
      val data: EitherT[Future, CBCErrors, (Hash, String, String, String, Boolean)] =
        for {
          data  <- cache.read[SummaryData]
          date  <- cache.read[SubmissionDate]
          cbcId <- cache.read[CBCId]
          formattedDate <- EitherT.fromEither[Future](
                             (nonFatalCatch opt date.date.format(dateFormat).replace("AM", "am").replace("PM", "pm"))
                               .toRight(
                                 UnexpectedState(s"Unable to format date: ${date.date} to format $dateFormat")
                                   .asInstanceOf[CBCErrors]
                               )
                           )
          emailSentAlready <- EitherT.right[CBCErrors](cache.readOption[ConfirmationEmailSent].map(_.isDefined))
          sentEmail <- if (!emailSentAlready) {
                         EitherT.right[CBCErrors](
                           emailService.sendEmail(makeSubmissionSuccessEmail(data, formattedDate, cbcId)).value
                         )
                       } else {
                         EitherT.fromEither[Future](None.asRight[CBCErrors])
                       }
          _ <- if (sentEmail.getOrElse(false)) {
                 EitherT.right[CBCErrors](cache.save[ConfirmationEmailSent](ConfirmationEmailSent()))
               } else {
                 EitherT.fromEither[Future](().asRight[CBCErrors])
               }
          hash = data.submissionMetaData.submissionInfo.hash
          cacheCleared <- EitherT.right[CBCErrors](cache.clear)
        } yield (hash, formattedDate, cbcId.value, userType, cacheCleared)

      data.fold[Result](
        (error: CBCErrors) => errorRedirect(error, views.notAuthorisedIndividual, views.errorTemplate),
        tuple5 => Ok(views.submitSuccessReceipt(tuple5._2, tuple5._1.value, tuple5._3, tuple5._4, tuple5._5))
      )
    }
  }

  private def makeSubmissionSuccessEmail(data: SummaryData, formattedDate: String, cbcId: CBCId) = {
    val submittedInfo = data.submissionMetaData.submitterInfo
    Email(
      List(submittedInfo.email.toString()),
      "cbcr_report_confirmation",
      Map(
        "name"        -> submittedInfo.fullName,
        "received_at" -> formattedDate,
        "hash"        -> data.submissionMetaData.submissionInfo.hash.value,
        "cbcrId"      -> cbcId.value
      )
    )
  }
}
