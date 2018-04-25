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

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import javax.inject.{Inject, Singleton}
import cats.data.{EitherT, NonEmptyList, OptionT}
import cats.instances.all._
import cats.syntax.all._
import play.api.{Configuration, Environment, Logger}
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.{I18nSupport, Messages, MessagesApi}
import play.api.libs.json.{JsString, Json}
import play.api.mvc.{Action, AnyContent, Request, Result}
import uk.gov.hmrc.auth.core.AffinityGroup.{Agent, Organisation}
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.retrieve.{Credentials, LegacyCredentials, Retrievals}
import uk.gov.hmrc.cbcrfrontend._
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.services.{CBCSessionCache, DocRefIdService, FileUploadService, ReportingEntityDataService}
import uk.gov.hmrc.cbcrfrontend.model.{ConfirmationEmailSent, SummaryData, _}
import uk.gov.hmrc.cbcrfrontend.services._
import uk.gov.hmrc.cbcrfrontend.typesclasses.{CbcrsUrl, FusFeUrl, FusUrl, ServiceUrl}
import uk.gov.hmrc.cbcrfrontend.views.html.includes
import uk.gov.hmrc.emailaddress.EmailAddress
import uk.gov.hmrc.http.cache.client.CacheMap
import uk.gov.hmrc.play.audit.AuditExtensions._
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}
import uk.gov.hmrc.play.audit.model.ExtendedDataEvent
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import uk.gov.hmrc.cbcrfrontend.form.SubmitterInfoForm.submitterInfoForm

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.Exception.nonFatalCatch
import scala.util.control.NonFatal
import uk.gov.hmrc.http.HeaderCarrier
import play.api.http.Status._
import uk.gov.hmrc.http.HeaderCarrier

@Singleton
class SubmissionController @Inject()(val messagesApi: MessagesApi,
                                     val fus:FileUploadService,
                                     val docRefIdService: DocRefIdService,
                                     val reportingEntityDataService: ReportingEntityDataService,
                                     val messageRefIdService: MessageRefIdService,
                                     val cbidService: CBCIdService,
                                     val audit: AuditConnector,
                                     val env:Environment,
                                     val authConnector:AuthConnector,
                                     val emailService:EmailService)
                                    (implicit ec: ExecutionContext,
                                     cache:CBCSessionCache,
                                     val config: Configuration,
                                     feConfig:FrontendAppConfig) extends FrontendController with AuthorisedFunctions with I18nSupport{


  implicit val credentialsFormat = uk.gov.hmrc.cbcrfrontend.controllers.credentialsFormat

  val dateFormat = DateTimeFormatter.ofPattern("dd MMMM yyyy 'at' HH:mm")

  def saveDocRefIds(x:CompleteXMLInfo)(implicit hc:HeaderCarrier): EitherT[Future,NonEmptyList[UnexpectedState],Unit] = {
    val cbcReportIds      = x.cbcReport.map(reports      => reports.docSpec.docRefId -> reports.docSpec.corrDocRefId)
    val additionalInfoIds = x.additionalInfo.map(addInfo => addInfo.docSpec.docRefId -> addInfo.docSpec.corrDocRefId)

    val allIds            = cbcReportIds ++ List(additionalInfoIds).flatten

    // The result of saving these DocRefIds/CorrDocRefIds from the cbcReports
    val result = NonEmptyList.fromList(allIds).map(_.map{
      case (doc, corr) => corr.map(docRefIdService.saveCorrDocRefID(_, doc)).getOrElse(docRefIdService.saveDocRefId(doc))
    }.sequence[({type λ[α] = OptionT[Future,α]})#λ,UnexpectedState]).getOrElse(OptionT.none[Future,NonEmptyList[UnexpectedState]])

    x.reportingEntity.docSpec.docType match {
      case OECD0                 => result.toLeft(())

      case OECD1 | OECD2 | OECD3 =>
        val reDocRef = x.reportingEntity.docSpec.docRefId
        val reCorr   = x.reportingEntity.docSpec.corrDocRefId

        val reResult = reCorr.map(c => docRefIdService.saveCorrDocRefID(c, reDocRef)).getOrElse(docRefIdService.saveDocRefId(reDocRef))

        EitherT((result.toLeft(()).toValidated |@| reResult.toLeft(()).toValidatedNel).map((a,b) => (a |@| b).map((_,_) => ()).toEither))
    }
  }

  def storeOrUpdateReportingEntityData(xml:CompleteXMLInfo)(implicit hc:HeaderCarrier) : ServiceResponse[Unit] =
    xml.reportingEntity.docSpec.docType match {
      // RESENT| NEW
      case OECD1 =>
        ReportingEntityData.extract(xml).fold[ServiceResponse[Unit]](
          errors => {
            EitherT.left(Future.successful(
              UnexpectedState(s"Unable to submit partially completed data when docType is ${xml.reportingEntity.docSpec.docType}\n${errors.toList.mkString("\n")}")
            ))
          },
          (data: ReportingEntityData) => reportingEntityDataService.saveReportingEntityData(data)
        )

      // UPDATE| DELETE
      case OECD0 => reportingEntityDataService.updateReportingEntityAdditionalData(PartialReportingEntityData.extract(xml))
      case OECD2 | OECD3 => reportingEntityDataService.updateReportingEntityData(PartialReportingEntityData.extract(xml))
    }

  def confirm = Action.async{ implicit request =>
    authorised().retrieve(Retrievals.credentials and Retrievals.affinityGroup) { retrieval =>
      (for {
        summaryData <- cache.read[SummaryData]
        xml <- cache.read[CompleteXMLInfo]
        _ <- fus.uploadMetadataAndRoute(summaryData.submissionMetaData)
        _ <- saveDocRefIds(xml).leftMap[CBCErrors] { es =>
          Logger.error(s"Errors saving Corr/DocRefIds : ${es.map(_.errorMsg).toList.mkString("\n")}")
          UnexpectedState("Errors in saving Corr/DocRefIds aborting submission")
        }
        _ <- messageRefIdService.saveMessageRefId(xml.messageSpec.messageRefID).toLeft {
          Logger.error(s"Errors saving MessageRefId")
          UnexpectedState("Errors in saving MessageRefId aborting submission")
        }
        _ <- right(cache.save(SubmissionDate(LocalDateTime.now)))
        _ <- storeOrUpdateReportingEntityData(xml)
        _ <- createSuccessfulSubmissionAuditEvent(retrieval.a, summaryData)
        userType = retrieval.b match {
          case Some(Agent) => "Agent"
          case _           => "Other"
        }
      } yield Redirect(routes.SubmissionController.submitSuccessReceipt(userType))).leftMap(errorRedirect).merge
    }
  }

  def notRegistered =  Action.async { implicit request =>
    authorised(AffinityGroup.Organisation and (User or Admin)) {
      Ok(views.html.submission.notRegistered())
    }
  }

  def noIndividuals =  Action.async { implicit request =>
    authorised(AffinityGroup.Individual) {
      Ok(views.html.not_authorised_individual())
    }
  }
  def noAssistants =  Action.async { implicit request =>
    authorised(AffinityGroup.Organisation and (Assistant)) {
      Ok(views.html.not_authorised_assistant())
    }
  }

  def createSuccessfulSubmissionAuditEvent(creds:Credentials,summaryData:SummaryData)
                                          (implicit hc:HeaderCarrier, request:Request[_]): ServiceResponse[AuditResult.Success.type] =
    for {
      result <- eitherT[AuditResult.Success.type ](audit.sendExtendedEvent(ExtendedDataEvent("Country-By-Country-Frontend", "CBCRFilingSuccessful",
        detail = Json.obj(
          "path"        -> JsString(request.uri),
          "summaryData" -> Json.toJson(summaryData),
          "creds"       -> Json.toJson(creds)
        )
      )).map{
        case AuditResult.Success         => Right(AuditResult.Success)
        case AuditResult.Failure(msg,_)  => Left(UnexpectedState(s"Unable to audit a successful submission: $msg"))
        case AuditResult.Disabled        => Right(AuditResult.Success)
      })
    } yield result

  val utrForm:Form[Utr] = Form(
    mapping(
      "utr" -> nonEmptyText.verifying(s => Utr(s).isValid)
    )(Utr.apply)(Utr.unapply)
  )

  val ultimateParentEntityForm: Form[UltimateParentEntity] = Form(
    mapping("ultimateParentEntity" -> nonEmptyText
    )(UltimateParentEntity.apply)(UltimateParentEntity.unapply)
  )



  val reconfirmEmailForm : Form[EmailAddress] = Form(
    mapping(
      "reconfirmEmail" -> email.verifying(EmailAddress.isValid(_))
    )(EmailAddress.apply)(EmailAddress.unapply)
  )


  val enterCompanyNameForm : Form[AgencyBusinessName] = Form(
    single(
      "companyName" -> nonEmptyText
    ).transform[AgencyBusinessName](AgencyBusinessName(_),_.name)
  )

  val upe = Action.async{ implicit request =>
    authorised() {
      Ok(views.html.submission.submitInfoUltimateParentEntity(
         ultimateParentEntityForm
      ))
    }
  }

  val submitUltimateParentEntity = Action.async{ implicit request =>
    authorised().retrieve(Retrievals.affinityGroup) { userType =>
      (for {
        reportingRole <- cache.read[CompleteXMLInfo].map(_.reportingEntity.reportingRole)
        redirect      <- right(ultimateParentEntityForm.bindFromRequest.fold[Future[Result]](
          formWithErrors => BadRequest(views.html.submission.submitInfoUltimateParentEntity( formWithErrors)),
          success        => cache.save(success).map { _ =>
            (userType, reportingRole) match {
              case (_,                  CBC702) => Redirect(routes.SubmissionController.utr())
              case (Some(Agent),        CBC703) => Redirect(routes.SubmissionController.enterCompanyName())
              case (Some(Organisation), CBC703) => Redirect(routes.SubmissionController.submitterInfo())
              case _                            => errorRedirect(UnexpectedState(s"Unexpected userType/ReportingRole combination: $userType $reportingRole"))
            }
          }
        ))
      } yield redirect).leftMap(errorRedirect).merge
    }
  }

  val utr = Action.async { implicit request =>
    authorised() {
      Ok(views.html.submission.utrCheck(utrForm))
    }
  }

  val submitUtr = Action.async { implicit request =>
    authorised().retrieve(Retrievals.affinityGroup) { userType =>
      utrForm.bindFromRequest.fold[Future[Result]](
        errors => BadRequest(views.html.submission.utrCheck(errors)),
        utr    => cache.save(TIN(utr.utr, "")).map { _ =>
          userType match {
            case Some(Organisation) => Redirect(routes.SubmissionController.submitterInfo())
            case Some(Agent)        => Redirect(routes.SubmissionController.enterCompanyName())
            case _                  => errorRedirect(UnexpectedState(s"Bad affinityGroup: $userType"))
          }
        }
      )
    }
  }


  def enterSubmitterInfo()(implicit request:Request[AnyContent]): Future[Result] = {

    cache.readOption[SubmitterInfo].map{ osi =>

      val form = (osi.map(_.fullName) |@| osi.map(_.contactPhone) |@| osi.map(_.email)).map{(name,phone,email) =>
        submitterInfoForm.bind(Map("fullName" -> name, "contactPhone" -> phone, "email" -> email.value))
      }.getOrElse(submitterInfoForm)

      Ok(views.html.submission.submitterInfo( form))

    }
  }


  val submitterInfo = Action.async{ implicit request =>
    authorised() {

      cache.read[CompleteXMLInfo].map(kXml => kXml.reportingEntity.reportingRole match {

        case CBC701 =>
          (cache.save(FilingType(CBC701)) *>
            cache.save(UltimateParentEntity(kXml.reportingEntity.name))).map(_ => ())

        case CBC702 | CBC703 =>
          cache.save(FilingType(CBC702))

      }).semiflatMap(_ => enterSubmitterInfo()).leftMap(errorRedirect).merge
    }

  }


  val submitSubmitterInfo = Action.async{ implicit request =>
    authorised().retrieve(Retrievals.affinityGroup){ userType =>
        submitterInfoForm.bindFromRequest.fold(
          formWithErrors => Future.successful(BadRequest(views.html.submission.submitterInfo(
             formWithErrors
          ))),
          success => {
            val result = for {
              straightThrough <- right[Boolean](cache.readOption[CBCId].map(_.isDefined))
              xml             <- cache.read[CompleteXMLInfo]
              name            <- right(OptionT(cache.readOption[AgencyBusinessName]).getOrElse(AgencyBusinessName(xml.reportingEntity.name)))
              _               <- right[CacheMap](cache.save(success.copy( affinityGroup = userType, agencyBusinessName = Some(name))))
              result          <- userType match {
                case Some(Organisation) if straightThrough => pure(Redirect(routes.SubmissionController.submitSummary()))
                case Some(Organisation)                    => pure(Redirect(routes.SharedController.enterCBCId()))
                case Some(Agent)                           => right(cache.save(xml.messageSpec.sendingEntityIn)).map(_ => Redirect(routes.SharedController.verifyKnownFactsAgent()))
                case _                                     => left[Result](UnexpectedState(s"Invalid affinityGroup: $userType"))
              }
            } yield result

            result.leftMap(errorRedirect).merge

          }
        )
      }
  }



  val submitSummary = Action.async { implicit request =>
    authorised().retrieve(Retrievals.credentials) { credentials =>

      val result = for {
        smd <- EitherT(generateMetadataFile(cache, credentials).map(_.toEither)).leftMap(_.head)
        sd  <- createSummaryData(smd)
      } yield Ok(views.html.submission.submitSummary(sd))

      result.leftMap(errors => errorRedirect(errors)).merge.recover {
        case NonFatal(e) =>
          Logger.error(e.getMessage, e)
          errorRedirect(UnexpectedState(e.getMessage))
      }
    }

  }

  def createSummaryData(submissionMetaData: SubmissionMetaData)(implicit hc:HeaderCarrier) : ServiceResponse[SummaryData] = {
    for {
      keyXMLFileInfo <- cache.read[CompleteXMLInfo]
      bpr            <- cache.read[BusinessPartnerRecord]
      summaryData    = SummaryData(bpr, submissionMetaData, keyXMLFileInfo)
      _              <- right(cache.save[SummaryData](summaryData))
    } yield summaryData
  }

  def enterCompanyName = Action.async{ implicit request =>
    authorised() {
      Ok(views.html.submission.enterCompanyName( enterCompanyNameForm))
    }
  }

  def saveCompanyName = Action.async { implicit request =>
    authorised() {
      enterCompanyNameForm.bindFromRequest().fold(
        errors => BadRequest(views.html.submission.enterCompanyName( errors)),
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
          formattedDate <- fromEither((nonFatalCatch opt date.date.format(dateFormat)).toRight(UnexpectedState(s"Unable to format date: ${date.date} to format $dateFormat")))
          emailSentAlready <- right(cache.readOption[ConfirmationEmailSent].map(_.isDefined))
          sentEmail <- if (!emailSentAlready) right(emailService.sendEmail(makeSubmissionSuccessEmail(data, formattedDate, cbcId)).value)
          else pure(None)
          _ <- if (sentEmail.getOrElse(false)) right(cache.save[ConfirmationEmailSent](ConfirmationEmailSent()))
          else pure(())
          hash = data.submissionMetaData.submissionInfo.hash
          cacheCleared <- right(cache.clear)
        } yield (hash, formattedDate, cbcId.value, userType, cacheCleared)


      data.fold[Result](
        (error: CBCErrors) => errorRedirect(error),
        tuple5 => {
          Ok(views.html.submission.submitSuccessReceipt(tuple5._2, tuple5._1.value, tuple5._3, tuple5._4, tuple5._5))
        }
      )
    }
  }

  private def makeSubmissionSuccessEmail(data:SummaryData,formattedDate:String,cbcId: CBCId):Email ={
    val submittedInfo = data.submissionMetaData.submitterInfo
    Email(List(submittedInfo.email.toString()),
      "cbcr_report_confirmation",
      Map(
        "name" → submittedInfo.fullName,
        "received_at" → formattedDate,
        "hash"   → data.submissionMetaData.submissionInfo.hash.value,
        "cbcrId" -> cbcId.value
      ))
  }


}
