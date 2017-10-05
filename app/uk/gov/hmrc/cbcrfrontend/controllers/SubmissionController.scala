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

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import javax.inject.{Inject, Singleton}

import cats.data.{EitherT, NonEmptyList, OptionT}
import cats.instances.all._
import cats.syntax.all._
import play.api.Logger
import play.api.Play.current
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.Messages.Implicits._
import play.api.libs.json.Json
import play.api.mvc.{Action, Request, Result}
import uk.gov.hmrc.cbcrfrontend._
import uk.gov.hmrc.cbcrfrontend.auth.SecuredActions
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.model._
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
import uk.gov.hmrc.play.frontend.auth.AuthContext
import uk.gov.hmrc.play.frontend.auth.connectors.AuthConnector
import uk.gov.hmrc.play.frontend.controller.FrontendController
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.Exception.nonFatalCatch
import scala.util.control.NonFatal


@Singleton
class SubmissionController @Inject()(val sec: SecuredActions,
                                     val fus:FileUploadService,
                                     val docRefIdService: DocRefIdService,
                                     val reportingEntityDataService: ReportingEntityDataService,
                                     val cbidService: CBCIdService,
                                     val emailService:EmailService)(implicit ec: ExecutionContext,cache:CBCSessionCache,auth:AuthConnector) extends FrontendController with ServicesConfig{


  implicit lazy val fusUrl   = new ServiceUrl[FusUrl] { val url = baseUrl("file-upload")}
  implicit lazy val fusFeUrl = new ServiceUrl[FusFeUrl] { val url = baseUrl("file-upload-frontend")}
  implicit lazy val cbcrsUrl = new ServiceUrl[CbcrsUrl] { val url = baseUrl("cbcr")}
  lazy val audit: AuditConnector = FrontendAuditConnector

  val dateFormat = DateTimeFormatter.ofPattern("dd MMMM yyyy 'at' HH:mm")

  def saveDocRefIds(x:XMLInfo)(implicit hc:HeaderCarrier): EitherT[Future,NonEmptyList[UnexpectedState],Unit] = {
    x.reportingEntity.docSpec.docType match {
      case OECD0 =>
        val reportIds = x.cbcReport.map { reports => reports.docSpec.docRefId -> reports.docSpec.corrDocRefId }
        val addIds = x.additionalInfo.map(addInfo => addInfo.docSpec.docRefId -> addInfo.docSpec.corrDocRefId)

        val crResult = OptionT.fromOption[Future](reportIds).flatMap {
          case (doc, corr) => corr.map(docRefIdService.saveCorrDocRefID(_, doc)).getOrElse(docRefIdService.saveDocRefId(doc))
        }
        val addResult = OptionT.fromOption[Future](addIds).flatMap {
          case (doc, corr) => corr.map(docRefIdService.saveCorrDocRefID(_, doc)).getOrElse(docRefIdService.saveDocRefId(doc))
        }

        EitherT((crResult.toLeft(()).toValidatedNel |@| addResult.toLeft(()).toValidatedNel).map(
          (a, b) => (a |@| b).map((_, _) => ()).toEither)
        )

      case OECD1 | OECD2 | OECD3 =>
        val reDocRef = x.reportingEntity.docSpec.docRefId
        val reCorr = x.reportingEntity.docSpec.corrDocRefId
        val reportIds = x.cbcReport.map { reports => reports.docSpec.docRefId -> reports.docSpec.corrDocRefId }
        val addIds = x.additionalInfo.map(addInfo => addInfo.docSpec.docRefId -> addInfo.docSpec.corrDocRefId)

        val reResult = reCorr.map(c => docRefIdService.saveCorrDocRefID(c, reDocRef)).getOrElse(docRefIdService.saveDocRefId(reDocRef))
        val crResult = OptionT.fromOption[Future](reportIds).flatMap {
          case (doc, corr) => corr.map(docRefIdService.saveCorrDocRefID(_, doc)).getOrElse(docRefIdService.saveDocRefId(doc))
        }
        val addResult = OptionT.fromOption[Future](addIds).flatMap {
          case (doc, corr) => corr.map(docRefIdService.saveCorrDocRefID(_, doc)).getOrElse(docRefIdService.saveDocRefId(doc))
        }

        EitherT((reResult.toLeft(()).toValidatedNel |@|
          crResult.toLeft(()).toValidatedNel |@|
          addResult.toLeft(()).toValidatedNel).map((a, b, c) => (a |@| b |@| c).map((_, _, _) => ()).toEither))

    }
  }

  def storeOrUpdateReportingEntityData(xml:XMLInfo)(implicit hc:HeaderCarrier) : ServiceResponse[Unit] =
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
      case OECD0 | OECD2 | OECD3 => reportingEntityDataService.updateReportingEntityData(PartialReportingEntityData.extract(xml))
    }

  def confirm = sec.AsyncAuthenticatedAction() { authContext => implicit request =>
    OptionT(cache.read[SummaryData]).toRight(InternalServerError(FrontendGlobal.internalServerErrorTemplate)).flatMap {summaryData =>
      (for {
            xml <- OptionT(cache.read[XMLInfo]).toRight(UnexpectedState("Unable to read XMLInfo from cache"))
            _   <- fus.uploadMetadataAndRoute(summaryData.submissionMetaData)
            _   <- saveDocRefIds(xml).leftMap[CBCErrors]{ es =>
              Logger.error(s"Errors saving Corr/DocRefIds : ${es.map(_.errorMsg).toList.mkString("\n")}")
              UnexpectedState("Errors in saving Corr/DocRefIds aborting submission")
            }
            _  <- right(cache.save(SubmissionDate(LocalDateTime.now)))
            _  <- storeOrUpdateReportingEntityData(xml)
          } yield Redirect(routes.SubmissionController.submitSuccessReceipt())).leftMap(errorRedirect)
    }.merge
  }

  def notRegistered =  sec.AsyncAuthenticatedAction(Some(Organisation)) { authContext => implicit request =>
    Ok(views.html.submission.notRegistered(includes.asideBusiness(), includes.phaseBannerBeta()))
  }
  def createSuccessfulSubmissionAuditEvent(authContext: AuthContext, summaryData:SummaryData)
                                          (implicit hc:HeaderCarrier, request:Request[_]): ServiceResponse[AuditResult.Success.type] =
    for {
      ggId   <- right(getUserGGId(authContext))
      result <- EitherT[Future,CBCErrors,AuditResult.Success.type ](audit.sendEvent(ExtendedDataEvent("Country-By-Country-Frontend", "CBCRFilingSuccessful",
        tags = hc.toAuditTags("CBCRFilingSuccessful", "N/A") + ("path" -> request.uri, "ggId" -> ggId.authProviderId),
        detail = Json.toJson(summaryData)
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

  val submitterInfoForm: Form[SubmitterInfo] = Form(
    mapping(
      "fullName"        -> nonEmptyText,
      "contactPhone" -> nonEmptyText,
      "email"       -> email.verifying(EmailAddress.isValid(_))
    )((fullName: String, contactPhone:String, email: String) => {
      SubmitterInfo(fullName, None, contactPhone, EmailAddress(email),None)
    }
    )(si => Some((si.fullName, si.contactPhone, si.email.value)))
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

  val upe = sec.AsyncAuthenticatedAction() { authContext => implicit request =>
    Future.successful(Ok(views.html.submission.submitInfoUltimateParentEntity(
      includes.asideBusiness(), includes.phaseBannerBeta(), ultimateParentEntityForm
    )))
  }

  val submitUltimateParentEntity = sec.AsyncAuthenticatedAction() { authContext => implicit request =>

    ultimateParentEntityForm.bindFromRequest.fold(
      formWithErrors => Future.successful(BadRequest(views.html.submission.submitInfoUltimateParentEntity(
        includes.asideBusiness(), includes.phaseBannerBeta(), formWithErrors))),
      success => {
        val result = for {
          _       <- OptionT.liftF(cache.save(success))
          xmlInfo <- OptionT(cache.read[XMLInfo])
        } yield xmlInfo.reportingEntity.reportingRole

        result.cata(
          errorRedirect(UnexpectedState("Unable to find KeyXMLFileInfo in cache")),
          {
            case CBC701 =>
              errorRedirect(UnexpectedState("ReportingRole was CBC701 - we should never be here"))
            case CBC702 =>
              Redirect(routes.SubmissionController.utr())
            case CBC703 =>
              Redirect(routes.SubmissionController.enterCompanyName())
          })
      }
    )
  }


  val utr = sec.AsyncAuthenticatedAction() { authContext => implicit request =>
    Ok(views.html.submission.utrCheck( includes.phaseBannerBeta(),utrForm ))
  }

  val submitUtr = sec.AsyncAuthenticatedAction() { authContext => implicit request =>

    utrForm.bindFromRequest.fold[Future[Result]](
      (formWithErrors: Form[Utr]) => BadRequest(views.html.submission.utrCheck(includes.phaseBannerBeta(),formWithErrors)),
      (utr: Utr)                  => cache.save(utr).map(_ => Redirect(routes.SubmissionController.enterCompanyName()))
    )

  }


  val submitterInfo = sec.AsyncAuthenticatedAction() { authContext => implicit request =>

      OptionT(cache.read[XMLInfo]).map(kXml => kXml.reportingEntity.reportingRole match {

        case CBC701 =>
          for {
            _ <- cache.save(kXml.reportingEntity.tin)
            _ <- cache.save(FilingType(CBC701))
            _ <- cache.save(UltimateParentEntity(kXml.reportingEntity.name))
          } yield ()

        case CBC702 =>
          cache.save(FilingType(CBC702))

        case CBC703 =>
          for {
            _ <- cache.save(kXml.reportingEntity.tin)
            _ <- cache.save(FilingType(CBC703))
          } yield ()

      }).cata(
        errorRedirect(UnexpectedState("Unable to read KeyXMLFileInfo from cache")),
        _ => Ok(views.html.submission.submitterInfo(includes.asideBusiness(), includes.phaseBannerBeta(), submitterInfoForm))
      )
  }


  val submitSubmitterInfo = sec.AsyncAuthenticatedAction() { authContext =>
    implicit request =>
      getUserType(authContext)(cache, auth, implicitly[HeaderCarrier], implicitly[ExecutionContext]).semiflatMap { userType =>
        submitterInfoForm.bindFromRequest.fold(
          formWithErrors => Future.successful(BadRequest(views.html.submission.submitterInfo(
            includes.asideBusiness(), includes.phaseBannerBeta(), formWithErrors
          ))),
          success => {
            val passStraightThrough = for {
              straightThrough <- right[Boolean](cache.read[CBCId].map(_.isDefined))
              ag              <- OptionT(cache.read[AffinityGroup]).toRight(UnexpectedState("Affinity group not found in cache"))
              name            <- OptionT(cache.read[AgencyBusinessName]).toRight(UnexpectedState("Agency/BusinessName not found in cache"))
              _               <- right[CacheMap](cache.save(success.copy( affinityGroup = Some(ag), agencyBusinessName = Some(name))))
              xml             <- OptionT(cache.read[XMLInfo]).toRight(UnexpectedState("XMLInfo not found in cache"))
              _               <- right(cache.save(xml.messageSpec.sendingEntityIn))
            } yield straightThrough

            passStraightThrough.fold(
              error           => errorRedirect(error),
              straightThrough => userType match{
                case Organisation =>
                  if (straightThrough) Redirect(routes.SubmissionController.submitSummary())
                  else Redirect(routes.SharedController.enterCBCId())
                case Agent =>
                  Redirect(routes.SharedController.verifyKnownFactsAgent())
              }
            )
          }
        )
      }.fold(
        error => errorRedirect(error),
        result => result
      )
  }



  val submitSummary = sec.AsyncAuthenticatedAction() { authContext => implicit request =>

    val result = for {
      smd     <- EitherT(generateMetadataFile(cache,authContext).map(_.toEither))
        .leftMap(errors => UnexpectedState(errors.toList.mkString("\n")))
      sd      <- createSummaryData(smd)
    } yield Ok(views.html.submission.submitSummary(includes.phaseBannerBeta(), sd))
    result.fold(
      errors => errorRedirect(errors),
      result => result
    ).recover{
      case NonFatal(e) =>
        Logger.error(e.getMessage,e)
        InternalServerError
    }


  }

  def createSummaryData(submissionMetaData: SubmissionMetaData)(implicit hc:HeaderCarrier) : ServiceResponse[SummaryData] = {
    for {
      keyXMLFileInfo <- OptionT(cache.read[XMLInfo]).toRight(UnexpectedState("XMLInfo not found in cache"))
      bpr            <- OptionT(cache.read[BusinessPartnerRecord]).toRight(UnexpectedState("BPR not found in cache"))
      summaryData    = SummaryData(bpr, submissionMetaData, keyXMLFileInfo)
      _              <- right(cache.save[SummaryData](summaryData))
    } yield summaryData
  }

  def enterCompanyName = sec.AsyncAuthenticatedAction() { authContext => implicit request =>
    Ok(views.html.submission.enterCompanyName(includes.asideBusiness(),includes.phaseBannerBeta(),enterCompanyNameForm))
  }

  def saveCompanyName = sec.AsyncAuthenticatedAction() { authContext => implicit request =>
    enterCompanyNameForm.bindFromRequest().fold(
      errors => BadRequest(views.html.submission.enterCompanyName(includes.asideBusiness(),includes.phaseBannerBeta(), errors)),
      name   => cache.save(name).map(_ => Redirect(routes.SubmissionController.submitterInfo()))
    )
  }

  def submitSuccessReceipt = sec.AsyncAuthenticatedAction() { authContext => implicit request =>

    val data: EitherT[Future, CBCErrors, (SummaryData, String)] =
      for {
        dataTuple          <- right((cache.read[SummaryData] |@| cache.read[SubmissionDate]).tupled)
        data               <- fromEither(dataTuple._1 toRight UnexpectedState("SummaryData not found in cache"))
        date               <- fromEither(dataTuple._2 toRight UnexpectedState("SubmissionDate not found in cache"))
        formattedDate      <- fromEither((nonFatalCatch opt date.date.format(dateFormat)).toRight(UnexpectedState(s"Unable to format date: ${date.date} to format $dateFormat")))
        emailSentAlready   <- right(cache.read[ConfirmationEmailSent].map(_.isDefined))
        sentEmail          <- if(!emailSentAlready)right(emailService.sendEmail(makeSubmissionSuccessEmail(data, formattedDate)).value)
                              else  pure(None)
        _                  <- if(sentEmail.getOrElse(false))right(cache.save[ConfirmationEmailSent](ConfirmationEmailSent()))
                              else pure(())
      } yield (data, formattedDate)



    data.flatMap(t =>
      createSuccessfulSubmissionAuditEvent(authContext,t._1).map(_ => {
      (t._1.submissionMetaData.submissionInfo.hash, t._2)
    })).fold(
      (error: CBCErrors) => errorRedirect(error),
      (tuple: (Hash, String))  => {
        Ok(views.html.submission.submitSuccessReceipt(includes.asideBusiness(), includes.phaseBannerBeta(), tuple._2, tuple._1.value))
      }
    )
  }

  private def makeSubmissionSuccessEmail(data:SummaryData,formattedDate:String):Email ={
    val submittedInfo = data.submissionMetaData.submitterInfo
    Email(List(submittedInfo.email.toString()),
      "cbcr_report_confirmation",
      Map(
        "name" → submittedInfo.fullName,
        "received_at" → formattedDate,
        "hash"   → data.submissionMetaData.submissionInfo.hash.value
      ))
  }

  val filingHistory = Action.async { implicit request => Ok(views.html.submission.filingHistory(includes.phaseBannerBeta())) }

}
