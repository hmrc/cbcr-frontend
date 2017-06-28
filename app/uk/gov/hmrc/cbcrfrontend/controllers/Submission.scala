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

import cats.data.{EitherT, OptionT}
import cats.instances.all._
import cats.syntax.all._
import play.api.Logger
import play.api.Play.current
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.Messages.Implicits._
import play.api.mvc.{Action, Result}
import uk.gov.hmrc.cbcrfrontend._
import uk.gov.hmrc.cbcrfrontend.auth.SecuredActions
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.services.{CBCSessionCache, FileUploadService}
import uk.gov.hmrc.cbcrfrontend.typesclasses.{CbcrsUrl, FusFeUrl, FusUrl, ServiceUrl}
import uk.gov.hmrc.cbcrfrontend.views.html.includes
import uk.gov.hmrc.emailaddress.EmailAddress
import uk.gov.hmrc.http.cache.client.CacheMap
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal


@Singleton
class Submission @Inject()(val sec: SecuredActions, val cache:CBCSessionCache,val fus:FileUploadService)(implicit ec: ExecutionContext) extends FrontendController with ServicesConfig{

  implicit lazy val fusUrl   = new ServiceUrl[FusUrl] { val url = baseUrl("file-upload")}
  implicit lazy val fusFeUrl = new ServiceUrl[FusFeUrl] { val url = baseUrl("file-upload-frontend")}
  implicit lazy val cbcrsUrl = new ServiceUrl[CbcrsUrl] { val url = baseUrl("cbcr")}

  def confirm = sec.AsyncAuthenticatedAction() { authContext => implicit request =>

    OptionT(cache.read[SummaryData]).toRight(InternalServerError(FrontendGlobal.internalServerErrorTemplate)).flatMap {
      summaryData => {
        summarySubmitForm.bindFromRequest.fold[EitherT[Future,Result,Result]](
          formWithErrors => EitherT.left(Future.successful(BadRequest(uk.gov.hmrc.cbcrfrontend.views.html.forms.submitSummary(includes.phaseBannerBeta(), summaryData, formWithErrors)))),
          _              => fus.uploadMetadataAndRoute(summaryData.submissionMetaData).bimap(
            errors => errorRedirect(errors),
            _      => {
              val submissionDateTime = LocalDateTime.now.format(DateTimeFormatter.ofPattern("dd/MM/yyyy 'at' HH.MM."))
              Redirect(routes.Submission.submitSuccessReceipt(submissionDateTime, summaryData.submissionMetaData.submissionInfo.hash.value))
            }
          )
        )
      }
    }.merge
  }

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
      "agencyBusinessName" -> nonEmptyText,
      "jobRole"        -> nonEmptyText,
      "contactPhone" -> nonEmptyText,
      "email"       -> email.verifying(EmailAddress.isValid(_))
    )((fullName: String, agencyBusinessName:String, jobRole:String, contactPhone:String, email: String) => {
      SubmitterInfo(fullName, agencyBusinessName, jobRole, contactPhone, EmailAddress(email),None)
    }
    )(si => Some((si.fullName,si.agencyBusinessName,si.jobRole, si.contactPhone, si.email.value)))
  )

  val reconfirmEmailForm : Form[EmailAddress] = Form(
    mapping(
      "reconfirmEmail" -> email.verifying(EmailAddress.isValid(_))
    )(EmailAddress.apply)(EmailAddress.unapply)
  )

  val summarySubmitForm : Form[Boolean] = Form(
    single(
      "cbcDeclaration" -> boolean.verifying(d => d)
    )
  )

  val upe = sec.AsyncAuthenticatedAction() { authContext => implicit request =>
    Future.successful(Ok(uk.gov.hmrc.cbcrfrontend.views.html.forms.submitInfoUltimateParentEntity(
      includes.asideBusiness(), includes.phaseBannerBeta(), ultimateParentEntityForm
    )))
  }

  val submitUltimateParentEntity = sec.AsyncAuthenticatedAction() { authContext => implicit request =>

    ultimateParentEntityForm.bindFromRequest.fold(
      formWithErrors => Future.successful(BadRequest(uk.gov.hmrc.cbcrfrontend.views.html.forms.submitInfoUltimateParentEntity(
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
              Redirect(routes.Submission.utr())
            case CBC703 =>
              Redirect(routes.Submission.submitterInfo())
          })
      }
    )
  }


  val utr = sec.AsyncAuthenticatedAction() { authContext => implicit request =>
    Ok(views.html.forms.utrCheck( includes.phaseBannerBeta(),utrForm ))
  }

  val submitUtr = sec.AsyncAuthenticatedAction() { authContext => implicit request =>

    utrForm.bindFromRequest.fold[Future[Result]](
      (formWithErrors: Form[Utr]) => BadRequest(views.html.forms.utrCheck(includes.phaseBannerBeta(),formWithErrors)),
      (utr: Utr)                  => cache.save(utr).map(_ => Redirect(routes.Submission.submitterInfo()))
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
        _ => Ok(uk.gov.hmrc.cbcrfrontend.views.html.forms.submitterInfo(includes.asideBusiness(), includes.phaseBannerBeta(), submitterInfoForm))
      )
  }

  val submitSubmitterInfo = sec.AsyncAuthenticatedAction() { authContext => implicit request =>
    submitterInfoForm.bindFromRequest.fold(
      formWithErrors => Future.successful(BadRequest(uk.gov.hmrc.cbcrfrontend.views.html.forms.submitterInfo(
        includes.asideBusiness(), includes.phaseBannerBeta(), formWithErrors
      ))),
      success => for {
        ag <- cache.read[AffinityGroup]
        _  <- cache.save(success.copy(affinityGroup = ag))
      } yield Redirect(routes.Submission.reconfirmEmail())
    )
  }


  val reconfirmEmail = sec.AsyncAuthenticatedAction() { authContext => implicit request =>

    OptionT(cache.read[SubmitterInfo]).toRight(InternalServerError(FrontendGlobal.internalServerErrorTemplate)).fold (
      error => error,
      submitterInfo => Ok(uk.gov.hmrc.cbcrfrontend.views.html.forms.reconfirmEmail(includes.asideBusiness(), includes.phaseBannerBeta(), reconfirmEmailForm.fill(submitterInfo.email), true))
    )
  }


  val reconfirmEmailSubmit = sec.AsyncAuthenticatedAction() { authContext => implicit request =>

    reconfirmEmailForm.bindFromRequest.fold (
      formWithErrors => Future.successful(BadRequest(uk.gov.hmrc.cbcrfrontend.views.html.forms.reconfirmEmail(
        includes.asideBusiness(), includes.phaseBannerBeta(), formWithErrors, true
      ))),
      success => (for {
        submitterInfo <- OptionT(cache.read[SubmitterInfo]).toRight(UnexpectedState("Submitter Info not found in the cache"))
        saved         <- EitherT.right[Future, CBCErrors,CacheMap](cache.save[SubmitterInfo](submitterInfo.copy(email = success)))
      } yield saved).fold(
        error => errorRedirect(error),
        _     => Redirect(routes.Submission.submitSummary())
      )
    )

  }

  val submitSummary = sec.AsyncAuthenticatedAction() { authContext => implicit request =>

    generateMetadataFile(authContext.user.userId,cache).flatMap { md =>
      md.fold(
        errors => {
          Logger.error(errors.toList.mkString(", "))
          Future.successful(InternalServerError(FrontendGlobal.internalServerErrorTemplate))
        },
        submissionMetaData => (for {
          keyXMLFileInfo <- OptionT(cache.read[XMLInfo]).toRight(UnexpectedState("XMLInfo not found in cache"))
          bpr            <- OptionT(cache.read[BusinessPartnerRecord]).toRight(UnexpectedState("BPR not found in cache"))
          summaryData     = SummaryData(bpr,submissionMetaData, keyXMLFileInfo)
          _              <- EitherT.right[Future,CBCErrors,CacheMap](cache.save[SummaryData](summaryData))
        } yield summaryData).fold(
          error       => errorRedirect(error),
          summaryData => Ok(uk.gov.hmrc.cbcrfrontend.views.html.forms.submitSummary( includes.phaseBannerBeta(), summaryData, summarySubmitForm))
        )
      )

    }.recover{
      case NonFatal(e) =>
        Logger.error(e.getMessage,e)
        InternalServerError
    }

  }

  def submitSuccessReceipt(submissionDateTime: String, hash: String) = sec.AsyncAuthenticatedAction() { authContext => implicit request =>
    Ok(uk.gov.hmrc.cbcrfrontend.views.html.forms.submitSuccessReceipt(includes.asideBusiness(), includes.phaseBannerBeta(), submissionDateTime: String, hash: String ))
  }

  val filingHistory = Action.async { implicit request =>
    Ok(uk.gov.hmrc.cbcrfrontend.views.html.forms.filingHistory(includes.phaseBannerBeta()))
  }

}
