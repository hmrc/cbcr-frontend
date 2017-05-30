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

import javax.inject.{Inject, Singleton}

import cats.data.{EitherT, OptionT, ValidatedNel}
import cats.instances.all._
import cats.syntax.all._
import play.api.Logger
import play.api.mvc.{Action, Result}
import uk.gov.hmrc.cbcrfrontend.auth.SecuredActions
import uk.gov.hmrc.cbcrfrontend.views.html.includes
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.frontend.controller.FrontendController
import play.api.i18n.Messages.Implicits._
import play.api.Play.current
import play.api.data.Form
import play.api.data.Forms._
import uk.gov.hmrc.cbcrfrontend.exceptions.UnexpectedState
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.services.{CBCSessionCache, FileUploadService}
import uk.gov.hmrc.cbcrfrontend.typesclasses.{CbcrsUrl, FusFeUrl, FusUrl, ServiceUrl}
import uk.gov.hmrc.emailaddress.EmailAddress
import uk.gov.hmrc.http.cache.client.CacheMap
import uk.gov.hmrc.play.http.HeaderCarrier
import uk.gov.hmrc.cbcrfrontend._
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.xmlextractor.XmlExtractor

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal


@Singleton
class Submission @Inject()(val sec: SecuredActions, val cache:CBCSessionCache,val fus:FileUploadService, val xmlExtractor: XmlExtractor)(implicit ec: ExecutionContext) extends FrontendController with ServicesConfig{

  implicit lazy val fusUrl   = new ServiceUrl[FusUrl] { val url = baseUrl("file-upload")}
  implicit lazy val fusFeUrl = new ServiceUrl[FusFeUrl] { val url = baseUrl("file-upload-frontend")}
  implicit lazy val cbcrsUrl = new ServiceUrl[CbcrsUrl] { val url = baseUrl("cbcr")}

/*
  def confirm = sec.AsyncAuthenticatedAction() { authContext => implicit request =>
    generateMetadataFile(authContext.user.userId,cache).flatMap {
      _.fold(
        errors => {
          Logger.error(errors.toList.mkString(", "))
          Future.successful(InternalServerError(errors.toList.mkString(", ")))
        },
        data   => fus.uploadMetadataAndRoute(data).fold(
          errors => {
            Logger.error(errors.errorMsg)
            InternalServerError
          },
          _      => Redirect(routes.Submission.submitSuccessReceipt())
        )
      )
    }.recover{
      case NonFatal(e) =>
        Logger.error(e.getMessage,e)
        InternalServerError
    }
  }
*/

  val filingTypeForm: Form[FilingType] = Form(
    mapping("filingType" -> nonEmptyText,
            "filingTypeText" -> nonEmptyText
    )((filingType: String, filingTypeText: String) => FilingType(filingType, filingTypeText)) (ft => Some((ft.filingType, ft.filingTypeText)))
  )

  val ultimateParentEntityForm: Form[UltimateParentEntity] = Form(
    mapping("ultimateParentEntity" -> nonEmptyText
    )((ultimateParentEntity: String) => UltimateParentEntity(ultimateParentEntity)) (upe => Some(upe.ultimateParentEntity))
  )

  val filingCapacityForm: Form[FilingCapacity] = Form(
    mapping("filingCapacity" -> nonEmptyText,
            "filingCapacityText" -> nonEmptyText
    )((filingCapacity: String, filingCapacityText: String) => FilingCapacity(filingCapacity, filingCapacityText)) (ft => Some((ft.filingCapacity, ft.filingCapacityText)))
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

  val summarySubmitForm : Form[Boolean] = Form(
    single(
      "cbcDeclaration" -> boolean.verifying(d => d == true)
    )
  )

  val filingType = sec.AsyncAuthenticatedAction() { authContext => implicit request =>
    Future.successful(Ok(uk.gov.hmrc.cbcrfrontend.views.html.forms.submitInfoFilingType(
      includes.asideBusiness(), includes.phaseBannerBeta(), filingTypeForm
    )))
  }

  val submitFilingType = sec.AsyncAuthenticatedAction() { authContext => implicit request =>

    filingTypeForm.bindFromRequest.fold(
      formWithErrors => Future.successful(BadRequest(uk.gov.hmrc.cbcrfrontend.views.html.forms.submitInfoFilingType(
        includes.asideBusiness(), includes.phaseBannerBeta(), formWithErrors))),
      success => {
        cache.save(success).map(_ =>
          Ok(uk.gov.hmrc.cbcrfrontend.views.html.forms.submitInfoUltimateParentEntity(
            includes.asideBusiness(), includes.phaseBannerBeta(), ultimateParentEntityForm
          ))
        )
      }
    )
  }



  val submitUltimateParentEntity = sec.AsyncAuthenticatedAction() { authContext => implicit request =>

    ultimateParentEntityForm.bindFromRequest.fold(
      formWithErrors => Future.successful(BadRequest(uk.gov.hmrc.cbcrfrontend.views.html.forms.submitInfoUltimateParentEntity(
        includes.asideBusiness(), includes.phaseBannerBeta(), formWithErrors))),
      success => {
        cache.save(success).map(_ =>
          Ok(uk.gov.hmrc.cbcrfrontend.views.html.forms.submitInfoFilingCapacity(
            includes.asideBusiness(), includes.phaseBannerBeta(),filingCapacityForm
          ))
        )
      }
    )
  }


  val submitFilingCapacity = sec.AsyncAuthenticatedAction(None) { authContext => implicit request =>

    filingCapacityForm.bindFromRequest.fold(
      formWithErrors => Future.successful(BadRequest(uk.gov.hmrc.cbcrfrontend.views.html.forms.submitInfoFilingCapacity(
        includes.asideBusiness(), includes.phaseBannerBeta(), formWithErrors))),
      success => cache.save(success).map(_ =>
        Ok(uk.gov.hmrc.cbcrfrontend.views.html.forms.submitterInfo(
          includes.asideBusiness(), includes.phaseBannerBeta(),submitterInfoForm
        ))
      )
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
      } yield Redirect(routes.Submission.submitSummary())
    )
  }

  val submitSummary = sec.AsyncAuthenticatedAction() { authContext => implicit request =>


    generateMetadataFile(authContext.user.userId,cache).flatMap { md =>
      md.fold(
        errors => {
          Logger.error(errors.toList.mkString(", "))
          Future.successful(InternalServerError(errors.toList.mkString(", ")))
        },
        submissionMetaData => (for {
          file <- fus.getFile(submissionMetaData.fileInfo.envelopeId.value, submissionMetaData.fileInfo.id.value)
          keyXMLFileInfo <- EitherT.fromEither[Future](xmlExtractor.getKeyXMLFileInfo(file).toEither).leftMap(_ => UnexpectedState("Problems extracting xml"))
          bpr  <- OptionT(cache.read[BusinessPartnerRecord]).toRight(UnexpectedState("BPR not found in cache"))
          summaryData <- EitherT.right[Future,UnexpectedState, SummaryData](Future.successful(SummaryData(bpr,submissionMetaData, keyXMLFileInfo)))
          _    <- EitherT.right[Future,UnexpectedState,CacheMap](cache.save[SummaryData](summaryData))
        } yield (summaryData)).fold(
            (error: UnexpectedState) => {
              Logger.error(s"Error getting file: ${error.errorMsg}")
              InternalServerError
            },
          summaryData => {
            Ok(uk.gov.hmrc.cbcrfrontend.views.html.forms.submitSummary(
              includes.phaseBannerBeta(), summaryData, summarySubmitForm))
          }
        )
      )

    }.recover{
      case NonFatal(e) =>
        Logger.error(e.getMessage,e)
        InternalServerError
    }
  }


  def confirm = sec.AsyncAuthenticatedAction() { authContext =>
    implicit request =>

      OptionT(cache.read[SummaryData]).toRight(UnexpectedState("Summary Data not found in cache")).fold(
        errors => InternalServerError,
        summaryData => {
          summarySubmitForm.bindFromRequest.fold(
            formWithErrors => BadRequest(uk.gov.hmrc.cbcrfrontend.views.html.forms.submitSummary(
              includes.phaseBannerBeta(), summaryData, formWithErrors)),
            _ => {
                fus.uploadMetadataAndRoute(summaryData.submissionMetaData)
                Redirect(routes.Submission.submitSuccessReceipt())
            }
          )
        }
      )
  }

  val submitSuccessReceipt = Action.async { implicit request =>
    Future.successful(Ok(uk.gov.hmrc.cbcrfrontend.views.html.forms.submitSuccessReceipt(
      includes.asideBusiness(), includes.phaseBannerBeta()
    )))
  }

  val filingHistory = Action.async { implicit request =>
    Future.successful(Ok(uk.gov.hmrc.cbcrfrontend.views.html.forms.filingHistory(
      includes.phaseBannerBeta()
    )))
  }
}
