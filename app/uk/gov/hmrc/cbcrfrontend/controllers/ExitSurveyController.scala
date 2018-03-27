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

import javax.inject.{Inject, Singleton}

import cats.data.EitherT
import com.typesafe.config.Config
import uk.gov.hmrc.cbcrfrontend.views.html._
import play.api.{Configuration, Logger}
import uk.gov.hmrc.cbcrfrontend.auth.SecuredActions
import play.api.Play.current
import play.api.i18n.Messages.Implicits._
import configs.syntax._
import play.api.libs.json.Json
import play.api.mvc.{Action, Request}
import uk.gov.hmrc.cbcrfrontend.FrontendAuditConnector
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.form.SurveyForm
import uk.gov.hmrc.cbcrfrontend.model.{CBCErrors, SurveyAnswers, UnexpectedState}
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}
import uk.gov.hmrc.play.frontend.controller.FrontendController
import uk.gov.hmrc.cbcrfrontend.views.html.includes
import uk.gov.hmrc.play.audit.model.ExtendedDataEvent
import uk.gov.hmrc.play.audit.AuditExtensions._
import cats.instances.future._

import scala.concurrent.Future

@Singleton
class ExitSurveyController @Inject()(val sec: SecuredActions, val config:Configuration) extends FrontendController {

  lazy val audit: AuditConnector = FrontendAuditConnector

  val doSurvey = Action{ implicit request =>
    Ok(survey.exitSurvey(includes.asideBusiness(), includes.phaseBannerBeta(), SurveyForm.surveyForm))
  }

  val surveyAcknowledge =  Action { implicit request =>
    Ok(survey.exitSurveyComplete(includes.phaseBannerBeta()))
  }


  val submit = Action.async{ implicit request =>
    SurveyForm.surveyForm.bindFromRequest().fold(
      errors  => Future.successful(BadRequest(survey.exitSurvey(includes.asideBusiness(), includes.phaseBannerBeta(), errors))),
      answers => auditSurveyAnswers(answers).fold(
        errors => {
          Logger.error(errors.toString)//          Redirect(routes.SharedController.guidance())
          Redirect(routes.ExitSurveyController.surveyAcknowledge())
        },
        _      => Redirect(routes.ExitSurveyController.surveyAcknowledge())
      )
    )
  }

//  val continue = Action.async{ implicit request =>
//    Future.successful(Ok(uk.gov.hmrc.cbcrfrontend.views.html.guidance.guidanceOverviewQa()))
//  }


  def auditSurveyAnswers(answers: SurveyAnswers)(implicit request:Request[_]) : ServiceResponse[AuditResult.Success.type ] = {
    eitherT[AuditResult.Success.type](audit.sendEvent(ExtendedDataEvent("Country-By-Country-Frontend", "CBCRExitSurvey",
      tags = hc.toAuditTags("CBCRExitSurvey", "N/A"),
      detail = Json.toJson(Map("answers" -> Json.toJson(answers)))
    )).map {
      case AuditResult.Disabled        => Right(AuditResult.Success)
      case AuditResult.Success         => Right(AuditResult.Success)
      case AuditResult.Failure(msg, _) => Left(UnexpectedState(s"Unable to audit an exit survey: $msg"))
    })

  }

}
