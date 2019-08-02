/*
 * Copyright 2019 HM Revenue & Customs
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
import cats.instances.future._
import play.api.i18n.{I18nSupport, Messages, MessagesApi}
import play.api.libs.json.Json
import play.api.mvc.{Action, MessagesControllerComponents, Request}
import play.api.{Configuration, Logger}
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.form.SurveyForm
import uk.gov.hmrc.cbcrfrontend.model.{SurveyAnswers, UnexpectedState}
import uk.gov.hmrc.cbcrfrontend.views.html._
import uk.gov.hmrc.play.audit.AuditExtensions._
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}
import uk.gov.hmrc.play.audit.model.ExtendedDataEvent
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class ExitSurveyController @Inject()(val config:Configuration,
                                     val audit:AuditConnector)
                                    (implicit conf:FrontendAppConfig,
                                     override val messagesApi:MessagesApi,
                                     val ec: ExecutionContext,
                                     messagesControllerComponents: MessagesControllerComponents) extends CBCRFrontendController(messagesControllerComponents) with I18nSupport{

  val doSurvey = Action{ implicit request =>
    Ok(survey.exitSurvey( SurveyForm.surveyForm))
  }

  val surveyAcknowledge =  Action { implicit request =>
    Ok(survey.exitSurveyComplete())
  }


  val submit = Action.async{ implicit request =>
    SurveyForm.surveyForm.bindFromRequest().fold(
      errors  => Future.successful(BadRequest(survey.exitSurvey( errors))),
      answers => auditSurveyAnswers(answers).fold(
        errors => {
          Logger.error(errors.toString)//          Redirect(routes.SharedController.guidance())
          Redirect(routes.ExitSurveyController.surveyAcknowledge())
        },
        _      => Redirect(routes.ExitSurveyController.surveyAcknowledge())
      )
    )
  }

  def auditSurveyAnswers(answers: SurveyAnswers)(implicit request:Request[_]) : ServiceResponse[AuditResult.Success.type ] = {
    eitherT[AuditResult.Success.type](audit.sendExtendedEvent(ExtendedDataEvent("Country-By-Country-Frontend", "CBCRExitSurvey",
      detail = Json.toJson(answers)
    )).map {
      case AuditResult.Disabled        => Right(AuditResult.Success)
      case AuditResult.Success         => Right(AuditResult.Success)
      case AuditResult.Failure(msg, _) => Left(UnexpectedState(s"Unable to audit an exit survey: $msg"))
    })

  }

}
