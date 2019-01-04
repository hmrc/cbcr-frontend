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

import java.nio.file.{Path, Paths}

import javax.inject.{Inject, Singleton}
import cats.data.{EitherT, OptionT}
import cats.instances.all._
import cats.syntax.all._
import play.api.data.Form
import play.api.data.Forms._
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationError}
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.libs.json.{JsString, Json}
import play.api.mvc.Results.Unauthorized
import play.api.mvc.{Action, AnyContent, Request, Result}
import play.api.{Configuration, Environment, Logger}
import uk.gov.hmrc.auth.core.AffinityGroup.{Agent, Individual}
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.retrieve.Retrievals
import uk.gov.hmrc.cbcrfrontend._
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.services._
import uk.gov.hmrc.cbcrfrontend.views.html._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.AuditExtensions._
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}
import uk.gov.hmrc.play.audit.model.ExtendedDataEvent
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import uk.gov.hmrc.cbcrfrontend.views.html.subscription.notAuthorised

import scala.concurrent.Future


@Singleton
class SharedController @Inject()(val messagesApi: MessagesApi,
                                 val subDataService: SubscriptionDataService,
                                 val knownFactsService: BPRKnownFactsService,
                                 val rrService: DeEnrolReEnrolService,
                                 val audit: AuditConnector,
                                 val env:Environment,
                                 val authConnector:AuthConnector
                                )(implicit val cache:CBCSessionCache,
                                  val config: Configuration,
                                  feConfig:FrontendAppConfig) extends FrontendController with AuthorisedFunctions with I18nSupport {

  val utrConstraint: Constraint[String] = Constraint("constraints.utrcheck"){
    case utr if Utr(utr).isValid => Valid
    case _                       => Invalid(ValidationError("UTR is not valid"))
  }

  val knownFactsForm = Form(
    mapping(
      "utr" -> nonEmptyText.verifying(utrConstraint),
      "postCode" -> text
    )((u,p) => BPRKnownFacts(Utr(u),p))((facts: BPRKnownFacts) => Some(facts.utr.value -> facts.postCode))
  )

  val cbcIdForm : Form[CBCId] = Form(
    single( "cbcId" -> of[CBCId])
  )

  val technicalDifficulties = Action{ implicit request =>
    InternalServerError(uk.gov.hmrc.cbcrfrontend.views.html.error_template ("Internal Server Error", "Internal Server Error", "Something went wrong"))
  }

  val sessionExpired = Action{ implicit request =>
    Ok(shared.sessionExpired())
  }

  val enterCBCId = Action.async{ implicit request =>
    authorised(){
      Ok(submission.enterCBCId( cbcIdForm))
    }
  }

  private def cacheSubscriptionDetails(s:SubscriptionDetails, id:CBCId)(implicit hc:HeaderCarrier): Future[Unit] =
    (cache.save(TIN(s.utr.value,"")) *> cache.save(s.businessPartnerRecord) *> cache.save(id)).map(_ => ())

  val submitCBCId = Action.async { implicit request => authorised(AffinityGroup.Organisation).retrieve(cbcEnrolment) { cbcEnrolment =>
      cbcIdForm.bindFromRequest().fold[Future[Result]](
        errors => BadRequest(submission.enterCBCId( errors)),
        id => {
          subDataService.retrieveSubscriptionData(id).value.flatMap(_.fold(
            error => errorRedirect(error),
            details => details.fold[Future[Result]] {
              BadRequest(submission.enterCBCId( cbcIdForm, true))
            }(subscriptionDetails =>
              cbcEnrolment match {
                case Some(enrolment) => cbcEnrolment.toRight (UnexpectedState ("Could not find valid enrolment") ).ensure (InvalidSession) (e => subscriptionDetails.cbcId.contains (e.cbcId) ).fold[Future[Result]] ( {
                  case InvalidSession => BadRequest (submission.enterCBCId (cbcIdForm, false, true) )
                  case error => errorRedirect (error)
                  },
                  _ => cacheSubscriptionDetails (subscriptionDetails, id).map (_ =>
                  Redirect (routes.SubmissionController.submitSummary () )
                  )

                  )

                /**************************************************
                * user logged in with GG account
                * not used to register the organisation
                **************************************************/
                case None => cacheSubscriptionDetails (subscriptionDetails, id).map (_ =>
                  Redirect (routes.SubmissionController.submitSummary () )
                )
              }
            )
          ))
        }
      )
    }
  }


  val signOut = Action.async { implicit request =>
    authorised() {
      Future.successful(Redirect(s"${feConfig.governmentGatewaySignOutUrl}/gg/sign-out?continue=${feConfig.cbcrGuidanceUrl}"))
    }
  }

  val signOutGG = Action.async { implicit request =>
    {
      Future.successful(Redirect(s"${feConfig.governmentGatewaySignInUrl}?continue=${feConfig.cbcrFrontendBaseUrl}/country-by-country-reporting/"))
    }
  }

  val signOutSurvey = Action.async { implicit request =>
    authorised() {
      val continue = s"?continue=${feConfig.cbcrFrontendHost}${routes.ExitSurveyController.doSurvey().url}"
      Future.successful(Redirect(s"${feConfig.governmentGatewaySignOutUrl}/gg/sign-out$continue"))
    }
  }

  val keepSessionAlive = Action.async { implicit request =>
    authorised() {
      Future.successful(Ok("OK"))
    }
  }

  val pred = AffinityGroup.Organisation and (User or Admin)

  val verifyKnownFactsOrganisation = Action.async{ implicit request =>
    authorised(pred).retrieve(cbcEnrolment){ enrolment => enterKnownFacts(enrolment)}
  }

  val verifyKnownFactsAgent = Action.async{ implicit request =>
    authorised(AffinityGroup.Agent)(enterKnownFacts(None))
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


  def auditBPRKnowFactsFailure(cbcIdFromXml: Option[CBCId], bpr: BusinessPartnerRecord, bPRKnownFacts: BPRKnownFacts)(implicit request:Request[AnyContent]): Unit ={

    val cbcrKnownFactsFailure = "CBCRKnownFactsFailure"

    audit.sendExtendedEvent(ExtendedDataEvent("Country-By-Country-Frontend", cbcrKnownFactsFailure,
      detail = Json.obj(
          "path"         -> JsString(request.uri),
          "cbcIdFromXml" -> JsString(cbcIdFromXml.map(cbcid => cbcid.value).getOrElse("No CBCId present")),
          "safeId"       -> JsString(bpr.safeId),
          "utr"          -> JsString(bPRKnownFacts.utr.utr),
          "postcode"     -> JsString(bPRKnownFacts.postCode)
        )
      )).map {
        case AuditResult.Success         => ()
        case AuditResult.Failure(msg, _) => Logger.error(s"Failed to audit $cbcrKnownFactsFailure")
        case AuditResult.Disabled        => ()
      }
  }


  def enterKnownFacts(cbcEnrolment:Option[CBCEnrolment])(implicit request:Request[AnyContent]): Future[Result] = {
    for {
      postCode <- cache.readOption[BusinessPartnerRecord].map(_.flatMap(_.address.postalCode))
      utr <- cache.readOption[Utr].map(_.map(_.utr))
      result <- cbcEnrolment.map(enrolment =>
        if (CBCId.isPrivateBetaCBCId(enrolment.cbcId)) {
          auditDeEnrolReEnrolEvent(enrolment, rrService.deEnrolReEnrol(enrolment)).fold[Result](
            errors => errorRedirect(errors),
            (id: CBCId) => Ok(shared.regenerate(id))
          )
        } else {
          Future.successful(NotAcceptable(subscription.alreadySubscribed()))
        }
      ).fold[Future[Result]](
        {
          val form = (utr |@| postCode).map((utr: String, postCode: String) =>
            knownFactsForm.bind(Map("utr" -> utr, "postCode" -> postCode))
          ).getOrElse(knownFactsForm)
          Ok(shared.enterKnownFacts(form, false))
        })((result: Future[Result]) => result)
    } yield result
  }

  def NotFoundView(knownFacts:BPRKnownFacts)(implicit request:Request[_]): Result =
    NotFound(shared.enterKnownFacts( knownFactsForm.fill(knownFacts), noMatchingBusiness = true))

  val checkKnownFacts: Action[AnyContent] = Action.async { implicit request =>
    authorised().retrieve(Retrievals.affinityGroup) {
      case None => errorRedirect(UnexpectedState("Could not retrieve affinityGroup"))
      case Some(userType) => {


        knownFactsForm.bindFromRequest.fold[EitherT[Future, Result, Result]](
          formWithErrors => EitherT.left(BadRequest(shared.enterKnownFacts( formWithErrors, false))),
          knownFacts => for {
            bpr <- knownFactsService.checkBPRKnownFacts(knownFacts).toRight {
              Logger.warn("The BPR was not found when looking it up with the knownFactsService")
              NotFoundView(knownFacts)
            }
            cbcIdFromXml <- EitherT.right[Future, Result, Option[CBCId]](OptionT(cache.readOption[CompleteXMLInfo]).map(_.messageSpec.sendingEntityIn).value)
            subscriptionDetails <- subDataService.retrieveSubscriptionData(knownFacts.utr).leftMap(errorRedirect)
            _ <- EitherT.fromEither[Future](userType match {
              case AffinityGroup.Agent if subscriptionDetails.isEmpty =>
                Logger.error(s"Agent supplying known facts for a UTR that is not registered. Check for an internal error!")
                Left(NotFoundView(knownFacts))
              case AffinityGroup.Agent if subscriptionDetails.flatMap(_.cbcId) != cbcIdFromXml && cbcIdFromXml.isDefined => {
                Logger.warn(s"Agent submitting Xml where the CBCId associated with the UTR does not match that in the Xml File. Request the original Xml File and Known Facts from the Agent")
                auditBPRKnowFactsFailure(cbcIdFromXml, bpr, knownFacts)
                Left(NotFoundView(knownFacts))
              }
              case AffinityGroup.Agent if cbcIdFromXml.isEmpty => {
                Logger.error(s"Agent submitting Xml where the CBCId is not in the Xml. Check for an internal error!")
                Left(NotFoundView(knownFacts))
              }
              case AffinityGroup.Organisation if subscriptionDetails.isDefined =>
                Left(Redirect(routes.SubscriptionController.alreadySubscribed()))
              case _ =>
                Right(())
            })
            _ <- EitherT.right[Future, Result, Unit](
              (cache.save(bpr) *>
                cache.save(knownFacts.utr) *>
                cache.save(TIN(knownFacts.utr.value, ""))
                ).map(_ => ()))
          } yield Redirect(routes.SharedController.knownFactsMatch())

        )
      }.merge
    }
  }

  def knownFactsMatch = Action.async { implicit request =>
    authorised().retrieve(Retrievals.affinityGroup) {
      case None           => errorRedirect(UnexpectedState("Unable to get AffinityGroup"))
      case Some(userType) => val result: ServiceResponse[Result] = for {
        bpr <- cache.read[BusinessPartnerRecord]
        utr <- cache.read[Utr].leftMap(s => s: CBCErrors)
      } yield Ok(subscription.subscribeMatchFound(bpr.organisation.map(_.organisationName).getOrElse(""), bpr.address.postalCode.orEmpty, utr.value, userType))

        result.leftMap(errorRedirect).merge
    }
  }

  def unsupportedAffinityGroup = Action.async { implicit request => {
    authorised().retrieve(Retrievals.affinityGroup) {
      case None             => errorRedirect(UnexpectedState("Unable to query AffinityGroup"))
      case Some(Individual) => Unauthorized(views.html.not_authorised_individual())
      case _                => Unauthorized(views.html.subscription.notAuthorised())
    }
  }
  }

}


