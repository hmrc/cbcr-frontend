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

import cats.data.{EitherT, OptionT}
import cats.instances.all._
import cats.syntax.all._
import play.api.data.Form
import play.api.data.Forms._
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationError}
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.libs.json.{JsString, Json}
import play.api.mvc._
import play.api.{Configuration, Environment, Logger}
import uk.gov.hmrc.auth.core.AffinityGroup.Individual
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.cbcrfrontend._
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.services._
import uk.gov.hmrc.cbcrfrontend.views.Views
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}
import uk.gov.hmrc.play.audit.model.ExtendedDataEvent
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}
@Singleton
class SharedController @Inject()(
  override val messagesApi: MessagesApi,
  val subDataService: SubscriptionDataService,
  val knownFactsService: BPRKnownFactsService,
  val audit: AuditConnector,
  val env: Environment,
  val authConnector: AuthConnector,
  messagesControllerComponents: MessagesControllerComponents,
  views: Views)(
  implicit val cache: CBCSessionCache,
  val config: Configuration,
  feConfig: FrontendAppConfig,
  val ec: ExecutionContext)
    extends FrontendController(messagesControllerComponents) with AuthorisedFunctions with I18nSupport {

  lazy val logger: Logger = Logger(this.getClass)

  val utrConstraint: Constraint[String] = Constraint("constraints.utrcheck") {
    case utr if Utr(utr).isValid => Valid
    case _                       => Invalid(ValidationError("UTR is not valid"))
  }

  val knownFactsForm = Form(
    mapping(
      "utr"                                                                              -> nonEmptyText.verifying(utrConstraint),
      "postCode"                                                                         -> text
    )((u, p) => BPRKnownFacts(Utr(u), p))((facts: BPRKnownFacts) => Some(facts.utr.value -> facts.postCode))
  )

  val cbcIdForm: Form[CBCId] = Form(
    single("cbcId" -> of[CBCId])
  )

  val technicalDifficulties = Action { implicit request =>
    InternalServerError(views.errorTemplate("Internal Server Error", "Internal Server Error", "Something went wrong"))
  }

  val sessionExpired = Action { implicit request =>
    Ok(views.sessionExpired())
  }

  val enterCBCId = Action.async { implicit request =>
    authorised() {

      for {
        form <- cache.read[CBCId].map(cbcId => cbcIdForm.bind(Map("cbcId" -> cbcId.value))).getOrElse(cbcIdForm)
      } yield {
        Ok(views.enterCBCId(form))
      }
    }
  }

  private def cacheSubscriptionDetails(s: SubscriptionDetails, id: CBCId)(implicit hc: HeaderCarrier): Future[Unit] =
    (cache.save(TIN(s.utr.value, "")) *> cache.save(s.businessPartnerRecord) *> cache.save(id)).map(_ => ())

  val submitCBCId = Action.async { implicit request =>
    authorised(AffinityGroup.Organisation).retrieve(cbcEnrolment) { cbcEnrolment =>
      cbcIdForm
        .bindFromRequest()
        .fold[Future[Result]](
          errors => BadRequest(views.enterCBCId(errors)),
          id => {
            subDataService
              .retrieveSubscriptionData(id)
              .value
              .flatMap(_.fold(
                error => errorRedirect(error, views.notAuthorisedIndividual, views.errorTemplate),
                details =>
                  details.fold[Future[Result]] {
                    BadRequest(views.enterCBCId(cbcIdForm, true))
                  }(subscriptionDetails =>
                    cbcEnrolment match {
                      case Some(enrolment) =>
                        cbcEnrolment
                          .toRight(UnexpectedState("Could not find valid enrolment"))
                          .ensure(InvalidSession)(e => subscriptionDetails.cbcId.contains(e.cbcId))
                          .fold[Future[Result]](
                            {
                              case InvalidSession => BadRequest(views.enterCBCId(cbcIdForm, false, true))
                              case error          => errorRedirect(error, views.notAuthorisedIndividual, views.errorTemplate)
                            },
                            _ =>
                              cacheSubscriptionDetails(subscriptionDetails, id).map(_ =>
                                Redirect(routes.SubmissionController.submitSummary))
                          )

                      /**************************************************
                        * user logged in with GG account
                        * not used to register the organisation
                **************************************************/
                      case None =>
                        cacheSubscriptionDetails(subscriptionDetails, id).map(_ =>
                          Redirect(routes.SubmissionController.submitSummary))
                  })
              ))
          }
        )
    }
  }

  val signOut = Action.async { implicit request =>
    authorised() {
      Future.successful(Redirect(
        s"${feConfig.governmentGatewaySignOutUrl}/bas-gateway/sign-out-without-state?continue=${feConfig.cbcrGuidanceUrl}"))
    }
  }

  val signOutGG = Action.async { _ =>
    {
      Future.successful(Redirect(
        s"${feConfig.governmentGatewaySignInUrl}?continue_url=${feConfig.cbcrFrontendBaseUrl}/country-by-country-reporting/"))
    }
  }

  val signOutSurvey = Action.async { implicit request =>
    authorised() {
      val continue = s"?continue=${feConfig.cbcrFrontendHost}${routes.ExitSurveyController.doSurvey.url}"
      Future.successful(
        Redirect(s"${feConfig.governmentGatewaySignOutUrl}/bas-gateway/sign-out-without-state$continue"))
    }
  }

  val keepSessionAlive = Action.async { implicit request =>
    authorised() {
      Future.successful(Ok("OK"))
    }
  }

  val pred = AffinityGroup.Organisation and User

  val verifyKnownFactsOrganisation = Action.async { implicit request =>
    authorised(pred).retrieve(cbcEnrolment) { enrolment =>
      enterKnownFacts(enrolment, AffinityGroup.Organisation)
    }
  }

  val verifyKnownFactsAgent = Action.async { implicit request =>
    authorised(AffinityGroup.Agent)(enterKnownFacts(None, AffinityGroup.Agent))
  }

  def auditBPRKnowFactsFailure(cbcIdFromXml: Option[CBCId], bpr: BusinessPartnerRecord, bPRKnownFacts: BPRKnownFacts)(
    implicit request: Request[AnyContent]): Unit = {

    val cbcrKnownFactsFailure = "CBCRKnownFactsFailure"

    audit
      .sendExtendedEvent(
        ExtendedDataEvent(
          "Country-By-Country-Frontend",
          cbcrKnownFactsFailure,
          detail = Json.obj(
            "path"         -> JsString(request.uri),
            "cbcIdFromXml" -> JsString(cbcIdFromXml.map(cbcid => cbcid.value).getOrElse("No CBCId present")),
            "safeId"       -> JsString(bpr.safeId),
            "utr"          -> JsString(bPRKnownFacts.utr.utr),
            "postcode"     -> JsString(bPRKnownFacts.postCode)
          )
        ))
      .map {
        case AuditResult.Success         => ()
        case AuditResult.Failure(msg, _) => logger.error(s"Failed to audit $cbcrKnownFactsFailure")
        case AuditResult.Disabled        => ()
      }
  }

  def enterKnownFacts(cbcEnrolment: Option[CBCEnrolment], userType: AffinityGroup)(
    implicit request: Request[AnyContent]): Future[Result] =
    for {
      postCode <- cache.readOption[BusinessPartnerRecord].map(_.flatMap(_.address.postalCode))
      utr      <- cache.readOption[Utr].map(_.map(_.utr))
      result <- cbcEnrolment
                 .map(_ => Future.successful(NotAcceptable(views.alreadySubscribed())))
                 .fold[Future[Result]]({
                   val form = (utr |@| postCode)
                     .map((utr: String, postCode: String) =>
                       knownFactsForm.bind(Map("utr" -> utr, "postCode" -> postCode)))
                     .getOrElse(knownFactsForm)
                   Ok(views.enterKnownFacts(form, false, userType))
                 })((result: Future[Result]) => result)
    } yield result

  def NotFoundView(knownFacts: BPRKnownFacts, userType: AffinityGroup)(implicit request: Request[_]): Result =
    NotFound(views.enterKnownFacts(knownFactsForm.fill(knownFacts), noMatchingBusiness = true, userType))

  val checkKnownFacts: Action[AnyContent] = Action.async { implicit request =>
    authorised().retrieve(Retrievals.affinityGroup) {
      case None =>
        errorRedirect(
          UnexpectedState("Could not retrieve affinityGroup"),
          views.notAuthorisedIndividual,
          views.errorTemplate)
      case Some(userType) => {

        knownFactsForm.bindFromRequest.fold[EitherT[Future, Result, Result]](
          formWithErrors => EitherT.left(BadRequest(views.enterKnownFacts(formWithErrors, false, userType))),
          knownFacts =>
            for {
              bpr <- knownFactsService.checkBPRKnownFacts(knownFacts).toRight {
                      logger.warn("The BPR was not found when looking it up with the knownFactsService")
                      NotFoundView(knownFacts, userType)
                    }
              cbcIdFromXml <- EitherT.right(
                               OptionT(cache.readOption[CompleteXMLInfo]).map(_.messageSpec.sendingEntityIn).value)
              subscriptionDetails <- subDataService
                                      .retrieveSubscriptionData(knownFacts.utr)
                                      .leftMap((error: CBCErrors) =>
                                        errorRedirect(error, views.notAuthorisedIndividual, views.errorTemplate))
              _ <- EitherT.fromEither[Future](userType match {
                    case AffinityGroup.Agent if subscriptionDetails.isEmpty =>
                      logger.error(
                        s"Agent supplying known facts for a UTR that is not registered. Check for an internal error!")
                      Left(NotFoundView(knownFacts, AffinityGroup.Agent))
                    case AffinityGroup.Agent
                        if subscriptionDetails.flatMap(_.cbcId) != cbcIdFromXml && cbcIdFromXml.isDefined => {
                      logger.warn(
                        s"Agent submitting Xml where the CBCId associated with the UTR does not match that in the Xml File. Request the original Xml File and Known Facts from the Agent")
                      auditBPRKnowFactsFailure(cbcIdFromXml, bpr, knownFacts)
                      Left(NotFoundView(knownFacts, AffinityGroup.Agent))
                    }
                    case AffinityGroup.Agent if cbcIdFromXml.isEmpty => {
                      logger.error(
                        s"Agent submitting Xml where the CBCId is not in the Xml. Check for an internal error!")
                      Left(NotFoundView(knownFacts, AffinityGroup.Agent))
                    }
                    case AffinityGroup.Organisation if subscriptionDetails.isDefined =>
                      Left(Redirect(routes.SubscriptionController.alreadySubscribed))
                    case _ =>
                      Right(())
                  })
              _ <- EitherT.right(
                    (cache.save(bpr) *>
                      cache.save(knownFacts.utr) *>
                      cache.save(TIN(knownFacts.utr.value, ""))).map(_ => ()))
            } yield Redirect(routes.SharedController.knownFactsMatch)
        )
      }.merge
    }
  }

  def knownFactsMatch = Action.async { implicit request =>
    authorised().retrieve(Retrievals.affinityGroup) {
      case None =>
        errorRedirect(
          UnexpectedState("Unable to get AffinityGroup"),
          views.notAuthorisedIndividual,
          views.errorTemplate)
      case Some(userType) =>
        val result: ServiceResponse[Result] = for {
          bpr <- cache.read[BusinessPartnerRecord]
          utr <- cache.read[Utr].leftMap(s => s: CBCErrors)
        } yield
          Ok(
            views.subscribeMatchFound(
              bpr.organisation.map(_.organisationName).getOrElse(""),
              bpr.address.postalCode.orEmpty,
              utr.value,
              userType))

        result
          .leftMap((error: CBCErrors) => errorRedirect(error, views.notAuthorisedIndividual, views.errorTemplate))
          .merge
    }
  }

  def unsupportedAffinityGroup = Action.async { implicit request =>
    {
      authorised().retrieve(Retrievals.affinityGroup) {
        case None =>
          errorRedirect(
            UnexpectedState("Unable to query AffinityGroup"),
            views.notAuthorisedIndividual,
            views.errorTemplate)
        case Some(Individual) => Unauthorized(views.notAuthorisedIndividual())
        case _                => Unauthorized(views.notAuthorised())
      }
    }
  }

}
