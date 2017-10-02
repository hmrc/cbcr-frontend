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

import java.nio.file.{Path, Paths}
import javax.inject.{Inject, Singleton}

import cats.data.{EitherT, OptionT}
import cats.instances.all._
import cats.syntax.all._
import play.api.Logger
import play.api.Play.current
import play.api.data.Form
import play.api.data.Forms._
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationError}
import play.api.i18n.Messages.Implicits._
import play.api.mvc.{Action, AnyContent, Request, Result}
import uk.gov.hmrc.cbcrfrontend._
import uk.gov.hmrc.cbcrfrontend.auth.SecuredActions
import uk.gov.hmrc.cbcrfrontend.connectors.EnrolmentsConnector
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.services.{BPRKnownFactsService, CBCSessionCache, SubscriptionDataService}
import uk.gov.hmrc.cbcrfrontend.views.html._
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.frontend.auth.AuthContext
import uk.gov.hmrc.play.frontend.auth.connectors.AuthConnector
import uk.gov.hmrc.play.frontend.controller.FrontendController
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.Future

@Singleton
class SharedController @Inject()(val sec: SecuredActions,
                                 val subDataService: SubscriptionDataService,
                                 val enrolments:EnrolmentsConnector,
                                 val authConnector:AuthConnector,
                                 val knownFactsService: BPRKnownFactsService)(implicit val auth:AuthConnector, val cache:CBCSessionCache)  extends FrontendController with ServicesConfig {

  val utrConstraint: Constraint[String] = Constraint("constraints.utrcheck"){
    case utr if Utr(utr).isValid => Valid
    case _                       => Invalid(ValidationError("UTR is not valid"))
  }

  val knownFactsForm = Form(
    mapping(
      "utr" -> nonEmptyText.verifying(utrConstraint),
      "postCode" -> nonEmptyText
    )((u,p) => BPRKnownFacts(Utr(u),p))((facts: BPRKnownFacts) => Some(facts.utr.value -> facts.postCode))
  )

  val cbcIdForm : Form[CBCId] = Form(
    single( "cbcId" -> of[CBCId])
  )

  val technicalDifficulties = Action{ implicit request =>
    InternalServerError(FrontendGlobal.internalServerErrorTemplate)
  }

  val enterCBCId = sec.AsyncAuthenticatedAction(){ _ => implicit request =>
      Ok(submission.enterCBCId(includes.asideCbc(), includes.phaseBannerBeta(), cbcIdForm))
  }

  private def getCBCEnrolment(implicit hc:HeaderCarrier) : EitherT[Future,UnexpectedState,CBCEnrolment] = for {
    enrolment <- OptionT(enrolments.getEnrolments.map(_.find(_.key == "HMRC-CBC-ORG")))
      .toRight(UnexpectedState("Enrolment not found"))
    cbcString <- OptionT.fromOption[Future](enrolment.identifiers.find(_.key.equalsIgnoreCase("cbcid")).map(_.value))
      .toRight(UnexpectedState("Enrolment did not contain a cbcid"))
    cbcId     <- OptionT.fromOption[Future](CBCId(cbcString))
      .toRight(UnexpectedState(s"Enrolment contains an invalid cbcid: $cbcString"))
    utrString <- OptionT.fromOption[Future](enrolment.identifiers.find(_.key.equalsIgnoreCase("utr")).map(_.value))
      .toRight(UnexpectedState(s"Enrolment does not contain a utr"))
    utr       <- OptionT.fromOption[Future](if(Utr(utrString).isValid){ Some(Utr(utrString)) } else { None })
      .toRight(UnexpectedState(s"Enrolment contains an invalid utr $utrString"))
  } yield CBCEnrolment(cbcId,utr)


  private def cacheSubscriptionDetails(s:SubscriptionDetails)(implicit hc:HeaderCarrier): Future[Unit] = for {
    _ <- cache.save(s.utr)
    _ <- cache.save(s.businessPartnerRecord)
    _ <- cache.save(s.cbcId)
  } yield ()

  val submitCBCId = sec.AsyncAuthenticatedAction(Some(Organisation)) { authContext => implicit request =>
      cbcIdForm.bindFromRequest().fold[Future[Result]](
        errors => BadRequest(submission.enterCBCId(includes.asideCbc(), includes.phaseBannerBeta(), errors)),
        id     => {
          subDataService.retrieveSubscriptionData(id).value.flatMap(_.fold(
            error => errorRedirect(error),
            details => details.fold[Future[Result]] {
              BadRequest(submission.enterCBCId(includes.asideCbc(), includes.phaseBannerBeta(), cbcIdForm, true))
            }(subscriptionDetails => getCBCEnrolment.ensure(InvalidSession)(e => subscriptionDetails.cbcId.contains(e.cbcId)).fold[Future[Result]](
              {
                case InvalidSession => BadRequest(submission.enterCBCId(includes.asideCbc(), includes.phaseBannerBeta(), cbcIdForm, false, true))
                case error => errorRedirect(error)
              },
              _ => {
                cacheSubscriptionDetails(subscriptionDetails).map(_ =>
                  Redirect(routes.SubmissionController.submitSummary())
                )
              }
            ).flatten)
          ))
        }
      )
  }


  val signOut = sec.AsyncAuthenticatedAction() { authContext => implicit request => {
    val continue = s"?continue=${FrontendAppConfig.cbcrFrontendHost}${routes.SharedController.guidance.url}"
    Future.successful(Redirect(s"${FrontendAppConfig.governmentGatewaySignOutUrl}/gg/sign-out$continue"))
  }}

  def volunteer = Action.async{ implicit request =>
    Future.successful(Ok(uk.gov.hmrc.cbcrfrontend.views.html.guidance.volunteer()))
  }

  def register = Action.async{ implicit request =>
    Future.successful(Ok(uk.gov.hmrc.cbcrfrontend.views.html.guidance.register()))
  }

  def report = Action.async{ implicit request =>
   Future.successful(Ok(uk.gov.hmrc.cbcrfrontend.views.html.guidance.report()))
  }

  def downloadGuide = Action.async{ implicit request =>
    val file: Path = Paths.get("conf/downloads/cbcguide.pdf")
    Future.successful(Ok.sendPath(file,inline = false,fileName = _ => "cbcGuide.pdf"))
  }

  def guidance =  Action.async { implicit request =>
    Future.successful(Ok(uk.gov.hmrc.cbcrfrontend.views.html.guidance.guidanceOverviewQa()))
  }

  def businessRules = Action.async{ implicit request =>
    Future.successful(Ok(uk.gov.hmrc.cbcrfrontend.views.html.guidance.businessRules()))
  }

  def alreadyEnrolled(implicit hc:HeaderCarrier): Future[Boolean] =
    enrolments.getEnrolments.map(_.exists(_.key == "HMRC-CBC-ORG"))

  val verifyKnownFactsOrganisation = sec.AsyncAuthenticatedAction(Some(Organisation)) { authContext =>
    implicit request => enterKnownFacts(authContext)
  }

  val verifyKnownFactsAgent = sec.AsyncAuthenticatedAction() { authContext =>
    implicit request => enterKnownFacts(authContext)
  }

  def enterKnownFacts(authContext: AuthContext)(implicit request:Request[AnyContent]) =
    getUserType(authContext).semiflatMap{ userType =>
      alreadyEnrolled.flatMap(subscribed =>
        if (subscribed) {
          NotAcceptable(subscription.alreadySubscribed(includes.asideCbc(), includes.phaseBannerBeta()))
        } else {
          Ok(shared.enterKnownFacts(includes.asideCbc(), includes.phaseBannerBeta(), knownFactsForm, false, userType))
        }
      )
    }.fold(
      (errors: CBCErrors) => errorRedirect(errors),
      (result: Result)    => result
    )

  val checkKnownFacts = sec.AsyncAuthenticatedAction() { authContext => implicit request =>

    getUserType(authContext).leftMap(errorRedirect).flatMap { userType =>

      knownFactsForm.bindFromRequest.fold[EitherT[Future,Result,Result]](
        formWithErrors => EitherT.left(BadRequest(shared.enterKnownFacts(includes.asideCbc(), includes.phaseBannerBeta(), formWithErrors, false, userType))),
        knownFacts =>  for {
          bpr                 <- knownFactsService.checkBPRKnownFacts(knownFacts).toRight(
            NotFound(shared.enterKnownFacts(includes.asideCbc(), includes.phaseBannerBeta(), knownFactsForm, noMatchingBusiness = true, userType))
          )
          cbcId               <- EitherT.right[Future,Result,Option[CBCId]](OptionT(cache.read[XMLInfo]).map(_.messageSpec.sendingEntityIn).value)
          subscriptionDetails <- subDataService.retrieveSubscriptionData(knownFacts.utr).leftMap(errorRedirect)
          _                   <- EitherT.fromEither[Future](userType match {
            case Agent if subscriptionDetails.isEmpty =>
              Left(NotFound(shared.enterKnownFacts(includes.asideCbc(), includes.phaseBannerBeta(), knownFactsForm, noMatchingBusiness = true, userType)))
            case Agent if subscriptionDetails.flatMap(_.cbcId) != cbcId || cbcId.isEmpty =>
              Left(NotFound(shared.enterKnownFacts(includes.asideCbc(), includes.phaseBannerBeta(), knownFactsForm, noMatchingBusiness = true, userType)))
            case Organisation if subscriptionDetails.isDefined =>
              Left(Redirect(routes.SubscriptionController.alreadySubscribed()))
            case _                                             =>
              Right(())
          })
          _                   <- EitherT.right[Future, Result, Unit]((cache.save(bpr) |@| cache.save(knownFacts.utr)).map((_,_) => ()))
        } yield Redirect(routes.SharedController.knownFactsMatch())

      )
    }.merge
  }

  def knownFactsMatch = sec.AsyncAuthenticatedAction(){ authContext => implicit request =>
    val result:ServiceResponse[Result] = for {
      userType <- getUserType(authContext)
      bpr      <- OptionT(cache.read[BusinessPartnerRecord]).toRight(UnexpectedState("Unable to read BusinessPartnerRecord from cache"))
      utr      <- OptionT(cache.read[Utr]).toRight(UnexpectedState("Unable to read Utr from cache"):CBCErrors)
    } yield Ok(subscription.subscribeMatchFound(includes.asideCbc(), includes.phaseBannerBeta(), bpr.organisation.map(_.organisationName).getOrElse(""), bpr.address.postalCode.orEmpty, utr.value , userType))

    result.leftMap(errorRedirect).merge
  }

}


