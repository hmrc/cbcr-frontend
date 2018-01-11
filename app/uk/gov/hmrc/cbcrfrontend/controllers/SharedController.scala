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

import java.nio.file.{Path, Paths}
import javax.inject.{Inject, Singleton}

import cats.data.{EitherT, OptionT}
import cats.instances.all._
import cats.syntax.all._
import play.api.{Configuration, Logger}
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
import uk.gov.hmrc.cbcrfrontend.services._
import uk.gov.hmrc.cbcrfrontend.views.html._
import uk.gov.hmrc.play.audit.AuditExtensions._
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}
import uk.gov.hmrc.play.audit.model.ExtendedDataEvent
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
                                 val knownFactsService: BPRKnownFactsService,
                                 val configuration: Configuration,
                                 val rrService: DeEnrolReEnrolService,
                                 runMode: RunMode
                                )(implicit val auth:AuthConnector, val cache:CBCSessionCache)  extends FrontendController with ServicesConfig {

  lazy val audit: AuditConnector = FrontendAuditConnector

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

  val sessionExpired = Action{ implicit request =>
    Ok(shared.sessionExpired(includes.asideCbc(), includes.phaseBannerBeta()))
  }

  val enterCBCId = sec.AsyncAuthenticatedAction(){ _ => implicit request =>
      Ok(submission.enterCBCId(includes.asideCbc(), includes.phaseBannerBeta(), cbcIdForm))
  }

  private def cacheSubscriptionDetails(s:SubscriptionDetails, id:CBCId)(implicit hc:HeaderCarrier): Future[Unit] = for {
    _ <- cache.save(TIN(s.utr.value,""))
    _ <- cache.save(s.businessPartnerRecord)
    _ <- cache.save(id)
  } yield ()

  val submitCBCId = sec.AsyncAuthenticatedAction(Some(Organisation(true))) { authContext => implicit request =>
      cbcIdForm.bindFromRequest().fold[Future[Result]](
        errors => BadRequest(submission.enterCBCId(includes.asideCbc(), includes.phaseBannerBeta(), errors)),
        id     => {
          subDataService.retrieveSubscriptionData(id).value.flatMap(_.fold(
            error => errorRedirect(error),
            details => details.fold[Future[Result]] {
              BadRequest(submission.enterCBCId(includes.asideCbc(), includes.phaseBannerBeta(), cbcIdForm, true))
            }(subscriptionDetails => enrolments.getCBCEnrolment.toRight(UnexpectedState("Could not find valid enrolment")).ensure(InvalidSession)(e => subscriptionDetails.cbcId.contains(e.cbcId)).fold[Future[Result]](
              {
                case InvalidSession => BadRequest(submission.enterCBCId(includes.asideCbc(), includes.phaseBannerBeta(), cbcIdForm, false, true))
                case error => errorRedirect(error)
              },
              _ => {
                cacheSubscriptionDetails(subscriptionDetails, id).map(_ =>
                  Redirect(routes.SubmissionController.submitSummary())
                )
              }
            ).flatten)
          ))
        }
      )
  }


  val signOut = sec.AsyncAuthenticatedAction() { authContext => implicit request => {
    val continue = s"?continue=${FrontendAppConfig.cbcrFrontendHost}${routes.SharedController.guidance().url}"
    Future.successful(Redirect(s"${FrontendAppConfig.governmentGatewaySignOutUrl}/gg/sign-out$continue"))
  }}

  val signOutSurvey = sec.AsyncAuthenticatedAction() { authContext => implicit request => {
    val continue = s"?continue=${FrontendAppConfig.cbcrFrontendHost}${routes.ExitSurveyController.doSurvey().url}"
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
    val guideVer: String = configuration.getString(s"${runMode.env}.oecd-guide-version").getOrElse(throw new Exception(s"Missing configuration ${runMode.env}.oecd-guide-version"))
    val file: Path = Paths.get(s"conf/downloads/HMRC_CbC_XML_User_Guide_V$guideVer.pdf")
    Future.successful(Ok.sendPath(file,inline = false,fileName = _ => s"HMRC_CbC_XML_User_Guide_V$guideVer.pdf"))
  }

  def guidance =  Action.async { implicit request =>
    Future.successful(Ok(uk.gov.hmrc.cbcrfrontend.views.html.guidance.guidanceOverviewQa()))
  }

  def businessRules = Action.async{ implicit request =>
    Future.successful(Ok(uk.gov.hmrc.cbcrfrontend.views.html.guidance.businessRules()))
  }

  val verifyKnownFactsOrganisation = sec.AsyncAuthenticatedAction(Some(Organisation(true))) { authContext =>
    implicit request => enterKnownFacts(authContext)
  }

  val verifyKnownFactsAgent = sec.AsyncAuthenticatedAction() { authContext =>
    implicit request => enterKnownFacts(authContext)
  }

  def auditDeEnrolReEnrolEvent(enrolment: CBCEnrolment,result:ServiceResponse[CBCId])(implicit request:Request[AnyContent]) : ServiceResponse[CBCId] = {
    EitherT(result.value.flatMap { e =>
      audit.sendEvent(ExtendedDataEvent("Country-By-Country-Frontend", "CBCR-DeEnrolReEnrol",
        tags = hc.toAuditTags("CBCR-DeEnrolReEnrol", "N/A") + (
          "path"     -> request.uri,
          "newCBCId" -> e.map(_.value).getOrElse("Failed to get new CBCId"),
          "oldCBCId" -> enrolment.cbcId.value,
          "utr"      -> enrolment.utr.utr)
      )).map {
        case AuditResult.Success         => e
        case AuditResult.Failure(msg, _) => Left(UnexpectedState(s"Unable to audit a successful submission: $msg"))
        case AuditResult.Disabled        => e
      }
    })
  }


  def auditBPRKnowFactsFailure(cbcIdFromXml: Option[CBCId], bpr: BusinessPartnerRecord, bPRKnownFacts: BPRKnownFacts)(implicit request:Request[AnyContent]): Unit ={

    val cBCRKnownFactsFailure = "CBCRKnownFactsFailure"

    audit.sendEvent(ExtendedDataEvent("Country-By-Country-Frontend", cBCRKnownFactsFailure,
        tags = hc.toAuditTags("CBCR-DeEnrolReEnrol", "N/A") + (
          "path"     -> request.uri,
          "cbcIdFromXml" -> cbcIdFromXml.map(cbcid => cbcid.value).getOrElse("No CBCId present"),
          "safeId" -> bpr.safeId,
          "utr"      -> bPRKnownFacts.utr.utr,
          "postcode" -> bPRKnownFacts.postCode
        )
      )).map {
        case AuditResult.Success         => ()
        case AuditResult.Failure(msg, _) => Logger.error(s"Failed to audit $cBCRKnownFactsFailure")
        case AuditResult.Disabled        => ()
      }
  }


  def enterKnownFacts(authContext: AuthContext)(implicit request:Request[AnyContent]): Future[Result] =
    getUserType(authContext).semiflatMap{ userType =>
      for {
        postCode <- cache.readOption[BusinessPartnerRecord].map(_.flatMap(_.address.postalCode))
        utr      <- cache.readOption[Utr].map(_.map(_.utr))
        result   <- enrolments.getCBCEnrolment.semiflatMap(enrolment => {
          if (CBCId.isPrivateBetaCBCId(enrolment.cbcId)) {
            auditDeEnrolReEnrolEvent(enrolment, rrService.deEnrolReEnrol(enrolment)).fold[Result](
              errors => errorRedirect(errors),
              (id: CBCId) => Ok(shared.regenerate(includes.asideCbc(), includes.phaseBannerBeta(), id))
            )
          } else {
            Future.successful(NotAcceptable(subscription.alreadySubscribed(includes.asideCbc(), includes.phaseBannerBeta())))
          }
        }).cata(
          {
            val form = (utr |@| postCode).map((utr: String, postCode: String) =>
              knownFactsForm.bind(Map("utr" -> utr, "postCode" -> postCode))
            ).getOrElse(knownFactsForm)
            Ok(shared.enterKnownFacts(includes.asideCbc(), includes.phaseBannerBeta(), form, false, userType))
          },
          (result: Result) => result
        )
      } yield result
    }.leftMap(errorRedirect).merge


  val checkKnownFacts: Action[AnyContent] = sec.AsyncAuthenticatedAction() { authContext =>implicit request =>

    def NotFoundView(knownFacts:BPRKnownFacts, userType: UserType): Result = {
      NotFound(shared.enterKnownFacts(includes.asideCbc(), includes.phaseBannerBeta(), knownFactsForm.fill(knownFacts), noMatchingBusiness = true, userType))
    }

    getUserType(authContext).leftMap(errorRedirect).flatMap { userType =>

      knownFactsForm.bindFromRequest.fold[EitherT[Future,Result,Result]](
        formWithErrors => EitherT.left(BadRequest(shared.enterKnownFacts(includes.asideCbc(), includes.phaseBannerBeta(), formWithErrors, false, userType))),
        knownFacts =>  for {
          bpr                 <- knownFactsService.checkBPRKnownFacts(knownFacts).toRight{
            Logger.warn("The BPR was not found when looking it up with the knownFactsService")
            NotFoundView(knownFacts, userType)
          }
          cbcIdFromXml        <- EitherT.right[Future,Result,Option[CBCId]](OptionT(cache.readOption[CompleteXMLInfo]).map(_.messageSpec.sendingEntityIn).value)
          subscriptionDetails <- subDataService.retrieveSubscriptionData(knownFacts.utr).leftMap(errorRedirect)
          _                   <- EitherT.fromEither[Future](userType match {
            case Agent() if subscriptionDetails.isEmpty =>
              Logger.error(s"Agent supplying known facts for a UTR that is not registered. Check for an internal error!")
              Left(NotFoundView(knownFacts, userType))
            case Agent() if subscriptionDetails.flatMap(_.cbcId) != cbcIdFromXml && cbcIdFromXml.isDefined => {
              Logger.warn(s"Agent submitting Xml where the CBCId associated with the UTR does not match that in the Xml File. Request the original Xml File and Known Facts from the Agent")
              auditBPRKnowFactsFailure(cbcIdFromXml, bpr, knownFacts)
              Left(NotFoundView(knownFacts, userType))
            }
            case Agent() if cbcIdFromXml.isEmpty => {
              Logger.error(s"Agent submitting Xml where the CBCId is not in the Xml. Check for an internal error!")
              Left(NotFoundView(knownFacts, userType))
            }
            case Organisation(_) if subscriptionDetails.isDefined =>
              Left(Redirect(routes.SubscriptionController.alreadySubscribed()))
            case _                                             =>
              Right(())
          })
          _                   <- EitherT.right[Future,Result,Unit](
            (cache.save(bpr) *>
              cache.save(knownFacts.utr) *>
              cache.save(TIN(knownFacts.utr.value,""))
              ).map(_ => ()))
        } yield Redirect(routes.SharedController.knownFactsMatch())

      )
    }.merge
  }

  def knownFactsMatch = sec.AsyncAuthenticatedAction(){ authContext => implicit request =>
    val result:ServiceResponse[Result] = for {
      userType <- getUserType(authContext)
      bpr      <- cache.read[BusinessPartnerRecord]
      utr      <- cache.read[Utr].leftMap(s => s:CBCErrors)
    } yield Ok(subscription.subscribeMatchFound(includes.asideCbc(), includes.phaseBannerBeta(), bpr.organisation.map(_.organisationName).getOrElse(""), bpr.address.postalCode.orEmpty, utr.value , userType))

    result.leftMap(errorRedirect).merge
  }

}


