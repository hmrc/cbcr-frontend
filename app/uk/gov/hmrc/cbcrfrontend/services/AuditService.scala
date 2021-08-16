/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.cbcrfrontend.services

import cats.instances.all._
import cats.syntax.all._
import play.api.i18n.Messages
import play.api.libs.json.{JsObject, JsString, Json, OFormat}
import play.api.mvc.Request
import uk.gov.hmrc.auth.core.AffinityGroup
import uk.gov.hmrc.auth.core.AffinityGroup.{Agent, Organisation}
import uk.gov.hmrc.auth.core.retrieve.Credentials
import uk.gov.hmrc.cbcrfrontend.controllers.{eitherT, right}
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.model.upscan.{UploadId, UploadedSuccessfully}
import uk.gov.hmrc.cbcrfrontend.util.ErrorUtil
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}
import uk.gov.hmrc.play.audit.model.ExtendedDataEvent

import javax.inject.Inject
import scala.concurrent.ExecutionContext.Implicits.global

class AuditService @Inject()(cache: CBCSessionCache, val audit: AuditConnector) {

  implicit val credentialsFormat: OFormat[Credentials] = uk.gov.hmrc.cbcrfrontend.controllers.credentialsFormat

  def auditFailedSubmission(
    creds: Credentials,
    affinity: Option[AffinityGroup],
    enrolment: Option[CBCEnrolment],
    reason: String)(
    implicit hc: HeaderCarrier,
    request: Request[_],
    messages: Messages): ServiceResponse[AuditResult.Success.type] =
    for {
      md        <- right(cache.readOption[UploadedSuccessfully])
      all_error <- (right(cache.readOption[AllBusinessRuleErrors]) |@| right(cache.readOption[XMLErrors])).tupled
      c         <- right(cache.readOption[CBCId])
      cbcId = if (enrolment.isEmpty) c else Option(enrolment.get.cbcId)
      u <- right(cache.readOption[Utr])
      utr = if (enrolment.isEmpty) u else Option(enrolment.get.utr)
      result <- eitherT[AuditResult.Success.type](
                 audit
                   .sendExtendedEvent(ExtendedDataEvent(
                     "Country-By-Country-Frontend",
                     "CBCRFilingFailed",
                     detail = Json.obj(
                       "reason"        -> JsString(reason),
                       "path"          -> JsString(request.uri),
                       "file metadata" -> Json.toJson(md.map(getCCParams).getOrElse(Map.empty[String, String])),
                       "creds"         -> Json.toJson(creds),
                       "registration"  -> auditDetailAffinity(affinity.get, cbcId, utr),
                       "errorTypes"    -> auditDetailErrors(all_error)
                     )
                   ))
                   .map {
                     case AuditResult.Success => Right(AuditResult.Success)
                     case AuditResult.Failure(msg, _) =>
                       Left(UnexpectedState(s"Unable to audit a failed submission: $msg"))
                     case AuditResult.Disabled => Right(AuditResult.Success)
                   })
    } yield result

  //Turn a Case class into a map
  private def getCCParams(cc: AnyRef): Map[String, String] =
    cc.getClass.getDeclaredFields.foldLeft[Map[String, String]](Map.empty) { (acc, field) =>
      field.setAccessible(true)
      acc + (field.getName -> field.get(cc).toString)
    }

  private def auditDetailAffinity(affinity: AffinityGroup, cbc: Option[CBCId], utr: Option[Utr]): JsObject =
    affinity match {
      case Organisation =>
        Json.obj(
          "affinityGroup" -> Json.toJson(affinity),
          "utr"           -> JsString(utr.getOrElse("none retrieved").toString),
          "cbcId"         -> JsString(cbc.getOrElse("none retrieved").toString)
        )
      case Agent => Json.obj("affinityGroup" -> Json.toJson(affinity))
      case _     => Json.obj("affinityGroup" -> "none retrieved")
    }

  private def auditDetailErrors(all_errors: (Option[AllBusinessRuleErrors], Option[XMLErrors]))(
    implicit messages: Messages): JsObject =
    (
      all_errors._1.exists(bre => if (bre.errors.isEmpty) false else true),
      all_errors._2.exists(xml => if (xml.errors.isEmpty) false else true)) match {
      case (true, true) =>
        Json.obj(
          "businessRuleErrors" -> Json.toJson(ErrorUtil.errorsToMap(all_errors._1.get.errors)),
          "xmlErrors"          -> Json.toJson(ErrorUtil.errorsToMap(List(all_errors._2.get)))
        )
      case (true, false) =>
        Json.obj("businessRuleErrors" -> Json.toJson(ErrorUtil.errorsToMap(all_errors._1.get.errors)))
      case (false, true) => Json.obj("xmlErrors" -> Json.toJson(ErrorUtil.errorsToMap(List(all_errors._2.get))))
      case _             => Json.obj("none"      -> "no business rule or schema errors")

    }

}
