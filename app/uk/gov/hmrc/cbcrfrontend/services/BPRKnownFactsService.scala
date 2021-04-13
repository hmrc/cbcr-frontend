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

import javax.inject.{Inject, Singleton}
import cats.data.OptionT
import cats.instances.future._
import play.api.Logger
import play.api.libs.json
import play.api.libs.json.{JsString, Json}
import uk.gov.hmrc.cbcrfrontend.connectors.BPRKnownFactsConnector
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.play.audit.AuditExtensions._
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}
import uk.gov.hmrc.play.audit.model.ExtendedDataEvent

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import uk.gov.hmrc.http.{HeaderCarrier, NotFoundException}

/**
  * Use the provided KnownFactsConnector to query a UTR
  * Optionally return the [[uk.gov.hmrc.cbcrfrontend.model.BusinessPartnerRecord]] depending on whether it contains
  * the same postcode as the provided [[BPRKnownFacts]]
  */
@Singleton
class BPRKnownFactsService @Inject()(dc: BPRKnownFactsConnector, audit: AuditConnector) {

  private val AUDIT_TAG = "CBCRBPRKnowFacts"

  private def sanitisePostCode(s: String): String = s.toLowerCase.replaceAll("\\s", "")

  lazy val logger: Logger = Logger(this.getClass)

  def checkBPRKnownFacts(kf: BPRKnownFacts)(implicit hc: HeaderCarrier): OptionT[Future, BusinessPartnerRecord] = {
    val response = OptionT(
      dc.lookup(kf.utr.value)
        .map { response =>
          val bpr: Option[BusinessPartnerRecord] = Json.parse(response.body).validate[BusinessPartnerRecord].asOpt
          auditBpr(bpr, kf)
          bpr
        }
        .recover {
          case _: NotFoundException => None
        })
    response.subflatMap { r =>
      val bprPostCode = r.address.postalCode.getOrElse("")
      val postCodeMatches = sanitisePostCode(bprPostCode) == sanitisePostCode(kf.postCode)
      if (postCodeMatches) Some(r)
      else None
    }
  }

  private def auditBpr(bpr: Option[BusinessPartnerRecord], kf: BPRKnownFacts)(
    implicit hc: HeaderCarrier): Future[Unit] =
    bpr.fold(Future.successful(logger.error("Des Connector did not return anything from lookup")))(
      bpr =>
        audit
          .sendExtendedEvent(
            ExtendedDataEvent(
              "Country-By-Country-Frontend",
              AUDIT_TAG,
              detail = Json.obj(
                "utr"      -> JsString(kf.utr.utr),
                "postcode" -> JsString(kf.postCode),
                "bpr"      -> Json.toJson(bpr)
              )))
          .map {
            case AuditResult.Disabled        => logger.info("Audit disabled for BPRKnownFactsService")
            case AuditResult.Success         => logger.info("Successful Audit for BPRKnownFactsService")
            case AuditResult.Failure(msg, _) => logger.error(s"Unable to audit a BPRKnowFacts lookup: $msg")
        })

}
