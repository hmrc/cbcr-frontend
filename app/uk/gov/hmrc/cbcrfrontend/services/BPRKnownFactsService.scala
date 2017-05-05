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

package uk.gov.hmrc.cbcrfrontend.services

import cats.data.{EitherT, OptionT}
import cats.instances.future._

import scala.concurrent.ExecutionContext.Implicits.global
import play.api.Logger
import play.api.libs.json.Json
import uk.gov.hmrc.cbcrfrontend.connectors.BPRKnownFactsConnector
import uk.gov.hmrc.cbcrfrontend.exceptions.UnexpectedState
import uk.gov.hmrc.cbcrfrontend.model.{BusinessPartnerRecord, BPRKnownFacts, OrganisationResponse}
import cats.syntax.either._
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.Future

/**
  * Use the provided KnownFactsConnector to query a UTR
  * Optionally return the [[uk.gov.hmrc.cbcrfrontend.model.BusinessPartnerRecord]] depending on whether it contains
  * the same postcode as the provided [[BPRKnownFacts]]
  */
class BPRKnownFactsService(dc:BPRKnownFactsConnector) {

  private def sanitisePostCode(s:String) : String = s.toLowerCase.replaceAll("\\s", "")

  def checkBPRKnownFacts(kf:BPRKnownFacts)(implicit hc:HeaderCarrier) : OptionT[Future,BusinessPartnerRecord] = {
    val response = EitherT(dc.lookup(kf.utr.value).map { response =>
      Json.parse(response.body).validate[BusinessPartnerRecord].asEither.leftMap(_ => UnexpectedState(response.body))
    })
    response.leftMap(e => Logger.warn(s"Match request failed: ${e.errorMsg}"))
    response.toOption.subflatMap{ r =>
      val postCodeMatches = r.address.postalCode.exists(pc => sanitisePostCode(pc) == sanitisePostCode(kf.postCode))
      if(postCodeMatches) Some(r)
      else None
    }
  }

}
