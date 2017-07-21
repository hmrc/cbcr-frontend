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

import javax.inject.{Inject,Singleton}

import cats.data.OptionT
import cats.instances.future._
import play.api.libs.json.Json
import uk.gov.hmrc.cbcrfrontend.connectors.BPRKnownFactsConnector
import uk.gov.hmrc.cbcrfrontend.model.{BPRKnownFacts, BusinessPartnerRecord}
import uk.gov.hmrc.play.http.{HeaderCarrier, NotFoundException}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  * Use the provided KnownFactsConnector to query a UTR
  * Optionally return the [[uk.gov.hmrc.cbcrfrontend.model.BusinessPartnerRecord]] depending on whether it contains
  * the same postcode as the provided [[BPRKnownFacts]]
  */
@Singleton
class BPRKnownFactsService @Inject() (dc:BPRKnownFactsConnector) {

  private def sanitisePostCode(s:String) : String = s.toLowerCase.replaceAll("\\s", "")

  def checkBPRKnownFacts(kf:BPRKnownFacts)(implicit hc:HeaderCarrier) : OptionT[Future,BusinessPartnerRecord] = {
    val response = OptionT(dc.lookup(kf.utr.value).map { response =>
      Json.parse(response.body).validate[BusinessPartnerRecord].asOpt
    }.recover{
      case _:NotFoundException=> None
    })
    response.subflatMap{ r =>
      val postCodeMatches = r.address.postalCode.exists{ pc =>
        sanitisePostCode(pc) == sanitisePostCode(kf.postCode)
      }
      if(postCodeMatches) Some(r)
      else None
    }
  }

}
