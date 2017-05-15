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

package uk.gov.hmrc

import cats.data.{EitherT, OptionT}
import cats.instances.future._
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.exceptions.UnexpectedState
import uk.gov.hmrc.cbcrfrontend.model.{AffinityGroup, Agent, Organisation, UserType}
import uk.gov.hmrc.cbcrfrontend.services.CBCSessionCache
import uk.gov.hmrc.play.frontend.auth.AuthContext
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.ExecutionContext

package object cbcrfrontend {

  def getUserType(ac:AuthContext)(implicit cache:CBCSessionCache, sec:FrontendAuthConnector, hc:HeaderCarrier, ec:ExecutionContext): ServiceResponse[UserType] = {
    def affinityGroupToUserType(a:AffinityGroup): Either[UnexpectedState,UserType] = a.affinityGroup.toLowerCase.trim match {
      case "agent"        => Right(Agent)
      case "organisation" => Right(Organisation)
      case other          => Left(UnexpectedState(s"Unknown affinity group: $other"))
    }

    EitherT(OptionT(cache.read[AffinityGroup])
      .getOrElseF {
        sec.getUserDetails[AffinityGroup](ac)
          .flatMap(ag => cache.save[AffinityGroup](ag)
            .map(_ => ag))
      }
      .map(affinityGroupToUserType)
    )

  }

}
