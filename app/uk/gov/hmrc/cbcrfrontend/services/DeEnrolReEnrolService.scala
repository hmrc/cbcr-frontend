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

import javax.inject.{Inject, Singleton}

import cats.data.EitherT
import cats.instances.all._
import play.api.Logger
import uk.gov.hmrc.cbcrfrontend.connectors._
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.model.{CBCErrors, _}
import uk.gov.hmrc.play.frontend.auth.connectors.AuthConnector
import uk.gov.hmrc.play.frontend.controller.FrontendController
import uk.gov.hmrc.play.http
import uk.gov.hmrc.play.http.{HeaderCarrier, HttpException, HttpResponse}

import scala.concurrent.{ExecutionContext, Future}


@Singleton
class DeEnrolReEnrolService @Inject()(val subData: SubscriptionDataService,
                                      val kfService: CBCKnownFactsService,
                                      val taxEnrolments:TaxEnrolmentsConnector)
                                     (implicit ec: ExecutionContext, auth:AuthConnector) extends FrontendController {

  /**
    * Given a CBCEnrolment:
    * - lookup the SubscriptionDetails for the CBCEnrolment.utr
    * - ensure they are different
    * - un-enrol the user from CBC
    * - re-enrol with the existing UTR and the CBCId found in the SubscriptionDetails
    * @param enrolment - the enrolment taken from the Bearer Token
    * @param hc - The [[HeaderCarrier]]
    * @return an EitherT[Future,CBCError,CBCId]
    */
  def deEnrolReEnrol(enrolment: CBCEnrolment)(implicit hc:HeaderCarrier): ServiceResponse[CBCId] = for {
    details <- subData.retrieveSubscriptionData(Left(enrolment.utr)).subflatMap(_.toRight(UnexpectedState("Could not retreive subscription data")))
    cbcId   <- EitherT.fromEither[Future](details.cbcId.toRight(UnexpectedState("No CBCId in subscription data found")))
    _       <- EitherT.cond[Future](cbcId == enrolment.cbcId, (), UnexpectedState("CBCId in bearer token is the same as in mongo - already regenerated?"))
    unenrol <- EitherT.right[Future, CBCErrors, HttpResponse](taxEnrolments.deEnrol.recover {
      case e: HttpException => http.HttpResponse(e.responseCode, responseString = Some(e.message))
    })
    _       <- EitherT.cond[Future](unenrol.status == 200, (), UnexpectedState(s"Could not un-enrol user: ${unenrol.body}"))
    _       <- kfService.addKnownFactsToGG(CBCKnownFacts(enrolment.utr, cbcId))
  } yield cbcId

}
