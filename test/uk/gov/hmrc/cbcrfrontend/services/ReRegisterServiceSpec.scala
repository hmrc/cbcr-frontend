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

import cats.data.NonEmptyList
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.scalatest.concurrent.{Eventually, ScalaFutures}
import org.scalatest.mock.MockitoSugar
import org.scalatestplus.play.OneAppPerSuite
import play.api.http.Status
import play.api.libs.json.{JsString, Json}
import uk.gov.hmrc.cbcrfrontend.controllers.{CSRFTest, FakeAuthConnector}
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.services._
import uk.gov.hmrc.cbcrfrontend.connectors._
import uk.gov.hmrc.play.http.{HeaderCarrier, HttpResponse, NotFoundException}
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.duration._
import uk.gov.hmrc.cbcrfrontend.auth.SecuredActions

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import uk.gov.hmrc.cbcrfrontend.controllers.auth.{SecuredActionsTest, TestUsers}
import cats.data.OptionT
import cats.data.EitherT
import cats.instances.future._
import org.scalatest.BeforeAndAfterEach
import play.api.test.FakeRequest
import uk.gov.hmrc.emailaddress.EmailAddress

class ReRegisterServiceSpec extends UnitSpec with ScalaFutures with OneAppPerSuite with CSRFTest with MockitoSugar with FakeAuthConnector with BeforeAndAfterEach with Eventually{
  val subData: SubscriptionDataService = mock[SubscriptionDataService]
  val enrol: EnrolmentsConnector = mock[EnrolmentsConnector]
  val sec: SecuredActions = mock[SecuredActions]
  val tax: TaxEnrolmentsConnector = mock[TaxEnrolmentsConnector]
  val cbcId: CBCIdService = mock[CBCIdService]
  val cbcKF: CBCKnownFactsService = mock[CBCKnownFactsService]
  val backEnd: CBCRBackendConnector = mock[CBCRBackendConnector]
  val email: EmailService = mock[EmailService]

  implicit val authCon = authConnector(TestUsers.cbcrUser)

  implicit val fakeRequestSubmit = addToken(FakeRequest("GET", "/submitter-info"))
  implicit val hc = HeaderCarrier()

  val id = CBCId.create(99).toOption
  val idOld = CBCId("XHCBC0000000002")

  val utr = Utr("7000000002")

  val subDetails = SubscriptionDetails(
    BusinessPartnerRecord("safeid", None, EtmpAddress("Line1", None, None, None, None, "GB")),
    SubscriberContact("firstName", "lastName", "lkasjdf", EmailAddress("max@max.com")),
    id,
    utr
  )

  val enrolment = CBCEnrolment(idOld.getOrElse(fail("bad cbid")),utr)

  "The ReRegisterService" should {
    val rrs = new DeEnrolReEnrolService(subData, cbcKF, tax)
    when(subData.retrieveSubscriptionData(any())(any(), any())) thenReturn EitherT.right[Future, CBCErrors, Option[SubscriptionDetails]](Some(subDetails))
    when(tax.deEnrol(any())) thenReturn Future.successful(HttpResponse(200, None, Map.empty))
    when(cbcKF.addKnownFactsToGG(any())(any())) thenReturn EitherT.pure[Future,CBCErrors,Unit](())

    "find their subscriptionData from the UTR" in {
      rrs.deEnrolReEnrol(enrolment)
      eventually{verify(subData).retrieveSubscriptionData(any())(any(), any())}
    }
    "de-enrol them" in {
      eventually{verify(tax).deEnrol(any())}
    }
    "re-enrol them" in {
      eventually{verify(cbcKF).addKnownFactsToGG(any())(any())}
    }
  }
}
