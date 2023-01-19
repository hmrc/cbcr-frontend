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

import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mockito.MockitoSugar
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Configuration
import play.api.Play.materializer
import play.api.http.Status
import play.api.i18n.{Messages, MessagesApi}
import play.api.mvc.MessagesControllerComponents
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.retrieve.{Retrieval, ~}
import uk.gov.hmrc.auth.core.{AffinityGroup, AuthConnector}
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.model.{CBCEnrolment, CBCId, Utr}
import uk.gov.hmrc.cbcrfrontend.services.CBCSessionCache
import uk.gov.hmrc.cbcrfrontend.util.{FeatureSwitch, UnitSpec}
import uk.gov.hmrc.cbcrfrontend.views.Views

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._

class StartControllerSpec
    extends UnitSpec with ScalaFutures with GuiceOneAppPerSuite with CSRFTest with MockitoSugar
    with BeforeAndAfterEach {

  implicit val feConf = mock[FrontendAppConfig]
  implicit val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
  implicit val configuration = new Configuration(ConfigFactory.load("application.conf"))
  implicit val cache: CBCSessionCache = mock[CBCSessionCache]
  implicit val timeout = Timeout(5 seconds)

  val authConnector = mock[AuthConnector]
  val mcc = app.injector.instanceOf[MessagesControllerComponents]
  val views: Views = app.injector.instanceOf[Views]
  val controller = new StartController(messagesApi, authConnector, mcc, views)
  val newCBCEnrolment = CBCEnrolment(CBCId.create(99).getOrElse(fail("booo")), Utr("1234567890"))
  val langSwitch = mock[FeatureSwitch]

  def getMessages(r: FakeRequest[_]): Messages = messagesApi.preferred(r)

  "Calling start controller" should {
    val fakeRequest = addToken(FakeRequest("GET", "/"))

    "return 303 if authorised and Agent" in {
      when(authConnector.authorise(any(), any[Retrieval[Option[AffinityGroup] ~ Option[CBCEnrolment]]]())(any(), any()))
        .thenReturn(Future.successful(
          new ~[Option[AffinityGroup], Option[CBCEnrolment]](Some(AffinityGroup.Agent), Some(newCBCEnrolment))))
      status(controller.start(fakeRequest)) shouldBe Status.SEE_OTHER
    }

    "return 200 if authorized and registered Organisation for CBCR" in {
      when(authConnector.authorise(any(), any[Retrieval[Option[AffinityGroup] ~ Option[CBCEnrolment]]]())(any(), any()))
        .thenReturn(Future.successful(
          new ~[Option[AffinityGroup], Option[CBCEnrolment]](Some(AffinityGroup.Organisation), Some(newCBCEnrolment))))
      status(controller.start(fakeRequest)) shouldBe Status.OK
    }

    "return 303 if authorised Organisation but not registered for CBCR" in {
      when(authConnector.authorise(any(), any[Retrieval[Option[AffinityGroup] ~ Option[CBCEnrolment]]]())(any(), any()))
        .thenReturn(
          Future.successful(new ~[Option[AffinityGroup], Option[CBCEnrolment]](Some(AffinityGroup.Organisation), None)))
      status(controller.start(fakeRequest)) shouldBe Status.SEE_OTHER
    }

    "return 403 if authorised Individual" in {
      when(authConnector.authorise(any(), any[Retrieval[Option[AffinityGroup] ~ Option[CBCEnrolment]]]())(any(), any()))
        .thenReturn(
          Future.successful(new ~[Option[AffinityGroup], Option[CBCEnrolment]](Some(AffinityGroup.Individual), None)))
      status(controller.start(fakeRequest)) shouldBe Status.FORBIDDEN
    }
    "return 303 if submit returns upload" in {
      val fakeRequest = addToken(FakeRequest("POST", "/")).withFormUrlEncodedBody("choice" -> "upload")
      when(authConnector.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful(())
      val result = call(controller.submit, fakeRequest)
      status(result) shouldBe Status.SEE_OTHER
    }

    "return 303 if submit returns editSubscriberInfo" in {
      val fakeRequest = addToken(FakeRequest("POST", "/")).withFormUrlEncodedBody("choice" -> "editSubscriberInfo")
      when(authConnector.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful(())
      val result = call(controller.submit, fakeRequest)
      status(result) shouldBe Status.SEE_OTHER
    }

    "return 303 if submit returns no choice" in {
      val fakeRequest = addToken(FakeRequest("POST", "/")).withFormUrlEncodedBody("choice" -> "")
      when(authConnector.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful(())
      val result = call(controller.submit, fakeRequest)
      status(result) shouldBe Status.BAD_REQUEST
    }
  }
}
