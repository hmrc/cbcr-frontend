/*
 * Copyright 2019 HM Revenue & Customs
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




import akka.actor.ActorSystem
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import org.mockito.Matchers.any
import org.mockito.Mockito._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mockito.MockitoSugar
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.{Configuration, Environment}
import play.api.http.HeaderNames.LOCATION
import play.api.i18n.{Lang, Messages, MessagesApi}
import play.api.mvc.Result
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.{AffinityGroup, AuthConnector}
import uk.gov.hmrc.auth.core.retrieve.{Retrieval, ~}
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.model.{CBCEnrolment, CBCId, UnexpectedState, Utr}
import uk.gov.hmrc.cbcrfrontend.services.CBCSessionCache
import uk.gov.hmrc.play.test.UnitSpec
import play.api.http.Status
import uk.gov.hmrc.cbcrfrontend.model.DocRefIdResponses.Ok

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import play.api.libs.json.{JsValue, Json}
import uk.gov.hmrc.cbcrfrontend.util.FeatureSwitch


class StartControllerSpec extends UnitSpec with ScalaFutures with GuiceOneAppPerSuite with CSRFTest with MockitoSugar with BeforeAndAfterEach{


  implicit val feConf                   = mock[FrontendAppConfig]
  implicit val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
  implicit val configuration            = new Configuration(ConfigFactory.load("application.conf"))
  implicit val cache: CBCSessionCache   = mock[CBCSessionCache]
  implicit val timeout                  = Timeout(5 seconds)

  val authConnector                     = mock[AuthConnector]
  val controller                        = new StartController(messagesApi,authConnector)
  val newCBCEnrolment                   = CBCEnrolment(CBCId.create(99).getOrElse(fail("booo")), Utr("1234567890"))
  val langSwitch                        = mock[FeatureSwitch]

  def getMessages(r: FakeRequest[_]): Messages = messagesApi.preferred(r)

  "Calling start controller" should {
    val fakeRequest = addToken(FakeRequest("GET", "/"))
    when(feConf.analyticsHost) thenReturn "host"
    when(feConf.analyticsToken) thenReturn "token"

    "return 303 if authorised and Agent" in {
      when(authConnector.authorise(any(), any[Retrieval[Option[AffinityGroup] ~ Option[CBCEnrolment]]]())(any(), any()))
        .thenReturn(Future.successful(new ~[Option[AffinityGroup], Option[CBCEnrolment]](Some(AffinityGroup.Agent), Some(newCBCEnrolment))))
      status(controller.start(fakeRequest)) shouldBe Status.SEE_OTHER
    }

    "return 200 if authorized and registered Organisation for CBCR" in {
      when(authConnector.authorise(any(), any[Retrieval[Option[AffinityGroup] ~ Option[CBCEnrolment]]]())(any(), any()))
        .thenReturn(Future.successful(new ~[Option[AffinityGroup], Option[CBCEnrolment]](Some(AffinityGroup.Organisation), Some(newCBCEnrolment))))
      status(controller.start(fakeRequest)) shouldBe Status.OK
    }

    "return 303 if authorised Organisation but not registered for CBCR" in {
      when(authConnector.authorise(any(), any[Retrieval[Option[AffinityGroup] ~ Option[CBCEnrolment]]]())(any(), any()))
        .thenReturn(Future.successful(new ~[Option[AffinityGroup], Option[CBCEnrolment]](Some(AffinityGroup.Organisation), None)))
      status(controller.start(fakeRequest)) shouldBe Status.SEE_OTHER
      }

    "return 501 if authorised Individual" in {
      when(authConnector.authorise(any(), any[Retrieval[Option[AffinityGroup] ~ Option[CBCEnrolment]]]())(any(), any()))
        .thenReturn(Future.successful(new ~[Option[AffinityGroup], Option[CBCEnrolment]](Some(AffinityGroup.Individual), None)))
      status(controller.start(fakeRequest)) shouldBe Status.INTERNAL_SERVER_ERROR
      }

    "return 303 if submit returns upload" in {
      val data = Json.obj("choice" -> "upload")
      val fakeRequest = addToken(FakeRequest("GET", "/")).withJsonBody(data)
      when(authConnector.authorise[Any](any(),any())(any(),any())) thenReturn Future.successful(())
      status(controller.submit(fakeRequest)) shouldBe  Status.SEE_OTHER
    }

    "return 303 if submit returns editSubscriberInfo" in {
      val data = Json.obj("choice" -> "editSubscriberInfo")
      val fakeRequest = addToken(FakeRequest("GET", "/")).withJsonBody(data)
      when(authConnector.authorise[Any](any(),any())(any(),any())) thenReturn Future.successful(())
      status(controller.submit(fakeRequest)) shouldBe  Status.SEE_OTHER
    }

    "return 303 if submit returns no choice" in {
      val data = Json.obj("choice" -> "")
      val fakeRequest = addToken(FakeRequest("GET", "/")).withJsonBody(data)
      when(authConnector.authorise[Any](any(),any())(any(),any())) thenReturn Future.successful(())
      status(controller.submit(fakeRequest)) shouldBe  Status.BAD_REQUEST
    }

  }

}
