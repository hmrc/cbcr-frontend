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

import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mockito.MockitoSugar
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.mvc.{DefaultMessagesControllerComponents, MessagesControllerComponents}
import play.api.test.{FakeRequest, Helpers}
import play.api.{Configuration, Environment}
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.cbcrfrontend.connectors.TaxEnrolmentsConnector
import uk.gov.hmrc.cbcrfrontend.model.{CBCId, Utr}
import uk.gov.hmrc.http.HttpResponse
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}


class EnrolControllerSpec extends UnitSpec with ScalaFutures with GuiceOneAppPerSuite with CSRFTest with MockitoSugar with BeforeAndAfterEach{



  implicit val ec       = app.injector.instanceOf[ExecutionContext]
  implicit val timeout  = Timeout(5 seconds)

  val config            = new Configuration(ConfigFactory.load("application.conf"))

  val authConnector     = mock[AuthConnector]
  val enrolConnector    = mock[TaxEnrolmentsConnector]
  val env               = mock[Environment]
  val mcc               = app.injector.instanceOf[MessagesControllerComponents]
  val controller        = new EnrolController(config,enrolConnector,authConnector,env, mcc)

  val id = CBCId.create(5678).getOrElse(fail("bad cbcid"))
  val utr = Utr("9000000001")

  "Calling enrol controller" should {

    "return 200 when calling deEnrol if authorised Organisation and User or Admin" in {
      val fakeRequest = addToken(FakeRequest("GET", "/de-enrol"))
      val response: HttpResponse = mock[HttpResponse]
      when(authConnector.authorise[Any](any(), any())(any(), any())) thenReturn Future.successful(())
      when(enrolConnector.deEnrol(any())) thenReturn Future.successful(response)
      when(response.body) thenReturn("deEnrol")
      status(controller.deEnrol()(fakeRequest)) shouldBe Status.OK
    }

    "return 200 when calling getEnrolments if authorised" in {
      val fakeRequest = addToken(FakeRequest("GET", "/enrolments"))
      val enrolments = Enrolments(Set(Enrolment("CBC",Seq(EnrolmentIdentifier("cbcId",id.toString)),"something")))
      when(authConnector.authorise[Enrolments](any(), any())(any(), any())) thenReturn Future.successful(enrolments)
      status(controller.getEnrolments(fakeRequest)) shouldBe Status.OK
    }
  }

}
