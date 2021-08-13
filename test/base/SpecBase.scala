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

package base

import org.mockito.Mockito.reset
import org.scalatest.Matchers.fail
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest._
import org.scalatestplus.mockito.MockitoSugar
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Application
import play.api.i18n.{Messages, MessagesApi}
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.inject.{Injector, bind}
import play.api.libs.json.Json
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.retrieve.Credentials
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.controllers.actions.{FakeIdentifierAction, IdentifierAction}
import uk.gov.hmrc.cbcrfrontend.model.upscan.UploadId
import uk.gov.hmrc.cbcrfrontend.model.{CBCEnrolment, CBCId, Utr}
import uk.gov.hmrc.cbcrfrontend.services.CBCSessionCache
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.cache.client.CacheMap

import scala.concurrent.Future

trait SpecBase
    extends FreeSpec with MustMatchers with GuiceOneAppPerSuite with OptionValues with TryValues with ScalaFutures
    with IntegrationPatience with MockitoSugar with BeforeAndAfterEach {

  val mockCache: CBCSessionCache = mock[CBCSessionCache]

  override def beforeEach = {
    super.beforeEach()
    reset(mockCache)
  }

  def messages(app: Application): Messages = app.injector.instanceOf[MessagesApi].preferred(FakeRequest())
  def injector: Injector = app.injector

  def frontendAppConfig: FrontendAppConfig = injector.instanceOf[FrontendAppConfig]

  def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]

  implicit def fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("", "")

  implicit val hc: HeaderCarrier = HeaderCarrier()

  val mockAppConfig: FrontendAppConfig = mock[FrontendAppConfig]

  implicit def messages: Messages = messagesApi.preferred(fakeRequest)

  override def fakeApplication(): Application =
    guiceApplicationBuilder()
      .build()

  protected def guiceApplicationBuilder(): GuiceApplicationBuilder =
    new GuiceApplicationBuilder()
      .overrides(
        bind[IdentifierAction].to[FakeIdentifierAction],
        bind[CBCSessionCache].toInstance(mockCache)
      )

  val enrolment: CBCEnrolment = CBCEnrolment(CBCId.create(99).getOrElse(fail("booo")), Utr("1234567890"))
  val uploadId: UploadId = UploadId("123")
  val creds: Credentials = Credentials("totally", "legit")
  val cacheMap = CacheMap("x", Map("x" -> Json.toJson[String]("x")))
}
