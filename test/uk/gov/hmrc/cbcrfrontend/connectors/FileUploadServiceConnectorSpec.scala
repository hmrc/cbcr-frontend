/*
 * Copyright 2024 HM Revenue & Customs
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

package uk.gov.hmrc.cbcrfrontend.connectors

import com.github.tomakehurst.wiremock.WireMockServer
import com.github.tomakehurst.wiremock.client.WireMock
import com.github.tomakehurst.wiremock.client.WireMock._
import com.github.tomakehurst.wiremock.core.WireMockConfiguration.wireMockConfig
import com.github.tomakehurst.wiremock.matching.MultipartValuePatternBuilder
import org.mockito.MockitoSugar
import org.scalatest.BeforeAndAfterEach
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Application
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.{Json, OFormat}
import play.api.test.Helpers.{await, defaultAwaitTimeout}
import uk.gov.hmrc.auth.core.AffinityGroup.Individual
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.emailaddress.EmailAddress
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import scala.concurrent.ExecutionContext

class FileUploadServiceConnectorSpec
    extends AnyWordSpec with GuiceOneAppPerSuite with BeforeAndAfterEach with MockitoSugar {
  val stubPort = 11122
  val stubHost = "localhost"
  val wireMockServer = new WireMockServer(wireMockConfig().port(stubPort))

  override lazy val fakeApplication: Application = new GuiceApplicationBuilder()
    .configure(
      "microservice.services.file-upload-frontend.host" -> "127.0.0.1",
      "microservice.services.file-upload-frontend.port" -> stubPort
    )
    .build()

  implicit val ec: ExecutionContext = fakeApplication.injector.instanceOf[ExecutionContext]

  trait Setup {
    implicit val hc: HeaderCarrier = HeaderCarrier()

    val config: ServicesConfig = fakeApplication.injector.instanceOf[ServicesConfig]
    val httpClient: HttpClientV2 = fakeApplication.injector.instanceOf[HttpClientV2]
    val fileUploadConnector = new FileUploadServiceConnector(httpClient, config)
    implicit val format: OFormat[UploadFile] = Json.format[UploadFile]
  }

  override def beforeEach(): Unit = {
    wireMockServer.start()
    configureFor(stubHost, stubPort)
  }

  override def afterEach(): Unit =
    wireMockServer.stop()

  "Upload file " should {
    "verify submitted file part is a valid json payload" in new Setup {
      private val fileId = FileId("file_id")
      private val envelopeId = EnvelopeId("test_envelope_id")

      private val metadata = SubmissionMetaData(
        SubmissionInfo(
          "",
          CBCId.create(42).getOrElse(fail("unable to create cbcid")),
          "",
          Hash(""),
          "",
          TIN("", ""),
          FilingType(CBC701),
          UltimateParentEntity("")
        ),
        SubmitterInfo("", Some(AgencyBusinessName("")), "", EmailAddress("abc@xyz.com"), Some(Individual)),
        FileInfo(FileId(""), envelopeId, "", "", "", BigDecimal(0), "")
      )

      val uploadFile: UploadFile =
        UploadFile(envelopeId, fileId, "metadata.json", "application/json; charset=UTF-8", metadata)

      await(fileUploadConnector.uploadFile(uploadFile))

      WireMock.verify(
        postRequestedFor(
          urlEqualTo(s"/file-upload/upload/envelopes/$envelopeId/files/$fileId")
        ).withRequestBodyPart(
          new MultipartValuePatternBuilder()
            .withBody(equalToJson(Json.toJson(uploadFile.metadata).toString()))
            .build()
        )
      )
    }
  }
}
