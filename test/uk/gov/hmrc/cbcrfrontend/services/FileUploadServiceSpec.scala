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

package uk.gov.hmrc.cbcrfrontend.services

import akka.actor.ActorSystem
import akka.testkit.TestKit
import akka.util.ByteString
import mockws.{MockWS, MockWSHelpers}
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import org.mockito.ArgumentMatchersSugar.*
import org.mockito.{ArgumentMatchersSugar, IdiomaticMockito}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import play.api.i18n.Messages
import play.api.libs.json.{JsObject, Json}
import play.api.mvc.Results.{InternalServerError, Ok}
import play.api.test.Helpers
import play.api.test.Helpers.{GET, LOCATION, await, defaultAwaitTimeout}
import uk.gov.hmrc.auth.core.AffinityGroup.Individual
import uk.gov.hmrc.cbcrfrontend.config.{FileUploadFrontEndWS, FrontendAppConfig}
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.typesclasses.{CreateEnvelope, RouteEnvelopeRequest, UploadFile}
import uk.gov.hmrc.emailaddress.EmailAddress
import uk.gov.hmrc.http.{HeaderCarrier, HttpClient, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import java.time.{Clock, Instant, ZoneId}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class FileUploadServiceSpec
    extends TestKit(ActorSystem()) with AnyWordSpecLike with Matchers with IdiomaticMockito with MockWSHelpers
    with BeforeAndAfterAll {
  private val envelopeIdString = "test-envelope-id"
  private val envelopeId = EnvelopeId(envelopeIdString)

  private val fileIdString = "test-file-id"

  private val otherHttpStatus = 500

  private val metadata = SubmissionMetaData(
    SubmissionInfo(
      "",
      CBCId.create(42).getOrElse(fail("unable to create cbcid")),
      "",
      Hash(""),
      "",
      TIN("", ""),
      FilingType(CBC701),
      UltimateParentEntity("")),
    SubmitterInfo("", Some(AgencyBusinessName("")), "", EmailAddress("abc@xyz.com"), Some(Individual)),
    FileInfo(FileId(""), envelopeId, "", "", "", BigDecimal(0), "")
  )

  private val validationErrors = List(
    XMLErrors(List("test error 1", "test error 2", "test error 3")),
    TestDataError,
    FatalSchemaErrors(None),
    InvalidFileType("test type"),
    AllBusinessRuleErrors(
      List(
        MessageRefIDDuplicate,
        FileNameError("test found", "test expected"),
        InvalidXMLError("test xml error"),
        AdditionalInfoDRINotFound("test firstCdri", "test missingCdri")
      ))
  )

  private val clock = Clock.fixed(Instant.parse("2014-12-22T10:15:30Z"), ZoneId.of("UTC"))

  private val expiryDateString =
    DateTimeFormat.forPattern("YYYY-MM-dd'T'HH:mm:ss'Z'").print(new DateTime(clock.millis()).plusDays(7))

  private val envelopeRequestJson = Json
    .toJson(EnvelopeRequest("/cbcr/file-upload-response", expiryDateString, MetaData(), Constraints()))
    .as[JsObject]
  private val ws = MockWS {
    case (GET, s"/file-upload/envelopes/$envelopeIdString/files/$fileId/content") =>
      Action {
        Ok("{}")
      }
  }
  private val mockFrontendAppConfig = mock[FrontendAppConfig]
  private val mockServicesConfig = mock[ServicesConfig]
  private implicit val mockHttpClient: HttpClient = mock[HttpClient]

  private implicit val mockFileUploadFrontEndWS: FileUploadFrontEndWS = mock[FileUploadFrontEndWS]

  private implicit val hc: HeaderCarrier = HeaderCarrier()

  private val fileUploadService =
    new FileUploadService(ws, mockFrontendAppConfig, mockServicesConfig, clock)

  override def afterAll(): Unit = {
    shutdownHelpers()
    ws.close()
  }

  "The FileUploadService" when {
    "createEnvelope is called" should {
      mockFrontendAppConfig.envelopeExpiryDays returns 7

      "return successful response with envelope ID" in {
        mockFileUploadFrontEndWS.POST[JsObject, HttpResponse](
          "/file-upload/envelopes",
          CreateEnvelope(envelopeRequestJson).body,
          Seq.empty)(*, *, *, *) returns Future.successful(
          HttpResponse(200, "", Map(LOCATION -> Seq(s"envelopes/$envelopeIdString"))))

        val response = await(fileUploadService.createEnvelope.value)

        response shouldBe Right(envelopeId)
      }

      "return Left(UnexpectedState) when response LOCATION header is missing" in {
        mockFileUploadFrontEndWS.POST[JsObject, HttpResponse](
          "/file-upload/envelopes",
          CreateEnvelope(envelopeRequestJson).body,
          Seq.empty)(*, *, *, *) returns Future.successful(HttpResponse(200, ""))

        val response = await(fileUploadService.createEnvelope.value)

        response shouldBe Left(UnexpectedState("Header Location not found"))
      }

      "return Left(UnexpectedState) when response LOCATION header is invalid" in {
        mockFileUploadFrontEndWS.POST[JsObject, HttpResponse](
          "/file-upload/envelopes",
          CreateEnvelope(envelopeRequestJson).body,
          Seq.empty)(*, *, *, *) returns Future.successful(HttpResponse(200, "", Map(LOCATION -> Seq(s"invalid"))))

        val response = await(fileUploadService.createEnvelope.value)

        response shouldBe Left(UnexpectedState(s"EnvelopeId in $LOCATION header: invalid not found"))
      }
    }

    "getFileUploadResponse is called" should {
      "return successful response when http status is 200 and FileUploadCallbackResponse json is valid" in {
        val status = "test-status"
        val fileUploadResponseJson =
          s"""
             |{
             |"envelopeId": "$envelopeIdString",
             |"fileId": "$fileIdString",
             |"status": "$status"
             |}
             |""".stripMargin
        mockHttpClient.GET[HttpResponse](s"/cbcr/file-upload-response/$envelopeIdString")(*, *, *) returns Future
          .successful(HttpResponse(200, fileUploadResponseJson))

        val response = await(fileUploadService.getFileUploadResponse(envelopeIdString).value)

        response shouldBe Right(Some(FileUploadCallbackResponse(envelopeIdString, fileIdString, status, None)))
      }

      "return Left(UnexpectedState) when http status is 200 and FileUploadCallbackResponse json is invalid" in {
        mockHttpClient.GET[HttpResponse](s"/cbcr/file-upload-response/$envelopeIdString")(*, *, *) returns Future
          .successful(HttpResponse(200, """{}""".stripMargin))

        val response = await(fileUploadService.getFileUploadResponse(envelopeIdString).value)

        response.fold({
          case e: UnexpectedState => e.errorMsg should startWith("Problems extracting File Upload response message")
          case _                  => fail()
        }, _ => {
          fail()
        })
      }

      "return Right(None) when http status is 204" in {
        mockHttpClient.GET[HttpResponse](s"/cbcr/file-upload-response/$envelopeIdString")(*, *, *) returns Future
          .successful(HttpResponse(204, ""))

        val response = await(fileUploadService.getFileUploadResponse(envelopeIdString).value)

        response shouldBe Right(None)
      }

      "return Left(UnexpectedState) for any other http status" in {
        mockHttpClient.GET[HttpResponse](s"/cbcr/file-upload-response/$envelopeIdString")(*, *, *) returns Future
          .successful(HttpResponse(otherHttpStatus, ""))

        val response = await(fileUploadService.getFileUploadResponse(envelopeIdString).value)

        response shouldBe Left(UnexpectedState("Problems getting File Upload response message"))
      }
    }

    "getFile is called" should {
      "return successful response with file" in {
        val response = await(fileUploadService.getFile(envelopeIdString, fileIdString).value)

        response.fold(_ => fail(), f => f.getPath should include(s"$envelopeIdString"))
      }

      "return Left(UnexpectedState) for any other http status" in {
        val ws = MockWS {
          case (GET, s"/file-upload/envelopes/$envelopeIdString/files/$fileId/content") =>
            Action {
              InternalServerError
            }
        }
        val fileUploadService =
          new FileUploadService(ws, mockFrontendAppConfig, mockServicesConfig, clock)

        val response = await(fileUploadService.getFile(envelopeIdString, fileIdString).value)

        response shouldBe Left(
          UnexpectedState(
            s"Failed to retrieve file $fileIdString from envelope $envelopeId - received $otherHttpStatus response"))
      }
    }

    "getFileMetaData is called" should {
      "return successful response with file metadata" in {
        val fileMetadataJson =
          """
            |{
            |"id": "test-metadata-id",
            |"status": "test-metadata-status",
            |"name": "test-metadata-name",
            |"contentType": "test-metadata-content-type",
            |"length": 0,
            |"created": "test-metadata-created",
            |"metadata": "test-metadata",
            |"href": "test-metadata-href"
            |}
            |""".stripMargin
        mockHttpClient.GET[HttpResponse](s"/file-upload/envelopes/$envelopeId/files/$fileIdString/metadata")(*, *, *) returns Future
          .successful(HttpResponse(200, fileMetadataJson))

        val response = await(fileUploadService.getFileMetaData(envelopeIdString, fileIdString).value)

        response shouldBe Right(Some(Json.parse(fileMetadataJson).as[FileMetadata]))
      }

      "return Left(UnexpectedState) for other http status" in {
        mockHttpClient.GET[HttpResponse](s"/file-upload/envelopes/$envelopeId/files/$fileIdString/metadata")(*, *, *) returns Future
          .successful(HttpResponse(500, ""))

        val response = await(fileUploadService.getFileMetaData(envelopeIdString, fileIdString).value)

        response shouldBe Left(UnexpectedState("Problems getting File Metadata"))
      }
    }

    "uploadMetadataAndRoute is called" should {
      "return successful response with body when route envelope request returns 201" in {
        mockFileUploadFrontEndWS.doFormPartPost(
          ArgumentMatchersSugar.startsWith(s"/file-upload/upload/envelopes/$envelopeIdString/files/"),
          "metadata.json",
          "application/json; charset=UTF-8",
          ByteString.fromArray(
            UploadFile(envelopeId, FileId(fileIdString), "", "", Json.toJson(metadata).toString().getBytes).body),
          Seq("CSRF-token" -> "nocheck")
        )(*, *) returns Future.successful(HttpResponse(200, ""))

        mockFileUploadFrontEndWS.POST[RouteEnvelopeRequest, HttpResponse](
          "/file-routing/requests",
          RouteEnvelopeRequest(envelopeId, "cbcr", "OFDS"))(*, *, *, *) returns Future.successful(
          HttpResponse(201, "route-envelope-response"))

        val response = await(fileUploadService.uploadMetadataAndRoute(metadata).value)

        response shouldBe Right("route-envelope-response")
      }

      "return Left(UnexpectedState) response when route envelope request returns other http status" in {
        mockFileUploadFrontEndWS.doFormPartPost(
          ArgumentMatchersSugar.startsWith(s"/file-upload/upload/envelopes/$envelopeIdString/files/"),
          "metadata.json",
          "application/json; charset=UTF-8",
          ByteString.fromArray(
            UploadFile(envelopeId, FileId(fileIdString), "", "", Json.toJson(metadata).toString().getBytes).body),
          Seq("CSRF-token" -> "nocheck")
        )(*, *) returns Future.successful(HttpResponse(200, ""))

        mockFileUploadFrontEndWS.POST[RouteEnvelopeRequest, HttpResponse](
          "/file-routing/requests",
          RouteEnvelopeRequest(envelopeId, "cbcr", "OFDS"))(*, *, *, *) returns Future.successful(HttpResponse(500, ""))

        val response = await(fileUploadService.uploadMetadataAndRoute(metadata).value)

        response shouldBe Left(
          UnexpectedState(s"[FileUploadService][uploadMetadataAndRoute] Failed to create route request, received 500"))
      }
    }

    "errorsToMap is called" should {
      "convert list of validation errors to map of strings" in {
        implicit val messages: Messages = Helpers.stubMessages()

        val response = fileUploadService.errorsToMap(validationErrors)

        response.size shouldBe 5
        response shouldBe Map(
          "error_1" -> "xmlError.header \n\n test error 1\ntest error 2\ntest error 3",
          "error_2" -> "error.TestDataError",
          "error_3" -> "Fatal Schema Error",
          "error_4" -> "File test type is an invalid file type",
          "error_5" -> "messageRefIdError.Duplicate,error.FileNameError1 \r\n error.FileNameError2 test found \r\n error.FileNameError3 test expected,InvalidXMLError: test xml error,error.AdditionalInfoDRINotFound1 test missingCdri error.AdditionalInfoDRINotFound2 \r\n error.AdditionalInfoDRINotFound3 test firstCdri error.AdditionalInfoDRINotFound4"
        )
      }
    }

    "errorsToString is called" should {
      "convert list of validation errors to a string" in {
        implicit val messages: Messages = Helpers.stubMessages()

        val response = fileUploadService.errorsToString(validationErrors)

        response shouldBe
          "xmlError.header \n\n" +
            " test error 1\n" +
            "test error 2\n" +
            "test error 3\r\n" +
            "error.TestDataError\r\n" +
            "Fatal Schema Error\r\n" +
            "File test type is an invalid file type\r\n" +
            "messageRefIdError.Duplicate,error.FileNameError1 \r\n" +
            " error.FileNameError2 test found \r\n" +
            " error.FileNameError3 test expected,InvalidXMLError: test xml error,error.AdditionalInfoDRINotFound1 test missingCdri error.AdditionalInfoDRINotFound2 \r\n" +
            " error.AdditionalInfoDRINotFound3 test firstCdri error.AdditionalInfoDRINotFound4"
      }
    }

    "errorsToFile is called" should {
      "convert list of validation errors to a File" in {
        implicit val messages: Messages = Helpers.stubMessages()

        val response = fileUploadService.errorsToFile(validationErrors, "testFile")

        response.getName should startWith("testFile")
        response.getName should endWith(".txt")

        val bufferedSource = scala.io.Source.fromFile(response)
        val text = bufferedSource.getLines().mkString("\n")
        bufferedSource.close()

        text shouldBe
          "xmlError.header \n\n" +
            " test error 1\n" +
            "test error 2\n" +
            "test error 3\n" +
            "error.TestDataError\n" +
            "Fatal Schema Error\n" +
            "File test type is an invalid file type\n" +
            "messageRefIdError.Duplicate,error.FileNameError1 \n" +
            " error.FileNameError2 test found \n" +
            " error.FileNameError3 test expected,InvalidXMLError: test xml error,error.AdditionalInfoDRINotFound1 test missingCdri error.AdditionalInfoDRINotFound2 \n" +
            " error.AdditionalInfoDRINotFound3 test firstCdri error.AdditionalInfoDRINotFound4"
      }
    }
  }
}
