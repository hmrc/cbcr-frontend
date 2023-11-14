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

import org.mockito.ArgumentMatchersSugar.*
import org.mockito.IdiomaticMockito
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.Configuration
import play.api.libs.json.Json
import play.api.test.Helpers.{await, defaultAwaitTimeout}
import uk.gov.hmrc.cbcrfrontend.model.{EnvelopeId, ExpiredSession}
import uk.gov.hmrc.http.cache.client.CacheMap
import uk.gov.hmrc.http.{HeaderCarrier, HttpClient, HttpResponse, SessionId, UpstreamErrorResponse}

import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.control.NonFatal

class CBCSessionCacheSpec extends AnyWordSpec with Matchers with IdiomaticMockito {
  private val config = Configuration.from(
    Map(
      "microservice.services.cachable.session-cache" -> Map(
        "protocol" -> "testtp",
        "host"     -> "test-host",
        "port"     -> 1234,
        "domain"   -> "test-domain"
      )
    )
  )

  private val httpClient = mock[HttpClient]

  private val cache = new CBCSessionCache(config, httpClient)

  private implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId(UUID.randomUUID().toString)))

  "CBCSessionCache" should {
    "save" should {
      "save session data and return CacheMap" in {
        val data = EnvelopeId("test id")

        val json = Map(
          "" -> Json.toJson(data)
        )

        httpClient.PUT[EnvelopeId, CacheMap](*, data, *)(*, *, *, *) returns Future.successful(
          CacheMap("some result", json))

        val result = await(cache.save[EnvelopeId](data))

        result shouldBe CacheMap("some result", json)
      }
    }

    "read" should {
      "read session data and return Right(data)" in {
        val data = EnvelopeId("test id")

        val json = Map(
          "EnvelopeId" -> Json.toJson(data)
        )

        httpClient.GET[CacheMap](*, *, *)(*, *, *) returns Future.successful(CacheMap("some result", json))

        val result = await(cache.read[EnvelopeId].value)

        result shouldBe Right(data)
      }

      "return Left(error) when error occurs" in {
        httpClient.GET[CacheMap](*, *, *)(*, *, *) returns Future.failed(
          UpstreamErrorResponse("something went wrong", 404))

        val result = await(cache.read[EnvelopeId].value)

        result shouldBe Left(ExpiredSession("Unable to read uk.gov.hmrc.cbcrfrontend.model.EnvelopeId from cache"))
      }
    }

    "readOption" should {
      "read session data and return Option(data)" in {
        val data = EnvelopeId("test id")

        val json = Map(
          "EnvelopeId" -> Json.toJson(data)
        )

        httpClient.GET[CacheMap](*, *, *)(*, *, *) returns Future.successful(CacheMap("some result", json))

        val result = await(cache.readOption[EnvelopeId])

        result shouldBe Some(data)
      }
    }

    "create" should {
      "save session data and return OptionT" in {
        val data = EnvelopeId("test id")

        val json = Map(
          "" -> Json.toJson(data)
        )

        httpClient.PUT[EnvelopeId, CacheMap](*, data, *)(*, *, *, *) returns Future.successful(
          CacheMap("some result", json))

        val result = await(cache.create[EnvelopeId](data).value)

        result shouldBe Some(data)
      }
    }

    "clear" should {
      "clear session data and return true when response is OK" in {
        httpClient.DELETE[HttpResponse](*, *)(*, *, *) returns Future.successful(HttpResponse(200, ""))

        val result = await(cache.clear)

        result shouldBe true
      }

      "clear session data and return true when response is NO_CONTENT" in {
        httpClient.DELETE[HttpResponse](*, *)(*, *, *) returns Future.successful(HttpResponse(204, ""))

        val result = await(cache.clear)

        result shouldBe true
      }

      "return true when downstream response is not either OK nor NO_CONTENT" in {
        httpClient.DELETE[HttpResponse](*, *)(*, *, *) returns Future.successful(HttpResponse(400, ""))

        val result = await(cache.clear)

        result shouldBe false
      }

      "return true when future fails" in {
        httpClient.DELETE[HttpResponse](*, *)(*, *, *) returns Future.failed(new RuntimeException())

        val result = await(cache.clear)

        result shouldBe false
      }
    }
  }
}
