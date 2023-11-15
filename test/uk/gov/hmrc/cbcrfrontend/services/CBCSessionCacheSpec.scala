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

import org.mockito.IdiomaticMockito
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.Configuration
import play.api.libs.json.{JsObject, Json}
import play.api.test.Helpers.{await, defaultAwaitTimeout}
import uk.gov.hmrc.cbcrfrontend.model.{EnvelopeId, ExpiredSession}
import uk.gov.hmrc.http.{HeaderCarrier, SessionId}
import uk.gov.hmrc.mongo.CurrentTimestampSupport
import uk.gov.hmrc.mongo.cache.CacheIdType.SessionCacheId.NoSessionException
import uk.gov.hmrc.mongo.test.{CleanMongoCollectionSupport, MongoSupport}

import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global

class CBCSessionCacheSpec
    extends AnyWordSpec with Matchers with IdiomaticMockito with MongoSupport with CleanMongoCollectionSupport {
  private val config = Configuration.from(
    Map(
      "mongodb" -> Map(
        "uri"                 -> "mongodb://localhost:27017/test-cbcr-frontend",
        "session.expireAfter" -> "1 minute"
      )
    )
  )

  private val cache =
    new CBCSessionCache(config, mongoComponent, timestampSupport = new CurrentTimestampSupport)

  private val sessionId = SessionId(UUID.randomUUID().toString)
  private implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(sessionId))

  "CBCSessionCache" should {
    "save" should {
      "save session data and return CacheItem" in {
        val data = EnvelopeId("test id")

        val json = JsObject(Map("EnvelopeId" -> Json.toJson(data)))

        val result = await(cache.save[EnvelopeId](data))

        result.id shouldBe sessionId.value
        result.data shouldBe json
      }

      "return exception when there is no session id in header carrier" in {
        implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = None)

        val data = EnvelopeId("test id")

        cache.save[EnvelopeId](data).failed.futureValue shouldBe NoSessionException
      }
    }

    "create" should {
      "create session data and return OptionT" in {
        val data = EnvelopeId("test id")

        val result = await(cache.create[EnvelopeId](data).value)

        result shouldBe Some(data)
      }

      "return exception when there is no session id in header carrier" in {
        implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = None)

        val data = EnvelopeId("test id")

        cache.create[EnvelopeId](data).value.failed.futureValue shouldBe NoSessionException
      }
    }

    "read" should {
      "read session data" in {
        val data = EnvelopeId("test id")

        await(cache.save[EnvelopeId](data))

        val result = await(cache.read[EnvelopeId].value)

        result shouldBe Right(data)
      }

      "return Left(error) when error occurs reading data" in {
        val result = await(cache.read[EnvelopeId].value)

        result shouldBe Left(ExpiredSession("Unable to read EnvelopeId from cache"))
      }

      "return exception when there is no session id in header carrier" in {
        implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = None)

        cache.read[EnvelopeId].value.failed.futureValue shouldBe NoSessionException
      }
    }

    "readOption" should {
      "read session data and return Some(data)" in {
        val data = EnvelopeId("test id")

        await(cache.save[EnvelopeId](data))

        val result = await(cache.readOption[EnvelopeId])

        result shouldBe Some(data)
      }

      "return exception when there is no session id in header carrier" in {
        implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = None)

        cache.readOption[EnvelopeId].failed.futureValue shouldBe NoSessionException
      }
    }

    "clear" should {
      "clear session data and return true" in {
        val result = await(cache.clear)

        result shouldBe true
      }

      "return exception when there is no session id in header carrier" in {
        implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = None)

        cache.clear.failed.futureValue shouldBe NoSessionException
      }
    }
  }
}
