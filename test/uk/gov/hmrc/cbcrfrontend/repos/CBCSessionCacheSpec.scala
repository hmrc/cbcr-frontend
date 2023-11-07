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

package uk.gov.hmrc.cbcrfrontend.repos

import org.mockito.IdiomaticMockito
import org.scalatest.concurrent.Eventually
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Configuration
import play.api.test.Helpers._
import uk.gov.hmrc.cbcrfrontend.model.EnvelopeId
import uk.gov.hmrc.cbcrfrontend.services.CBCSessionCache
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.mongo.CurrentTimestampSupport
import uk.gov.hmrc.mongo.cache.CacheItem
import uk.gov.hmrc.mongo.test.DefaultPlayMongoRepositorySupport

import scala.concurrent.ExecutionContext.Implicits.global

class CBCSessionCacheSpec
    extends AnyWordSpec with Matchers with DefaultPlayMongoRepositorySupport[CacheItem] with Eventually
    with GuiceOneAppPerSuite with IdiomaticMockito {
  override val repository = new CBCSessionCache(mongoComponent, mock[Configuration], new CurrentTimestampSupport())
  implicit val hc: HeaderCarrier = HeaderCarrier()

  "SessionStoreImpl" must {
    "be able to insert data into mongo and read it back" in {
      val data = new EnvelopeId("some old data")
      val result = repository.save(data)

      await(result) should be(Right(()))

      eventually {
        val getResult = repository.get[EnvelopeId]
        await(getResult) should be(Right(Some(data)))
      }
    }

    "return no SessionData if there is no data in mongo" in {
      await(repository.get[EnvelopeId]) should be(Right(None))
    }

    "return an error" when {
      "there is no session id in the header carrier" in {
        implicit val hc: HeaderCarrier = HeaderCarrier()
        val sessionData = new EnvelopeId("other data")

        await(repository.save(sessionData)).isLeft shouldBe true
        await(repository.get[EnvelopeId]).isLeft shouldBe true
      }
    }
  }
}
