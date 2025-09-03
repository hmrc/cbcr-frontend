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

package uk.gov.hmrc.cbcrfrontend.repositories

import izumi.reflect.Tag
import cats.data.{EitherT, OptionT}
import cats.implicits.catsStdInstancesForFuture
import play.api.libs.json.{Reads, Writes}
import play.api.{Configuration, Logging}
import uk.gov.hmrc.cbcrfrontend.model.ExpiredSession
import uk.gov.hmrc.http._
import uk.gov.hmrc.mongo.cache.CacheIdType.SessionCacheId.NoSessionException
import uk.gov.hmrc.mongo.cache.{CacheIdType, CacheItem, DataKey, MongoCacheRepository}
import uk.gov.hmrc.mongo.{MongoComponent, TimestampSupport}
import uk.gov.hmrc.mdc.Mdc.preservingMdc

import javax.inject.{Inject, Singleton}
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class CBCSessionCache @Inject() (
  config: Configuration,
  mongoComponent: MongoComponent,
  timestampSupport: TimestampSupport
)(implicit ec: ExecutionContext)
    extends MongoCacheRepository(
      mongoComponent = mongoComponent,
      collectionName = "session-data",
      ttl = config.get[FiniteDuration]("mongodb.session.expireAfter"),
      timestampSupport = timestampSupport,
      cacheIdType = CacheIdType.SimpleCacheId
    ) with Logging {

  private def extractCacheId(implicit hc: HeaderCarrier): Future[String] =
    hc.sessionId.fold(Future.failed[String](NoSessionException))(c => Future.successful(c.value))

  private def extractType[T: Tag] = {
    val tag = summon[Tag[T]]
    tag.tag.shortName
  }

  def save[T: Writes: Tag](body: T)(implicit hc: HeaderCarrier): Future[CacheItem] =
    preservingMdc {
      for {
        cacheId <- extractCacheId
        result  <- put[T](cacheId)(DataKey[T](extractType[T]), body)
      } yield result
    }

  def read[T: Reads: Tag](implicit hc: HeaderCarrier): EitherT[Future, ExpiredSession, T] =
    EitherT.fromOptionF(readOption[T], ExpiredSession(s"Unable to read ${extractType[T]} from cache"))

  def readOption[T: Reads: Tag](implicit hc: HeaderCarrier): Future[Option[T]] =
    preservingMdc {
      for {
        cacheId <- extractCacheId
        result  <- get[T](cacheId)(DataKey[T](extractType[T]))
      } yield result
    }

  def create[T: Writes: Tag](data: T)(implicit hc: HeaderCarrier): OptionT[Future, T] =
    OptionT.liftF(save[T](data).map(_ => data))

  def clear(implicit hc: HeaderCarrier): Future[Boolean] =
    preservingMdc {
      for {
        cacheId <- extractCacheId
        _       <- deleteEntity(cacheId)
      } yield true
    }
}
