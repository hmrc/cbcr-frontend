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

import cats.data.{EitherT, OptionT}
import cats.instances.future._
import play.api.libs.json.{Format, Reads, Writes}
import play.api.{Configuration, Logging}
import uk.gov.hmrc.cbcrfrontend.model.ExpiredSession
import uk.gov.hmrc.http._
import uk.gov.hmrc.mongo.cache.CacheIdType.SessionCacheId.NoSessionException
import uk.gov.hmrc.mongo.cache.{CacheIdType, CacheItem, DataKey, MongoCacheRepository}
import uk.gov.hmrc.mongo.{MongoComponent, TimestampSupport}

import javax.inject.{Inject, Singleton}
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions
import scala.reflect.runtime.universe._

@Singleton
class CBCSessionCache @Inject()(
  config: Configuration,
  mongoComponent: MongoComponent,
  timestampSupport: TimestampSupport)(implicit ec: ExecutionContext)
    extends MongoCacheRepository(
      mongoComponent = mongoComponent,
      collectionName = "session-data",
      ttl = config.get[FiniteDuration]("mongodb.session.expireAfter"),
      timestampSupport = timestampSupport,
      cacheIdType = CacheIdType.SimpleCacheId
    ) with Logging {

  private val noSession = Future.failed[String](NoSessionException)

  private def extractCacheId(implicit hc: HeaderCarrier): Future[String] =
    hc.sessionId.fold(noSession)(c => Future.successful(c.value))

  private def extractType[T: TypeTag] = typeOf[T].typeSymbol.name.toString

  def save[T: Writes: TypeTag](body: T)(implicit hc: HeaderCarrier): Future[CacheItem] =
    for {
      cacheId <- extractCacheId
      result  <- put[T](cacheId)(DataKey[T](extractType), body)
    } yield result

  def read[T: Reads: TypeTag](implicit hc: HeaderCarrier): EitherT[Future, ExpiredSession, T] =
    EitherT[Future, ExpiredSession, T](
      readOption[T]
        .map(_.toRight(ExpiredSession(s"Unable to read $extractType from cache"))))

  def readOption[T: Reads: TypeTag](implicit hc: HeaderCarrier): Future[Option[T]] =
    for {
      cacheId <- extractCacheId
      result  <- get[T](cacheId)(DataKey[T](extractType))
    } yield result

  def create[T: Format: TypeTag](data: T)(implicit hc: HeaderCarrier): OptionT[Future, T] =
    OptionT.liftF(save[T](data).map(_ => data))

  def clear(implicit hc: HeaderCarrier): Future[Boolean] =
    for {
      cacheId <- extractCacheId
      result  <- deleteEntity(cacheId).map(_ => true)
    } yield result
}
