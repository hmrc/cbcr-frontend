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

import play.api.libs.json.{Reads, Writes}
import play.api.{Configuration, Logging}
import uk.gov.hmrc.cbcrfrontend.model.{CBCErrors, InvalidSession}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.mongo.cache.{CacheIdType, DataKey, MongoCacheRepository}
import uk.gov.hmrc.mongo.{MongoComponent, TimestampSupport}

import javax.inject.{Inject, Singleton}
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.runtime.universe._
import scala.util.control.NonFatal

@Singleton
class CBCSessionCache @Inject()(
  mongoComponent: MongoComponent,
  configuration: Configuration,
  timestampSupport: TimestampSupport
)(implicit ec: ExecutionContext)
    extends MongoCacheRepository(
      mongoComponent = mongoComponent,
      collectionName = "cbcr-frontend",
      ttl = configuration.get[FiniteDuration]("cache.expiry"),
      timestampSupport = timestampSupport,
      cacheIdType = CacheIdType.SimpleCacheId
    ) with Logging {

  def save[T: Writes: TypeTag](body: T)(implicit hc: HeaderCarrier): Future[Either[CBCErrors, Unit]] =
    put(hc.sessionId.get.toString)(DataKey("cbcr-session"), body).map(_ => Right())

  def get[T: Reads: TypeTag](implicit hc: HeaderCarrier): Future[Either[CBCErrors, Option[T]]] =
    hc.sessionId.map(_.value) match {
      case Some(sessionId) =>
        get[T](sessionId)(DataKey("cbcr-session")).map(maybe => Right(maybe))
      case None => Future.successful(Left(InvalidSession))
    }

  def clear(implicit hc: HeaderCarrier): Future[Boolean] =
    hc.sessionId match {
      case Some(s) =>
        deleteEntity(s.value)
          .map { _ =>
            true
          }
          .recover {
            case NonFatal(t) =>
              logger.info(s"CBCSessionCache Failed - error message: ${t.getMessage}")
              false
          }
      case None => Future.successful(false)
    }
}
