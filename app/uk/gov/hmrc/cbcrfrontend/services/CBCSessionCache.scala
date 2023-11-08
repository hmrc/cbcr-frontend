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
import com.typesafe.config.Config
import configs.syntax._
import play.api.http.Status
import play.api.libs.json.{Format, Reads, Writes}
import play.api.{Configuration, Logger}
import uk.gov.hmrc.cbcrfrontend.model.ExpiredSession
import uk.gov.hmrc.http.cache.client.{CacheMap, SessionCache}
import uk.gov.hmrc.http.{HttpClient, _}

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.runtime.universe._
import scala.util.control.NonFatal

@Singleton
class CBCSessionCache @Inject()(val config: Configuration, val http: HttpClient)(implicit ec: ExecutionContext)
    extends SessionCache {

  val conf: Config = config.underlying.get[Config]("microservice.services.cachable.session-cache").value

  lazy val logger: Logger = Logger(this.getClass)

  override def defaultSource: String = "cbcr-frontend"

  override def baseUri: String =
    (for {
      protocol <- conf.get[String]("protocol")
      host     <- conf.get[String]("host")
      port     <- conf.get[Int]("port")
    } yield s"$protocol://$host:$port").value

  override def domain: String = conf.get[String]("domain").value

  def save[T: Writes: TypeTag](body: T)(implicit hc: HeaderCarrier): Future[CacheMap] =
    cache(stripPackage(typeOf[T].toString), body)

  def read[T: Reads: TypeTag](implicit hc: HeaderCarrier): EitherT[Future, ExpiredSession, T] =
    EitherT[Future, ExpiredSession, T](
      fetchAndGetEntry(stripPackage(typeOf[T].toString))
        .map(_.toRight(ExpiredSession(s"Unable to read ${typeOf[T]} from cache")))
    )

  def readOption[T: Reads: TypeTag](implicit hc: HeaderCarrier): Future[Option[T]] =
    fetchAndGetEntry(stripPackage(typeOf[T].toString))

  def create[T: Format: TypeTag](f: => OptionT[Future, T])(implicit hc: HeaderCarrier): OptionT[Future, T] =
    OptionT(f.semiflatMap { t =>
      save(t).map(_ => t)
    }.value)

  private def stripPackage(s: String): String = s.split('.').last

  def clear(implicit hc: HeaderCarrier): Future[Boolean] =
    super
      .remove()
      .map { response =>
        response.status match {
          case Status.OK         => true
          case Status.NO_CONTENT => true
          case _                 => false
        }
      }
      .recover {
        case NonFatal(t) =>
          logger.info(s"CBCSessionCache Failed - error message: ${t.getMessage}")
          false
      }
}
