/*
 * Copyright 2018 HM Revenue & Customs
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

import javax.inject.{Inject, Singleton}

import cats.data.{EitherT, OptionT}
import cats.instances.future._
import com.typesafe.config.Config
import configs.syntax._
import play.api.Configuration
import play.api.libs.json.{Format, Reads, Writes}
import uk.gov.hmrc.cbcrfrontend.model.ExpiredSession
import uk.gov.hmrc.http._
import uk.gov.hmrc.http.cache.client.{CacheMap, SessionCache}
import uk.gov.hmrc.play.bootstrap.http.HttpClient

import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.runtime.universe._

@Singleton
class CBCSessionCache @Inject()(val config:Configuration, val http:HttpClient)(implicit ec: ExecutionContext) extends SessionCache {

  val conf: Config = config.underlying.get[Config]("microservice.services.cachable.session-cache").value

  override def defaultSource: String = "cbcr-frontend"

  override def baseUri: String = (for{
    protocol <- conf.get[String]("protocol")
    host     <- conf.get[String]("host")
    port     <- conf.get[Int]("port")
  } yield s"$protocol://$host:$port").value

  override def domain: String = conf.get[String]("domain").value

  def save[T:Writes:TypeTag](body:T)(implicit hc:HeaderCarrier): Future[CacheMap] =
    cache(stripPackage(typeOf[T].toString),body)

  def read[T:Reads:TypeTag](implicit hc:HeaderCarrier): EitherT[Future,ExpiredSession,T] = EitherT[Future,ExpiredSession,T](
    fetchAndGetEntry(stripPackage(typeOf[T].toString)).map(_.toRight(ExpiredSession(s"Unable to read ${typeOf[T]} from cache")))
  )

  def readOption[T:Reads:TypeTag](implicit hc:HeaderCarrier): Future[Option[T]] =
    fetchAndGetEntry(stripPackage(typeOf[T].toString))

  def readOrCreate[T:Format:TypeTag](f: => OptionT[Future,T])(implicit hc:HeaderCarrier) : OptionT[Future, T] =
    OptionT(readOption[T].flatMap(_.fold(
      f.semiflatMap{t => save(t).map(_ => t)}.value
    )(t => Future.successful(Some(t)))))

  def stripPackage(s:String) : String = s.split('.').last
}
