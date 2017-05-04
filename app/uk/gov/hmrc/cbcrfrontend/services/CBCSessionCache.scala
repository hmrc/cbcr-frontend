package uk.gov.hmrc.cbcrfrontend.services

import javax.inject.{Inject, Singleton}

import com.typesafe.config.Config
import configs.syntax._
import play.api.Configuration
import uk.gov.hmrc.http.cache.client.SessionCache
import uk.gov.hmrc.play.http.{HttpDelete, HttpGet, HttpPut}

@Singleton
class CBCSessionCache @Inject() (val config:Configuration, val http:HttpGet with HttpPut with HttpDelete) extends SessionCache{

  val conf: Config = config.underlying.get[Config]("microservice.services.cachable.session-cache").value

  override def defaultSource: String = "cbcr-frontend"

  override def baseUri: String = (for{
    protocol <- conf.get[String]("protocol")
    host     <- conf.get[String]("host")
    port     <- conf.get[Int]("port")
  }yield s"$protocol://$host:$port").value

  override def domain: String = conf.get[String]("domain").value

}
