package uk.gov.hmrc.cbcrfrontend.connectors

import javax.inject.Inject

import com.typesafe.config.Config
import play.api.Configuration
import uk.gov.hmrc.play.http.{HttpPost, HttpResponse}

import scala.concurrent.{ExecutionContext, Future}
import configs.syntax._
import play.api.libs.json.Json

/**
  * Created by max on 23/05/17.
  */
@Singleton
class TaxEnrolmentsConnector @Inject() (httpPost: HttpPost, config:Configuration)(implicit ec:ExecutionContext) {

  val conf = config.underlying.get[Config]("microservice.services.tax-enrolments").value

  val url: String = (for {
    host    <- conf.get[String]("host")
    port    <- conf.get[Int]("port")
    service <- conf.get[String]("url")
  } yield s"http://$host:$port/$service").value


  def deEnrol: Future[HttpResponse] =
    httpPost.POST(url + "/de-enrol/HMRC-CBC-ORG",Json.obj("keepAgentAllocations" -> false))

}
