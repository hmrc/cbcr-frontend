/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.cbcrfrontend

import javax.inject.Singleton

import akka.actor.ActorSystem
import akka.stream.scaladsl.Source
import akka.stream.{ActorMaterializer, Materializer}
import akka.util.ByteString
import play.api.Logger
import play.api.http.HttpVerbs.{POST => POST_VERB}
import play.api.mvc.MultipartFormData.FilePart
import play.api.mvc.Results.{Forbidden, NotImplemented, Redirect}
import play.api.mvc.{Call, Filter, RequestHeader, Result}
import uk.gov.hmrc.play.audit.http.config.LoadAuditingConfig
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector => Auditing}
import uk.gov.hmrc.play.config.{AppName, RunMode, ServicesConfig}
import uk.gov.hmrc.play.frontend.auth.connectors.AuthConnector
import uk.gov.hmrc.play.http.hooks.HttpHook
import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._
import uk.gov.hmrc.play.http.ws.{WSDelete, WSGet, WSPost, WSPut, _}
import uk.gov.hmrc.play.http.{HeaderCarrier, HttpReads, HttpResponse}

import scala.concurrent.Future


object FrontendAuditConnector extends Auditing with AppName {
  override lazy val auditingConfig = LoadAuditingConfig(s"auditing")
}

object WSHttp extends WSGet with WSPut with WSPost with WSDelete with AppName with RunMode {
  override val hooks = NoneRequired
}

@Singleton
class FrontendAuthConnector extends AuthConnector with ServicesConfig {
  lazy val serviceUrl = baseUrl("auth")
  lazy val http = WSHttp
}

object FileUploadFrontEndWS extends WSPost with AppName {

  def doFormPartPost(
                      url: String,
                      fileName: String,
                      contentType: String,
                      body: ByteString,
                      headers: Seq[(String, String)]
                    )(
                      implicit
                      hc: HeaderCarrier,
                      rds: HttpReads[HttpResponse]
                    ): Future[HttpResponse] = {
    val source = Source(FilePart(fileName, fileName, Some(contentType), Source.single(body)) :: Nil)

    withTracing(POST_VERB, url) {
      val httpResponse = buildRequest(url).withHeaders(headers: _*).post(source).map(new WSHttpResponse(_))
      //executeHooks(url, POST_VERB, Option(Json.stringify(wts.writes(body))), httpResponse)
      mapErrors(POST_VERB, url, httpResponse).map(rds.read(POST_VERB, url, _))
    }
  }

  override val hooks: Seq[HttpHook] = Seq.empty[HttpHook]

}

object WhitelistFilter extends Filter
  with RunMode {

  implicit val system = ActorSystem("wlf")

  implicit def mat: Materializer = ActorMaterializer()

  def whitelist: Seq[String] = FrontendAppConfig.whitelist


  def excludedPaths: Seq[Call] = {
    FrontendAppConfig.whitelistExcluded.map { path =>
      Call("GET", path)
    }
  }

  val trueClient = "True-Client-IP"

  private def toCall(rh: RequestHeader): Call = Call(rh.method, rh.uri)

  def apply
  (f: (RequestHeader) => Future[Result])
  (rh: RequestHeader): Future[Result] =
    if (excludedPaths contains toCall(rh)) {
      f(rh)
    } else {
      rh.headers.get(trueClient) map {
        ip =>
          if (whitelist.contains(ip)) {
            Logger.debug(s"Whitelist allowing request ${rh.method} ${rh.uri} from ${ip}")
            f(rh)
          }
          else {
            Logger.warn(s"Request for ${rh.method} ${rh.uri} was blocked by Whitelist from ${ip}")
            Future.successful(NotImplemented)
          }
      } getOrElse Future.successful({
        Logger.warn(s"No ${trueClient} http header found, request for ${rh.method} ${rh.uri} was blocked by Whitelist")
        NotImplemented
      })
    }

}