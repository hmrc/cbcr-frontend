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

  package uk.gov.hmrc.cbcrfrontend.connectors

  import play.api.Logger
  import play.api.http.Status._
  import play.api.libs.json.{JsValue, Json}
  import uk.gov.hmrc.cbcrfrontend.model.FindBusinessData
  import uk.gov.hmrc.cbcrfrontend.{FrontendGlobal, WSHttp}
  import uk.gov.hmrc.play.audit.AuditExtensions._
  import uk.gov.hmrc.play.audit.model.{Audit, DataEvent}
  import uk.gov.hmrc.play.config.ServicesConfig
  import uk.gov.hmrc.play.http.{HeaderCarrier, _}
  import uk.gov.hmrc.play.http.logging.Authorization

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.Future

  trait ETMPConnector extends ServicesConfig with RawResponseReads {

    def serviceUrl: String

    def orgLookupURI: String

    def urlHeaderEnvironment: String

    def urlHeaderAuthorization: String

    def http: HttpGet with HttpPost

    def audit = new Audit("known-fact-checking", FrontendGlobal.auditConnector)

    def lookup(lookupData: FindBusinessData, utr: String): Future[HttpResponse] = {
      implicit val hc: HeaderCarrier = createHeaderCarrier
      http.POST[JsValue, HttpResponse](s"$serviceUrl/$orgLookupURI/$utr", Json.toJson(lookupData)).map { response =>
        response.status match {
          case OK        =>
          case status    =>
            Logger.warn(s"[EtmpConnector][lookup] - status: $status")
            doFailedAudit("lookupFailed", lookupData.toString, response.body)
        }
        response
      }
    }

    def createHeaderCarrier: HeaderCarrier =
      HeaderCarrier(extraHeaders = Seq("Environment" -> urlHeaderEnvironment), authorization = Some(Authorization(urlHeaderAuthorization)))

    def doFailedAudit(auditType: String, request: String, response: String)(implicit hc:HeaderCarrier): Unit = {
      val auditDetails = Map("request" -> request,
        "response" -> response)

      audit.sendDataEvent(DataEvent("business-matching", auditType,
        tags = hc.toAuditTags("", "N/A"),
        detail = hc.toAuditDetails(auditDetails.toSeq: _*)))
    }
  }

  object ETMPConnector extends ETMPConnector {
    val serviceUrl: String = baseUrl("etmp-hod")
    val orgLookupURI: String = "registration/organisation"
    val urlHeaderEnvironment: String = config("etmp-hod").getString("environment").getOrElse("")
    val urlHeaderAuthorization: String = s"Bearer ${config("etmp-hod").getString("authorization-token").getOrElse("")}"
    val http = WSHttp
  }
