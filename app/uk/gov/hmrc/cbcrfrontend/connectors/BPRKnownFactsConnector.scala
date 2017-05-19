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

import java.net.URL
import javax.inject.{Inject, Singleton}

import uk.gov.hmrc.cbcrfrontend.typesclasses.{CbcrsUrl, ServiceUrl}
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.http.{HeaderCarrier, HttpGet, HttpResponse, NotFoundException}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

@Singleton
class BPRKnownFactsConnector @Inject()(http:HttpGet)(implicit ec:ExecutionContext) extends ServicesConfig{

  implicit lazy val url = new ServiceUrl[CbcrsUrl] { val url = baseUrl("cbcr")}

  def lookup(utr:String)(implicit hc:HeaderCarrier) : Future[HttpResponse] = http.GET(generateUrl(url.url,utr).toString)

  private def generateUrl(baseUrl:String,utr:String) : URL = new URL(s"$baseUrl/cbcr/getBusinessPartnerRecord/$utr")

}
