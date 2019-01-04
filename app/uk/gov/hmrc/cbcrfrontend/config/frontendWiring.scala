/*
 * Copyright 2019 HM Revenue & Customs
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

import akka.stream.scaladsl.Source
import akka.util.ByteString
import play.api.http.HttpVerbs.{POST => POST_VERB}
import play.api.mvc.MultipartFormData.FilePart
import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._
import uk.gov.hmrc.play.http.ws.{WSPost, _}

import scala.concurrent.Future
import uk.gov.hmrc.http._
import uk.gov.hmrc.http.hooks.HttpHook

object FileUploadFrontEndWS extends HttpPost with WSPost {

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
      mapErrors(POST_VERB, url, httpResponse).map(rds.read(POST_VERB, url, _))
    }
  }


  override val hooks: Seq[HttpHook] = Seq.empty[HttpHook]

}