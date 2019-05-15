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

package uk.gov.hmrc.cbcrfrontend.controllers

import javax.inject.Inject
import play.api.Configuration
import play.api.i18n.{I18nSupport, Lang, Messages, MessagesApi}
import play.api.libs.json._
import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.connectors.CBCRBackendConnector
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import uk.gov.hmrc.cbcrfrontend.views.html.{show_all_docRefIds, tepm_admin_page}

import scala.concurrent.ExecutionContext

class AdminController @Inject()(frontendAppConfig: FrontendAppConfig,
                                val config:Configuration,
                                val audit:AuditConnector,
                                cbcrBackendConnector: CBCRBackendConnector)
                               (implicit conf:FrontendAppConfig,
                                val messagesApi:MessagesApi,
                                val ec: ExecutionContext) extends FrontendController with I18nSupport {

  implicit val hc: HeaderCarrier = HeaderCarrier()

  lazy val credentials = Creds(frontendAppConfig.username, frontendAppConfig.password)


  def showAdminPage: Action[AnyContent] = AuthenticationController(credentials) {
    implicit request =>
      Ok(tepm_admin_page())
  }

  def showAllDocRefIdOver200: Action[AnyContent] = AuthenticationController(credentials).async {
    implicit request =>
      cbcrBackendConnector.getDocRefIdOver200.map(
        documents => Ok(show_all_docRefIds(documents.docs))
      )
  }

}


case class AdminDocRefId(id:String)
object AdminDocRefId {
  implicit val format: Format[AdminDocRefId] = new Format[AdminDocRefId] {
    override def writes(o: AdminDocRefId): JsValue = JsString(o.id)

    override def reads(json: JsValue): JsResult[AdminDocRefId] = json.asOpt[JsString].map(v => AdminDocRefId(v.value)).fold[JsResult[AdminDocRefId]](
      JsError(s"Unable to deserialise $json as a DocRefId"))(
      (id: AdminDocRefId) => JsSuccess(id)
    )
  }
}

case class AdminDocRefIdRecord(id:AdminDocRefId,valid:Boolean)
object AdminDocRefIdRecord {

implicit val format:Format[AdminDocRefIdRecord] = Json.format[AdminDocRefIdRecord]
}

case class ListDocRefIdRecord(docs: List[AdminDocRefIdRecord])
object ListDocRefIdRecord {
  implicit val format:Format[ListDocRefIdRecord] = Json.format[ListDocRefIdRecord]
}
