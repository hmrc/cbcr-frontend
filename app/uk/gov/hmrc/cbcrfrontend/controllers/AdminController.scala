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

import java.time.LocalDate

import javax.inject.Inject
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms.{localDate, mapping, nonEmptyText}
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.libs.json._
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.connectors.CBCRBackendConnector
import uk.gov.hmrc.cbcrfrontend.model.ReportingEntityData
import uk.gov.hmrc.cbcrfrontend.services.ReportingEntityDataService
import uk.gov.hmrc.cbcrfrontend.views.html._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

class AdminController @Inject()(frontendAppConfig: FrontendAppConfig,
                                val config:Configuration,
                                val audit:AuditConnector,
                                cbcrBackendConnector: CBCRBackendConnector,
                                reportingEntityDataService: ReportingEntityDataService)
                               (implicit conf:FrontendAppConfig,
                                override val messagesApi:MessagesApi,
                                val ec: ExecutionContext,
                                messagesControllerComponents: MessagesControllerComponents) extends FrontendController(messagesControllerComponents) with I18nSupport {

  implicit val hc: HeaderCarrier = HeaderCarrier()
  implicit val defaultParser = messagesControllerComponents.parsers.defaultBodyParser

  lazy val credentials = Creds(frontendAppConfig.username, frontendAppConfig.password)

  case class AdminDocRefId(id:String)
  case class AdminCbcIdAndDate(cbcId:String, date: LocalDate)
  case class AdminTinAndDate(tin:String, date: LocalDate)

  val adminQueryDocRefIdForm: Form[AdminDocRefId] = Form(
    mapping(
      "id" -> nonEmptyText
    )(AdminDocRefId.apply)(AdminDocRefId.unapply)
  )

  val adminQueryWithCbcIdAndDate = Form(
    mapping(
      "cbcId" -> nonEmptyText,
      "date" -> localDate
    )(AdminCbcIdAndDate.apply)(AdminCbcIdAndDate.unapply)
  )

  val adminQueryWithTinAndDate = Form(
    mapping(
      "tin" -> nonEmptyText,
      "date" -> localDate
    )(AdminTinAndDate.apply)(AdminTinAndDate.unapply)
  )


  def showAdminPage: Action[AnyContent] = AuthenticationController(credentials).apply {
    implicit request =>
      Ok(tepm_admin_page())
  }

  def showAllDocRefIdOver200: Action[AnyContent] = AuthenticationController(credentials).async {
    implicit request =>
      cbcrBackendConnector.getDocRefIdOver200.map(
        documents => Ok(show_all_docRefIds(documents.docs))
  )
}

  def showAmendDocRefIdPage = AuthenticationController(credentials).apply {
    implicit request =>
      Ok(admin_docRefId_Editor())
  }

  def showAddReportingEntityPage = AuthenticationController(credentials).async {
    implicit request =>
      Future.successful(Ok(addReportingEntityPage()))
  }

  def queryReportingEntityByDocRefId = AuthenticationController(credentials).async {
    implicit request =>
      adminQueryDocRefIdForm.bindFromRequest().fold(
        errors => Future.successful(BadRequest("Error")),
        docRefId =>
          cbcrBackendConnector.adminReportingEntityDataQuery(docRefId.id).map(doc =>
            Ok(showReportingEntity(doc.json.validate[ReportingEntityData].get)))
      )
  }


  def queryReportingEntityByCbcIdAndDate = AuthenticationController(credentials).async {
    implicit request =>
      adminQueryWithCbcIdAndDate.bindFromRequest().fold(
        errors => Future.successful(BadRequest("Error")),
        query =>
          cbcrBackendConnector.adminReportingEntityCBCIdAndReportingPeriod(query.cbcId, query.date).map(doc =>
            Ok(showReportingEntity(doc.json.validate[ReportingEntityData].get)))
      )
  }

  def queryReportingEntityByTinAndDate = AuthenticationController(credentials).async {
    implicit request =>
      adminQueryWithTinAndDate.bindFromRequest().fold(
        errors => Future.successful(BadRequest("Error")),
        query =>
          cbcrBackendConnector.adminReportingEntityDataQueryTin(query.tin, query.date.toString).map(doc =>
            Ok(showReportingEntity(doc.json.validate[ReportingEntityData].get)))
      )
  }

  def editDIR() = AuthenticationController(credentials).async {
    implicit result =>
      adminQueryDocRefIdForm.bindFromRequest().fold(
        errors => Future.successful(BadRequest("Error")),
        docRefId =>
          cbcrBackendConnector.adminEditDocRefId(docRefId.id).map(_ =>
            Ok("Doc Ref Id has been Validated"))
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

