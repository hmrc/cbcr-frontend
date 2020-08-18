/*
 * Copyright 2020 HM Revenue & Customs
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
import play.api.data.{Form, FormError}
import play.api.data.Forms.{list, localDate, mapping, nonEmptyText, of, optional}
import play.api.data.format.{Formats, Formatter}
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.libs.json._
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.connectors.CBCRBackendConnector
import uk.gov.hmrc.cbcrfrontend.model.{DocRefId, ReportingEntityData}
import uk.gov.hmrc.cbcrfrontend.services.ReportingEntityDataService
import uk.gov.hmrc.cbcrfrontend.views.Views
import uk.gov.hmrc.cbcrfrontend.views.html._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

/** This case class is used to show all the DocRefIds stored in our mongo DB. The one we had in models had too many parameters and we only retrieve id and valid fields from DB which isn't enough to make the original
  *   case class.
  * */
case class AdminDocRefId(id: String)

object AdminDocRefId {

  implicit val format: Format[AdminDocRefId] = new Format[AdminDocRefId] {
    override def writes(o: AdminDocRefId): JsValue = JsString(o.id)

    override def reads(json: JsValue): JsResult[AdminDocRefId] =
      json
        .asOpt[JsString]
        .map(v => AdminDocRefId(v.value))
        .fold[JsResult[AdminDocRefId]](JsError(s"Unable to deserialise $json as a DocRefId"))(
          (id: AdminDocRefId) => JsSuccess(id)
        )
  }

  implicit val adminDocRefIdFormatter = new Formatter[AdminDocRefId] {
    override def bind(key: String, data: Map[String, String]): Either[Seq[FormError], AdminDocRefId] =
      Formats.stringFormat.bind(key, data).right.map(string => AdminDocRefId(string))

    override def unbind(key: String, value: AdminDocRefId): Map[String, String] = Map(key -> value.id)
  }
}

/** This case class is used to re validate Doc Ref Ids that has been invalidated by users.*/
case class AdminDocRefIdRecord(id: AdminDocRefId, valid: Boolean)

object AdminDocRefIdRecord {
  implicit val format: Format[AdminDocRefIdRecord] = Json.format[AdminDocRefIdRecord]
}

case class ListDocRefIdRecord(docs: List[AdminDocRefIdRecord])

object ListDocRefIdRecord {
  implicit val format: Format[ListDocRefIdRecord] = Json.format[ListDocRefIdRecord]
}

/** This case class is used when searching for reporting entity data using cbc Id date */
case class AdminCbcIdAndDate(cbcId: String, date: LocalDate)

/** This case class is used when searching for reporting entity data tin cbc Id date */
case class AdminTinAndDate(tin: String, date: LocalDate)

/** This case class is used when we manually need to the customers reporting entity and docref if and addtional info.*/
case class AdminReportingEntityDataRequestForm(
  selector: AdminDocRefId,
  cbcReportsDRI: List[AdminDocRefId],
  additionalInfoDRI: Option[List[AdminDocRefId]],
  reportingEntityDRI: AdminDocRefId)

object AdminReportingEntityDataRequestForm {
  implicit val format = Json.format[AdminReportingEntityDataRequestForm]
}

case class AdminReportingEntityData(
  cbcReportsDRI: List[AdminDocRefId],
  additionalInfoDRI: Option[List[AdminDocRefId]],
  reportingEntityDRI: AdminDocRefId)

object AdminReportingEntityData {
  implicit val format = Json.format[AdminReportingEntityData]
}

class AdminController @Inject()(
  frontendAppConfig: FrontendAppConfig,
  val config: Configuration,
  val audit: AuditConnector,
  cbcrBackendConnector: CBCRBackendConnector,
  views: Views)(
  implicit conf: FrontendAppConfig,
  override val messagesApi: MessagesApi,
  val ec: ExecutionContext,
  messagesControllerComponents: MessagesControllerComponents)
    extends FrontendController(messagesControllerComponents) with I18nSupport {

  implicit val hc: HeaderCarrier = HeaderCarrier()
  implicit val defaultParser = messagesControllerComponents.parsers.defaultBodyParser

  lazy val credentials = Creds(frontendAppConfig.username, frontendAppConfig.password)

  def removeControllerCharacters(string: String) =
    string.filter(_ >= ' ')

  val adminReportingEntityDataRequestForm = Form(
    mapping(
      "selector" -> of[AdminDocRefId],
      "cbcReportsDRI" -> nonEmptyText.transform[List[AdminDocRefId]](
        removeControllerCharacters(_).split(",").toList.map(elem => AdminDocRefId(elem)),
        l => l.map(_.id).mkString),
      "additionalInfoDRI" -> optional(nonEmptyText).transform[Option[List[AdminDocRefId]]](
        str => str.map(input => removeControllerCharacters(input).split(",").toList.map(AdminDocRefId(_))),
        list => list.map(l => l.map(_.id).mkString)),
      "reportingEntityDRI" -> of[AdminDocRefId]
    )(AdminReportingEntityDataRequestForm.apply)(AdminReportingEntityDataRequestForm.unapply)
  )

  val adminQueryDocRefIdForm: Form[AdminDocRefId] = Form(
    mapping(
      "id" -> nonEmptyText
    )(AdminDocRefId.apply)(AdminDocRefId.unapply)
  )

  val adminQueryWithCbcIdAndDate = Form(
    mapping(
      "cbcId" -> nonEmptyText,
      "date"  -> localDate
    )(AdminCbcIdAndDate.apply)(AdminCbcIdAndDate.unapply)
  )

  val adminQueryWithTinAndDate = Form(
    mapping(
      "tin"  -> nonEmptyText,
      "date" -> localDate
    )(AdminTinAndDate.apply)(AdminTinAndDate.unapply)
  )

  def showAdminPage: Action[AnyContent] = AuthenticationController(credentials).apply { implicit request =>
    Ok(views.tepmAdminPage())
  }

  def showAllDocRefIdOver200: Action[AnyContent] = AuthenticationController(credentials).async { implicit request =>
    cbcrBackendConnector.getDocRefIdOver200.map(
      documents => Ok(views.showAllDocRefIds(documents.docs))
    )
  }

  def showAmendDocRefIdPage = AuthenticationController(credentials).apply { implicit request =>
    Ok(views.adminDocRefIdEditor())
  }

  def showAddReportingEntityPage = AuthenticationController(credentials).async { implicit request =>
    Future.successful(Ok(views.addReportingEntityPage()))
  }

  def queryReportingEntityByDocRefId = AuthenticationController(credentials).async { implicit request =>
    adminQueryDocRefIdForm
      .bindFromRequest()
      .fold(
        errors => Future.successful(BadRequest("Error")),
        docRefId => cbcrBackendConnector.adminReportingEntityDataQuery(docRefId.id).map(doc => Ok(doc.json))
      )
      .recover {
        case _: Exception => Ok(s"couldnt find Reporting Entity")
      }
  }

  def queryReportingEntityByCbcIdAndDate = AuthenticationController(credentials).async { implicit request =>
    adminQueryWithCbcIdAndDate
      .bindFromRequest()
      .fold(
        errors => Future.successful(BadRequest("Error")),
        query =>
          cbcrBackendConnector
            .adminReportingEntityCBCIdAndReportingPeriod(query.cbcId, query.date)
            .map(doc => Ok(doc.json))
      )
      .recover {
        case _: Exception => Ok(s"couldnt find Reporting Entity")
      }
  }

  def queryReportingEntityByTinAndDate = AuthenticationController(credentials).async { implicit request =>
    adminQueryWithTinAndDate
      .bindFromRequest()
      .fold(
        errors => Future.successful(BadRequest("Error")),
        query =>
          cbcrBackendConnector
            .adminReportingEntityDataQueryTin(query.tin, query.date.toString)
            .map(doc => Ok(views.showReportingEntity(doc.json.validate[ReportingEntityData].get)))
      )
      .recover {
        case _: Exception => Ok(s"couldnt find Reporting Entity")
      }
  }

  def editDIR() = AuthenticationController(credentials).async { implicit result =>
    adminQueryDocRefIdForm
      .bindFromRequest()
      .fold(
        errors => Future.successful(BadRequest("Error")),
        docRefId => cbcrBackendConnector.adminEditDocRefId(docRefId.id).map(_ => Ok("Doc Ref Id has been Validated"))
      )
  }

  def showEditReportingEntityPage = AuthenticationController(credentials).async { implicit request =>
    Future.successful(Ok(views.adminEditReportingEntityData(adminReportingEntityDataRequestForm)))
  }

  def editReportingEntity = AuthenticationController(credentials).async { implicit request =>
    adminReportingEntityDataRequestForm
      .bindFromRequest()
      .fold(
        errors => Future.successful(BadRequest("Error")),
        editForm => {
          val adminReportingEntityData =
            AdminReportingEntityData(editForm.cbcReportsDRI, editForm.additionalInfoDRI, editForm.reportingEntityDRI)
          cbcrBackendConnector
            .editAdminReportingEntity(editForm.selector, adminReportingEntityData)
            .map(
              _ =>
                Ok(s"Replaced document with ${editForm.selector} field with ${Json.toJson(adminReportingEntityData)}")
            )
        }
      )
      .recover {
        case _: Exception => Ok(s"Couldn't edit ${views.adminEditReportingEntityData}")
      }
  }

  def adminAddDocRefId = AuthenticationController(credentials).async { implicit request =>
    adminQueryDocRefIdForm
      .bindFromRequest()
      .fold(
        errors => Future.successful(BadRequest("Error binding doc ref id from text box")),
        docRefId =>
          cbcrBackendConnector.adminSaveDocRefId(docRefId).map { res =>
            Ok(s"${docRefId.id} was added to Database")
        }
      )
  }

}
