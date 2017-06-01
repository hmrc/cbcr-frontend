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

package uk.gov.hmrc.cbcrfrontend.model
import cats.Show
import cats.syntax.show._
import play.api.Logger
import play.api.libs.json._
import uk.gov.hmrc.cbcrfrontend.services.XmlErrorHandler

sealed trait ValidationErrors
case class XMLErrors(errors:List[String]) extends ValidationErrors
sealed trait BusinessRuleErrors extends ValidationErrors

sealed trait MessageRefIDError extends BusinessRuleErrors
case object MessageRefIDMissing extends MessageRefIDError
case object MessageRefIDFormatError extends  MessageRefIDError
case object MessageRefIDDuplicate extends MessageRefIDError
case object MessageRefIDCBCIdMismatch extends MessageRefIDError
case object MessageRefIDReportingPeriodMismatch extends MessageRefIDError
case object MessageRefIDTimestampError extends MessageRefIDError

object ValidationErrors {
  implicit val validationErrorShows: Show[ValidationErrors] = Show.show[ValidationErrors]{
    case x:XMLErrors             => x.show
    case x:BusinessRuleErrors => x.show
  }
}
object BusinessRuleErrors {
  implicit val format = new Format[BusinessRuleErrors] {
    override def writes(o: BusinessRuleErrors): JsValue = o match {
      case m:MessageRefIDError => Json.toJson[MessageRefIDError](m)(MessageRefIDError.format)
    }

    override def reads(json: JsValue): JsResult[BusinessRuleErrors] = Json.fromJson[MessageRefIDError](json)
  }
  implicit val eShows: Show[BusinessRuleErrors] =  Show.show[BusinessRuleErrors]{
    case m:MessageRefIDError => m.show
  }
}

object MessageRefIDError {
  implicit val format = new Format[MessageRefIDError] {
    override def writes(o: MessageRefIDError): JsValue = JsString(o.toString)

    override def reads(json: JsValue): JsResult[MessageRefIDError] = json.asOpt[String].map(_.toLowerCase.trim) match {
      case Some("messagerefidmissing") => JsSuccess(MessageRefIDMissing)
      case Some("messagerefidcbcidmismatch") => JsSuccess(MessageRefIDCBCIdMismatch)
      case Some("messagerefidformaterror") => JsSuccess(MessageRefIDFormatError)
      case Some("messagerefidduplicate") => JsSuccess(MessageRefIDDuplicate)
      case Some("messagerefidreportingperiodmismatch") => JsSuccess(MessageRefIDReportingPeriodMismatch)
      case Some("messagerefidtimestamperror") => JsSuccess(MessageRefIDTimestampError)
      case other => JsError(s"Unable to serialise $other to a MessageRefIDError")
    }
  }

  implicit val idMissingShows: Show[MessageRefIDError] = Show.show[MessageRefIDError] {
    case MessageRefIDMissing                 => "ErrorCode: 70000 - Message Reference must be completed"
    case MessageRefIDCBCIdMismatch           => "The CbC ID within the MessageRefId does not match the CbC ID in the SendingEntityIN field"
    case MessageRefIDFormatError             => "MessageRefID must match defined format"
    case MessageRefIDDuplicate               => "ErrorCode: 50009 - The referenced file has a duplicate MessageRefID value that was received on a previous file"
    case MessageRefIDReportingPeriodMismatch => "The ReportingPeriod element of the MessageRefId does not match the Year of the Reporting Period field in the XML"
    case MessageRefIDTimestampError          => "The XML Creation Time stamp element is not a valid UTC date time"
  }
}

object XMLErrors {
  implicit val format = Json.format[XMLErrors]
  def errorHandlerToXmlErrors(x:XmlErrorHandler) : XMLErrors = XMLErrors(x.errorsCollection ++ x.warningsCollection)
  implicit val xmlShows: Show[XMLErrors] = Show.show[XMLErrors](e => "ErrorCode: 50007 - The referenced file failed validation against the CbC XML Schema\n\n" + e.errors.mkString("\n"))
}

case class AllBusinessRuleErrors(errors:List[BusinessRuleErrors])
object AllBusinessRuleErrors {
  implicit val format = new Format[AllBusinessRuleErrors] {
    override def writes(o: AllBusinessRuleErrors): JsValue = Json.obj("AllBusinessRuleErrors" -> o.errors)

    override def reads(json: JsValue): JsResult[AllBusinessRuleErrors] = (for {
      obj <- json.asOpt[JsObject]
      bre <- obj.value.get("AllBusinessRuleErrors")
      a   <- bre.asOpt[List[BusinessRuleErrors]]
    } yield AllBusinessRuleErrors(a)).fold[JsResult[AllBusinessRuleErrors]](
      JsError(s"Unable to serialise BusinessRuleErrors: $json")
    )(bre => JsSuccess(bre))
  }
}


