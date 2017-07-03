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
import play.api.libs.json._
import uk.gov.hmrc.cbcrfrontend.services.XmlErrorHandler


sealed trait CBCErrors extends Product with Serializable

object CBCErrors {
  implicit val show = Show.show[CBCErrors]{
    case UnexpectedState(errorMsg,_) => errorMsg
    case v:ValidationErrors          => v.show
    case InvalidSession              => InvalidSession.toString
  }
}

case class UnexpectedState(errorMsg: String, json: Option[JsValue] = None) extends CBCErrors
object UnexpectedState{
  implicit val invalidStateFormat: OFormat[UnexpectedState] = Json.format[UnexpectedState]
}

case object InvalidSession extends CBCErrors
sealed trait ValidationErrors extends CBCErrors

case class InvalidFileType(file:String) extends ValidationErrors
case class XMLErrors(errors:List[String]) extends ValidationErrors
case object FatalSchemaErrors extends ValidationErrors
sealed trait BusinessRuleErrors extends ValidationErrors

case object FileNameError extends BusinessRuleErrors
case object TestDataError extends BusinessRuleErrors
case object SendingEntityError extends BusinessRuleErrors
case object ReceivingCountryError extends BusinessRuleErrors
case object MessageTypeIndicError extends BusinessRuleErrors
case class InvalidXMLError(error:String) extends BusinessRuleErrors {
  override def toString: String = s"InvalidXMLError: $error"
}

case object CorrDocRefIdUnknownRecord extends BusinessRuleErrors
case object CorrDocRefIdInvalidRecord extends BusinessRuleErrors
case object DocRefIdDuplicate extends BusinessRuleErrors

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
    case x:BusinessRuleErrors    => x.show
    case FatalSchemaErrors       => "Fatal Schema Error"
    case InvalidFileType(f)      => s"File $f is an invalid file type"
    case AllBusinessRuleErrors(e)=> e.map(_.show).mkString(",")
  }
}
object BusinessRuleErrors {
  implicit val format = new Format[BusinessRuleErrors] {
    override def writes(o: BusinessRuleErrors): JsValue = o match {
      case m:MessageRefIDError   => Json.toJson[MessageRefIDError](m)(MessageRefIDError.format)
      case TestDataError         => JsString(TestDataError.toString)
      case SendingEntityError    => JsString(SendingEntityError.toString)
      case ReceivingCountryError => JsString(ReceivingCountryError.toString)
      case FileNameError         => JsString(FileNameError.toString)
      case MessageTypeIndicError => JsString(MessageTypeIndicError.toString)
      case m:InvalidXMLError     => JsString(m.toString)
    }

    override def reads(json: JsValue): JsResult[BusinessRuleErrors] =
      Json.fromJson[MessageRefIDError](json).orElse[BusinessRuleErrors]{
        json.asOpt[String].map(_.toLowerCase.trim) match {
          case Some("messagetypeindicerror") => JsSuccess(MessageTypeIndicError)
          case Some("filenameerror")         => JsSuccess(FileNameError)
          case Some("testdataerror")         => JsSuccess(TestDataError)
          case Some("sendingentityerror")    => JsSuccess(SendingEntityError)
          case Some("receivingcountryerror") => JsSuccess(ReceivingCountryError)
          case Some(otherError) if otherError.startsWith("invalidxmlerror: ") =>
            JsSuccess(InvalidXMLError(otherError.replaceAll("^invalidxmlerror: ", "")))
          case other                         => JsError(s"Unable to serialise $other to a BusinessRuleError")
        }
      }
  }
  implicit val eShows: Show[BusinessRuleErrors] =  Show.show[BusinessRuleErrors]{
    case m:MessageRefIDError => m.show
    case TestDataError         => "ErrorCode: 50010 - The referenced file contains one or more records with a DocTypeIndic value in the range OECD11OECD13, indicating test data. As a result, the receiving Competent Authority cannot accept this file as a valid CbC file submission."
    case SendingEntityError    => "The SendingEntityIN field must match your CBCId"
    case ReceivingCountryError => """The ReceivingCountry field must equal "GB""""
    case FileNameError         => "MessageRefID must match filename"
    case MessageTypeIndicError => "Error DocTypeIndic (Correction): If MessageTypeIndic is provided and completed with \"CBC402\" message can only contain DocTypeIndic \"OECD2\" or \"OECD3\". (With 1 execption ReportingEntity can contain DocTypeIndic \"OECD0\" where ReportingEntity information is unchanged. \"OECD0\" cannot be used in DocSpec\\DocTypeIndic for CbCReports or AdditionalInfo)"
    case i:InvalidXMLError     => i.toString
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
  def errorHandlerToXmlErrors(x:XmlErrorHandler) : XMLErrors = XMLErrors(x.fatalErrorsCollection ++ x.errorsCollection ++ x.warningsCollection)
  implicit val xmlShows: Show[XMLErrors] = Show.show[XMLErrors](e =>
    "ErrorCode: 50007 - The referenced file failed validation against the CbC XML Schema\n\n" + e.errors.mkString("\n")
  )
}

case class AllBusinessRuleErrors(errors:List[BusinessRuleErrors]) extends ValidationErrors
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


