/*
 * Copyright 2018 HM Revenue & Customs
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
import cats.kernel.Semigroup
import cats.syntax.show._
import play.api.Logger
import play.api.libs.json._
import uk.gov.hmrc.cbcrfrontend.services.XmlErrorHandler


sealed trait CBCErrors extends Product with Serializable

object CBCErrors {

  implicit val show = Show.show[CBCErrors]{
    case UnexpectedState(errorMsg,_) => errorMsg
    case v:ValidationErrors          => v.show
    case InvalidSession              => InvalidSession.toString
    case ExpiredSession(msg)         => msg
  }
}

case class UnexpectedState(errorMsg: String, json: Option[JsValue] = None) extends CBCErrors
object UnexpectedState{
  implicit val invalidStateFormat: OFormat[UnexpectedState] = Json.format[UnexpectedState]
}

case class ExpiredSession(msg:String) extends CBCErrors
case object InvalidSession extends CBCErrors
sealed trait ValidationErrors extends CBCErrors

case class InvalidFileType(file:String) extends ValidationErrors
case class XMLErrors(errors:List[String]) extends ValidationErrors
case class FatalSchemaErrors(size:Option[Int]) extends ValidationErrors
sealed trait BusinessRuleErrors extends ValidationErrors


case object OriginalSubmissionNotFound extends BusinessRuleErrors

case class FileNameError(foundName:String, expectedName:String) extends BusinessRuleErrors
object FileNameError {
  implicit val format = Json.format[FileNameError]
}

case object TestDataError extends BusinessRuleErrors
case object SendingEntityError extends BusinessRuleErrors
case object SendingEntityOrganisationMatchError extends BusinessRuleErrors
case object ReceivingCountryError extends BusinessRuleErrors
case object MessageTypeIndicError extends BusinessRuleErrors
case class InvalidXMLError(error:String) extends BusinessRuleErrors {
  override def toString: String = s"InvalidXMLError: $error"
}
case object MessageTypeIndicDocTypeIncompatible extends BusinessRuleErrors
case object IncompatibleOECDTypes extends BusinessRuleErrors
case object CorrDocRefIdMissing extends BusinessRuleErrors
case object CorrDocRefIdNotNeeded extends BusinessRuleErrors
case object CorrDocRefIdUnknownRecord extends BusinessRuleErrors
case object CorrDocRefIdInvalidRecord extends BusinessRuleErrors
case object CorrDocRefIdDuplicate extends BusinessRuleErrors
case object DocRefIdDuplicate extends BusinessRuleErrors
case object DocRefIdInvalidParentGroupElement extends BusinessRuleErrors
case object CorrDocRefIdInvalidParentGroupElement extends BusinessRuleErrors
case object InvalidDocRefId extends BusinessRuleErrors
case object InvalidCorrDocRefId extends BusinessRuleErrors
case object ResentDataIsUnknownError extends BusinessRuleErrors
case object CorrectedFileToOld extends BusinessRuleErrors

case object CbcOecdVersionError extends BusinessRuleErrors
case object XmlEncodingError extends BusinessRuleErrors
case object PrivateBetaCBCIdError extends BusinessRuleErrors

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
    case FatalSchemaErrors(_)    => "Fatal Schema Error"
    case InvalidFileType(f)      => s"File $f is an invalid file type"
    case AllBusinessRuleErrors(e)=> e.map(_.show).mkString(",")
  }
}
object BusinessRuleErrors {
  implicit val format = new Format[BusinessRuleErrors] {
    override def writes(o: BusinessRuleErrors): JsValue = o match {
      case m:MessageRefIDError       => Json.toJson[MessageRefIDError](m)(MessageRefIDError.format)
      case TestDataError             => JsString(TestDataError.toString)
      case SendingEntityError        => JsString(SendingEntityError.toString)
      case SendingEntityOrganisationMatchError        => JsString(SendingEntityOrganisationMatchError.toString)
      case ReceivingCountryError     => JsString(ReceivingCountryError.toString)
      case fne:FileNameError         => Json.toJson(fne)
      case MessageTypeIndicError     => JsString(MessageTypeIndicError.toString)
      case m:InvalidXMLError         => JsString(m.toString)
      case InvalidDocRefId           => JsString(InvalidDocRefId.toString)
      case InvalidCorrDocRefId       => JsString(InvalidCorrDocRefId.toString)
      case CorrDocRefIdInvalidRecord => JsString(CorrDocRefIdInvalidRecord.toString)
      case CorrDocRefIdUnknownRecord => JsString(CorrDocRefIdUnknownRecord.toString)
      case DocRefIdDuplicate         => JsString(DocRefIdDuplicate.toString)
      case DocRefIdInvalidParentGroupElement => JsString(DocRefIdInvalidParentGroupElement.toString)
      case CorrDocRefIdInvalidParentGroupElement => JsString(CorrDocRefIdInvalidParentGroupElement.toString)
      case CorrDocRefIdMissing       => JsString(CorrDocRefIdMissing.toString)
      case CorrDocRefIdNotNeeded     => JsString(CorrDocRefIdNotNeeded.toString)
      case CorrDocRefIdDuplicate     => JsString(CorrDocRefIdDuplicate.toString)
      case IncompatibleOECDTypes     => JsString(IncompatibleOECDTypes.toString)
      case MessageTypeIndicDocTypeIncompatible => JsString(MessageTypeIndicDocTypeIncompatible.toString)
      case CbcOecdVersionError       => JsString(CbcOecdVersionError.toString)
      case XmlEncodingError           => JsString(XmlEncodingError.toString)
      case OriginalSubmissionNotFound => JsString(OriginalSubmissionNotFound.toString)
      case PrivateBetaCBCIdError     => JsString(PrivateBetaCBCIdError.toString)
      case ResentDataIsUnknownError  => JsString(ResentDataIsUnknownError.toString)
      case CorrectedFileToOld        => JsString(CorrectedFileToOld.toString)
    }

    override def reads(json: JsValue): JsResult[BusinessRuleErrors] =
      Json.fromJson[MessageRefIDError](json)
        .orElse[BusinessRuleErrors](Json.fromJson(json)(FileNameError.format))
        .orElse[BusinessRuleErrors]{
        json.asOpt[String].map(_.toLowerCase.trim) match {
          case Some("messagetypeindicerror") => JsSuccess(MessageTypeIndicError)
          case Some("testdataerror")         => JsSuccess(TestDataError)
          case Some("sendingentityerror")    => JsSuccess(SendingEntityError)
          case Some("sendingentityorganisationmatcherror") =>JsSuccess(SendingEntityOrganisationMatchError)
          case Some("receivingcountryerror") => JsSuccess(ReceivingCountryError)
          case Some("invaliddocrefid")       => JsSuccess(InvalidDocRefId)
          case Some("invalidcorrdocrefid")       => JsSuccess(InvalidCorrDocRefId)
          case Some("corrdocrefidinvalidrecord") => JsSuccess(CorrDocRefIdInvalidRecord)
          case Some("corrdocrefidunknownrecord") => JsSuccess(CorrDocRefIdUnknownRecord)
          case Some("corrdocrefidduplicate")     => JsSuccess(CorrDocRefIdDuplicate)
          case Some("docrefidduplicate")         => JsSuccess(DocRefIdDuplicate)
          case Some("docrefidinvalidparentgroupelement") => JsSuccess(DocRefIdInvalidParentGroupElement)
          case Some("corrdocrefidinvalidparentgroupelement") => JsSuccess(CorrDocRefIdInvalidParentGroupElement)
          case Some("corrdocrefidmissing")       => JsSuccess(CorrDocRefIdMissing)
          case Some("corrdocrefidnotneeded")     => JsSuccess(CorrDocRefIdNotNeeded)
          case Some("incompatibleoecdtypes")     => JsSuccess(IncompatibleOECDTypes)
          case Some("messagetypeindicdoctypeincompatible") => JsSuccess(MessageTypeIndicDocTypeIncompatible)
          case Some("cbcoecdversionerror")    => JsSuccess(CbcOecdVersionError)
          case Some("xmlencodingerror")       => JsSuccess(XmlEncodingError)
          case Some("originalsubmissionnotfound") => JsSuccess(OriginalSubmissionNotFound)
          case Some("privatebetacbciderror")      => JsSuccess(PrivateBetaCBCIdError)
          case Some("resentdataisunknownerror")   => JsSuccess(ResentDataIsUnknownError)
          case Some("correctedfiletoold")         => JsSuccess(CorrectedFileToOld)
          case Some(otherError) if otherError.startsWith("invalidxmlerror: ") =>
            JsSuccess(InvalidXMLError(otherError.replaceAll("^invalidxmlerror: ", "")))
          case other                         => JsError(s"Unable to serialise $other to a BusinessRuleError")
        }
      }
  }
  implicit val eShows: Show[BusinessRuleErrors] =  Show.show[BusinessRuleErrors]{
    case m:MessageRefIDError => m.show
    case TestDataError         => "ErrorCode: 50010 - The referenced file contains one or more records with a DocTypeIndic value in the range OECD11OECD13, indicating test data. As a result, the receiving Competent Authority cannot accept this file as a valid CbC file submission."
    case SendingEntityError    => "The CBCId in the SendingEntityIN field has not been registered"
    case SendingEntityOrganisationMatchError => "The CBCId in the SendingEntityIN does not match that of the Organisation"
    case ReceivingCountryError => """The ReceivingCountry field must equal "GB""""
    case FileNameError(f,e)         => s"The filename and messageRefID should match." +"\r\n" + s"You entered this filename: $f" +"\r\n" + s"It should read: $e"
    case MessageTypeIndicError => "Error DocTypeIndic (Correction): If MessageTypeIndic is provided and completed with \"CBC402\" message can only contain DocTypeIndic \"OECD2\" or \"OECD3\". (With 1 execption ReportingEntity can contain DocTypeIndic \"OECD0\" where ReportingEntity information is unchanged. \"OECD0\" cannot be used in DocSpec\\DocTypeIndic for CbCReports or AdditionalInfo)"
    case CorrDocRefIdInvalidRecord => "Error Code 80003 CorrDocRefId (record no longer valid): The corrected record is no longer valid (invalidated or outdated by a previous correction message). As a consequence, no further information should have been received on this version of the record."
    case CorrDocRefIdUnknownRecord => "Error Code 80002 CorrDocRefId (unknown record): The CorrDocRefId refers to an unknown record"
    case DocRefIdDuplicate         => "Error Code 80001: DocRefId (already used)"
    case DocRefIdInvalidParentGroupElement => "Error Code 80000 DocRefId (format): The structure of the DocRefID is not in the correct format, as set out in the User Guide."
    case CorrDocRefIdInvalidParentGroupElement => "Error Code 80000 CorrDocRefId (format): The structure of the CorrDocRefId is not in the correct format, as set out in the User Guide."
    case CorrDocRefIdMissing   => "Error Code 80005 CorrDocRefId (missing): CorrDocRefId must be provided when DocTypeIndic is OECD2 or OECD3"
    case CorrDocRefIdNotNeeded => "Error Code 80004 CorrDocRefId (Initial record): CorrDocRefId cannot be provided when DocTypeIndic is OECD1"
    case IncompatibleOECDTypes => "Error DocTypeIndic: Document must not contain a mixture of New (OECD1) and corrected (OECD2 & OECD3) DocTypeIndics"
    case MessageTypeIndicDocTypeIncompatible => "Error DocTypeIndic (New): If MessageTypeIndic is provided and completed with \"CBC401\" DocTypeIndic must be \"OECD1\""
    case InvalidDocRefId       => "Error Code 80000 DocRefId (format): The structure of the DocRefID is not in the correct format, as set out in the User Guide."
    case InvalidCorrDocRefId   => "Error Code 8000 CorrDocRefId (format): The structure of the CorrDocRefID is not in the correct format, as set out in the User Guide."
    case CbcOecdVersionError   => """CBC_OECD version must equal 1.0.1"""
    case XmlEncodingError      => """XML encoding must equal UTF8"""
    case OriginalSubmissionNotFound => "Original submission could not be identified"
    case PrivateBetaCBCIdError => """ The country-by-country ID you entered has changed. You will need to use the new ID which we have emailed to you. If you are operating as an agent, contact your client for the new country-by-country ID."""
    case CorrDocRefIdDuplicate => """Error Code 80011 CorrDocRefId (Duplicate):The same DocRefID cannot be corrected or deleted twice in the same message."""
    case ResentDataIsUnknownError => """OECD0 must be used for resent data, but a previous submission with this DocRefID has not been received"""
    case CorrectedFileToOld    => "Corrections only allowed upto 3 years after the initial submission date for the Reporting Period"
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
  def errorHandlerToXmlErrors(x:XmlErrorHandler) : XMLErrors = XMLErrors(x.fatalErrorsCollection ++ x.errorsCollection)
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


