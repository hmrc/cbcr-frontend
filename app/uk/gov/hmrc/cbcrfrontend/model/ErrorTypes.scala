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

case class AdditionalInfoDRINotFound(firstCdri:String, missingCdri:String) extends BusinessRuleErrors
object AdditionalInfoDRINotFound {
  implicit val format = Json.format[AdditionalInfoDRINotFound]
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
case object MultipleCbcBodies extends BusinessRuleErrors
case object CorrectedFileToOld extends BusinessRuleErrors
case object ReportingEntityOrConstituentEntityEmpty extends BusinessRuleErrors
case object CorrMessageRefIdNotAllowedInMessageSpec extends BusinessRuleErrors
case object CorrMessageRefIdNotAllowedInDocSpec extends BusinessRuleErrors
case object ReportingPeriodInvalid extends BusinessRuleErrors
//case object AdditionalInfoDRINotFound extends BusinessRuleErrors

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
      case m:MessageRefIDError                      => Json.toJson[MessageRefIDError](m)(MessageRefIDError.format)
      case TestDataError                            => JsString(TestDataError.toString)
      case SendingEntityError                       => JsString(SendingEntityError.toString)
      case SendingEntityOrganisationMatchError      => JsString(SendingEntityOrganisationMatchError.toString)
      case ReceivingCountryError                    => JsString(ReceivingCountryError.toString)
      case fne:FileNameError                        => Json.toJson(fne)
      case MessageTypeIndicError                    => JsString(MessageTypeIndicError.toString)
      case m:InvalidXMLError                        => JsString(m.toString)
      case InvalidDocRefId                          => JsString(InvalidDocRefId.toString)
      case InvalidCorrDocRefId                      => JsString(InvalidCorrDocRefId.toString)
      case CorrDocRefIdInvalidRecord                => JsString(CorrDocRefIdInvalidRecord.toString)
      case CorrDocRefIdUnknownRecord                => JsString(CorrDocRefIdUnknownRecord.toString)
      case DocRefIdDuplicate                        => JsString(DocRefIdDuplicate.toString)
      case DocRefIdInvalidParentGroupElement        => JsString(DocRefIdInvalidParentGroupElement.toString)
      case CorrDocRefIdInvalidParentGroupElement    => JsString(CorrDocRefIdInvalidParentGroupElement.toString)
      case CorrDocRefIdMissing                      => JsString(CorrDocRefIdMissing.toString)
      case CorrDocRefIdNotNeeded                    => JsString(CorrDocRefIdNotNeeded.toString)
      case CorrDocRefIdDuplicate                    => JsString(CorrDocRefIdDuplicate.toString)
      case IncompatibleOECDTypes                    => JsString(IncompatibleOECDTypes.toString)
      case MessageTypeIndicDocTypeIncompatible      => JsString(MessageTypeIndicDocTypeIncompatible.toString)
      case CbcOecdVersionError                      => JsString(CbcOecdVersionError.toString)
      case XmlEncodingError                         => JsString(XmlEncodingError.toString)
      case OriginalSubmissionNotFound               => JsString(OriginalSubmissionNotFound.toString)
      case PrivateBetaCBCIdError                    => JsString(PrivateBetaCBCIdError.toString)
      case ResentDataIsUnknownError                 => JsString(ResentDataIsUnknownError.toString)
      case MultipleCbcBodies                        => JsString(MultipleCbcBodies.toString)
      case CorrectedFileToOld                       => JsString(CorrectedFileToOld.toString)
      case ReportingEntityOrConstituentEntityEmpty  => JsString(ReportingEntityOrConstituentEntityEmpty.toString)
      case CorrMessageRefIdNotAllowedInMessageSpec  => JsString(CorrMessageRefIdNotAllowedInMessageSpec.toString)
      case CorrMessageRefIdNotAllowedInDocSpec      => JsString(CorrMessageRefIdNotAllowedInDocSpec.toString)
      case ReportingPeriodInvalid                   => JsString(ReportingPeriodInvalid.toString)
      case aidnf:AdditionalInfoDRINotFound          => Json.toJson(aidnf)
    }

    implicit class CaseInsensitiveRegex(sc: StringContext) {
      def ci = ( "(?i)" + sc.parts.mkString ).r
    }

    override def reads(json: JsValue): JsResult[BusinessRuleErrors] =
      Json.fromJson[MessageRefIDError](json)
        .orElse[BusinessRuleErrors](Json.fromJson(json)(FileNameError.format))
      .orElse[BusinessRuleErrors](Json.fromJson(json)(AdditionalInfoDRINotFound.format))
        .orElse[BusinessRuleErrors]{
        json.asOpt[String] match {
          case Some(ci"multiplecbcbodies")     => JsSuccess(MultipleCbcBodies)
          case Some(ci"messagetypeindicerror") => JsSuccess(MessageTypeIndicError)
          case Some(ci"testdataerror")         => JsSuccess(TestDataError)
          case Some(ci"sendingentityerror")    => JsSuccess(SendingEntityError)
          case Some(ci"sendingentityorganisationmatcherror") =>JsSuccess(SendingEntityOrganisationMatchError)
          case Some(ci"receivingcountryerror") => JsSuccess(ReceivingCountryError)
          case Some(ci"invaliddocrefid")       => JsSuccess(InvalidDocRefId)
          case Some(ci"invalidcorrdocrefid")       => JsSuccess(InvalidCorrDocRefId)
          case Some(ci"corrdocrefidinvalidrecord") => JsSuccess(CorrDocRefIdInvalidRecord)
          case Some(ci"corrdocrefidunknownrecord") => JsSuccess(CorrDocRefIdUnknownRecord)
          case Some(ci"corrdocrefidduplicate")     => JsSuccess(CorrDocRefIdDuplicate)
          case Some(ci"docrefidduplicate")         => JsSuccess(DocRefIdDuplicate)
          case Some(ci"docrefidinvalidparentgroupelement") => JsSuccess(DocRefIdInvalidParentGroupElement)
          case Some(ci"corrdocrefidinvalidparentgroupelement") => JsSuccess(CorrDocRefIdInvalidParentGroupElement)
          case Some(ci"corrdocrefidmissing")       => JsSuccess(CorrDocRefIdMissing)
          case Some(ci"corrdocrefidnotneeded")     => JsSuccess(CorrDocRefIdNotNeeded)
          case Some(ci"incompatibleoecdtypes")     => JsSuccess(IncompatibleOECDTypes)
          case Some(ci"messagetypeindicdoctypeincompatible") => JsSuccess(MessageTypeIndicDocTypeIncompatible)
          case Some(ci"cbcoecdversionerror")    => JsSuccess(CbcOecdVersionError)
          case Some(ci"xmlencodingerror")       => JsSuccess(XmlEncodingError)
          case Some(ci"originalsubmissionnotfound") => JsSuccess(OriginalSubmissionNotFound)
          case Some(ci"privatebetacbciderror")      => JsSuccess(PrivateBetaCBCIdError)
          case Some(ci"resentdataisunknownerror")   => JsSuccess(ResentDataIsUnknownError)
          case Some(ci"correctedfiletoold")         => JsSuccess(CorrectedFileToOld)
          case Some(ci"reportingentityorconstituententityempty") => JsSuccess(ReportingEntityOrConstituentEntityEmpty)
          case Some(ci"corrmessagerefidnotallowedinmessagespec") => JsSuccess(CorrMessageRefIdNotAllowedInMessageSpec)
          case Some(ci"corrmessagerefidnotallowedindocspec") => JsSuccess(CorrMessageRefIdNotAllowedInDocSpec)
          case Some(ci"reportingperiodinvalid") => JsSuccess(ReportingPeriodInvalid)
          case Some(otherError) if otherError.startsWith("InvalidXMLError:") =>
            JsSuccess(InvalidXMLError(otherError.replaceAll("^InvalidXMLError: ", "")))
          case other                         => JsError(s"Unable to serialise $other to a BusinessRuleError")
        }
      }
  }
  implicit val eShows: Show[BusinessRuleErrors] =  Show.show[BusinessRuleErrors]{
    case m:MessageRefIDError => m.show
    case TestDataError         => "error.TestDataError"
    case SendingEntityError    => "error.SendingEntityError"
    case SendingEntityOrganisationMatchError => "error.SendingEntityOrganisationMatchError"
    case ReceivingCountryError => "error.ReceivingCountryError"
    case FileNameError(f,e)         => s"error.FileNameError1" +" \r\n" + s" error.FileNameError2 $f" +" \r\n" + s" error.FileNameError3 $e"
    case MessageTypeIndicError => "error.MessageTypeIndicError"
    case CorrDocRefIdInvalidRecord => "error.CorrDocRefIdInvalidRecord"
    case CorrDocRefIdUnknownRecord => "error.CorrDocRefIdUnknownRecord"
    case DocRefIdDuplicate         => "error.DocRefIdDuplicate"
    case DocRefIdInvalidParentGroupElement => "error.DocRefIdInvalidParentGroupElement"
    case CorrDocRefIdInvalidParentGroupElement => "error.CorrDocRefIdInvalidParentGroupElement"
    case CorrDocRefIdMissing   => "error.CorrDocRefIdMissing"
    case CorrDocRefIdNotNeeded => "error.CorrDocRefIdNotNeeded"
    case IncompatibleOECDTypes => "error.IncompatibleOECDTypes"
    case MessageTypeIndicDocTypeIncompatible => "error.MessageTypeIndicDocTypeIncompatible"
    case InvalidDocRefId       => "error.InvalidDocRefId"
    case InvalidCorrDocRefId   => "error.InvalidCorrDocRefId"
    case CbcOecdVersionError   => "error.CbcOecdVersionError"
    case XmlEncodingError      => "error.XmlEncodingError"
    case OriginalSubmissionNotFound => "error.OriginalSubmissionNotFound"
    case PrivateBetaCBCIdError => "error.PrivateBetaCBCIdError"
    case CorrDocRefIdDuplicate => "error.CorrDocRefIdDuplicate"
    case ResentDataIsUnknownError => "error.ResentDataIsUnknownError"
    case MultipleCbcBodies        => "error.MultipleCbcBodies"
    case CorrectedFileToOld    => "error.CorrectedFileToOld"
    case ReportingEntityOrConstituentEntityEmpty => "error.ReportingEntityOrConstituentEntityEmpty"
    case i:InvalidXMLError     => i.toString
    case CorrMessageRefIdNotAllowedInMessageSpec => "error.CorrMessageRefIdNotAllowedInMessageSpec"
    case CorrMessageRefIdNotAllowedInDocSpec => "error.CorrMessageRefIdNotAllowedInDocSpec"
    case ReportingPeriodInvalid => "error.ReportingPeriodInvalid"
    case AdditionalInfoDRINotFound(f,m) => s"error.AdditionalInfoDRINotFound1 $m error.AdditionalInfoDRINotFound2" + " \r\n" + s" error.AdditionalInfoDRINotFound3 $f error.AdditionalInfoDRINotFound4"

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
    case MessageRefIDMissing                 => "messageRefIDError.IDMissing"
    case MessageRefIDCBCIdMismatch           => "messageRefIDError.CBCIdMismatch"
    case MessageRefIDFormatError             => "messageRefIDError.FormatError"
    case MessageRefIDDuplicate               => "messageRefIdError.Duplicate"
    case MessageRefIDReportingPeriodMismatch => "messageRefIdError.ReportingPeriodMismatch"
    case MessageRefIDTimestampError          => "messageRefIdError.TimestampError"
  }
}

object XMLErrors {
  implicit val format = Json.format[XMLErrors]
  def errorHandlerToXmlErrors(x:XmlErrorHandler) : XMLErrors = XMLErrors(x.fatalErrorsCollection ++ x.errorsCollection)
  implicit val xmlShows: Show[XMLErrors] = Show.show[XMLErrors](e =>
    "xmlError.header " + "\n\n " + e.errors.mkString("\n")
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


