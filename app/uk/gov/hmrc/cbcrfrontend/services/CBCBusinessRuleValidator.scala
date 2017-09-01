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

package uk.gov.hmrc.cbcrfrontend.services


import java.time.{LocalDate, LocalDateTime, Year}
import javax.inject.Inject

import cats.data.{EitherT, NonEmptyList, Validated, ValidatedNel}
import cats.syntax.all._
import cats.instances.all._
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

class CBCBusinessRuleValidator @Inject() (messageRefService:MessageRefIdService,
                                          docRefIdService: DocRefIdService,
                                          subscriptionDataService: SubscriptionDataService)(implicit ec:ExecutionContext) {


  val oecd2Or3 = "OECD[23]"
  val oecd0Or2Or3 = "OECD[023]"
  val testData = "OECD1[0123]"

  def validateBusinessRules(in:RawXMLInfo, fileName:String)(implicit hc:HeaderCarrier) : EitherT[Future,NonEmptyList[BusinessRuleErrors],XMLInfo] = {
    EitherT(
      (validateMessageRefIdD(in.messageSpec) |@|
        validateDocSpec(in.reportingEntity.docSpec) |@|
        validateDocSpec(in.cbcReport.docSpec) |@|
        validateDocSpec(in.additionalInfo.docSpec) |@|
        validateSendingEntity(in.messageSpec) ).map {
        (messageRefIdVal, reDocSpec, cbcDocSpec, addDocSpec, sendingEntity) =>

        val otherRules = (
          validateTestDataPresent(in).toValidatedNel |@|
            validateReceivingCountry(in.messageSpec).toValidatedNel |@|
            validateFileName(in.messageSpec, fileName).toValidatedNel |@|
            validateReportingRole(in.reportingEntity).toValidatedNel |@|
            validateTIN(in.reportingEntity).toValidatedNel |@|
            validateMessageTypeIndic(in).toValidatedNel |@|
            crossValidateDocRefIds(
              in.reportingEntity.docSpec.docRefId,
              in.cbcReport.docSpec.docRefId,
              in.additionalInfo.docSpec.docRefId
            ).toValidatedNel |@|
            validateMessageTypeIndic(in).toValidatedNel |@|
            validateCorrDocRefIdExists(in.cbcReport.docSpec).toValidatedNel |@|
            validateCorrDocRefIdExists(in.reportingEntity.docSpec).toValidatedNel |@|
            validateCorrDocRefIdExists(in.additionalInfo.docSpec).toValidatedNel |@|
            validateMessageTypeIndicCompatible(in).toValidatedNel |@|
            crossValidateCorrDocRefIds(in.cbcReport.docSpec,in.reportingEntity.docSpec,in.additionalInfo.docSpec).toValidatedNel |@|
            validateCbcOecdVersion(in.cbcVal).toValidatedNel |@|
            validateXmlEncodingVal(in.xmlEncoding).toValidatedNel
          ).map((_, rc, _, reportingRole, tin, mti,_,_,_,_,_,_,_,_,_) => (rc, reportingRole, tin, mti))


        (otherRules |@| messageRefIdVal |@| reDocSpec |@| cbcDocSpec |@| addDocSpec |@| sendingEntity |@| validateReportingPeriod(in.messageSpec).toValidatedNel).map(
          (values, msgRefId, reDocSpec,cbcDocSpec,addDocSpec, _, reportingPeriod) =>
            XMLInfo(
              MessageSpec(
                msgRefId,values._1,
                msgRefId.cBCId,
                msgRefId.creationTimestamp,
                reportingPeriod,
                values._4
              ),
              ReportingEntity(values._2,reDocSpec,values._3,in.reportingEntity.name),
              CbcReports(cbcDocSpec),
              AdditionalInfo(addDocSpec)
            )
        ).toEither
    })
  }

  private def validateXmlEncodingVal(xe:RawXmlEncodingVal):Validated[BusinessRuleErrors,Unit] ={
    if(xe.xmlEncodingVal != "UTF-8"){
      XmlEncodingError.invalid
    } else {
      ().valid
    }
  }

  private def validateCbcOecdVersion(cv:RawCbcVal):Validated[BusinessRuleErrors,Unit] = {
    if(cv.cbcVer != "1.0"){
      CbcOecdVersionError.invalid
    } else {
      ().valid
    }
  }

  private def validateMessageTypeIndicCompatible(r:RawXMLInfo):Validated[BusinessRuleErrors,Unit] = {
    if(r.messageSpec.messageType.contains(CBC401.toString) &&
      (r.additionalInfo.docSpec.docType != OECD1.toString ||
        r.reportingEntity.docSpec.docType != OECD1.toString ||
        r.cbcReport.docSpec.docType != OECD1.toString)
    ){
      MessageTypeIndicDocTypeIncompatible.invalid
    } else {
      ().valid
    }
  }

  private def crossValidateCorrDocRefIds(re:RawDocSpec, cb:RawDocSpec, ad:RawDocSpec) : Validated[BusinessRuleErrors,Unit] = {
    val all = List(re.docType,cb.docType,ad.docType).toSet
    if(all.size > 1 && all.contains(OECD1.toString)){
      IncompatibleOECDTypes.invalid
    } else {
      ().valid
    }
  }

  private def validateCorrDocRefIdExists(d:RawDocSpec):Validated[BusinessRuleErrors,Unit] = {
    if(d.docType.matches(oecd2Or3) && d.corrDocRefId.isEmpty){
      CorrDocRefIdMissing.invalid
    } else if(d.docType == OECD1.toString && d.corrDocRefId.isDefined){
      CorrDocRefIdNotNeeded.invalid
    } else {
      ().valid
    }
  }

  private def crossValidateDocRefIds(re:String,cb:String,ad:String): Validated[BusinessRuleErrors,Unit] =
    Either.cond(re != cb && re != ad && cb != ad,(),DocRefIdDuplicate).toValidated

  private def validateDocSpec(d:RawDocSpec)(implicit hc:HeaderCarrier) : Future[ValidatedNel[BusinessRuleErrors,DocSpec]] = {
    validateDocRefId(d.docRefId).zip(validateCorrDocRefId(d.corrDocRefId)).map {
      case (x,c) => (x.toValidatedNel |@| c.toValidatedNel).map( (doc,corr) =>
        DocSpec(OECD1,doc,corr)
      )
    }
  }

  private def validateCorrDocRefId(corrDocRefIdString:Option[String])(implicit hc:HeaderCarrier) : Future[Validated[BusinessRuleErrors,Option[CorrDocRefId]]] = {
    corrDocRefIdString.map(DocRefId(_).fold[Future[Validated[BusinessRuleErrors,Option[CorrDocRefId]]]](
      Future.successful(InvalidCorrDocRefId.invalid))(
      d => docRefIdService.queryDocRefId(d).map {
        case DocRefIdResponses.Valid => Some(CorrDocRefId(d)).valid
        case DocRefIdResponses.Invalid => CorrDocRefIdInvalidRecord.invalid
        case DocRefIdResponses.DoesNotExist => CorrDocRefIdUnknownRecord.invalid
      })
    ).getOrElse(Future.successful(None.valid))
  }

  private def validateDocRefId(docRefIdString:String)(implicit hc:HeaderCarrier) : Future[Validated[BusinessRuleErrors,DocRefId]] = {
    DocRefId(docRefIdString).fold[Future[Validated[BusinessRuleErrors,DocRefId]]](
      Future.successful(InvalidDocRefId.invalid))(
      d => docRefIdService.queryDocRefId(d).map{
        case DocRefIdResponses.DoesNotExist => d.valid
        case _                              => DocRefIdDuplicate.invalid
      }
    )
  }

  private def validateMessageTypeIndic(r:RawXMLInfo) : Validated[BusinessRuleErrors,Option[MessageTypeIndic]] = {
    r.messageSpec.messageType.flatMap(MessageTypeIndic.parseFrom).map{
      case CBC401                                                            => Some(CBC401).valid
      case CBC402 if !r.cbcReport.docSpec.docType.matches(oecd2Or3)
                  || !r.additionalInfo.docSpec.docType.matches(oecd2Or3)
                  || !r.reportingEntity.docSpec.docType.matches(oecd0Or2Or3) => MessageTypeIndicError.invalid
      case CBC402                                                            => Some(CBC402).valid
    }.getOrElse((None:Option[MessageTypeIndic]).valid)

  }

  private def validateTIN(in:RawReportingEntity) : Validated[BusinessRuleErrors, Utr] = {
    if(Utr(in.tin).isValid){ Some(Utr(in.tin)) }
    else { None }
  }.toValid(InvalidXMLError("ReportingEntity.Entity.TIN field invalid"))

  private def validateReportingRole(in:RawReportingEntity): Validated[BusinessRuleErrors, ReportingRole] =
    ReportingRole.parseFromString(in.reportingRole).toValid(InvalidXMLError("ReportingEntity.ReportingRole not found or invalid"))

  private def validateFileName(in:RawMessageSpec,fileName:String) : Validated[BusinessRuleErrors,Unit] =
    if(fileName.split("""\.""").headOption.contains(in.messageRefID)){ ().valid }
    else { FileNameError.invalid }

  private def validateSendingEntity(in:RawMessageSpec)(implicit hc:HeaderCarrier) : Future[ValidatedNel[BusinessRuleErrors,CBCId]] =
    CBCId(in.sendingEntityIn).fold[Future[ValidatedNel[BusinessRuleErrors,CBCId]]](
      Future.successful(SendingEntityError.invalidNel[CBCId]))(
      cbcId => subscriptionDataService.retrieveSubscriptionData(Right(cbcId)).fold[ValidatedNel[BusinessRuleErrors,CBCId]](
        (_: CBCErrors)                              => SendingEntityError.invalidNel,
        (maybeDetails: Option[SubscriptionDetails]) => maybeDetails match {
          case None    => SendingEntityError.invalidNel
          case Some(_) => cbcId.validNel
        }
      )
    )

  private def validateReceivingCountry(in:RawMessageSpec) : Validated[BusinessRuleErrors,String] =
    if(in.receivingCountry equalsIgnoreCase "GB"){ in.receivingCountry.valid }
    else { ReceivingCountryError.invalid }

  private def validateTestDataPresent(in:RawXMLInfo) : Validated[BusinessRuleErrors,Unit] =
    if(in.cbcReport.docSpec.docType.matches(testData)) TestDataError.invalid
    else if(in.additionalInfo.docSpec.docType.matches(testData)) TestDataError.invalid
    else if(in.reportingEntity.docSpec.docType.matches(testData)) TestDataError.invalid
    else ().valid

  private def validateCBCId(in:RawMessageSpec, messageRefID: MessageRefID) : Validated[MessageRefIDError,Unit] =
    if(in.sendingEntityIn.equalsIgnoreCase(messageRefID.cBCId.value)) { ().valid}
    else MessageRefIDCBCIdMismatch.invalid

  private def validateReportingPeriod(in:RawMessageSpec) : Validated[BusinessRuleErrors,LocalDate] =
    Validated.catchNonFatal(LocalDate.parse(in.reportingPeriod))
      .leftMap(_ => InvalidXMLError("Invalid Date for reporting period"))

  private def validateReportingPeriodMatches(in:RawMessageSpec, year:Year) : Validated[MessageRefIDError,Year] =
    if(in.reportingPeriod.startsWith(year.toString)) { year.valid }
    else { MessageRefIDReportingPeriodMismatch.invalid }

  private def validateDateStamp(in:RawMessageSpec) : Validated[MessageRefIDError,LocalDateTime] =
    Validated.catchNonFatal(LocalDateTime.parse(in.timestamp,MessageRefID.dateFmt)).leftMap(
      _   => MessageRefIDTimestampError
    )

  private def isADuplicate(msgRefId:MessageRefID)(implicit hc:HeaderCarrier) : Future[Validated[MessageRefIDError,Unit]] =
    messageRefService.messageRefIdExists(msgRefId).map(result =>
      if(result) MessageRefIDDuplicate.invalid else ().valid
    )

  private def validateMessageRefIdD(in:RawMessageSpec)(implicit hc:HeaderCarrier) : Future[ValidatedNel[MessageRefIDError, MessageRefID]] = {
    MessageRefID(in.messageRefID).toEither.fold(
      errors   => Future.successful(errors.invalid[MessageRefID]),
      msgRefId => isADuplicate(msgRefId).map(dup =>
        (dup.toValidatedNel |@| validateCBCId(in, msgRefId).toValidatedNel |@| validateReportingPeriodMatches(in, msgRefId.reportingPeriod).toValidatedNel).map(
          (_, _, _) => msgRefId
        )
      )
    )
  }

}
