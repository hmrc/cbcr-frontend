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


import java.time.{LocalDateTime, Year}
import javax.inject.Inject

import cats.data.{EitherT, NonEmptyList, Validated, ValidatedNel}
import cats.syntax.all._
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

class CBCBusinessRuleValidator @Inject() (messageRefService:MessageRefIdService)(implicit ec:ExecutionContext) {

  def validateBusinessRules(in:RawXMLInfo, cBCId: CBCId, fileName:String)(implicit hc:HeaderCarrier) : EitherT[Future,NonEmptyList[BusinessRuleErrors],XMLInfo] = {
    EitherT(messageRefIDCheck(in.messageSpec).map { messageRefIdVal =>
      val otherRules = (
        validateTestDataPresent(in).toValidatedNel |@|
          validateReceivingCountry(in.messageSpec).toValidatedNel |@|
          validateSendingEntity(in.messageSpec, cBCId).toValidatedNel |@|
          validateFileName(in.messageSpec, fileName).toValidatedNel |@|
          validateReportingRole(in.reportingEntity).toValidatedNel |@|
          validateTIN(in.reportingEntity).toValidatedNel
        ).map((_, rc, _, _, reportingRole, tin) => (rc, reportingRole, tin))

      (otherRules |@| messageRefIdVal).map((values, msgRefId) =>
        XMLInfo(
          MessageSpec(msgRefId,values._1,msgRefId.cBCId,msgRefId.creationTimestamp,msgRefId.reportingPeriod),
          ReportingEntity(values._2,rawDocSpecToDocSpec(in.reportingEntity.docSpec),values._3,in.reportingEntity.name),
          CbcReports(rawDocSpecToDocSpec(in.cbcReport.docSpec)),
          AdditionalInfo(rawDocSpecToDocSpec(in.additionalInfo.docSpec))
        )
      ).toEither

    })
  }


  private def rawDocSpecToDocSpec(r:RawDocSpec):DocSpec = {
    DocSpec(OECD1,DocRefId(r.docRefId),None)
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

  private def validateSendingEntity(in:RawMessageSpec,cbcId:CBCId) : Validated[BusinessRuleErrors,CBCId] =
    if(in.sendingEntityIn equalsIgnoreCase cbcId.value){ cbcId.valid }
    else { SendingEntityError.invalid }

  private def validateReceivingCountry(in:RawMessageSpec) : Validated[BusinessRuleErrors,String] =
    if(in.receivingCountry equalsIgnoreCase "GB"){ in.receivingCountry.valid }
    else { ReceivingCountryError.invalid }

  private def validateTestDataPresent(in:RawXMLInfo) : Validated[BusinessRuleErrors,Unit] =
    if(in.cbcReport.docSpec.docType.matches("OECD1[123]")) TestDataError.invalid
    else if(in.additionalInfo.docSpec.docType.matches("OECD1[123]")) TestDataError.invalid
    else if(in.reportingEntity.docSpec.docType.matches("OECD1[123]")) TestDataError.invalid
    else ().valid

  //The CbC ID within the MessageRefId does not match the CbC ID in the SendingEntityIN field
  private def validateCBCId(in:RawMessageSpec, messageRefID: MessageRefID) : Validated[MessageRefIDError,Unit] =
    if(in.sendingEntityIn.equalsIgnoreCase(messageRefID.cBCId.value)) { ().valid}
    else MessageRefIDCBCIdMismatch.invalid

  private def validateReportingPeriod(in:RawMessageSpec, year:Year) : Validated[MessageRefIDError,Year] =
    if(in.reportingPeriod.startsWith(year.toString)) { year.valid }
    else { MessageRefIDReportingPeriodMismatch.invalid }

  private def validateDateStamp(in:RawMessageSpec) : Validated[MessageRefIDError,LocalDateTime] =
    Validated.catchNonFatal(LocalDateTime.parse(in.timestamp,MessageRefID.dateFmt)).bimap(
      _   => MessageRefIDTimestampError,
      ldt => ldt
    )

  private def isADuplicate(msgRefId:MessageRefID)(implicit hc:HeaderCarrier) : Future[Validated[MessageRefIDError,Unit]] =
    messageRefService.messageRefIdExists(msgRefId).map(result =>
      if(result) MessageRefIDDuplicate.invalid else ().valid
    )

  private def messageRefIDCheck(in:RawMessageSpec)(implicit hc:HeaderCarrier) : Future[ValidatedNel[MessageRefIDError, MessageRefID]] = {
    MessageRefID(in.messageRefID).toEither.fold(
      errors   => Future.successful(errors.invalid[MessageRefID]),
      msgRefId => isADuplicate(msgRefId).map(dup =>
        (dup.toValidatedNel |@| validateCBCId(in, msgRefId).toValidatedNel |@| validateReportingPeriod(in, msgRefId.reportingPeriod).toValidatedNel).map(
          (_, _, _) => msgRefId
        )
      )
    )
  }

}
