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

import java.time.LocalDate
import javax.inject.Inject

import cats.data.Validated.{Invalid, Valid}
import cats.data._
import cats.instances.all._
import cats.syntax.all._
import cats.{Applicative, Functor}
import play.api.{Configuration, Logger}
import uk.gov.hmrc.cbcrfrontend.{FutureValidResult, ValidResult}
import uk.gov.hmrc.cbcrfrontend.functorInstance
import uk.gov.hmrc.cbcrfrontend.applicativeInstance
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

/**
  * This class exposes two methods:
  *
  * 1) [[validateBusinessRules()]] which takes a [[RawXMLInfo]] and returns an [[XMLInfo]] or a list of [[BusinessRuleErrors]]
  *
  * 2) [[recoverReportingEntity()]] which, given an [[XMLInfo]] will return a [[ReportingEntity]], extracting it either
  *    from the XML object, or from our [[ReportingEntityDataService]]
  *
  * The rest of the methods fall into 2 categories
  *
  * 1) extract* methods that do the minimal possible validation to transform the raw Strings from the [[RawXMLInfo]]
  * into their typesafe versions found in [[XMLInfo]]
  *
  * 2) validate* methods that do further validation to the fields, including cross field validation, duplicate checks e.t.c.
  *
  * The methods reflect the structure of the XML document i.e. [[validateDocSpec]] makes further calls to [[validateDocRefId()]]
  *
  */
class CBCBusinessRuleValidator @Inject() (messageRefService:MessageRefIdService,
                                          docRefIdService: DocRefIdService,
                                          subscriptionDataService: SubscriptionDataService,
                                          reportingEntityDataService: ReportingEntityDataService,
                                          configuration: Configuration,
                                          runMode: RunMode
                                         )(implicit ec:ExecutionContext) {

  private val testData = "OECD1[0123]"

  private val cbcVersion = configuration.getString(s"${runMode.env}.oecd-schema-version").getOrElse(
    throw new Exception(s"Missing configuration key: ${runMode.env}.oecd-schema-version")
  )

  // <<<<<<<<<<<<:::Extraction Methods:::>>>>>>>>>>>>

  /** Top level extraction method */
  private def extractXMLInfo(in:RawXMLInfo) : ValidResult[XMLInfo] =
    (extractCbcOecdVersion(in.cbcVal) *>
      in.xmlEncoding.map(extractXmlEncodingVal).sequence[ValidResult,Unit] *>
      extractMessageSpec(in.messageSpec)                                                   |@|
      in.reportingEntity.map(extractReportingEntity).sequence[ValidResult,ReportingEntity] |@|
      in.cbcReport.map(extractCBCReports).sequence[ValidResult,CbcReports]                 |@|
      in.additionalInfo.map(extractAdditionalInfo).sequence[ValidResult,AdditionalInfo]).map(XMLInfo(_,_,_,_))

  private def extractMessageSpec(in:RawMessageSpec) : ValidResult[MessageSpec] =
    (
      extractMessageRefID(in)     |@|
      extractReceivingCountry(in) |@|
      extractSendingEntityIn(in)  |@|
      extractReportingPeriod(in)  |@|
      extractMessageTypeIndic(in)
    ).map(
      (messageRefID,receivingCountry,sendingEntityIn,reportingPeriod,messageTypeInic) =>
        MessageSpec(
          messageRefID,
          receivingCountry,
          sendingEntityIn,
          messageRefID.creationTimestamp,
          reportingPeriod,
          messageTypeInic
        )
    )

  private def extractReportingEntity(in:RawReportingEntity) : ValidResult[ReportingEntity] =
    (extractReportingRole(in)      |@|
     extractDocSpec(in.docSpec,ENT) |@|
     extractTIN(in)).map(ReportingEntity(_,_,_,in.name))

  private def extractCBCReports(in:RawCbcReports) : ValidResult[CbcReports] =
    extractDocSpec(in.docSpec,REP).map(CbcReports(_))

  private def extractAdditionalInfo(in:RawAdditionalInfo) : ValidResult[AdditionalInfo] =
    extractDocSpec(in.docSpec,ADD).map(AdditionalInfo(_))

  private def extractDocSpec(d:RawDocSpec,parentGroupElement: ParentGroupElement) : ValidResult[DocSpec] =
    (extractDocTypeInidc(d.docType)                  |@|
      extractDocRefId(d.docRefId,parentGroupElement) |@|
      extractCorrDocRefId(d.corrDocRefId,parentGroupElement)).map(DocSpec(_,_,_))

  private def extractDocTypeInidc(docType:String) : ValidResult[DocTypeIndic] =
    DocTypeIndic.fromString(docType).fold[ValidResult[DocTypeIndic]]{
      if(docType.matches(testData)) TestDataError.invalidNel
      else InvalidXMLError("Invalid DocTypeIndic").invalidNel}(
      _.validNel)

  private def extractCorrDocRefId(corrDocRefIdString:Option[String], parentGroupElement: ParentGroupElement) : ValidResult[Option[CorrDocRefId]] = {
    corrDocRefIdString.map(DocRefId(_).fold[ValidResult[Option[CorrDocRefId]]](
      InvalidCorrDocRefId.invalidNel)(
      d => {
        if (d.parentGroupElement != parentGroupElement) CorrDocRefIdInvalidParentGroupElement.invalidNel
        else Some(CorrDocRefId(d)).validNel
      })).getOrElse(None.validNel)
  }

  private def extractDocRefId(docRefIdString:String, parentGroupElement: ParentGroupElement) : ValidResult[DocRefId] =
    DocRefId(docRefIdString).fold[ValidResult[DocRefId]](
      InvalidDocRefId.invalidNel)(
      d => {
        if(d.parentGroupElement != parentGroupElement) DocRefIdInvalidParentGroupElement.invalidNel
        else d.validNel
      })

  private def extractMessageTypeIndic(r:RawMessageSpec) : ValidResult[Option[MessageTypeIndic]] =
    r.messageType.flatMap(MessageTypeIndic.parseFrom).validNel[BusinessRuleErrors]

  private def extractTIN(in:RawReportingEntity) : ValidResult[TIN] = TIN(in.tin,in.tinIssuedBy).validNel

  private def extractReportingRole(in:RawReportingEntity): ValidResult[ReportingRole] =
    ReportingRole.parseFromString(in.reportingRole).toValidNel(InvalidXMLError("ReportingEntity.ReportingRole not found or invalid"))

  private def extractSendingEntityIn(in:RawMessageSpec): ValidResult[CBCId] = {
    CBCId(in.sendingEntityIn).fold[ValidResult[CBCId]](
      SendingEntityError.invalidNel[CBCId])(
      cbcId => {
        if (CBCId.isPrivateBetaCBCId(cbcId)) {
          PrivateBetaCBCIdError.invalidNel[CBCId]
        } else {
          cbcId.validNel
        }
      }
    )
  }

  private def extractReceivingCountry(in:RawMessageSpec) : ValidResult[String] =
    if(in.receivingCountry equalsIgnoreCase "GB") in.receivingCountry.validNel else ReceivingCountryError.invalidNel

  private def extractReportingPeriod(in:RawMessageSpec) : ValidResult[LocalDate] =
    Validated.catchNonFatal(LocalDate.parse(in.reportingPeriod))
      .leftMap(_ => InvalidXMLError("Invalid Date for reporting period")).toValidatedNel

  private def extractMessageRefID(in:RawMessageSpec) : ValidResult[MessageRefID] =
    MessageRefID(in.messageRefID).fold(
      errors   => errors.invalid[MessageRefID],
      msgRefId => msgRefId.validNel[MessageRefIDError]
    )

  /** These are dummy extraction methods - they're actually performing validation, but we dont' actually care
    * about the values later on */
  private def extractXmlEncodingVal(xe: RawXmlEncodingVal): ValidResult[Unit] =
    if (!xe.xmlEncodingVal.equalsIgnoreCase("UTF-8")) XmlEncodingError.invalidNel
    else ().validNel

  private def extractCbcOecdVersion(cv:RawCbcVal):ValidResult[Unit] =
    if(cv.cbcVer != cbcVersion) CbcOecdVersionError.invalidNel
    else ().validNel


  // <<<<<<<<<<:::Validation methods:::>>>>>>>>>>>>>

  /** Top level validation methods */
  private def validateXMLInfo(x:XMLInfo, fileName:String)(implicit hc: HeaderCarrier) : FutureValidResult[XMLInfo] = {
    validateMessageRefIdD(x.messageSpec) *>
    validateReportingEntity(x) *>
    validateMessageTypes(x) *>
    validateDocSpecs(x) *>
    validateMessageTypeIndic(x) *>
    validateFileName(x,fileName)
  }

  private def validateReportingEntity(in: XMLInfo)(implicit hc: HeaderCarrier): FutureValidResult[XMLInfo] =
    in.reportingEntity.map(re =>
      validateDocSpec(re.docSpec).map(vds =>
        (vds |@| validateTIN(re.tin, re.reportingRole)).map((_, _) => in)
      )
    ).getOrElse(Future.successful(in.validNel))


  /** Ensure that if the messageType is [[CBC401]] there are no [[DocTypeIndic]] other than [[OECD1]]*/
  private def validateMessageTypes(r:XMLInfo):ValidResult[XMLInfo] = {
    lazy val docTypes = List(
      r.additionalInfo.map(_.docSpec.docType),
      r.reportingEntity.map(_.docSpec.docType)
    ).flatten ++ r.cbcReport.map(_.docSpec.docType)

    if(r.messageSpec.messageType.contains(CBC401) && docTypes.exists(_ != OECD1)){
      MessageTypeIndicDocTypeIncompatible.invalidNel
    } else {
      r.validNel
    }
  }

  /** Ensure there is not a mixture of OECD1 and other DocTypes within the document */
  private def validateDocTypes(in:List[DocSpec]) : ValidResult[List[DocSpec]] = {
    val all = in.map(_.docType).toSet
    if(all.size > 1 && all.contains(OECD1)) IncompatibleOECDTypes.invalidNel
    else in.validNel
  }

  private def validateDocSpecs(in:XMLInfo)(implicit hc:HeaderCarrier) : FutureValidResult[XMLInfo] = {
   val allDocSpecs = in.cbcReport.map(_.docSpec)  ++ List(
     in.additionalInfo.map(_.docSpec),
     in.reportingEntity.map(_.docSpec)
   ).flatten

    functorInstance.map(
      allDocSpecs.map(validateDocSpec).sequence[FutureValidResult, DocSpec] *>
      validateDocTypes(allDocSpecs) *>
      validateDistinctDocRefIds(allDocSpecs.map(_.docRefId))
    )(_ => in)

  }

  /**
    * Ensure that if a [[CorrDocRefId]] is required, it really exist
    * Ensure that if a [[CorrDocRefId]] is not required, it does not exist
    */
  private def validateCorrDocRefIdRequired(d:DocSpec):ValidResult[DocSpec] = d.docType match {
    case OECD2 | OECD3 if d.corrDocRefId.isEmpty => CorrDocRefIdMissing.invalidNel
    case OECD1 if d.corrDocRefId.isDefined       => CorrDocRefIdNotNeeded.invalidNel
    case _                                       => d.validNel
  }

  /** Ensure that the list of DocRefIds are unique */
  private def validateDistinctDocRefIds(ids:List[DocRefId]): ValidResult[Unit] =
    Either.cond(ids.distinct.size == ids.size, (), DocRefIdDuplicate).toValidatedNel

  /** Do further validation on the DocSpec **/
  private def validateDocSpec(d:DocSpec)(implicit hc:HeaderCarrier) : FutureValidResult[DocSpec] =
    (validateDocRefId(d) zip d.corrDocRefId.map(validateCorrDocRefId).sequence[FutureValidResult,CorrDocRefId] ).map {
      case (doc,corrDoc) => (doc |@| corrDoc |@| validateCorrDocRefIdRequired(d)).map((_,_,_) => d)
    }

  /** Do further validation on the provided [[CorrDocRefId]] */
  private def validateCorrDocRefId(corrDocRefId: CorrDocRefId)(implicit hc:HeaderCarrier) : FutureValidResult[CorrDocRefId] = {
    corrDocRefIdDuplicateCheck(corrDocRefId)
  }

  /** Query the [[DocRefIdService]] to find out if this [[CorrDocRefId]] is a duplicate */
  private def corrDocRefIdDuplicateCheck(corrDocRefId: CorrDocRefId)(implicit hc:HeaderCarrier) : FutureValidResult[CorrDocRefId] = {
    docRefIdService.queryDocRefId(corrDocRefId.cid).map {
      case DocRefIdResponses.Valid        => corrDocRefId.validNel
      case DocRefIdResponses.Invalid      => CorrDocRefIdInvalidRecord.invalidNel
      case DocRefIdResponses.DoesNotExist => CorrDocRefIdUnknownRecord.invalidNel
    }
  }

  /** Do further validation on the provided [[DocRefId]] */
  private def validateDocRefId(docSpec:DocSpec)(implicit hc:HeaderCarrier) : FutureValidResult[DocRefId] =
    docRefIdDuplicateCheck(docSpec)

  /**
    * Query the [[DocRefIdService]] to find out if this docRefId is a duplicate
    * If the doc type is OECD0, when don't check for duplicates
    */
  private def docRefIdDuplicateCheck(docSpec:DocSpec)(implicit hc:HeaderCarrier) : FutureValidResult[DocRefId] = {
    if(docSpec.docType == OECD0) docSpec.docRefId.validNel
    else docRefIdService.queryDocRefId(docSpec.docRefId).map {
      case DocRefIdResponses.DoesNotExist => docSpec.docRefId.validNel
      case _                              => DocRefIdDuplicate.invalidNel
    }
  }

  /** Ensure the messageTypes and docTypes are valid and not in conflict */
  private def validateMessageTypeIndic(xmlInfo: XMLInfo) : ValidResult[XMLInfo] = {

    lazy val CBCReportsAreNotAllCorrectionsOrDeletions: Boolean = !xmlInfo.cbcReport.forall(r =>
      r.docSpec.docType == OECD2 ||
      r.docSpec.docType == OECD3
    )

    lazy val AdditionalInfoIsNotCorrectionsOrDeletions: Boolean = !xmlInfo.additionalInfo.forall(r =>
      r.docSpec.docType == OECD2 ||
      r.docSpec.docType == OECD3
    )

    lazy val ReportingEntityIsNotCorrectionsOrDeletionsOrResent: Boolean = !xmlInfo.reportingEntity.forall(r =>
      r.docSpec.docType == OECD2 ||
      r.docSpec.docType == OECD3 ||
      r.docSpec.docType == OECD0
    )

    xmlInfo.messageSpec.messageType match {
      case Some(CBC401)                                       => xmlInfo.validNel
      case Some(CBC402) if CBCReportsAreNotAllCorrectionsOrDeletions
        || AdditionalInfoIsNotCorrectionsOrDeletions
        || ReportingEntityIsNotCorrectionsOrDeletionsOrResent => MessageTypeIndicError.invalidNel
      case Some(CBC402)                                       => xmlInfo.validNel
    }

  }

  /** Validate the TIN and TIN.issuedBy against the [[ReportingRole]] */
  private def validateTIN(tin:TIN, rr:ReportingRole) : ValidResult[TIN] = rr match {
    case CBC701 | CBC703 if !tin.issuedBy.equalsIgnoreCase("gb") =>
      InvalidXMLError("ReportingEntity.Entity.TIN@issuedBy must be 'GB' for local or primary filings").invalidNel
    case CBC701 | CBC703 if !Utr(tin.value).isValid              =>
      InvalidXMLError("ReportingEntity.Entity.TIN must be a valid UTR for filings issued in 'GB'").invalidNel
    case _                                                       =>
      tin.validNel
  }

  /** Ensure the provided filename matches the given MessageRefID (minus the extension) */
  private def validateFileName(in:XMLInfo, fileName:String) : ValidResult[XMLInfo] =
    if(fileName.split("""\.""").headOption.contains(in.messageSpec.messageRefID.show)) in.validNel
    else { FileNameError.invalidNel }

  /**
    * Ensure SendingEntityIn CBCId is:
    * 1) Not a private Beta CBCId
    * 2) Has already been registered
    */
  private def validateSendingEntity(cbcId:CBCId)(implicit hc:HeaderCarrier) : FutureValidResult[CBCId] =
    if (CBCId.isPrivateBetaCBCId(cbcId)) Future.successful(PrivateBetaCBCIdError.invalidNel[CBCId])
    else {
      subscriptionDataService.retrieveSubscriptionData(Right(cbcId)).fold[ValidResult[CBCId]](
        (_: CBCErrors) => SendingEntityError.invalidNel,
        (maybeDetails: Option[SubscriptionDetails]) => maybeDetails match {
          case None    => SendingEntityError.invalidNel
          case Some(_) => cbcId.validNel
        }
      )
    }

  /** Ensure the [[CBCId]] found in the [[MessageRefID]] matches the [[CBCId]] in the SendingEntityIN field */
  private def validateCBCId(messageRefID: MessageRefID, messageSpec: MessageSpec) : ValidResult[MessageRefID] =
    if(messageRefID.cBCId == messageSpec.sendingEntityIn) { messageRefID.validNel}
    else MessageRefIDCBCIdMismatch.invalidNel

  /** Ensure the reportingPeriod in the [[MessageRefID]] matches the reportingPeriod field in the MessageSpec */
  private def validateReportingPeriodMatches(messageRefID: MessageRefID,messageSpec:MessageSpec) : ValidResult[MessageRefID] =
    if(messageRefID.reportingPeriod.getValue == messageSpec.reportingPeriod.getYear) { messageRefID.validNel }
    else { MessageRefIDReportingPeriodMismatch.invalidNel }

  /** Calls the [[MessageRefIdService]] to see if this provided [[MessageRefID]] already exists */
  private def isADuplicate(msgRefId:MessageRefID)(implicit hc:HeaderCarrier) : FutureValidResult[MessageRefID] =
    messageRefService.messageRefIdExists(msgRefId).map(result =>
      if(result) MessageRefIDDuplicate.invalidNel else msgRefId.validNel
    )

  /**
    * Validates the messageRefID by performing 3 checks:
    * 1) Checks if a duplicate messageRefID exists (by calling the backend)
    * 2) Checks that the SendingEntityIn in the messageSpec matches the CBCId in the messageRefId
    * 3) Checks the reportingPeriod in the messageRefID matches the reportingPeriod in the messageSpec
    */
  private def validateMessageRefIdD(messageSpec: MessageSpec)(implicit hc:HeaderCarrier) : FutureValidResult[MessageRefID] = {
    validateSendingEntity(messageSpec.sendingEntityIn) *>
    isADuplicate(messageSpec.messageRefID) *>
    validateCBCId(messageSpec.messageRefID,messageSpec) *>
    validateReportingPeriodMatches(messageSpec.messageRefID, messageSpec)
  }

  def validateBusinessRules(in: RawXMLInfo, fileName: String)(implicit hc: HeaderCarrier): FutureValidResult[XMLInfo] =
    extractXMLInfo(in).flatMap{
      case Valid(v)   => validateXMLInfo(v,fileName)
      case Invalid(i) => Future.successful(i.invalid)
    }

  def recoverReportingEntity(in:XMLInfo)(implicit hc: HeaderCarrier) : FutureValidResult[CompleteXMLInfo] = in.reportingEntity match {
    case Some(re) => Future.successful(CompleteXMLInfo(in.messageSpec,re,in.cbcReport,in.additionalInfo).validNel)
    case None     =>
      val id = in.cbcReport.find(_.docSpec.corrDocRefId.isDefined).flatMap(_.docSpec.corrDocRefId).orElse(in.additionalInfo.flatMap(_.docSpec.corrDocRefId))
      val rr = in.cbcReport.headOption.map(_.docSpec.docType).orElse(in.additionalInfo.map(_.docSpec.docType))

      (id |@| rr).map { (drid, dti) =>
        reportingEntityDataService.queryReportingEntityData(drid.cid).leftMap{
          cbcErrors => {
            Logger.error(s"Got error back: $cbcErrors")
            throw new Exception(s"Error communicating with backend: $cbcErrors")
          }
        }.subflatMap{
          case Some(red) => {
            val re = ReportingEntity(red.reportingRole, DocSpec(OECD0, red.reportingEntityDRI, None), TIN(red.tin.value,"gb"), red.ultimateParentEntity.ultimateParentEntity)
            Right(CompleteXMLInfo(in,re))
          }
          case None      => Left(OriginalSubmissionNotFound)
        }.toValidatedNel
      }.getOrElse{
        Future.successful(OriginalSubmissionNotFound.invalidNel)
      }
  }


}
