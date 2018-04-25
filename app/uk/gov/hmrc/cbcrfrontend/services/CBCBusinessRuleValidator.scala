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

package uk.gov.hmrc.cbcrfrontend.services

import java.time.{LocalDate, LocalDateTime}

import javax.inject.Inject

import cats.data.Validated.{Invalid, Valid}
import cats.data._
import cats.instances.all._
import cats.syntax.all._
import cats.{Applicative, Functor}
import play.api.{Configuration, Logger}
import uk.gov.hmrc.cbcrfrontend.{FutureValidBusinessResult, ValidBusinessResult}
import uk.gov.hmrc.cbcrfrontend.functorInstance
import uk.gov.hmrc.cbcrfrontend.applicativeInstance
import uk.gov.hmrc.cbcrfrontend.model.{CorrectedFileToOld, _}

import scala.concurrent.{ExecutionContext, Future}
import uk.gov.hmrc.http.HeaderCarrier

import scala.util.Failure

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
                                          runMode: RunMode,
                                          creationDateService: CreationDateService
                                         )(implicit ec:ExecutionContext, cache:CBCSessionCache) {

  private val testData = "OECD1[0123]"

  private val cbcVersion = configuration.getString(s"${runMode.env}.oecd-schema-version").getOrElse(
    throw new Exception(s"Missing configuration key: ${runMode.env}.oecd-schema-version")
  )

  // <<<<<<<<<<<<:::Extraction Methods:::>>>>>>>>>>>>

  /** Top level extraction method */
  private def extractXMLInfo(in:RawXMLInfo) : ValidBusinessResult[XMLInfo] =
    (extractCbcOecdVersion(in.cbcVal) *>
      in.xmlEncoding.map(extractXmlEncodingVal).sequence[ValidBusinessResult,Unit] *>
      extractMessageSpec(in.messageSpec)                                                   |@|
      in.reportingEntity.map(extractReportingEntity).sequence[ValidBusinessResult,ReportingEntity] |@|
      in.cbcReport.map(extractCBCReports).sequence[ValidBusinessResult,CbcReports]                 |@|
      in.additionalInfo.map(extractAdditionalInfo).sequence[ValidBusinessResult,AdditionalInfo]).map(XMLInfo(_,_,_,_,Some(LocalDate.now())))

  private def extractMessageSpec(in:RawMessageSpec) : ValidBusinessResult[MessageSpec] =
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

  private def extractReportingEntity(in:RawReportingEntity) : ValidBusinessResult[ReportingEntity] =
    (extractReportingRole(in)      |@|
     extractDocSpec(in.docSpec,ENT) |@|
     extractTIN(in)).map(ReportingEntity(_,_,_,in.name))

  private def extractCBCReports(in:RawCbcReports) : ValidBusinessResult[CbcReports] =
    extractDocSpec(in.docSpec,REP).map(CbcReports(_))

  private def extractAdditionalInfo(in:RawAdditionalInfo) : ValidBusinessResult[AdditionalInfo] =
    extractDocSpec(in.docSpec,ADD).map(AdditionalInfo(_))

  private def extractDocSpec(d:RawDocSpec,parentGroupElement: ParentGroupElement) : ValidBusinessResult[DocSpec] =
    (extractDocTypeInidc(d.docType)                  |@|
      extractDocRefId(d.docRefId,parentGroupElement) |@|
      extractCorrDocRefId(d.corrDocRefId,parentGroupElement)).map(DocSpec(_,_,_))

  private def extractDocTypeInidc(docType:String) : ValidBusinessResult[DocTypeIndic] =
    DocTypeIndic.fromString(docType).fold[ValidBusinessResult[DocTypeIndic]]{
      if(docType.matches(testData)) TestDataError.invalidNel
      else InvalidXMLError("Invalid DocTypeIndic").invalidNel}(
      _.validNel)

  private def extractCorrDocRefId(corrDocRefIdString:Option[String], parentGroupElement: ParentGroupElement) : ValidBusinessResult[Option[CorrDocRefId]] = {
    corrDocRefIdString.map(DocRefId(_).fold[ValidBusinessResult[Option[CorrDocRefId]]](
      InvalidCorrDocRefId.invalidNel)(
      d => {
        if (d.parentGroupElement != parentGroupElement) CorrDocRefIdInvalidParentGroupElement.invalidNel
        else Some(CorrDocRefId(d)).validNel
      })).getOrElse(None.validNel)
  }

  private def extractDocRefId(docRefIdString:String, parentGroupElement: ParentGroupElement) : ValidBusinessResult[DocRefId] =
    DocRefId(docRefIdString).fold[ValidBusinessResult[DocRefId]](
      InvalidDocRefId.invalidNel)(
      d => {
        if(d.parentGroupElement != parentGroupElement) DocRefIdInvalidParentGroupElement.invalidNel
        else d.validNel
      })

  private def extractMessageTypeIndic(r:RawMessageSpec) : ValidBusinessResult[Option[MessageTypeIndic]] =
    r.messageType.flatMap(MessageTypeIndic.parseFrom).validNel[BusinessRuleErrors]

  private def extractTIN(in:RawReportingEntity) : ValidBusinessResult[TIN] = TIN(in.tin,in.tinIssuedBy).validNel

  private def extractReportingRole(in:RawReportingEntity): ValidBusinessResult[ReportingRole] =
    ReportingRole.parseFromString(in.reportingRole).toValidNel(InvalidXMLError("ReportingEntity.ReportingRole not found or invalid"))

  private def extractSendingEntityIn(in:RawMessageSpec): ValidBusinessResult[CBCId] = {
    CBCId(in.sendingEntityIn).fold[ValidBusinessResult[CBCId]](
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

  private def extractReceivingCountry(in:RawMessageSpec) : ValidBusinessResult[String] =
    if(in.receivingCountry equalsIgnoreCase "GB") in.receivingCountry.validNel else ReceivingCountryError.invalidNel

  private def extractReportingPeriod(in:RawMessageSpec) : ValidBusinessResult[LocalDate] =
    Validated.catchNonFatal(LocalDate.parse(in.reportingPeriod))
      .leftMap(_ => InvalidXMLError("Invalid Date for reporting period")).toValidatedNel

  private def extractMessageRefID(in:RawMessageSpec) : ValidBusinessResult[MessageRefID] =
    MessageRefID(in.messageRefID).fold(
      errors   => errors.invalid[MessageRefID],
      msgRefId => msgRefId.validNel[MessageRefIDError]
    )

  /** These are dummy extraction methods - they're actually performing validation, but we dont' actually care
    * about the values later on */
  private def extractXmlEncodingVal(xe: RawXmlEncodingVal): ValidBusinessResult[Unit] =
    if (!xe.xmlEncodingVal.equalsIgnoreCase("UTF-8")) XmlEncodingError.invalidNel
    else ().validNel

  private def extractCbcOecdVersion(cv:RawCbcVal):ValidBusinessResult[Unit] =
    if(cv.cbcVer != cbcVersion) CbcOecdVersionError.invalidNel
    else ().validNel


  // <<<<<<<<<<:::Validation methods:::>>>>>>>>>>>>>

  /** Top level validation methods */
  private def validateXMLInfo(x:XMLInfo, fileName:String)(implicit hc: HeaderCarrier) : FutureValidBusinessResult[XMLInfo] = {
    validateMessageRefIdD(x.messageSpec) *>
    validateReportingEntity(x) *>
    validateMessageTypes(x) *>
    validateDocSpecs(x) *>
    validateMessageTypeIndic(x) *>
    validateFileName(x,fileName) *>
    validateOrganisationCBCId(x) *>
    validateCreationDate(x)
  }

  private def validateReportingEntity(in: XMLInfo)(implicit hc: HeaderCarrier): FutureValidBusinessResult[XMLInfo] =
    in.reportingEntity.map { re =>
      val docRefId = if(re.docSpec.docType == OECD0) { ensureDocRefIdExists(re.docSpec.docRefId) }
                     else { Future.successful(re.docSpec.docRefId.validNel) }

      (validateDocSpec(re.docSpec) *>
        docRefId *>
        validateTIN(re.tin, re.reportingRole)).map(_.andThen(_ => in.validNel))

    }.getOrElse(Future.successful(in.validNel))

  private def ensureDocRefIdExists(docRefId: DocRefId)(implicit hc:HeaderCarrier): FutureValidBusinessResult[DocRefId] = {
    reportingEntityDataService.queryReportingEntityDataDocRefId(docRefId).leftMap(
    cbcErrors => {
      Logger.error(s"Got error back: $cbcErrors")
      throw new Exception(s"Error communicating with backend: $cbcErrors")
    }).subflatMap{
      case Some(_) => Right(docRefId)
      case None    => Left(ResentDataIsUnknownError)
    }.toValidatedNel
  }

  /** Ensure that if the messageType is [[CBC401]] there are no [[DocTypeIndic]] other than [[OECD1]]*/
  private def validateMessageTypes(r:XMLInfo):ValidBusinessResult[XMLInfo] = {
    lazy val docTypes = List(r.additionalInfo.map(_.docSpec.docType)).flatten ++ r.cbcReport.map(_.docSpec.docType)

    lazy val repDocTypes = r.reportingEntity.map(_.docSpec.docType)

    // For CBC401 all other DocTypes must be OECD1 except reportingEntity which can also be OECD0
    if(r.messageSpec.messageType.contains(CBC401) &&
      (docTypes.exists(_ != OECD1) || repDocTypes.exists(dt => dt != OECD1 && dt != OECD0))) {
      MessageTypeIndicDocTypeIncompatible.invalidNel
    } else {
      r.validNel
    }
  }

  /** Ensure there is not a mixture of OECD1 and other DocTypes within the document */
  private def validateDocTypes(in:List[DocSpec], repIn:Option[DocSpec]) : ValidBusinessResult[List[DocSpec]] = {
    val rep = repIn.map(_.docType)
    val all = in.map(_.docType).toSet ++ rep
    if(all.size > 1 && all.contains(OECD1) && rep.exists(_ != OECD0)) IncompatibleOECDTypes.invalidNel
    else in.validNel
  }

  private def validateDocSpecs(in:XMLInfo)(implicit hc:HeaderCarrier) : FutureValidBusinessResult[XMLInfo] = {
    val addEntDocSpecs = in.cbcReport.map(_.docSpec)  ++ in.additionalInfo.map(_.docSpec)
    val repDocSpec     = in.reportingEntity.map(_.docSpec)
    val allDocSpecs    = addEntDocSpecs ++ repDocSpec

    functorInstance.map(
      allDocSpecs.map(validateDocSpec).sequence[FutureValidBusinessResult, DocSpec] *>
      validateDocTypes(addEntDocSpecs, repDocSpec) *>
      validateDistinctDocRefIds(allDocSpecs.map(_.docRefId)) *>
      validateDistinctCorrDocRefIds(allDocSpecs.map(_.corrDocRefId).flatten)
    )(_ => in)

  }

  /**
    * Ensure that if a [[CorrDocRefId]] is required, it really exist
    * Ensure that if a [[CorrDocRefId]] is not required, it does not exist
    */
  private def validateCorrDocRefIdRequired(d:DocSpec):ValidBusinessResult[DocSpec] = d.docType match {
    case OECD2 | OECD3 if d.corrDocRefId.isEmpty => CorrDocRefIdMissing.invalidNel
    case OECD1 if d.corrDocRefId.isDefined       => CorrDocRefIdNotNeeded.invalidNel
    case _                                       => d.validNel
  }

  /** Ensure that the list of CorrDocRefIds are unique */
  private def validateDistinctCorrDocRefIds(ids:List[CorrDocRefId]): ValidBusinessResult[Unit] =
    Either.cond(ids.distinct.lengthCompare(ids.size) == 0, (), CorrDocRefIdDuplicate).toValidatedNel

  /** Ensure that the list of DocRefIds are unique */
  private def validateDistinctDocRefIds(ids:List[DocRefId]): ValidBusinessResult[Unit] =
    Either.cond(ids.distinct.lengthCompare(ids.size) == 0, (), DocRefIdDuplicate).toValidatedNel

  /** Do further validation on the DocSpec **/
  private def validateDocSpec(d:DocSpec)(implicit hc:HeaderCarrier) : FutureValidBusinessResult[DocSpec] =
    (validateDocRefId(d) zip d.corrDocRefId.map(validateCorrDocRefId).sequence[FutureValidBusinessResult,CorrDocRefId] ).map {
      case (doc,corrDoc) => (doc |@| corrDoc |@| validateCorrDocRefIdRequired(d)).map((_,_,_) => d)
    }

  /** Do further validation on the provided [[CorrDocRefId]] */
  private def validateCorrDocRefId(corrDocRefId: CorrDocRefId)(implicit hc:HeaderCarrier) : FutureValidBusinessResult[CorrDocRefId] = {
    corrDocRefIdDuplicateCheck(corrDocRefId)
  }

  /** Query the [[DocRefIdService]] to find out if this [[CorrDocRefId]] is a duplicate */
  private def corrDocRefIdDuplicateCheck(corrDocRefId: CorrDocRefId)(implicit hc:HeaderCarrier) : FutureValidBusinessResult[CorrDocRefId] = {
    docRefIdService.queryDocRefId(corrDocRefId.cid).map {
      case DocRefIdResponses.Valid        => corrDocRefId.validNel
      case DocRefIdResponses.Invalid      => CorrDocRefIdInvalidRecord.invalidNel
      case DocRefIdResponses.DoesNotExist => CorrDocRefIdUnknownRecord.invalidNel
    }
  }

  /** Do further validation on the provided [[DocRefId]] */
  private def validateDocRefId(docSpec:DocSpec)(implicit hc:HeaderCarrier) : FutureValidBusinessResult[DocRefId] =
    docRefIdDuplicateCheck(docSpec)

  /**
    * Query the [[DocRefIdService]] to find out if this docRefId is a duplicate
    * If the doc type is OECD0, when don't check for duplicates
    */
  private def docRefIdDuplicateCheck(docSpec:DocSpec)(implicit hc:HeaderCarrier) : FutureValidBusinessResult[DocRefId] = {
    if(docSpec.docType == OECD0) docSpec.docRefId.validNel
    else docRefIdService.queryDocRefId(docSpec.docRefId).map {
      case DocRefIdResponses.DoesNotExist => docSpec.docRefId.validNel
      case _                              => DocRefIdDuplicate.invalidNel
    }
  }

  /** Ensure the messageTypes and docTypes are valid and not in conflict */
  private def validateMessageTypeIndic(xmlInfo: XMLInfo) : ValidBusinessResult[XMLInfo] = {

    lazy val CBCReportsAreNeverResent: Boolean = xmlInfo.cbcReport.forall(r => r.docSpec.docType != OECD0 )
    lazy val AdditionalInfoIsNeverResent: Boolean = xmlInfo.additionalInfo.forall(r => r.docSpec.docType != OECD0)

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
      case Some(CBC402) if CBCReportsAreNotAllCorrectionsOrDeletions
        || AdditionalInfoIsNotCorrectionsOrDeletions
        || ReportingEntityIsNotCorrectionsOrDeletionsOrResent           => MessageTypeIndicError.invalidNel
      case _ if CBCReportsAreNeverResent && AdditionalInfoIsNeverResent => xmlInfo.validNel
      case _                                                            => MessageTypeIndicError.invalidNel
    }

  }

  /** Validate the TIN and TIN.issuedBy against the [[ReportingRole]] */
  private def validateTIN(tin:TIN, rr:ReportingRole) : ValidBusinessResult[TIN] = rr match {
    case CBC701 | CBC703 if !tin.issuedBy.equalsIgnoreCase("gb") =>
      InvalidXMLError("ReportingEntity.Entity.TIN@issuedBy must be 'GB' for local or primary filings").invalidNel
    case CBC701 | CBC703 if !Utr(tin.value).isValid              =>
      InvalidXMLError("ReportingEntity.Entity.TIN must be a valid UTR for filings issued in 'GB'").invalidNel
    case _                                                       =>
      tin.validNel
  }

  /** Ensure the provided filename matches the given MessageRefID (minus the extension) */
  private def validateFileName(in:XMLInfo, fileName:String) : ValidBusinessResult[XMLInfo] =
    if(fileName.split("""\.""").headOption.contains(in.messageSpec.messageRefID.show)) in.validNel
    else { FileNameError(fileName, s"${in.messageSpec.messageRefID.show}.xml").invalidNel }

  /**
    * Ensure SendingEntityIn CBCId is:
    * 1) Not a private Beta CBCId
    * 2) Has already been registered
    */
  private def validateSendingEntity(cbcId:CBCId)(implicit hc:HeaderCarrier) : FutureValidBusinessResult[CBCId] =
    if (CBCId.isPrivateBetaCBCId(cbcId)) Future.successful(PrivateBetaCBCIdError.invalidNel[CBCId])
    else {
      subscriptionDataService.retrieveSubscriptionData(Right(cbcId)).fold[ValidBusinessResult[CBCId]](
        (_: CBCErrors) => SendingEntityError.invalidNel,
        (maybeDetails: Option[SubscriptionDetails]) => maybeDetails match {
          case None    => SendingEntityError.invalidNel
          case Some(_) => cbcId.validNel
        }
      )
    }

  /** Ensure the [[CBCId]] found in the [[MessageRefID]] matches the [[CBCId]] in the SendingEntityIN field */
  private def validateCBCId(messageRefID: MessageRefID, messageSpec: MessageSpec) : ValidBusinessResult[MessageRefID] =
    if(messageRefID.cBCId == messageSpec.sendingEntityIn) { messageRefID.validNel}
    else MessageRefIDCBCIdMismatch.invalidNel

  /** If the User is an enrolled Organisation, ensure their CBCId matches the  CBCId in the Sending Entity **/
  private def validateOrganisationCBCId(in:XMLInfo)(implicit hc: HeaderCarrier) : FutureValidBusinessResult[XMLInfo]  =
    cache.readOption[CBCId].map { (maybeCBCId: Option[CBCId]) =>
      maybeCBCId match {
        case Some(organisationCBCId) => if (organisationCBCId == in.messageSpec.sendingEntityIn) in.validNel
                                        else SendingEntityOrganisationMatchError.invalidNel
        case None => in.validNel
      }
    }

  /** Ensure the reportingPeriod in the [[MessageRefID]] matches the reportingPeriod field in the MessageSpec */
  private def validateReportingPeriodMatches(messageRefID: MessageRefID,messageSpec:MessageSpec) : ValidBusinessResult[MessageRefID] =
    if(messageRefID.reportingPeriod.getValue == messageSpec.reportingPeriod.getYear) { messageRefID.validNel }
    else { MessageRefIDReportingPeriodMismatch.invalidNel }

  /** Calls the [[MessageRefIdService]] to see if this provided [[MessageRefID]] already exists */
  private def isADuplicate(msgRefId:MessageRefID)(implicit hc:HeaderCarrier) : FutureValidBusinessResult[MessageRefID] =
    messageRefService.messageRefIdExists(msgRefId).map(result =>
      if(result) MessageRefIDDuplicate.invalidNel else msgRefId.validNel
    )

  /**
    * Validates the messageRefID by performing 3 checks:
    * 1) Checks if a duplicate messageRefID exists (by calling the backend)
    * 2) Checks that the SendingEntityIn in the messageSpec matches the CBCId in the messageRefId
    * 3) Checks the reportingPeriod in the messageRefID matches the reportingPeriod in the messageSpec
    */
  private def validateMessageRefIdD(messageSpec: MessageSpec)(implicit hc:HeaderCarrier) : FutureValidBusinessResult[MessageRefID] = {
    validateSendingEntity(messageSpec.sendingEntityIn) *>
    isADuplicate(messageSpec.messageRefID) *>
    validateCBCId(messageSpec.messageRefID,messageSpec) *>
    validateReportingPeriodMatches(messageSpec.messageRefID, messageSpec)
  }

  private def validateCreationDate(xmlInfo: XMLInfo)(implicit hc:HeaderCarrier) : FutureValidBusinessResult[XMLInfo] ={
    lazy val CBCReportsAreContainsCorrectionsOrDeletions: Boolean = xmlInfo.cbcReport.exists(r =>
      r.docSpec.docType == OECD2 ||
      r.docSpec.docType == OECD3
    )

    lazy val AdditionalInfoContainsCorrectionsOrDeletions: Boolean = xmlInfo.additionalInfo.exists(r =>
      r.docSpec.docType == OECD2 ||
      r.docSpec.docType == OECD3
    )

    lazy val ReportingEntityContainsCorrectionsOrDeletionsOrResent: Boolean = xmlInfo.reportingEntity.exists(r =>
      r.docSpec.docType == OECD2 ||
      r.docSpec.docType == OECD3 ||
      r.docSpec.docType == OECD0
    )

    if (CBCReportsAreContainsCorrectionsOrDeletions || AdditionalInfoContainsCorrectionsOrDeletions || ReportingEntityContainsCorrectionsOrDeletionsOrResent) {
      creationDateService.checkDate(xmlInfo).map(result =>
        if(result) xmlInfo.validNel else CorrectedFileToOld.invalidNel
      )
    } else {
      xmlInfo.validNel
    }

  }

  def validateBusinessRules(in: RawXMLInfo, fileName: String)(implicit hc: HeaderCarrier): FutureValidBusinessResult[XMLInfo] =
    extractXMLInfo(in).flatMap{
      case Valid(v)   => validateXMLInfo(v,fileName)
      case Invalid(i) => Future.successful(i.invalid)
    }

  def recoverReportingEntity(in:XMLInfo)(implicit hc: HeaderCarrier) : FutureValidBusinessResult[CompleteXMLInfo] = in.reportingEntity match {
    case Some(re) => Future.successful(CompleteXMLInfo(in.messageSpec,re,in.cbcReport,in.additionalInfo, in.creationDate).validNel)
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
