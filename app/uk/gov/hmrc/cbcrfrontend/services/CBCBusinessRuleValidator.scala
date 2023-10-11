/*
 * Copyright 2023 HM Revenue & Customs
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

import cats.data.Validated.{Invalid, Valid}
import cats.data._
import cats.instances.all._
import cats.syntax.all._
import play.api.{Configuration, Logger}
import uk.gov.hmrc.auth.core.AffinityGroup.{Agent, Organisation}
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.util.BusinessRulesUtil
import uk.gov.hmrc.cbcrfrontend.{FutureValidBusinessResult, ValidBusinessResult, applicativeInstance, functorInstance}
import uk.gov.hmrc.http.HeaderCarrier

import java.time.LocalDate
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

/**
  * This class exposes two methods:
  *
  * 1) [[validateBusinessRules()]] which takes a [[RawXMLInfo]] and returns an [[XMLInfo]] or a list of [[BusinessRuleErrors]]
  *
  * 2) [[recoverReportingEntity()]] which, given an [[XMLInfo]] will return a [[ReportingEntity]], extracting it either
  * from the XML object, or from our [[ReportingEntityDataService]]
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
class CBCBusinessRuleValidator @Inject()(
  messageRefService: MessageRefIdService,
  docRefIdService: DocRefIdService,
  subscriptionDataService: SubscriptionDataService,
  reportingEntityDataService: ReportingEntityDataService,
  configuration: Configuration,
  creationDateService: CreationDateService)(implicit ec: ExecutionContext, cache: CBCSessionCache) {

  private val testData = "OECD1[0123]"

  private val cbcVersion: String = configuration.get[String]("Prod.oecd-schema-version")

  lazy val logger: Logger = Logger(this.getClass)

  // <<<<<<<<<<<<:::Extraction Methods:::>>>>>>>>>>>>

  /** Top level extraction method */
  private def extractXMLInfo(in: RawXMLInfo): ValidBusinessResult[XMLInfo] =
    if (in.numBodies > 1) MultipleCbcBodies.invalidNel[XMLInfo]
    else {
      (extractCbcOecdVersion(in.cbcVal) *>
        in.xmlEncoding.map(extractXmlEncodingVal).sequence[ValidBusinessResult, Unit] *>
        extractMessageSpec(in.messageSpec),
        in.reportingEntity.map(extractReportingEntity).sequence[ValidBusinessResult, ReportingEntity],
        in.cbcReport.map(extractCBCReports).sequence[ValidBusinessResult, CbcReports],
        in.additionalInfo.map(extractAdditionalInfo).sequence[ValidBusinessResult, AdditionalInfo])
        .mapN(
          XMLInfo(
            _,
            _,
            _,
            _,
            Some(LocalDate.now()),
            in.constEntityNames,
            in.currencyCodes.flatMap(elem => elem.currCodes)))
    }

  private def extractMessageSpec(in: RawMessageSpec): ValidBusinessResult[MessageSpec] =
    (
      extractMessageRefID(in),
        extractReceivingCountry(in),
        extractSendingEntityIn(in),
        extractReportingPeriod(in),
        extractMessageTypeIndic(in)
    ).mapN(
      (messageRefID, receivingCountry, sendingEntityIn, reportingPeriod, messageTypeInic) =>
        MessageSpec(
          messageRefID,
          receivingCountry,
          sendingEntityIn,
          messageRefID.creationTimestamp,
          reportingPeriod,
          messageTypeInic,
          in.corrMessageRefId
      )
    )

  private def extractReportingEntity(in: RawReportingEntity): ValidBusinessResult[ReportingEntity] =
    (extractReportingRole(in),
      extractDocSpec(in.docSpec, ENT),
      extractTIN(in),
      extractEntityReportingPeriod(in)).mapN(ReportingEntity(_, _, _, in.name, in.city, _))

  private def extractCBCReports(in: RawCbcReports): ValidBusinessResult[CbcReports] =
    extractDocSpec(in.docSpec, REP).map(CbcReports(_))

  private def extractAdditionalInfo(in: RawAdditionalInfo): ValidBusinessResult[AdditionalInfo] =
    extractDocSpec(in.docSpec, ADD).map(AdditionalInfo(_, in.otherInfo))

  private def extractDocSpec(d: RawDocSpec, parentGroupElement: ParentGroupElement): ValidBusinessResult[DocSpec] =
    (extractDocTypeInidc(d.docType),
      extractDocRefId(d.docRefId, parentGroupElement),
      extractCorrDocRefId(d.corrDocRefId, parentGroupElement)).mapN(DocSpec(_, _, _, d.corrMessageRefId))

  private def extractDocTypeInidc(docType: String): ValidBusinessResult[DocTypeIndic] =
    DocTypeIndic
      .fromString(docType)
      .fold[ValidBusinessResult[DocTypeIndic]] {
        if (docType.matches(testData)) TestDataError.invalidNel
        else InvalidXMLError("xmlValidationError.InvalidDocType").invalidNel
      }(_.validNel)

  private def extractCorrDocRefId(
    corrDocRefIdString: Option[String],
    parentGroupElement: ParentGroupElement): ValidBusinessResult[Option[CorrDocRefId]] =
    corrDocRefIdString
      .map(DocRefId(_).fold[ValidBusinessResult[Option[CorrDocRefId]]](InvalidCorrDocRefId.invalidNel)(d => {
        if (d.parentGroupElement != parentGroupElement) CorrDocRefIdInvalidParentGroupElement.invalidNel
        else Some(CorrDocRefId(d)).validNel
      }))
      .getOrElse(None.validNel)

  private def extractDocRefId(
    docRefIdString: String,
    parentGroupElement: ParentGroupElement): ValidBusinessResult[DocRefId] =
    DocRefId
      .applyNewDocRefIdRegex(docRefIdString)
      .fold[ValidBusinessResult[DocRefId]](InvalidDocRefId.invalidNel)(d => {
        if (d.parentGroupElement != parentGroupElement) DocRefIdInvalidParentGroupElement.invalidNel
        else d.validNel
      })

  private def extractMessageTypeIndic(r: RawMessageSpec): ValidBusinessResult[Option[MessageTypeIndic]] =
    r.messageType.flatMap(MessageTypeIndic.parseFrom).validNel[BusinessRuleErrors]

  private def extractTIN(in: RawReportingEntity): ValidBusinessResult[TIN] = TIN(in.tin, in.tinIssuedBy).validNel

  private def extractReportingRole(in: RawReportingEntity): ValidBusinessResult[ReportingRole] =
    ReportingRole.parseFromString(in.reportingRole).toValidNel(InvalidXMLError("xmlValidationError.ReportingRole"))

  private def extractSendingEntityIn(in: RawMessageSpec): ValidBusinessResult[CBCId] =
    CBCId(in.sendingEntityIn).fold[ValidBusinessResult[CBCId]](SendingEntityError.invalidNel[CBCId])(cbcId => {
      cbcId.validNel
    })

  private def extractReceivingCountry(in: RawMessageSpec): ValidBusinessResult[String] =
    if (in.receivingCountry equalsIgnoreCase "GB") in.receivingCountry.validNel else ReceivingCountryError.invalidNel

  private def extractReportingPeriod(in: RawMessageSpec): ValidBusinessResult[LocalDate] =
    Validated
      .catchNonFatal(LocalDate.parse(in.reportingPeriod))
      .leftMap(_ => InvalidXMLError("xmlValidationError.InvalidDate"))
      .toValidatedNel

  private def extractEntityReportingPeriod(in: RawReportingEntity): ValidBusinessResult[EntityReportingPeriod] =
    Validated
      .catchNonFatal(EntityReportingPeriod(LocalDate.parse(in.startDate), LocalDate.parse(in.endDate)))
      .leftMap(_ => InvalidXMLError("xmlValidationError.InvalidDate"))
      .toValidatedNel

  private def extractMessageRefID(in: RawMessageSpec): ValidBusinessResult[MessageRefID] =
    MessageRefID(in.messageRefID).fold(
      errors => errors.invalid[MessageRefID],
      msgRefId => msgRefId.validNel[MessageRefIDError]
    )

  /** These are dummy extraction methods - they're actually performing validation, but we dont' actually care
    * about the values later on */
  private def extractXmlEncodingVal(xe: RawXmlEncodingVal): ValidBusinessResult[Unit] =
    if (!xe.xmlEncodingVal.equalsIgnoreCase("UTF-8")) XmlEncodingError.invalidNel
    else ().validNel

  private def extractCbcOecdVersion(cv: RawCbcVal): ValidBusinessResult[Unit] =
    if (cv.cbcVer != cbcVersion) CbcOecdVersionError.invalidNel
    else ().validNel

  // <<<<<<<<<<:::Validation methods:::>>>>>>>>>>>>>

  /** Top level validation methods */
  private def validateXMLInfo(
    x: XMLInfo,
    fileName: String,
    enrolment: Option[CBCEnrolment],
    affinityGroup: Option[AffinityGroup])(implicit hc: HeaderCarrier): FutureValidBusinessResult[XMLInfo] =
    validateMessageRefIdD(x.messageSpec) *>
      validateCorrMessageRefIdD(x) *>
      validateReportingEntity(x) *>
      validateMessageTypes(x) *>
      validateDocSpecs(x) *>
      validateMessageTypeIndic(x) *>
      validateFileName(x, fileName) *>
      validateOrganisationCBCId(x, enrolment, affinityGroup) *>
      validateCreationDate(x) *>
      validateReportingPeriod(x) *>
      validateOtherInfo(x) *>
      validateMultipleFileUploadForSameReportingPeriod(x) *>
      validateMessageRefIds(x) *>
      validateCurrencyCodes(x) *>
      validateDeletion(x) *>
      validateDatesNotOverlapping(x)

  private def validateReportingEntity(in: XMLInfo)(implicit hc: HeaderCarrier): FutureValidBusinessResult[XMLInfo] =
    in.reportingEntity
      .map { re =>
        val docRefId = if (re.docSpec.docType == OECD0) {
          ensureDocRefIdExists(re.docSpec.docRefId)
        } else {
          Future.successful(re.docSpec.docRefId.validNel)
        }

        (docRefId *>
          validateTIN(re.tin, re.reportingRole) *>
          validateReportingEntityName(re) *>
          validateReportingEntityAddressCity(re) *>
          validateRepPeriodSameAsEndDate(re, in.messageSpec.reportingPeriod) *>
          validateStartDateBeforeEndDate(re) *>
          validateDatesNotInFuture(re, in.messageSpec.reportingPeriod) *>
          validateStartDateOnlyAfter01012016(re) *>
          validateConstEntities(in.constEntityNames)).map(_.andThen(_ => in.validNel))

      }
      .getOrElse(Future.successful(ReportingEntityElementMissing.invalidNel))

  private def validateReportingEntityName(entity: ReportingEntity): ValidBusinessResult[ReportingEntity] =
    if (entity.name.trim.nonEmpty) entity.validNel
    else ReportingEntityOrConstituentEntityEmpty.invalidNel

  private def validateReportingEntityAddressCity(entity: ReportingEntity): ValidBusinessResult[ReportingEntity] =
    entity.city match {
      case Some(x) => if (x.trim.isEmpty) AddressCityEmpty.invalidNel else entity.validNel
      case None    => entity.validNel
    }

  private def validateOtherInfo(xmlInfo: XMLInfo): ValidBusinessResult[XMLInfo] =
    if (xmlInfo.additionalInfo.forall(_.otherInfo.trim.nonEmpty)) xmlInfo.validNel
    else OtherInfoEmpty.invalidNel

  private def validateStartDateBeforeEndDate(entity: ReportingEntity): ValidBusinessResult[ReportingEntity] =
    if (entity.entityReportingPeriod.startDate.isAfter(entity.entityReportingPeriod.endDate)
        || entity.entityReportingPeriod.startDate.equals(entity.entityReportingPeriod.endDate))
      StartDateAfterEndDate.invalidNel
    else
      entity.validNel

  private def validateDatesNotInFuture(
    entity: ReportingEntity,
    reportingPeriod: LocalDate): ValidBusinessResult[ReportingEntity] = {
    val currentDate = LocalDate.now()
    if (reportingPeriod.isAfter(currentDate)
        || entity.entityReportingPeriod.startDate.isAfter(currentDate)
        || entity.entityReportingPeriod.endDate.isAfter(currentDate))
      AllReportingdatesInFuture.invalidNel
    else
      entity.validNel
  }

  private def validateRepPeriodSameAsEndDate(
    entity: ReportingEntity,
    reportingPeriod: LocalDate): ValidBusinessResult[ReportingEntity] =
    if (!entity.entityReportingPeriod.endDate.equals(reportingPeriod))
      EndDateSameAsReportingPeriod.invalidNel
    else
      entity.validNel

  private def validateStartDateOnlyAfter01012016(entity: ReportingEntity): ValidBusinessResult[ReportingEntity] = {
    val historicalStartDate = LocalDate.of(2016, 1, 1)
    if (entity.entityReportingPeriod.startDate.isBefore(historicalStartDate))
      StartDateNotBefore01012016.invalidNel
    else
      entity.validNel
  }

  private def validateConstEntities(reports: List[String]): ValidBusinessResult[List[String]] =
    if (reports.forall(_.trim.nonEmpty)) reports.validNel
    else ReportingEntityOrConstituentEntityEmpty.invalidNel

  private def ensureDocRefIdExists(docRefId: DocRefId)(
    implicit hc: HeaderCarrier): FutureValidBusinessResult[DocRefId] = {
    val res1: EitherT[Future, Boolean, Boolean] = reportingEntityDataService
      .queryReportingEntityDataDocRefId(docRefId)
      .leftMap(cbcErrors => {
        logger.error(s"Got error back: $cbcErrors")
        throw new Exception(s"Error communicating with backend: $cbcErrors")
      })
      .subflatMap {
        case Some(_) => Right(true)
        case None    => Left(false)
      }
    for {
      isValid: Boolean <- res1.isRight
      result <- if (isValid) {
                 val docRefIdToCheckForDeleteValid = docRefIdService.queryDocRefId(docRefId)
                 docRefIdToCheckForDeleteValid.map {
                   case DocRefIdResponses.Invalid => Left(ResendDocRefIdInvalid)
                   case _                         => Right(docRefId)
                 }
               } else {
                 docRefIdResendCheck(docRefId).map(d => Left(d))
               }
    } yield {
      result.toValidatedNel
    }
  }

  private def determineMessageTypeIndic(r: XMLInfo): Option[MessageTypeIndic] = {
    lazy val docTypes = List(r.additionalInfo.map(_.docSpec.docType)).flatten ++ r.cbcReport.map(_.docSpec.docType) ++ r.reportingEntity
      .map(_.docSpec.docType)

    lazy val distinctTypes = docTypes.distinct

    lazy val docType401 = distinctTypes.contains(OECD1) && distinctTypes.size == 1

    (r.messageSpec.messageType, docType401) match {
      case (Some(CBC401), true)  => Some(CBC401)
      case (Some(CBC401), false) => None
      case (Some(CBC402), _)     => Some(CBC402)
      case (None, true)          => Some(CBC401)
      case (None, false)         => Some(CBC402)
      case _                     => None
    }

  }

  /** Ensure that if the messageType is [[CBC401]] there are no [[DocTypeIndic]] other than [[OECD1]] */
  private def validateMessageTypes(r: XMLInfo): ValidBusinessResult[XMLInfo] =
    r.messageSpec.messageType.contains(CBC401) match {
      case true if determineMessageTypeIndic(r).contains(CBC401) => r.validNel
      case true                                                   => MessageTypeIndicDocTypeIncompatible.invalidNel
      case false                                                  => r.validNel
    }

  /** Ensure there is not a mixture of OECD1 and other DocTypes within the document */
  private def validateDocTypes(in: List[DocSpec], repIn: Option[DocSpec]): ValidBusinessResult[List[DocSpec]] = {
    val rep = repIn.map(_.docType)
    val all = in.map(_.docType).toSet ++ rep
    if (all.size > 1 && all.contains(OECD1) && rep.exists(_ != OECD0)) IncompatibleOECDTypes.invalidNel
    else in.validNel
  }

  private def validateDocSpecs(in: XMLInfo)(implicit hc: HeaderCarrier): FutureValidBusinessResult[XMLInfo] = {
    val addDocSpec = in.additionalInfo.map(_.docSpec)
    val entDocSpecs = in.cbcReport.map(_.docSpec)
    val repDocSpec = in.reportingEntity.map(_.docSpec)
    val allDocSpecs = entDocSpecs ++ repDocSpec ++ addDocSpec
    val addCorrCheck = addDocSpec.flatMap(_.corrDocRefId).map(c => (c, extractCorrDRI(in).get))

    functorInstance.map(
      allDocSpecs.map(validateDocSpec).sequence[FutureValidBusinessResult, DocSpec] *>
        validateDocTypes(entDocSpecs, repDocSpec) *>
        validateDistinctDocRefIds(allDocSpecs.map(_.docRefId)) *>
        allDocSpecs.map(docRefIdMatchDocTypeIndicCheck).sequence[ValidBusinessResult, DocRefId] *>
        validateDistinctCorrDocRefIds(allDocSpecs.flatMap(_.corrDocRefId)) *>
        addCorrCheck.map(validateAddInfoCorrDRI).sequence[FutureValidBusinessResult, CorrDocRefId]
    )(_ => in)

  }

  /**
    * Ensure that if a [[CorrDocRefId]] is required, it really exist
    * Ensure that if a [[CorrDocRefId]] is not required, it does not exist
    */
  private def validateCorrDocRefIdRequired(d: DocSpec): ValidBusinessResult[DocSpec] = d.docType match {
    case OECD2 | OECD3 if d.corrDocRefId.isEmpty => CorrDocRefIdMissing.invalidNel
    case OECD1 if d.corrDocRefId.isDefined       => CorrDocRefIdNotNeeded.invalidNel
    case _                                       => d.validNel
  }

  /** Ensure that the list of CorrDocRefIds are unique */
  private def validateDistinctCorrDocRefIds(ids: List[CorrDocRefId]): ValidBusinessResult[Unit] =
    Either.cond(ids.distinct.lengthCompare(ids.size) == 0, (), CorrDocRefIdDuplicate).toValidatedNel

  /** Ensure that the list of DocRefIds are unique */
  private def validateDistinctDocRefIds(ids: List[DocRefId]): ValidBusinessResult[Unit] =
    Either.cond(ids.distinct.lengthCompare(ids.size) == 0, (), DocRefIdDuplicate).toValidatedNel

  /** Do further validation on the DocSpec **/
  private def validateDocSpec(d: DocSpec)(implicit hc: HeaderCarrier): FutureValidBusinessResult[DocSpec] =
    (validateDocRefId(d) zip d.corrDocRefId.map(validateCorrDocRefId).sequence[FutureValidBusinessResult, CorrDocRefId])
      .map {
        case (doc, corrDoc) => (doc, corrDoc, validateCorrDocRefIdRequired(d)).mapN((_, _, _) => d)
      }

  /** Check if only 1st AddInfoCorrDocRefId was saved (before issue corrected) and if so And AddInfo coorDocRefId not found then show appropriate error */
  private def validateAddInfoCorrDRI(aiCid: (CorrDocRefId, CorrDocRefId))(
    implicit hc: HeaderCarrier): FutureValidBusinessResult[CorrDocRefId] =
    reportingEntityDataService
      .queryReportingEntityDataModel(aiCid._2.cid)
      .fold(
        (errors: CBCErrors) => throw new Exception(s"Error communicating with backend: $errors"),
        (maybeModel: Option[ReportingEntityDataModel]) =>
          maybeModel.toValidNel[BusinessRuleErrors](
            AdditionalInfoDRINotFound("error.AdditionalInfoDRINotFound5", aiCid._1.cid.show))
      )
      .flatMap(_.fold(
        (value: NonEmptyList[BusinessRuleErrors]) => Future.successful(value.invalid),
        (red: ReportingEntityDataModel) =>
          docRefIdService.queryDocRefId(aiCid._1.cid).map {
            case DocRefIdResponses.Valid => aiCid._1.validNel
            case DocRefIdResponses.DoesNotExist =>
              if (red.oldModel) AdditionalInfoDRINotFound(red.additionalInfoDRI.head.show, aiCid._1.cid.show).invalidNel
              else aiCid._1.validNel //To avoid duplicate error messages as this is also checked as part of another rule
            case _ => aiCid._1.validNel //To avoid duplicate error messages
        }
      ))

  /** Do further validation on the provided [[CorrDocRefId]] */
  private def validateCorrDocRefId(corrDocRefId: CorrDocRefId)(
    implicit hc: HeaderCarrier): FutureValidBusinessResult[CorrDocRefId] =
    corrDocRefIdDuplicateCheck(corrDocRefId)

  /** Query the [[DocRefIdService]] to find out if this [[CorrDocRefId]] is a duplicate */
  private def corrDocRefIdDuplicateCheck(corrDocRefId: CorrDocRefId)(
    implicit hc: HeaderCarrier): FutureValidBusinessResult[CorrDocRefId] =
    docRefIdService.queryDocRefId(corrDocRefId.cid).map {
      case DocRefIdResponses.Valid        => corrDocRefId.validNel
      case DocRefIdResponses.Invalid      => CorrDocRefIdInvalidRecord.invalidNel
      case DocRefIdResponses.DoesNotExist => CorrDocRefIdUnknownRecord.invalidNel
    }

  /** Do further validation on the provided [[DocRefId]] */
  private def validateDocRefId(docSpec: DocSpec)(implicit hc: HeaderCarrier): FutureValidBusinessResult[DocRefId] =
    docRefIdDuplicateCheck(docSpec)

  /**
    * Query the [[DocRefIdService]] to find out if this docRefId is a duplicate
    * If the doc type is OECD0, when don't check for duplicates
    */
  private def docRefIdDuplicateCheck(docSpec: DocSpec)(
    implicit hc: HeaderCarrier): FutureValidBusinessResult[DocRefId] =
    if (docSpec.docType == OECD0) docSpec.docRefId.validNel
    else
      docRefIdService.queryDocRefId(docSpec.docRefId).map {
        case DocRefIdResponses.DoesNotExist => docSpec.docRefId.validNel
        case _                              => DocRefIdDuplicate.invalidNel
      }

  /**
    * Query the [[DocRefIdService]] to find out if this docRefId is a duplicate
    * If the doc type is OECD0, when don't check for duplicates
    */
  private def docRefIdResendCheck(d: DocRefId)(implicit hc: HeaderCarrier): Future[BusinessRuleErrors] =
    docRefIdService.queryDocRefId(d).map {
      case DocRefIdResponses.Invalid => ResendDocRefIdInvalid
      case _                         => ResentDataIsUnknownError
    }

  private def docRefIdMatchDocTypeIndicCheck(docSpec: DocSpec): ValidBusinessResult[DocRefId] = {
    val docRefId = docSpec.docRefId
    docSpec.docType match {
      case OECD0 => docRefId.validNel
      case _ =>
        if (docRefId.docTypeIndic == docSpec.docType)
          docRefId.validNel
        else
          DocRefIdMismatch.invalidNel
    }
  }

  /** Ensure the messageTypes and docTypes are valid and not in conflict */
  private def validateMessageTypeIndic(xmlInfo: XMLInfo): ValidBusinessResult[XMLInfo] = {

    lazy val CBCReportsAreNeverResent: Boolean = xmlInfo.cbcReport.forall(r => r.docSpec.docType != OECD0)
    lazy val AdditionalInfoIsNeverResent: Boolean = xmlInfo.additionalInfo.forall(r => r.docSpec.docType != OECD0)

    lazy val CBCReportsAreNotAllCorrectionsOrDeletions: Boolean = !xmlInfo.cbcReport.forall(
      r =>
        r.docSpec.docType == OECD2 ||
          r.docSpec.docType == OECD3 ||
          r.docSpec.docType == OECD0)

    lazy val AdditionalInfoIsNotCorrectionsOrDeletions: Boolean = !xmlInfo.additionalInfo.forall(
      r =>
        r.docSpec.docType == OECD2 ||
          r.docSpec.docType == OECD3 ||
          r.docSpec.docType == OECD0)

    lazy val ReportingEntityIsNotCorrectionsOrDeletionsOrResent: Boolean = !xmlInfo.reportingEntity.forall(
      r =>
        r.docSpec.docType == OECD2 ||
          r.docSpec.docType == OECD3 ||
          r.docSpec.docType == OECD0)

    val messaggeTypeIndic = xmlInfo.messageSpec.messageType
    val determinedMessageTypeIndic = determineMessageTypeIndic(xmlInfo)

    messaggeTypeIndic match {
      case Some(CBC402)
          if CBCReportsAreNotAllCorrectionsOrDeletions
            || AdditionalInfoIsNotCorrectionsOrDeletions
            || ReportingEntityIsNotCorrectionsOrDeletionsOrResent =>
        MessageTypeIndicError.invalidNel
      case Some(CBCInvalidMessageTypeIndic)                                     => MessageTypeIndicInvalid.invalidNel
      case Some(_) if CBCReportsAreNeverResent && AdditionalInfoIsNeverResent   => xmlInfo.validNel
      case Some(_) if !CBCReportsAreNeverResent || !AdditionalInfoIsNeverResent => ResendOutsideRepEntError.invalidNel
      case Some(CBC401) if determinedMessageTypeIndic.isEmpty                   => MessageTypeIndicError.invalidNel
      case _                                                                    => MessageTypeIndicBlank.invalidNel
    }

  }

  /** Validate the TIN and TIN.issuedBy against the [[ReportingRole]] */
  private def validateTIN(tin: TIN, rr: ReportingRole): ValidBusinessResult[TIN] = rr match {
    case CBC701 | CBC703 | CBC704 if !tin.issuedBy.equalsIgnoreCase("gb") =>
      InvalidXMLError("xmlValidationError.TINIssuedBy").invalidNel
    case CBC701 | CBC703 | CBC704 if !Utr(tin.value).isValid =>
      InvalidXMLError("xmlValidationError.InvalidTIN").invalidNel
    case _ =>
      tin.validNel
  }

  /** Ensure the provided filename matches the given MessageRefID (minus the extension) */
  private def validateFileName(in: XMLInfo, fileName: String): ValidBusinessResult[XMLInfo] =
    if (fileName.split("""\.""").headOption.contains(in.messageSpec.messageRefID.show)) in.validNel
    else {
      FileNameError(fileName, s"${in.messageSpec.messageRefID.show}.xml").invalidNel
    }

  /**
    * Ensure SendingEntityIn CBCId is:
    * 1) Not a private Beta CBCId
    * 2) Has already been registered
    */
  private def validateSendingEntity(cbcId: CBCId)(implicit hc: HeaderCarrier): FutureValidBusinessResult[CBCId] =
    subscriptionDataService
      .retrieveSubscriptionData(Right(cbcId))
      .fold[ValidBusinessResult[CBCId]](
        (_: CBCErrors) => SendingEntityError.invalidNel,
        (maybeDetails: Option[SubscriptionDetails]) =>
          maybeDetails match {
            case None    => SendingEntityError.invalidNel
            case Some(_) => cbcId.validNel
        }
      )

  /** Ensure the [[CBCId]] found in the [[MessageRefID]] matches the [[CBCId]] in the SendingEntityIN field */
  private def validateCBCId(messageRefID: MessageRefID, messageSpec: MessageSpec): ValidBusinessResult[MessageRefID] =
    if (messageRefID.cBCId == messageSpec.sendingEntityIn) {
      messageRefID.validNel
    } else MessageRefIDCBCIdMismatch.invalidNel

  /** If the User is an enrolled Organisation, ensure their CBCId matches the  CBCId in the Sending Entity **/
  private def validateOrganisationCBCId(
    in: XMLInfo,
    enrolment: Option[CBCEnrolment],
    affinityGroup: Option[AffinityGroup])(implicit hc: HeaderCarrier): FutureValidBusinessResult[XMLInfo] =
    (affinityGroup, enrolment) match {
      case (Some(Organisation), Some(enrolment)) =>
        if (enrolment.cbcId.value == in.messageSpec.sendingEntityIn.value) in.validNel
        else SendingEntityOrganisationMatchError.invalidNel[XMLInfo]
      case (Some(Organisation), None) =>
        cache.readOption[CBCId].map { (maybeCBCId: Option[CBCId]) =>
          maybeCBCId match {
            case Some(maybeCBCId) =>
              if (maybeCBCId.value == in.messageSpec.sendingEntityIn.value) in.validNel
              else SendingEntityOrganisationMatchError.invalidNel[XMLInfo]
            case _ => in.validNel //user has logged in as an org but with an unregistered cbcId
          }
        }
      case (Some(Agent), _) => in.validNel
      case _                => SendingEntityOrganisationMatchError.invalidNel[XMLInfo]
    }

  /** Ensure the reportingPeriod in the [[MessageRefID]] matches the reportingPeriod field in the MessageSpec */
  private def validateReportingPeriodMatches(
    messageRefID: MessageRefID,
    messageSpec: MessageSpec): ValidBusinessResult[MessageRefID] =
    if (messageRefID.reportingPeriod.getValue == messageSpec.reportingPeriod.getYear) {
      messageRefID.validNel
    } else {
      MessageRefIDReportingPeriodMismatch.invalidNel
    }

  /** Calls the [[MessageRefIdService]] to see if this provided [[MessageRefID]] already exists */
  private def isADuplicate(msgRefId: MessageRefID)(
    implicit hc: HeaderCarrier): FutureValidBusinessResult[MessageRefID] =
    messageRefService
      .messageRefIdExists(msgRefId)
      .map(result => if (result) MessageRefIDDuplicate.invalidNel else msgRefId.validNel)

  /**
    * Validates the messageRefID by performing 3 checks:
    * 1) Checks if a duplicate messageRefID exists (by calling the backend)
    * 2) Checks that the SendingEntityIn in the messageSpec matches the CBCId in the messageRefId
    * 3) Checks the reportingPeriod in the messageRefID matches the reportingPeriod in the messageSpec
    */
  private def validateMessageRefIdD(messageSpec: MessageSpec)(
    implicit hc: HeaderCarrier): FutureValidBusinessResult[MessageRefID] =
    validateSendingEntity(messageSpec.sendingEntityIn) *>
      isADuplicate(messageSpec.messageRefID) *>
      validateCBCId(messageSpec.messageRefID, messageSpec) *>
      validateReportingPeriodMatches(messageSpec.messageRefID, messageSpec)

  private def validateCreationDate(xmlInfo: XMLInfo)(implicit hc: HeaderCarrier): FutureValidBusinessResult[XMLInfo] = {
    lazy val CBCReportsContainCorrectionsOrDeletions: Boolean = xmlInfo.cbcReport.exists(
      r =>
        r.docSpec.docType == OECD2 ||
          r.docSpec.docType == OECD3)

    lazy val AdditionalInfoContainsCorrectionsOrDeletions: Boolean = xmlInfo.additionalInfo.exists(
      r =>
        r.docSpec.docType == OECD2 ||
          r.docSpec.docType == OECD3)

    lazy val ReportingEntityContainsCorrectionsOrDeletionsOrResent: Boolean = xmlInfo.reportingEntity.exists(
      r =>
        r.docSpec.docType == OECD2 ||
          r.docSpec.docType == OECD3 ||
          r.docSpec.docType == OECD0)

    import uk.gov.hmrc.cbcrfrontend.services.xmlStatusEnum
    if (CBCReportsContainCorrectionsOrDeletions || AdditionalInfoContainsCorrectionsOrDeletions || ReportingEntityContainsCorrectionsOrDeletionsOrResent) {
      creationDateService
        .isDateValid(xmlInfo)
        .map {
          case xmlStatusEnum.dateCorrect => xmlInfo.validNel
          case xmlStatusEnum.dateMissing => CorrectedFileDateMissing.invalidNel
          case xmlStatusEnum.dateError => CorrectedFileDateMissing.invalidNel
          case xmlStatusEnum.dateOld => CorrectedFileTooOld.invalidNel
        }
    } else {
      xmlInfo.validNel
    }

  }

  private def validateReportingPeriod(xmlInfo: XMLInfo)(
    implicit hc: HeaderCarrier): FutureValidBusinessResult[XMLInfo] =
    determineMessageTypeIndic(xmlInfo) match {
      case Some(CBC401) => xmlInfo.validNel
      case Some(CBC402) =>
        val crid = extractCorrDRI(xmlInfo)

        crid
          .map { drid =>
            reportingEntityDataService
              .queryReportingEntityData(drid.cid)
              .leftMap { cbcErrors =>
                {
                  logger.error(s"Got error back: $cbcErrors")
                  throw new Exception(s"Error communicating with backend: $cbcErrors")
                }
              }
              .subflatMap {
                case Some(red) if red.reportingPeriod.isDefined =>
                  if (red.reportingPeriod.get == xmlInfo.messageSpec.reportingPeriod) Right(xmlInfo)
                  else Left(CorrectedFileDateMissing)
                case Some(_) =>
                  Right(xmlInfo) //reportingPeriod not persisted prior to this rules implementation so can't check
                case _ => Left(ReportingPeriodInvalid)
              }
              .toValidatedNel
          }
          .getOrElse {
            Future.successful(xmlInfo.validNel)
          }
      case _ =>
        Future.successful(xmlInfo.validNel) //No extra checks needed here as message type indic is mandatory so it will be validated against another business rule
    }

  private def validateMultipleFileUploadForSameReportingPeriod(x: XMLInfo)(
    implicit hc: HeaderCarrier): FutureValidBusinessResult[XMLInfo] =
    /** checking for Reporting type because this rule only applies to CBC401*/
    x.messageSpec.messageType.getOrElse(determineMessageTypeIndic(x)) match {
      case CBC401 =>
        val tin = x.reportingEntity.fold("")(_.tin.value)
        val currentReportingPeriod = x.messageSpec.reportingPeriod

        reportingEntityDataService
          .queryReportingEntityDataTin(tin, currentReportingPeriod.toString)
          .leftMap { cbcErrors =>
            {
              logger.error(s"Got error back: $cbcErrors")
              throw new Exception(s"Error communicating with backend: $cbcErrors")
            }
          }
          .subflatMap {
            case Some(reportEntityData) =>

              val reportEntityDocRefId = reportEntityData.reportingEntityDRI.show

              if (reportEntityDocRefId.contains("OECD3")) {
                Right(x)
              } else {
                Left(MultipleFileUploadForSameReportingPeriod)
              }
            case _ => Right(x)
          }
          .toValidatedNel
      case _ => Future.successful(x.validNel)
    }

  private def validateMessageRefIds(in: XMLInfo): FutureValidBusinessResult[XMLInfo] = {

    val messageSpecMessageRefId = in.messageSpec.messageRefID.show
    val cbcrReportsRefIds = in.cbcReport.map(_.docSpec.docRefId.msgRefID.show)
    val addInfoRefIds = in.additionalInfo.map(_.docSpec.docRefId.msgRefID.show)
    val reportingEntityRefIds = in.reportingEntity.map(_.docSpec.docRefId.msgRefID.show)
    val reportingEntityDocTypeIndicator = in.reportingEntity.map(_.docSpec.docType)

    val docRefIds = reportingEntityRefIds match {
      case Some(s) =>
        reportingEntityDocTypeIndicator match {
          case Some(indicator) if indicator == OECD0 => cbcrReportsRefIds ++ addInfoRefIds
          case _                                     => s :: cbcrReportsRefIds ++ addInfoRefIds
        }
      case _ => cbcrReportsRefIds ++ addInfoRefIds
    }

    val docRefIdsValidator = docRefIds.foldLeft(List[Boolean]()) { (resultingValue, currentValue) =>
      //This needs to be improved as the regex is a bit unstable due to multiple underscores that might appear in the messageRefId
      val results = if (currentValue == messageSpecMessageRefId) true else false
      resultingValue :+ results

    }

    if (docRefIdsValidator.contains(false)) {

      Future.successful(MessageRefIdDontMatchWithDocRefId.invalidNel)

    } else {

      Future.successful(in.validNel)
    }

  }

  private def extractCorrDRI(xmlInfo: XMLInfo) =
    xmlInfo.cbcReport
      .find(_.docSpec.corrDocRefId.isDefined)
      .flatMap(_.docSpec.corrDocRefId)
      .orElse(xmlInfo.additionalInfo.find(_.docSpec.corrDocRefId.isDefined).flatMap(_.docSpec.corrDocRefId))
      .orElse(xmlInfo.reportingEntity.find(_.docSpec.corrDocRefId.isDefined).flatMap(_.docSpec.corrDocRefId))

  private def validateCorrMessageRefIdD(x: XMLInfo): FutureValidBusinessResult[XMLInfo] =
    validateCorrMsgRefIdNotInMessageSpec(x) *>
      validateCorrMsgRefIdNotInDocSpec(x)

  private def validateCorrMsgRefIdNotInMessageSpec(x: XMLInfo): ValidBusinessResult[XMLInfo] =
    if (x.messageSpec.corrMessageRefId.isDefined) CorrMessageRefIdNotAllowedInMessageSpec.invalidNel
    else x.validNel

  private def validateCorrMsgRefIdNotInDocSpec(x: XMLInfo): ValidBusinessResult[XMLInfo] = {
    val corrMessageRefIdisPresent = x.cbcReport
      .find(_.docSpec.corrMessageRefId.isDefined)
      .flatMap(_.docSpec.corrMessageRefId)
      .orElse(x.additionalInfo.find(_.docSpec.corrMessageRefId.isDefined).flatMap(_.docSpec.corrMessageRefId))
      .orElse(x.reportingEntity.find(_.docSpec.corrMessageRefId.isDefined).flatMap(_.docSpec.corrMessageRefId))

    if (corrMessageRefIdisPresent.isDefined) CorrMessageRefIdNotAllowedInDocSpec.invalidNel
    else x.validNel
  }

  private def validateCurrencyCodes(x: XMLInfo)(implicit hc: HeaderCarrier): FutureValidBusinessResult[XMLInfo] = {
    val currCodes = x.currencyCodes
    if (currCodes.forall(_ == currCodes.head)) {
      determineMessageTypeIndic(x) match {
        case Some(CBC401) => Future.successful(x.validNel)
        case _ =>
          val tin = x.reportingEntity.fold("")(_.tin.value)
          val currentReportingPeriod = x.messageSpec.reportingPeriod

          reportingEntityDataService
            .queryReportingEntityDataTin(tin, currentReportingPeriod.toString)
            .leftMap { cbcErrors =>
              logger.error(s"Got error back: $cbcErrors")
              throw new Exception(s"Error communicating with backend: $cbcErrors")
            }
            .subflatMap {
              case Some(reportEntityData) =>
                reportEntityData.currencyCode match {
                  case Some(code) =>
                    val reports: List[String] =
                      reportEntityData.cbcReportsDRI.filterNot(_.docTypeIndic == OECD3).map(_.show)
                    val corrDocRefIds: List[String] = x.cbcReport
                      .map(_.docSpec.corrDocRefId)
                      .filter(_.isDefined)
                      .map(_.get.cid.show)

                    currCodes.headOption match {
                      case Some(currCode) =>
                        if (currCode == code) {
                          Right(x)
                        } else {
                          if (BusinessRulesUtil.isFullyCorrected(reports, corrDocRefIds)) {
                            Right(x)
                          } else {
                            Left(PartiallyCorrectedCurrency)
                          }
                        }
                      case None => Right(x)
                    }
                  case None => Right(x)
                }
              case None => Right(x)
            }
            .toValidatedNel
      }
    } else {
      Future.successful(InconsistentCurrencyCodes.invalidNel)
    }

  }

  private def validateDeletion(in: XMLInfo)(implicit hc: HeaderCarrier): FutureValidBusinessResult[XMLInfo] = {
    val reportingEntityDocTypeIndicator = in.reportingEntity.map(_.docSpec.docType)

    reportingEntityDocTypeIndicator match {
      case Some(indicator) if indicator == OECD3 =>
        val tin = in.reportingEntity.fold("")(_.tin.value)
        val currentReportingPeriod = in.messageSpec.reportingPeriod

        reportingEntityDataService
          .queryReportingEntityDataTin(tin, currentReportingPeriod.toString)
          .leftMap { cbcErrors =>
            logger.error(s"Got error back: $cbcErrors")
            throw new Exception(s"Error communicating with backend: $cbcErrors")
          }
          .subflatMap {
            case Some(reportEntityData) =>
              //extract only the doc ref ids that were not previously deleted
              val allDocs =
                (List(reportEntityData.reportingEntityDRI) ++ reportEntityData.cbcReportsDRI.toList ++ reportEntityData.additionalInfoDRI)
                  .filterNot(_.docTypeIndic == OECD3)
                  .map(_.show)

              val allCorrDocSpecs = BusinessRulesUtil.extractAllCorrDocRefIds(in)
              val allDocTypes = BusinessRulesUtil.extractAllDocTypes(in)

              if (allDocTypes.forall(_ == allDocTypes.head)) {
                if (allDocs.nonEmpty) {
                  if (BusinessRulesUtil.isFullyCorrected(allDocs, allCorrDocSpecs)) {
                    Right(in)
                  } else {
                    Left(PartialDeletion)
                  }
                } else {
                  Right(in)
                }
              } else {
                Left(PartialDeletion)
              }
            case None => Right(in)
          }
          .toValidatedNel
      case _ => Future.successful(in.validNel)
    }
  }

  private def validateDatesNotOverlapping(in: XMLInfo)(
    implicit hc: HeaderCarrier): FutureValidBusinessResult[XMLInfo] = {
    val tin = in.reportingEntity.fold("")(_.tin.value)
    val entityReportingPeriod = in.reportingEntity.map(_.entityReportingPeriod)
    entityReportingPeriod match {
      case Some(erp) =>
        reportingEntityDataService
          .queryReportingEntityDatesOverlaping(tin, erp)
          .leftMap { cbcErrors =>
            logger.error(s"Got error back: $cbcErrors")
            throw new Exception(s"Error communicating with backend to get dates overlap check: $cbcErrors")
          }
          .subflatMap {
            case Some(datesOverlap) =>
              if (datesOverlap.isOverlapping)
                Left(DatesOverlapInvalid)
              else {
                Right(in)
              }
            case _ => Right(in)
          }
          .toValidatedNel
      case _ => Future.successful(in.validNel)
    }
  }

  def validateBusinessRules(
    in: RawXMLInfo,
    fileName: String,
    enrolment: Option[CBCEnrolment],
    affinityGroup: Option[AffinityGroup])(implicit hc: HeaderCarrier): FutureValidBusinessResult[XMLInfo] =
    extractXMLInfo(in).flatMap {
      case Valid(v)   => validateXMLInfo(v, fileName, enrolment, affinityGroup)
      case Invalid(i) => Future.successful(i.invalid)
    }

  //Because ReportingEntity is mandatory in version 2.0 we will not treat None as valid but instead throw and error
  def recoverReportingEntity(in: XMLInfo): FutureValidBusinessResult[CompleteXMLInfo] =
    in.reportingEntity match {
      case Some(re) =>
        Future.successful(
          CompleteXMLInfo(
            in.messageSpec,
            re,
            in.cbcReport,
            in.additionalInfo,
            in.creationDate,
            in.constEntityNames,
            in.currencyCodes).validNel)
      case None => Future.successful(ReportingEntityElementMissing.invalidNel)
    }

}
