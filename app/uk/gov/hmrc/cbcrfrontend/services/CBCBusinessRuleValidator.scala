/*
 * Copyright 2024 HM Revenue & Customs
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
import cats.data.*
import cats.implicits._
import cats.instances.future._
import play.api.Logger
import uk.gov.hmrc.auth.core.AffinityGroup.{Agent, Organisation}
import uk.gov.hmrc.auth.core.*
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.model.*
import uk.gov.hmrc.cbcrfrontend.repositories.CBCSessionCache
import uk.gov.hmrc.cbcrfrontend.util.BusinessRulesUtil.*
import uk.gov.hmrc.cbcrfrontend.{FutureValidBusinessResult, ValidBusinessResult}
import uk.gov.hmrc.http.HeaderCarrier

import java.time.LocalDate
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}
import uk.gov.hmrc.cbcrfrontend.given

/** This class exposes two methods:
  *
  * 1) [[validateBusinessRules()]] which takes a [[RawXMLInfo]] and returns an [[XMLInfo]] or a list of
  * [[BusinessRuleErrors]]
  *
  * 2) [[recoverReportingEntity()]] which, given an [[XMLInfo]] will return a [[ReportingEntity]], extracting it either
  * from the XML object, or from our [[ReportingEntityDataService]]
  *
  * The rest of the methods fall into 2 categories
  *
  * 1) extract* methods that do the minimal possible validation to transform the raw Strings from the [[RawXMLInfo]]
  * into their typesafe versions found in [[XMLInfo]]
  *
  * 2) validate* methods that do further validation to the fields, including cross field validation, duplicate checks
  * e.t.c.
  *
  * The methods reflect the structure of the XML document i.e. [[validateDocSpec]] makes further calls to
  * [[validateDocRefId()]]
  */
class CBCBusinessRuleValidator @Inject() (
  messageRefService: MessageRefIdService,
  docRefIdService: DocRefIdService,
  subscriptionDataService: SubscriptionDataService,
  reportingEntityDataService: ReportingEntityDataService,
  configuration: FrontendAppConfig,
  creationDateService: CreationDateService,
  cache: CBCSessionCache
)(implicit ec: ExecutionContext) {

  private val testData = "OECD1[0123]"

  private val cbcVersion: String = configuration.oecdSchemaVersion

  lazy val logger: Logger = Logger(this.getClass)

  // <<<<<<<<<<<<:::Extraction Methods:::>>>>>>>>>>>>

  /** Top level extraction method */
  private def extractXMLInfo(in: RawXMLInfo): ValidBusinessResult[XMLInfo] =
    if (in.numBodies > 1) MultipleCbcBodies.invalidNel[XMLInfo]
    else {
      (
        extractCbcOecdVersion(in.cbcVal) *>
          in.xmlEncoding.map(extractXmlEncodingVal).sequence[ValidBusinessResult, Unit] *>
          extractMessageSpec(in.messageSpec),
        in.reportingEntity.map(extractReportingEntity).sequence[ValidBusinessResult, ReportingEntity],
        in.cbcReport.map(extractCBCReports).sequence[ValidBusinessResult, CbcReports],
        in.additionalInfo.map(extractAdditionalInfo).sequence[ValidBusinessResult, AdditionalInfo]
      )
        .mapN(
          XMLInfo(
            _,
            _,
            _,
            _,
            Some(LocalDate.now()),
            in.constEntityNames,
            in.currencyCodes.flatMap(elem => elem.currCodes)
          )
        )
    }

  private def extractMessageSpec(in: RawMessageSpec): ValidBusinessResult[MessageSpec] =
    (
      extractMessageRefID(in),
      extractReceivingCountry(in),
      extractSendingEntityIn(in),
      extractReportingPeriod(in),
      extractMessageTypeIndic(in)
    ).mapN((messageRefID, receivingCountry, sendingEntityIn, reportingPeriod, messageTypeInic) =>
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
    (extractReportingRole(in), extractDocSpec(in.docSpec, ENT), extractTIN(in), extractEntityReportingPeriod(in))
      .mapN(ReportingEntity(_, _, _, in.name, in.city, _))

  private def extractCBCReports(in: RawCbcReports): ValidBusinessResult[CbcReports] =
    extractDocSpec(in.docSpec, REP).map(CbcReports(_))

  private def extractAdditionalInfo(in: RawAdditionalInfo): ValidBusinessResult[AdditionalInfo] =
    extractDocSpec(in.docSpec, ADD).map(AdditionalInfo(_, in.otherInfo))

  private def extractDocSpec(d: RawDocSpec, parentGroupElement: ParentGroupElement): ValidBusinessResult[DocSpec] =
    (
      extractDocTypeInidc(d.docType),
      extractDocRefId(d.docRefId, parentGroupElement),
      extractCorrDocRefId(d.corrDocRefId, parentGroupElement)
    ).mapN(DocSpec(_, _, _, d.corrMessageRefId))

  private def extractDocTypeInidc(docType: String): ValidBusinessResult[DocTypeIndic] =
    DocTypeIndic
      .fromString(docType)
      .fold[ValidBusinessResult[DocTypeIndic]] {
        if (docType.matches(testData)) TestDataError.invalidNel
        else InvalidXMLError("xmlValidationError.InvalidDocType").invalidNel
      }(_.validNel)

  private def extractCorrDocRefId(
    corrDocRefIdString: Option[String],
    parentGroupElement: ParentGroupElement
  ): ValidBusinessResult[Option[CorrDocRefId]] =
    corrDocRefIdString match {
      case Some(s) =>
        DocRefId(s) match {
          case None => InvalidCorrDocRefId.invalidNel
          case Some(d) if d.parentGroupElement != parentGroupElement =>
            CorrDocRefIdInvalidParentGroupElement.invalidNel
          case Some(d) => Some(CorrDocRefId(d)).validNel
        }
      case None => None.validNel
    }

  private def extractDocRefId(
    docRefIdString: String,
    parentGroupElement: ParentGroupElement
  ): ValidBusinessResult[DocRefId] =
    DocRefId
      .applyNewDocRefIdRegex(docRefIdString)
      .fold[ValidBusinessResult[DocRefId]](InvalidDocRefId.invalidNel) { d =>
        if (d.parentGroupElement != parentGroupElement) DocRefIdInvalidParentGroupElement.invalidNel
        else d.validNel
      }

  private def extractMessageTypeIndic(r: RawMessageSpec): ValidBusinessResult[Option[MessageTypeIndic]] =
    r.messageType.flatMap(MessageTypeIndic.parseFrom).validNel[BusinessRuleErrors]

  private def extractTIN(in: RawReportingEntity): ValidBusinessResult[TIN] = TIN(in.tin, in.tinIssuedBy).validNel

  private def extractReportingRole(in: RawReportingEntity): ValidBusinessResult[ReportingRole] =
    ReportingRole.parseFromString(in.reportingRole).toValidNel(InvalidXMLError("xmlValidationError.ReportingRole"))

  private def extractSendingEntityIn(in: RawMessageSpec): ValidBusinessResult[CBCId] =
    CBCId(in.sendingEntityIn).fold[ValidBusinessResult[CBCId]] {
      logger.error("Missing cbcId in raw message")
      SendingEntityError.invalidNel[CBCId]
    } { cbcId =>
      cbcId.validNel
    }

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

  /** These are dummy extraction methods - they're actually performing validation, but we dont' actually care about the
    * values later on
    */
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
    affinityGroup: Option[AffinityGroup]
  )(implicit hc: HeaderCarrier): FutureValidBusinessResult[XMLInfo] =
    (
      validateMessageRefIdD(x, x.messageSpec),
      validateCbcMessagesNotEmpty(x),
      validateCorrMessageRefIdD(x),
      validateReportingEntity(x),
      validateMessageTypes(x),
      validateDocSpecs(x),
      validateMessageTypeIndic(x),
      validateFileName(x, fileName),
      validateOrganisationCBCId(x, enrolment, affinityGroup),
      validateCreationDate(x),
      validateReportingPeriod(x),
      validateOtherInfo(x),
      validateMultipleFileUploadForSameReportingPeriod(x),
      validateMessageRefIds(x),
      validateCurrencyCodes(x),
      validateDeletion(x),
      validateDatesNotOverlapping(x)
    ).mapN((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => x)

  private def validateReportingEntity(in: XMLInfo)(implicit hc: HeaderCarrier): FutureValidBusinessResult[XMLInfo] =
    in.reportingEntity
      .map { re =>
        val docRefId = if (re.docSpec.docType == OECD0) {
          ensureDocRefIdExists(re)
        } else {
          Future.successful(re.validNel)
        }
        (
          docRefId,
          validateTIN(re),
          validateReportingEntityName(re),
          validateReportingEntityAddressCity(re),
          validateRepPeriodSameAsEndDate(re, in.messageSpec.reportingPeriod),
          validateStartDateBeforeEndDate(re),
          validateDatesNotInFuture(re, in.messageSpec.reportingPeriod),
          validateStartDateOnlyAfter01012016(re),
          validateConstEntities(re, in.constEntityNames)
        ).mapN((_, _, _, _, _, _, _, _, _) => in)
      }
      .getOrElse(Future.successful(ReportingEntityElementMissing.invalidNel))

  private def validateReportingEntityName(entity: ReportingEntity): FutureValidBusinessResult[ReportingEntity] =
    if (entity.name.trim.nonEmpty) entity.validNel
    else ReportingEntityOrConstituentEntityEmpty.invalidNel

  private def validateReportingEntityAddressCity(entity: ReportingEntity): FutureValidBusinessResult[ReportingEntity] =
    entity.city match {
      case Some(x) => if (x.trim.isEmpty) AddressCityEmpty.invalidNel else entity.validNel
      case None    => entity.validNel
    }

  private def validateOtherInfo(xmlInfo: XMLInfo): FutureValidBusinessResult[XMLInfo] =
    if (xmlInfo.additionalInfo.forall(_.otherInfo.trim.nonEmpty)) xmlInfo.validNel
    else OtherInfoEmpty.invalidNel

  private def validateStartDateBeforeEndDate(entity: ReportingEntity): FutureValidBusinessResult[ReportingEntity] =
    if (
      entity.entityReportingPeriod.startDate.isAfter(entity.entityReportingPeriod.endDate)
      || entity.entityReportingPeriod.startDate.equals(entity.entityReportingPeriod.endDate)
    )
      StartDateAfterEndDate.invalidNel
    else
      entity.validNel

  private def validateDatesNotInFuture(
    entity: ReportingEntity,
    reportingPeriod: LocalDate
  ): FutureValidBusinessResult[ReportingEntity] = {
    val currentDate = LocalDate.now()
    if (
      reportingPeriod.isAfter(currentDate)
      || entity.entityReportingPeriod.startDate.isAfter(currentDate)
      || entity.entityReportingPeriod.endDate.isAfter(currentDate)
    )
      AllReportingdatesInFuture.invalidNel
    else
      entity.validNel
  }

  private def validateRepPeriodSameAsEndDate(
    entity: ReportingEntity,
    reportingPeriod: LocalDate
  ): FutureValidBusinessResult[ReportingEntity] =
    if (!entity.entityReportingPeriod.endDate.equals(reportingPeriod))
      EndDateSameAsReportingPeriod.invalidNel
    else
      entity.validNel

  private def validateStartDateOnlyAfter01012016(
    entity: ReportingEntity
  ): FutureValidBusinessResult[ReportingEntity] = {
    val historicalStartDate = LocalDate.of(2016, 1, 1)
    if (entity.entityReportingPeriod.startDate.isBefore(historicalStartDate))
      StartDateNotBefore01012016.invalidNel
    else
      entity.validNel
  }

  private def validateConstEntities(
    re: ReportingEntity,
    reports: List[String]
  ): FutureValidBusinessResult[ReportingEntity] =
    if (reports.forall(_.trim.nonEmpty)) re.validNel
    else ReportingEntityOrConstituentEntityEmpty.invalidNel

  private def ensureDocRefIdExists(
    re: ReportingEntity
  )(implicit hc: HeaderCarrier): FutureValidBusinessResult[ReportingEntity] = {
    val docRefId = re.docSpec.docRefId
    for {
      isValid: Boolean <- reportingEntityDataService
                            .queryReportingEntityDataDocRefId(docRefId)
                            .leftMap { cbcErrors =>
                              logger.error(s"Got error back: $cbcErrors")
                              throw new Exception(s"Error communicating with backend: $cbcErrors")
                            }
                            .subflatMap {
                              case Some(_) => Right(true)
                              case None    => Left(false)
                            }
                            .isRight
      result <- if (isValid) {
                  val docRefIdToCheckForDeleteValid = docRefIdService.queryDocRefId(docRefId)
                  docRefIdToCheckForDeleteValid.map {
                    case DocRefIdResponses.Invalid => Left(ResendDocRefIdInvalid)
                    case _                         => Right(docRefId)
                  }
                } else {
                  docRefIdResendCheck(docRefId).map(d => Left(d))
                }
    } yield result.toValidatedNel.map(_ => re)
  }

  private def determineMessageTypeIndic(r: XMLInfo): Option[MessageTypeIndic] = {
    lazy val docTypes =
      List(r.additionalInfo.map(_.docSpec.docType)).flatten ++ r.cbcReport.map(_.docSpec.docType) ++ r.reportingEntity
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

  private def isFirstTimeSubmission(x: XMLInfo): Boolean = determineMessageTypeIndic(x).contains(CBC401)

  /** Ensure that if the messageType is [[CBC401]] there are no [[DocTypeIndic]] other than [[OECD1]] */
  private def validateMessageTypes(r: XMLInfo): FutureValidBusinessResult[XMLInfo] =
    r.messageSpec.messageType.contains(CBC401) match {
      case true if determineMessageTypeIndic(r).contains(CBC401) => r.validNel
      case true                                                  => MessageTypeIndicDocTypeIncompatible.invalidNel
      case false                                                 => r.validNel
    }

  /** Ensure there is not a mixture of OECD1 and other DocTypes within the document */
  private def validateDocTypes(
    x: XMLInfo,
    in: List[DocSpec],
    repIn: Option[DocSpec]
  ): FutureValidBusinessResult[XMLInfo] = {
    val rep = repIn.map(_.docType)
    val all = in.map(_.docType).toSet ++ rep
    if (all.size > 1 && all.contains(OECD1) && rep.exists(_ != OECD0)) IncompatibleOECDTypes.invalidNel
    else x.validNel
  }

  private def validateDocSpecs(in: XMLInfo)(implicit hc: HeaderCarrier): FutureValidBusinessResult[XMLInfo] = {
    val addDocSpec = in.additionalInfo.map(_.docSpec)
    val entDocSpecs = in.cbcReport.map(_.docSpec)
    val repDocSpec = in.reportingEntity.map(_.docSpec)
    val allDocSpecs = entDocSpecs ++ repDocSpec ++ addDocSpec
    val addCorrCheck = addDocSpec.flatMap(_.corrDocRefId).map(c => (c, extractCorrDRI(in).get))

    (
      allDocSpecs
        .map(validateDocSpec(in, _))
        .sequence
        .map(_.map(_ => in)): FutureValidBusinessResult[XMLInfo],
      validateDocTypes(in, entDocSpecs, repDocSpec),
      validateDistinctDocRefIds(in, allDocSpecs.map(_.docRefId)),
      allDocSpecs
        .map(docRefIdMatchDocTypeIndicCheck(in, _))
        .sequence
        .map(_.map(_ => in)): FutureValidBusinessResult[XMLInfo],
      validateDistinctCorrDocRefIds(in, allDocSpecs.flatMap(_.corrDocRefId)),
      addCorrCheck.map(validateAddInfoCorrDRI(in, _)).sequence.map(_.map(_ => in)): FutureValidBusinessResult[XMLInfo]
    ).mapN((_, _, _, _, _, _) => in)
  }

  /** Ensure that if a [[CorrDocRefId]] is required, it really exist Ensure that if a [[CorrDocRefId]] is not required,
    * it does not exist
    */
  private def validateCorrDocRefIdRequired(d: DocSpec): ValidBusinessResult[DocSpec] = d.docType match {
    case OECD2 | OECD3 if d.corrDocRefId.isEmpty => CorrDocRefIdMissing.invalidNel
    case OECD1 if d.corrDocRefId.isDefined       => CorrDocRefIdNotNeeded.invalidNel
    case _                                       => d.validNel
  }

  /** Ensure that the list of CorrDocRefIds are unique */
  private def validateDistinctCorrDocRefIds(
    in: XMLInfo,
    ids: List[CorrDocRefId]
  ): FutureValidBusinessResult[XMLInfo] =
    Either.cond(ids.distinct.lengthCompare(ids.size) == 0, (), CorrDocRefIdDuplicate).toValidatedNel.map(_ => in)

  /** Ensure that the list of DocRefIds are unique */
  private def validateDistinctDocRefIds(in: XMLInfo, ids: List[DocRefId]): FutureValidBusinessResult[XMLInfo] =
    Either.cond(ids.distinct.lengthCompare(ids.size) == 0, (), DocRefIdDuplicate).toValidatedNel.map(_ => in)

  /** Do further validation on the DocSpec * */
  private def validateDocSpec(in: XMLInfo, d: DocSpec)(implicit hc: HeaderCarrier): FutureValidBusinessResult[XMLInfo] =
    (validateDocRefId(d) zip d.corrDocRefId.map(validateCorrDocRefId).sequence[FutureValidBusinessResult, CorrDocRefId])
      .map { case (doc, corrDoc) =>
        (doc, corrDoc, validateCorrDocRefIdRequired(d)).mapN((_, _, _) => in)
      }

  /** Check if only 1st AddInfoCorrDocRefId was saved (before issue corrected) and if so And AddInfo coorDocRefId not
    * found then show appropriate error
    */
  private def validateAddInfoCorrDRI(
    in: XMLInfo,
    aiCid: (CorrDocRefId, CorrDocRefId)
  )(implicit hc: HeaderCarrier): FutureValidBusinessResult[XMLInfo] =
    reportingEntityDataService
      .queryReportingEntityDataModel(aiCid._2.cid)
      .fold(
        (errors: CBCErrors) => throw new Exception(s"Error communicating with backend: $errors"),
        (maybeModel: Option[ReportingEntityDataModel]) =>
          maybeModel.toValidNel[BusinessRuleErrors](
            AdditionalInfoDRINotFound("error.AdditionalInfoDRINotFound5", aiCid._1.cid.show)
          )
      )
      .flatMap(
        _.fold(
          (value: NonEmptyList[BusinessRuleErrors]) => Future.successful(value.invalid),
          (red: ReportingEntityDataModel) =>
            docRefIdService.queryDocRefId(aiCid._1.cid).map {
              case DocRefIdResponses.Valid => in.validNel
              case DocRefIdResponses.DoesNotExist =>
                if (red.oldModel)
                  AdditionalInfoDRINotFound(red.additionalInfoDRI.head.show, aiCid._1.cid.show).invalidNel
                else
                  in.validNel // To avoid duplicate error messages as this is also checked as part of another rule
              case _ => in.validNel // To avoid duplicate error messages
            }
        )
      )

  /** Do further validation on the provided [[CorrDocRefId]] */
  private def validateCorrDocRefId(corrDocRefId: CorrDocRefId)(implicit
    hc: HeaderCarrier
  ): FutureValidBusinessResult[CorrDocRefId] =
    corrDocRefIdDuplicateCheck(corrDocRefId)

  /** Query the [[DocRefIdService]] to find out if this [[CorrDocRefId]] is a duplicate */
  private def corrDocRefIdDuplicateCheck(
    corrDocRefId: CorrDocRefId
  )(implicit hc: HeaderCarrier): FutureValidBusinessResult[CorrDocRefId] =
    docRefIdService.queryDocRefId(corrDocRefId.cid).map {
      case DocRefIdResponses.Valid        => corrDocRefId.validNel
      case DocRefIdResponses.Invalid      => CorrDocRefIdInvalidRecord.invalidNel
      case DocRefIdResponses.DoesNotExist => CorrDocRefIdUnknownRecord.invalidNel
    }

  /** Do further validation on the provided [[DocRefId]] */
  private def validateDocRefId(docSpec: DocSpec)(implicit hc: HeaderCarrier): FutureValidBusinessResult[DocRefId] =
    docRefIdDuplicateCheck(docSpec)

  /** Query the [[DocRefIdService]] to find out if this docRefId is a duplicate If the doc type is OECD0, when don't
    * check for duplicates
    */
  private def docRefIdDuplicateCheck(
    docSpec: DocSpec
  )(implicit hc: HeaderCarrier): FutureValidBusinessResult[DocRefId] =
    if (docSpec.docType == OECD0) Future.successful(docSpec.docRefId.validNel)
    else
      docRefIdService.queryDocRefId(docSpec.docRefId).map {
        case DocRefIdResponses.DoesNotExist => docSpec.docRefId.validNel
        case _                              => DocRefIdDuplicate.invalidNel
      }

  /** Query the [[DocRefIdService]] to find out if this docRefId is a duplicate If the doc type is OECD0, when don't
    * check for duplicates
    */
  private def docRefIdResendCheck(d: DocRefId)(implicit hc: HeaderCarrier): Future[BusinessRuleErrors] =
    docRefIdService.queryDocRefId(d).map {
      case DocRefIdResponses.Invalid => ResendDocRefIdInvalid
      case _                         => ResentDataIsUnknownError
    }

  private def docRefIdMatchDocTypeIndicCheck(in: XMLInfo, docSpec: DocSpec): FutureValidBusinessResult[XMLInfo] = {
    val docRefId = docSpec.docRefId
    docSpec.docType match {
      case OECD0                                       => docRefId.validNel.map(_ => in)
      case docType if docType == docRefId.docTypeIndic => docRefId.validNel.map(_ => in)
      case _                                           => DocRefIdMismatch.invalidNel
    }
  }

  private def isCorrectionOrDeletionOrResent(docSpec: DocSpec) = docSpec.docType match {
    case OECD2 | OECD3 | OECD0 => true
    case _                     => false
  }

  private def isCorrectionOrDeletions(docSpec: DocSpec) = docSpec.docType match {
    case OECD2 | OECD3 => true
    case _             => false
  }

  /** Ensure the messageTypes and docTypes are valid and not in conflict */
  private def validateMessageTypeIndic(xmlInfo: XMLInfo): FutureValidBusinessResult[XMLInfo] = {
    lazy val CBCReportsAreNeverResent: Boolean = xmlInfo.cbcReport.forall(_.docSpec.docType != OECD0)
    lazy val AdditionalInfoIsNeverResent: Boolean = xmlInfo.additionalInfo.forall(_.docSpec.docType != OECD0)

    val invalid =
      !xmlInfo.cbcReport.map(_.docSpec).forall(isCorrectionOrDeletionOrResent) ||
        !xmlInfo.additionalInfo.map(_.docSpec).forall(isCorrectionOrDeletionOrResent) ||
        !xmlInfo.reportingEntity.map(_.docSpec).forall(isCorrectionOrDeletionOrResent)

    lazy val determinedMessageTypeIndic = determineMessageTypeIndic(xmlInfo)

    xmlInfo.messageSpec.messageType match {
      case Some(CBC402) if invalid                                              => MessageTypeIndicError.invalidNel
      case Some(CBCInvalidMessageTypeIndic)                                     => MessageTypeIndicInvalid.invalidNel
      case Some(_) if CBCReportsAreNeverResent && AdditionalInfoIsNeverResent   => xmlInfo.validNel
      case Some(_) if !CBCReportsAreNeverResent || !AdditionalInfoIsNeverResent => ResendOutsideRepEntError.invalidNel
      case Some(CBC401) if determinedMessageTypeIndic.isEmpty                   => MessageTypeIndicError.invalidNel
      case _                                                                    => MessageTypeIndicBlank.invalidNel
    }

  }

  /** Validate the TIN and TIN.issuedBy against the [[ReportingRole]] */
  private def validateTIN(re: ReportingEntity): FutureValidBusinessResult[ReportingEntity] =
    re.reportingRole match {
      case CBC701 | CBC703 | CBC704 if !re.tin.issuedBy.equalsIgnoreCase("gb") =>
        InvalidXMLError("xmlValidationError.TINIssuedBy").invalidNel
      case CBC701 | CBC703 | CBC704 if !Utr(re.tin.value).isValid =>
        InvalidXMLError("xmlValidationError.InvalidTIN").invalidNel
      case _ =>
        re.validNel
    }

  /** Ensure the provided filename matches the given MessageRefID (minus the extension) */
  private def validateFileName(in: XMLInfo, fileName: String): FutureValidBusinessResult[XMLInfo] =
    if (fileName.split("""\.""").headOption.contains(in.messageSpec.messageRefID.show)) in.validNel
    else {
      FileNameError(fileName, s"${in.messageSpec.messageRefID.show}.xml").invalidNel
    }

  /** Ensure SendingEntityIn CBCId is: 1) Not a private Beta CBCId 2) Has already been registered
    */
  private def validateSendingEntity(x: XMLInfo, messageRefID: MessageRefID, cbcId: CBCId)(implicit
    hc: HeaderCarrier
  ): FutureValidBusinessResult[XMLInfo] =
    subscriptionDataService
      .retrieveSubscriptionData(Right(cbcId))
      .fold[ValidBusinessResult[XMLInfo]](
        (errors: CBCErrors) => {
          logger.error(s"Subscription retrieval failed with : ${errors.show}")
          SendingEntityError.invalidNel
        },
        (maybeDetails: Option[SubscriptionDetails]) =>
          maybeDetails match {
            case None =>
              logger.warn(s"No subscription data found for cbcId : $cbcId")
              SendingEntityError.invalidNel
            case Some(_) =>
              x.validNel
          }
      )

  /** Ensure the [[CBCId]] found in the [[MessageRefID]] matches the [[CBCId]] in the SendingEntityIN field */
  private def validateCBCId(
    x: XMLInfo,
    messageRefID: MessageRefID,
    messageSpec: MessageSpec
  ): FutureValidBusinessResult[XMLInfo] =
    if (messageRefID.cBCId == messageSpec.sendingEntityIn) {
      Future.successful(x.validNel)
    } else Future.successful(MessageRefIDCBCIdMismatch.invalidNel)

  /** If the User is an enrolled Organisation, ensure their CBCId matches the  CBCId in the Sending Entity * */
  private def validateOrganisationCBCId(
    in: XMLInfo,
    enrolment: Option[CBCEnrolment],
    affinityGroup: Option[AffinityGroup]
  )(implicit hc: HeaderCarrier): FutureValidBusinessResult[XMLInfo] =
    (affinityGroup, enrolment) match {
      case (Some(Organisation), Some(enrolment)) =>
        if (enrolment.cbcId.value == in.messageSpec.sendingEntityIn.value) Future.successful(in.validNel)
        else Future.successful(SendingEntityOrganisationMatchError.invalidNel[XMLInfo])
      case (Some(Organisation), None) =>
        cache.readOption[CBCId].map {
          case Some(maybeCBCId) if maybeCBCId.value != in.messageSpec.sendingEntityIn.value =>
            SendingEntityOrganisationMatchError.invalidNel[XMLInfo]
          case _ => in.validNel // user has logged in as an org but with an unregistered cbcId
        }
      case (Some(Agent), _) => Future.successful(in.validNel)
      case _                => Future.successful(SendingEntityOrganisationMatchError.invalidNel[XMLInfo])
    }

  /** Ensure the reportingPeriod in the [[MessageRefID]] matches the reportingPeriod field in the MessageSpec */
  private def validateReportingPeriodMatches(
    x: XMLInfo,
    messageRefID: MessageRefID,
    messageSpec: MessageSpec
  ): FutureValidBusinessResult[XMLInfo] =
    if (messageRefID.reportingPeriod.getValue == messageSpec.reportingPeriod.getYear) {
      Future.successful(x.validNel)
    } else {
      Future.successful(MessageRefIDReportingPeriodMismatch.invalidNel)
    }

  /** Calls the [[MessageRefIdService]] to see if this provided [[MessageRefID]] already exists */
  private def isADuplicate(
    x: XMLInfo,
    msgRefId: MessageRefID
  )(implicit hc: HeaderCarrier): FutureValidBusinessResult[XMLInfo] =
    messageRefService
      .messageRefIdExists(msgRefId)
      .map(result => if (result) MessageRefIDDuplicate.invalidNel else x.validNel)

  /** Validates the messageRefID by performing 3 checks: 1) Checks if a duplicate messageRefID exists (by calling the
    * backend) 2) Checks that the SendingEntityIn in the messageSpec matches the CBCId in the messageRefId 3) Checks the
    * reportingPeriod in the messageRefID matches the reportingPeriod in the messageSpec
    */
  private def validateMessageRefIdD(x: XMLInfo, messageSpec: MessageSpec)(implicit
    hc: HeaderCarrier
  ): FutureValidBusinessResult[XMLInfo] = (
    validateSendingEntity(x, messageSpec.messageRefID, messageSpec.sendingEntityIn),
    isADuplicate(x, messageSpec.messageRefID),
    validateCBCId(x, messageSpec.messageRefID, messageSpec),
    validateReportingPeriodMatches(x, messageSpec.messageRefID, messageSpec)
  ).mapN((_, _, _, _) => x)

  private def validateCreationDate(xmlInfo: XMLInfo)(implicit hc: HeaderCarrier): FutureValidBusinessResult[XMLInfo] =
    if (
      (xmlInfo.cbcReport.map(_.docSpec) ++ xmlInfo.additionalInfo.map(_.docSpec)).exists(isCorrectionOrDeletions)
      || xmlInfo.reportingEntity.map(_.docSpec).exists(isCorrectionOrDeletionOrResent)
    ) {
      creationDateService
        .isDateValid(xmlInfo)
        .map {
          case DateCorrect => xmlInfo.validNel
          case DateMissing => CorrectedFileDateMissing.invalidNel
          case DateError   => CorrectedFileDateMissing.invalidNel
          case DateOld     => CorrectedFileTooOld.invalidNel
        }
    } else {
      Future.successful(xmlInfo.validNel)
    }

  private def validateReportingPeriod(
    xmlInfo: XMLInfo
  )(implicit hc: HeaderCarrier): FutureValidBusinessResult[XMLInfo] =
    determineMessageTypeIndic(xmlInfo) match {
      case Some(CBC401) => Future.successful(xmlInfo.validNel)
      case Some(CBC402) =>
        val crid = extractCorrDRI(xmlInfo)

        crid
          .map { drid =>
            reportingEntityDataService
              .queryReportingEntityData(drid.cid)
              .leftMap { cbcErrors =>
                logger.error(s"Got error back: $cbcErrors")
                throw new Exception(s"Error communicating with backend: $cbcErrors")
              }
              .subflatMap {
                case Some(red) if red.reportingPeriod.isDefined =>
                  if (red.reportingPeriod.get == xmlInfo.messageSpec.reportingPeriod) Right(xmlInfo)
                  else Left(CorrectedFileDateMissing)
                case Some(_) =>
                  Right(xmlInfo) // reportingPeriod not persisted prior to this rules implementation so can't check
                case _ => Left(ReportingPeriodInvalid)
              }
              .toValidatedNel
          }
          .getOrElse {
            Future.successful(xmlInfo.validNel)
          }
      case _ =>
        Future.successful(
          xmlInfo.validNel
        ) // No extra checks needed here as message type indic is mandatory so it will be validated against another business rule
    }

  private def validateMultipleFileUploadForSameReportingPeriod(x: XMLInfo)(implicit
    hc: HeaderCarrier
  ): FutureValidBusinessResult[XMLInfo] =
    /** checking for Reporting type because this rule only applies to CBC401 */
    x.messageSpec.messageType.fold(determineMessageTypeIndic(x))(v => Some(v)) match {
      case Some(CBC401) =>
        val tin = x.reportingEntity.fold("")(_.tin.value)
        val currentReportingPeriod = x.messageSpec.reportingPeriod

        reportingEntityDataService
          .queryReportingEntityDataTin(tin, currentReportingPeriod.toString)
          .leftMap { cbcErrors =>
            logger.error(s"Got error back: $cbcErrors")
            throw new Exception(s"Error communicating with backend: $cbcErrors")
          }
          .subflatMap {
            case Some(reportEntityData) if !reportEntityData.reportingEntityDRI.show.contains("OECD3") =>
              Left(MultipleFileUploadForSameReportingPeriod)
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

    val docRefIds = (reportingEntityRefIds, reportingEntityDocTypeIndicator) match {
      case (Some(s), indicator) if !indicator.contains(OECD0) => s :: cbcrReportsRefIds ++ addInfoRefIds
      case _                                                  => cbcrReportsRefIds ++ addInfoRefIds
    }

    if (docRefIds.forall(_ == messageSpecMessageRefId)) {
      Future.successful(in.validNel)
    } else {
      Future.successful(MessageRefIdDontMatchWithDocRefId.invalidNel)
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

  private def validateCbcMessagesNotEmpty(x: XMLInfo): FutureValidBusinessResult[XMLInfo] =
    if (isFirstTimeSubmission(x) && x.cbcReport.isEmpty) NoCbcReports.invalidNel[XMLInfo]
    else x.validNel

  private def validateCorrMsgRefIdNotInMessageSpec(x: XMLInfo): FutureValidBusinessResult[XMLInfo] =
    if (x.messageSpec.corrMessageRefId.isDefined) {
      CorrMessageRefIdNotAllowedInMessageSpec.invalidNel
    } else {
      x.validNel
    }

  private def validateCorrMsgRefIdNotInDocSpec(x: XMLInfo): FutureValidBusinessResult[XMLInfo] = {
    val corrMessageRefIdisPresent = x.cbcReport
      .find(_.docSpec.corrMessageRefId.isDefined)
      .flatMap(_.docSpec.corrMessageRefId)
      .orElse(x.additionalInfo.find(_.docSpec.corrMessageRefId.isDefined).flatMap(_.docSpec.corrMessageRefId))
      .orElse(x.reportingEntity.find(_.docSpec.corrMessageRefId.isDefined).flatMap(_.docSpec.corrMessageRefId))

    if (corrMessageRefIdisPresent.isDefined) {
      CorrMessageRefIdNotAllowedInDocSpec.invalidNel
    } else {
      x.validNel
    }
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
            .subflatMap(maybeRed =>
              (for {
                reportEntityData <- maybeRed
                code             <- reportEntityData.currencyCode
              } yield {
                val reports = reportEntityData.cbcReportsDRI.filterNot(_.docTypeIndic == OECD3).map(_.show)
                val corrDocRefIds = x.cbcReport.flatMap(_.docSpec.corrDocRefId).map(_.cid.show)
                currCodes match {
                  case currCode :: _ if currCode == code             => Right(x)
                  case _ if isFullyCorrected(reports, corrDocRefIds) => Right(x)
                  case List()                                        => Right(x)
                  case _                                             => Left(PartiallyCorrectedCurrency)
                }
              }).getOrElse(Right(x))
            )
            .toValidatedNel
      }
    } else {
      Future.successful(InconsistentCurrencyCodes.invalidNel)
    }

  }

  private def validateDeletion(in: XMLInfo)(implicit hc: HeaderCarrier): FutureValidBusinessResult[XMLInfo] = {
    val reportingEntityDocTypeIndicator = in.reportingEntity.map(_.docSpec.docType)

    reportingEntityDocTypeIndicator match {
      case Some(OECD3) =>
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
              // extract only the doc ref ids that were not previously deleted
              val allDocs =
                (List(
                  reportEntityData.reportingEntityDRI
                ) ++ reportEntityData.cbcReportsDRI.toList ++ reportEntityData.additionalInfoDRI)
                  .filterNot(_.docTypeIndic == OECD3)
                  .map(_.show)

              if (
                extractAllDocTypes(in).distinct.length != 1 || !isFullyCorrected(allDocs, extractAllCorrDocRefIds(in))
              ) {
                Left(PartialDeletion)
              } else {
                Right(in)
              }
            case None => Right(in)
          }
          .toValidatedNel
      case _ => Future.successful(in.validNel)
    }
  }

  private def validateDatesNotOverlapping(
    in: XMLInfo
  )(implicit hc: HeaderCarrier): FutureValidBusinessResult[XMLInfo] = {
    val tin = in.reportingEntity.fold("")(_.tin.value)
    val entityReportingPeriod = in.reportingEntity.map(_.entityReportingPeriod)
    entityReportingPeriod match {
      case Some(erp) =>
        reportingEntityDataService
          .queryReportingEntityDatesOverlapping(tin, erp)
          .leftMap { cbcErrors =>
            logger.error(s"Got error back: $cbcErrors")
            throw new Exception(s"Error communicating with backend to get dates overlap check: $cbcErrors")
          }
          .subflatMap {
            case Some(datesOverlap) if datesOverlap.isOverlapping =>
              Left(DatesOverlapInvalid)
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
    affinityGroup: Option[AffinityGroup]
  )(implicit hc: HeaderCarrier): FutureValidBusinessResult[XMLInfo] =
    extractXMLInfo(in).flatMap {
      case Valid(v)   => validateXMLInfo(v, fileName, enrolment, affinityGroup)
      case Invalid(i) => Future.successful(i.invalid)
    }

  // Because ReportingEntity is mandatory in version 2.0 we will not treat None as valid but instead throw and error
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
            in.currencyCodes
          ).validNel
        )
      case None => Future.successful(ReportingEntityElementMissing.invalidNel)
    }

}
