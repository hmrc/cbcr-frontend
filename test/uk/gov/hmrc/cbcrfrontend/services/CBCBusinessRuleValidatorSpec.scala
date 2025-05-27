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

import cats.data.{EitherT, NonEmptyList, Validated, ValidatedNel}
import cats.implicits.catsStdInstancesForFuture
import org.apache.pekko.util.Timeout
import org.mockito.ArgumentMatchers.{any, eq as eqTo}
import org.mockito.Mockito.when
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.mockito.MockitoSugar
import play.api.test.Helpers.{await, defaultAwaitTimeout}
import uk.gov.hmrc.auth.core.AffinityGroup.Organisation
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.emailaddress.EmailAddress
import uk.gov.hmrc.cbcrfrontend.model.DocRefIdResponses.{DoesNotExist, Invalid, Valid}
import uk.gov.hmrc.cbcrfrontend.model.*
import uk.gov.hmrc.cbcrfrontend.repositories.CBCSessionCache
import uk.gov.hmrc.http.HeaderCarrier

import java.io.File
import java.time.{LocalDate, LocalDateTime}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.language.implicitConversions
import scala.util.chaining.scalaUtilChainingOps
import scala.concurrent.duration._

class CBCBusinessRuleValidatorSpec extends AnyWordSpec with Matchers with MockitoSugar {

  private val messageRefIdService = mock[MessageRefIdService]
  private val docRefIdService = mock[DocRefIdService]
  private val subscriptionDataService = mock[SubscriptionDataService]
  private val reportingEntity = mock[ReportingEntityDataService]
  private val configuration = mock[FrontendAppConfig]
  private val creationDateService = mock[CreationDateService]
  private val cache = mock[CBCSessionCache]

  private val docRefId1 =
    DocRefId("GB2016RGXLCBC0100000056CBC40120170311T090000X_7000000002OECD1ENT").getOrElse(fail("bad docrefid"))
  private val docRefId2 =
    DocRefId("GB2016RGXLCBC0100000056CBC40120170311T090000X_7000000002OECD1REP").getOrElse(fail("bad docrefid"))
  private val docRefId3 =
    DocRefId("GB2016RGXLCBC0100000056CBC40120170311T090000X_7000000002OECD1ADD").getOrElse(fail("bad docrefid"))
  private val docRefId4 =
    DocRefId("GB2016RGXLCBC0100000056CBC40120170311T090000X_7000000002OECD1REP2").getOrElse(fail("bad docrefid"))

  private val corrDocRefId1 =
    DocRefId("GB2016RGXLCBC0100000056CBC40120170311T090000X_7000000002OECD1ENTC").getOrElse(fail("bad docrefid"))
  private val corrDocRefId2 =
    DocRefId("GB2016RGXLCBC0100000056CBC40120170311T090000X_7000000002OECD1REPC").getOrElse(fail("bad docrefid"))
  private val corrDocRefId3 =
    DocRefId("GB2016RGXLCBC0100000056CBC40120170311T090000X_7000000002OECD1ADDC").getOrElse(fail("bad docrefid"))
  private val corrDocRefId4 =
    DocRefId("GB2016RGXLCBC0100000056CBC40120170311T090000X_7000000002OECD1REP2C").getOrElse(fail("bad docrefid"))
  private val corrDocRefId5 =
    DocRefId("GB2016RGXLCBC0100000056CBC40120170311T090000X_7000000002OECD1REPC2").getOrElse(fail("bad docrefid"))

  private val docRefId6 =
    DocRefId("GB2016RGXLCBC0100000056CBC40120170311T090000X_7000000002OECD2ENT").getOrElse(fail("bad docrefid"))
  private val docRefId7 =
    DocRefId("GB2016RGXLCBC0100000056CBC40120170311T090000X_7000000002OECD2REP").getOrElse(fail("bad docrefid"))
  private val docRefId8 =
    DocRefId("GB2016RGXLCBC0100000056CBC40120170311T090000X_7000000002OECD2ADD").getOrElse(fail("bad docrefid"))
  private val docRefId9 =
    DocRefId("GB2016RGXLCBC0100000056CBC40120170311T090000X_7000000002OECD2REP2").getOrElse(fail("bad docrefid"))

  private val cbcId = CBCId.create(56).toOption
  private val submissionData = SubscriptionDetails(
    BusinessPartnerRecord(
      "SAFEID",
      Some(OrganisationResponse("blagh")),
      EtmpAddress("Line1", None, None, None, Some("TF3 XFE"), "GB")
    ),
    SubscriberContact("Brian", "Lastname", "phonenum", EmailAddress("test@test.com")),
    cbcId,
    Utr("7000000002")
  )

  private val schemaVer = "2.0"
  when(docRefIdService.queryDocRefId(any)(any)).thenReturn(Future.successful(DoesNotExist))
  when(subscriptionDataService.retrieveSubscriptionData(any)(any))
    .thenReturn(EitherT.right(Future.successful(Some(submissionData))))
  when(configuration.oecdSchemaVersion).thenReturn(schemaVer)

  when(reportingEntity.queryReportingEntityDatesOverlapping(any, any)(any))
    .thenReturn(EitherT.right(Future.successful(Some(DatesOverlap(false)))))

  private def makeTheUserAnAgent =
    when(cache.readOption[CBCId](eqTo(CBCId.cbcIdFormat), any, any)).thenReturn(Future.successful(None))

  makeTheUserAnAgent

  private def makeTheUserAnOrganisation(cbcid: String) =
    when(cache.readOption[CBCId](eqTo(CBCId.cbcIdFormat), any, any)).thenReturn(Future.successful(CBCId(cbcid)))

  when(creationDateService.isDateValid(any)(any)).thenReturn(Future.successful(DateCorrect))

  implicit private val hc: HeaderCarrier = HeaderCarrier()
  private val extract = new XmlInfoExtract()
  implicit private def fileToXml(f: File): RawXMLInfo = extract.extract(f)

  private val filename = "GB2016RGXLCBC0100000056CBC40120170311T090000X.xml"
  private val filenameTemp = "GB2017RGXLCBC0100000056CBC40120170311T090000X.xml"

  private val cbcId2 = CBCId("XLCBC0100000056").getOrElse(fail("booo"))
  private val enrol = CBCEnrolment(cbcId2, Utr("7000000002"))

  private val docRefId = "GB2016RGXVCBC0000000056CBC40120170311T090000X_7000000002OECD1"

  private val actualDocRefId = DocRefId("GB2016RGXGCBC0100000132CBC40120170311T090000X_4590617080OECD2ADD62").get

  private val actualDocRefId2 = DocRefId("GB2016RGXGCBC0100000132CBC40120170311T090000X_4590617080OECD2ADD63").get

  private val red = ReportingEntityData(
    NonEmptyList.of(actualDocRefId),
    List(actualDocRefId2),
    actualDocRefId,
    TIN("asdf", "lkajsdf"),
    UltimateParentEntity("someone"),
    CBC701,
    Some(LocalDate.now()),
    None,
    None,
    None
  )

  private val redReportPeriod = ReportingEntityData(
    NonEmptyList.of(actualDocRefId),
    List(actualDocRefId2),
    actualDocRefId,
    TIN("asdf", "lkajsdf"),
    UltimateParentEntity("someone"),
    CBC701,
    Some(LocalDate.now()),
    Some(LocalDate.of(2018, 1, 1)),
    None,
    Some(EntityReportingPeriod(LocalDate.parse("2017-01-02"), LocalDate.parse("2018-01-01")))
  )

  private val redmTrue = ReportingEntityDataModel(
    NonEmptyList.of(actualDocRefId),
    List(actualDocRefId2),
    actualDocRefId,
    TIN("asdf", "lkajsdf"),
    UltimateParentEntity("someone"),
    CBC701,
    Some(LocalDate.now()),
    None,
    oldModel = true,
    None,
    None
  )

  private val redmFalse = ReportingEntityDataModel(
    NonEmptyList.of(actualDocRefId),
    List(actualDocRefId2),
    actualDocRefId,
    TIN("asdf", "lkajsdf"),
    UltimateParentEntity("someone"),
    CBC701,
    Some(LocalDate.now()),
    None,
    oldModel = false,
    None,
    None
  )

  private val xmlinfo = XMLInfo(
    MessageSpec(
      MessageRefID("GB2016RGXVCBC0000000056CBC40120170311T090000X").getOrElse(fail("waaaaa")),
      "GB",
      CBCId.create(99).getOrElse(fail("booo")),
      LocalDateTime.now(),
      LocalDate.parse("2017-01-30"),
      None,
      None
    ),
    None,
    List(CbcReports(DocSpec(OECD1, DocRefId(docRefId + "ENT").get, None, None))),
    List(AdditionalInfo(DocSpec(OECD1, DocRefId(docRefId + "ADD").get, None, None), "Some Other Info")),
    Some(LocalDate.now()),
    List.empty[String],
    List.empty[String]
  )

  private val validator = new CBCBusinessRuleValidator(
    messageRefIdService,
    docRefIdService,
    subscriptionDataService,
    reportingEntity,
    configuration,
    creationDateService,
    cache
  )

  def errors[A](s: ValidatedNel[BusinessRuleErrors, A]): List[BusinessRuleErrors] =
    s.fold(errors => errors.toList, _ => fail("Errors expected"))

  "The CBCBusinessRuleValidator" should {
    "throw an error if currency codes are not consistent in same xml report " in {
      when(messageRefIdService.messageRefIdExists(any)(any)).thenReturn(Future.successful(false))
      when(reportingEntity.queryReportingEntityDataTin(any, any)(any))
        .thenReturn(EitherT.right(Future.successful(None)))

      val inconsistentCurrency = new File("test/resources/cbcr-inconsistent-currency-codes.xml")
      val validFile = new File("test/resources/cbcr-valid-currency-codes.xml")

      val result1 =
        await(validator.validateBusinessRules(inconsistentCurrency, filename, Some(enrol), Some(Organisation)))
      val result2 = await(validator.validateBusinessRules(validFile, filenameTemp, Some(enrol), Some(Organisation)))

      result1 pipe errors should contain(InconsistentCurrencyCodes)
      result2.fold(
        errors => fail(s"Errors were generated ${errors.toList}"),
        _ => ()
      )
    }

    "throw an error when multiple file uploaded for the same reporting period of original submission when the previous submission exists" in {
      val firstOriginalReportingEntityDri =
        DocRefId("GB2016RGXLCBC0100000056CBC40120170311T090000X_7000000002OECD1ENT").get
      val firstOriginalCbcReportsDri = DocRefId("GB2016RGXLCBC0100000056CBC40120170311T090000X_7000000002OECD1REP").get
      val firstOriginalAdditionalInfoDri =
        DocRefId("GB2016RGXLCBC0100000056CBC40120170311T090000X_7000000002OECD1ADD").get

      val reportEntityData = ReportingEntityData(
        NonEmptyList.of(firstOriginalCbcReportsDri),
        List(firstOriginalAdditionalInfoDri),
        firstOriginalReportingEntityDri,
        TIN("7000000002", "GB"),
        UltimateParentEntity("someone"),
        CBC703,
        Some(LocalDate.now()),
        Some(LocalDate.of(2016, 3, 31)),
        None,
        Some(EntityReportingPeriod(LocalDate.parse("2016-01-01"), LocalDate.parse("2016-03-31")))
      )

      when(messageRefIdService.messageRefIdExists(any)(any)).thenReturn(Future.successful(false))

      when(reportingEntity.queryReportingEntityDataTin(any, any)(any))
        .thenReturn(EitherT.right(Future.successful(Some(reportEntityData))))

      val multipleSubmissionForSameReportingPeriod = new File("test/resources/cbcr-multiplefileupload-original.xml")

      val result = await(
        validator
          .validateBusinessRules(multipleSubmissionForSameReportingPeriod, filename, Some(enrol), Some(Organisation))
      )
      result pipe errors should contain(MultipleFileUploadForSameReportingPeriod)
    }

    "let the file go through when multiple file uploaded for different years" in {
      when(messageRefIdService.messageRefIdExists(any)(any)).thenReturn(Future.successful(false))
      when(reportingEntity.queryReportingEntityDataTin(any, any)(any))
        .thenReturn(EitherT.right(Future.successful(None)))

      val multipleSubmissionForSameReportingPeriod = new File("test/resources/cbcr-multiplefileupload-original.xml")

      val result = await(
        validator
          .validateBusinessRules(multipleSubmissionForSameReportingPeriod, filename, Some(enrol), Some(Organisation))
      )

      result.fold(
        errors => fail(s"Errors were generated ${errors.toList}"),
        _ => ()
      )
    }

    "when message-ref-id within doc-ref-id doesn't match with the message-ref-id of message spec" in {
      when(messageRefIdService.messageRefIdExists(any)(any)).thenReturn(Future.successful(false))
      when(reportingEntity.queryReportingEntityDataTin(any, any)(any))
        .thenReturn(EitherT.right(Future.successful(None)))

      val messageRefIdValidation =
        new File("test/resources/cbcr-messageRefId-dontMatchAgainst-messageRefId-inDocRefId.xml")

      val result =
        await(validator.validateBusinessRules(messageRefIdValidation, filename, Some(enrol), Some(Organisation)))
      result pipe errors should contain(MessageRefIdDontMatchWithDocRefId)
    }

    "return the correct error" when {
      "the reportingEntity name is an empty string" in {
        when(reportingEntity.queryReportingEntityDataByCbcId(any, any)(any))
          .thenReturn(EitherT.right(Future.successful(None)))
        when(messageRefIdService.messageRefIdExists(any)(any)).thenReturn(Future.successful(false))
        val multipleCbcBodies = new File("test/resources/cbcr-valid-reporting-entity-name.xml")
        val result =
          await(validator.validateBusinessRules(multipleCbcBodies, filename, Some(enrol), Some(Organisation)))
        result pipe errors should contain(ReportingEntityOrConstituentEntityEmpty)
      }

      "the City inside addressFix tag is an empty string" in {
        when(reportingEntity.queryReportingEntityDataByCbcId(any, any)(any))
          .thenReturn(EitherT.right(Future.successful(None)))
        when(messageRefIdService.messageRefIdExists(any)(any)).thenReturn(Future.successful(false))
        val multipleCbcBodies = new File("test/resources/cbcr-valid-reporting-entity-name.xml")
        val result =
          await(validator.validateBusinessRules(multipleCbcBodies, filename, Some(enrol), Some(Organisation)))
        result pipe errors should contain(AddressCityEmpty)
      }

      "the constEntity name is an empty string" in {
        when(messageRefIdService.messageRefIdExists(any)(any)).thenReturn(Future.successful(false))
        val multipleCbcBodies = new File("test/resources/cbcr-valid-const-entity-name.xml")
        val result =
          await(validator.validateBusinessRules(multipleCbcBodies, filename, Some(enrol), Some(Organisation)))
        result pipe errors should contain(ReportingEntityOrConstituentEntityEmpty)
      }

      "there are multiple CbcBody elements" in {
        when(messageRefIdService.messageRefIdExists(any)(any)).thenReturn(Future.successful(false))
        val multipleCbcBodies = new File("test/resources/cbcr-valid-multiple-bodies.xml")
        val result =
          await(validator.validateBusinessRules(multipleCbcBodies, filename, Some(enrol), Some(Organisation)))
        result pipe errors should contain(MultipleCbcBodies)
      }

      "there are no CbcReports elements" in {
        when(messageRefIdService.messageRefIdExists(any)(any)).thenReturn(Future.successful(false))
        val file = new File("test/resources/cbcr-valid-no-cbcreports.xml")
        val result = await(validator.validateBusinessRules(file, filename, Some(enrol), Some(Organisation)))
        result pipe errors should contain(NoCbcReports)
      }

      "messageRefId is empty" in {
        val missingMessageRefID = new File("test/resources/cbcr-invalid-empty-messageRefID.xml")
        val result =
          await(validator.validateBusinessRules(missingMessageRefID, filename, Some(enrol), Some(Organisation)))
        result pipe errors should contain(MessageRefIDMissing)
      }

      "messageRefId is null" in {
        val nullMessageRefID = new File("test/resources/cbcr-invalid-null-messageRefID.xml")
        val result = await(validator.validateBusinessRules(nullMessageRefID, filename, Some(enrol), Some(Organisation)))
        result pipe errors should contain(MessageRefIDMissing)
      }

      "messageRefId format is wrong" in {
        val invalidMessageRefID = new File("test/resources/cbcr-invalid-invalid-messageRefID.xml")
        val result =
          await(validator.validateBusinessRules(invalidMessageRefID, filename, Some(enrol), Some(Organisation)))
        result pipe errors should contain(MessageRefIDFormatError)
      }

      "messageRefId contains a CBCId that doesnt match the CBCId in the SendingEntityIN field" in {
        when(messageRefIdService.messageRefIdExists(any)(any)).thenReturn(Future.successful(false))
        when(reportingEntity.queryReportingEntityDataTin(any, any)(any))
          .thenReturn(EitherT.right(Future.successful(None)))
        when(reportingEntity.queryReportingEntityData(any)(any))
          .thenReturn(EitherT.right(Future.successful(Some(red))))

        val invalidMessageRefID = new File("test/resources/cbcr-invalid-cbcId-messageRefID.xml")
        val result =
          await(validator.validateBusinessRules(invalidMessageRefID, filename, Some(enrol), Some(Organisation)))
        result pipe errors should contain(MessageRefIDCBCIdMismatch)
      }

      "the Organisation user has a CBCId that does not match that in the SendingEntityIn field on straight through journey" in {
        makeTheUserAnOrganisation("XTCBC0100000001")

        val validFile = new File("test/resources/cbcr-valid.xml")
        val result = await(validator.validateBusinessRules(validFile, filename, None, Some(Organisation)))
        result pipe errors should contain(SendingEntityOrganisationMatchError)
        makeTheUserAnAgent
      }

      "the Organisation user has a CBCId that does match that in the SendingEntityIn field on straight through journey" in {
        when(messageRefIdService.messageRefIdExists(any)(any)).thenReturn(Future.successful(false))
        when(reportingEntity.queryReportingEntityDataTin(any, any)(any))
          .thenReturn(EitherT.right(Future.successful(None)))

        when(reportingEntity.queryReportingEntityDataByCbcId(any, any)(any))
          .thenReturn(EitherT.right(Future.successful(None)))

        makeTheUserAnOrganisation("XLCBC0100000056")

        val validFile = new File("test/resources/cbcr-valid.xml")
        val result = await(validator.validateBusinessRules(validFile, filename, None, Some(Organisation)))
        result.fold(
          errors => fail(s"Errors were generated ${errors.toList}"),
          _ => ()
        )
        makeTheUserAnAgent
      }

      "the Organisation user has a CBCId that does not match that in the SendingEntityIn field" in {
        val cbcId3 = CBCId("XTCBC0100000001").getOrElse(fail("booo"))
        val enrol2 = CBCEnrolment(cbcId3, Utr("7000000002"))

        val validFile = new File("test/resources/cbcr-valid.xml")
        val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol2), Some(Organisation)))
        result pipe errors should contain(SendingEntityOrganisationMatchError)
        makeTheUserAnAgent
      }

      "the Organisation user has a CBCId matches that in the SendingEntityIn field" in {
        when(messageRefIdService.messageRefIdExists(any)(any)).thenReturn(Future.successful(false))
        when(reportingEntity.queryReportingEntityDataTin(any, any)(any))
          .thenReturn(EitherT.right(Future.successful(None)))
        when(reportingEntity.queryReportingEntityDataByCbcId(any, any)(any))
          .thenReturn(EitherT.right(Future.successful(None)))
        makeTheUserAnOrganisation("XLCBC0100000056")

        val validFile = new File("test/resources/cbcr-valid.xml")
        val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
        result.fold(
          errors => fail(s"Errors were generated ${errors.toList}"),
          _ => ()
        )
        makeTheUserAnAgent
      }

      "messageRefId contains a Reporting Year that doesn't match the year in the ReportingPeriod field" in {
        when(messageRefIdService.messageRefIdExists(any)(any)).thenReturn(Future.successful(false))
        when(reportingEntity.queryReportingEntityDataTin(any, any)(any))
          .thenReturn(EitherT.right(Future.successful(None)))
        when(reportingEntity.queryReportingEntityData(any)(any))
          .thenReturn(EitherT.right(Future.successful(Some(red))))

        val invalidMessageRefID = new File("test/resources/cbcr-invalid-reportingYear-messageRefID.xml")
        val result =
          await(validator.validateBusinessRules(invalidMessageRefID, filename, Some(enrol), Some(Organisation)))
        result pipe errors should contain(MessageRefIDReportingPeriodMismatch)
      }

      "messageRefId contains a creation timestamp that isn't valid" in {
        when(messageRefIdService.messageRefIdExists(any)(any)).thenReturn(Future.successful(false))
        val invalidMessageRefID = new File("test/resources/cbcr-invalid-creationTimestamp-messageRefID.xml")
        val result =
          await(validator.validateBusinessRules(invalidMessageRefID, filename, Some(enrol), Some(Organisation)))
        result pipe errors should contain(MessageRefIDTimestampError)
      }

      "messageRefId has been seen before" in {
        when(messageRefIdService.messageRefIdExists(any)(any)).thenReturn(Future.successful(true))
        val validFile = new File("test/resources/cbcr-valid.xml")
        val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
        result pipe errors should contain(MessageRefIDDuplicate)
      }

      "test data is present" when {
        "the xml file has a single CBCReports element" in {
          val validFile = new File("test/resources/cbcr-testData.xml")
          val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
          result pipe errors should contain(TestDataError)
        }

        "the xml file has a multiple CBCReports elements" in {
          val validFile = new File("test/resources/cbcr-testDataM.xml")
          val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
          result pipe errors should contain(TestDataError)
        }
      }

      "SendingEntityIn does not match any CBCId in the database" in {
        when(messageRefIdService.messageRefIdExists(any)(any)).thenReturn(Future.successful(false))
        when(subscriptionDataService.retrieveSubscriptionData(any)(any))
          .thenReturn(EitherT.right(Future.successful(None)))
        val validFile = new File("test/resources/cbcr-valid.xml")
        val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
        result pipe errors shouldBe List(SendingEntityError)
      }

      "ReceivingCountry does not equal GB" in {
        when(subscriptionDataService.retrieveSubscriptionData(any)(any))
          .thenReturn(EitherT.right(Future.successful(Some(submissionData))))
        val validFile = new File("test/resources/cbcr-invalidReceivingCountry.xml")
        val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
        result pipe errors should contain(ReceivingCountryError)
      }

      "Filename does not match MessageRefId" in {
        when(messageRefIdService.messageRefIdExists(any)(any)).thenReturn(Future.successful(false))
        when(reportingEntity.queryReportingEntityDataTin(any, any)(any))
          .thenReturn(EitherT.right(Future.successful(None)))
        when(reportingEntity.queryReportingEntityData(any)(any))
          .thenReturn(EitherT.right(Future.successful(Some(red))))

        val validFile = new File("test/resources/cbcr-valid.xml")
        val invalidFilename = "INVALID" + filename
        val result = await(validator.validateBusinessRules(validFile, invalidFilename, Some(enrol), Some(Organisation)))
        result pipe errors shouldBe List(FileNameError(invalidFilename, filename))
      }

      "ReportingEntity is missing" in {
        when(reportingEntity.queryReportingEntityData(any)(any)).thenReturn(EitherT.right(Future.successful(None)))
        val result = await(validator.recoverReportingEntity(xmlinfo))
        result pipe errors should contain(ReportingEntityElementMissing)
      }

      "CBCReports.docTypeIndic is OECD0" in {
        val validFile = new File("test/resources/cbcr-cbcReportsOECD0.xml")
        val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
        result pipe errors should not contain MessageTypeIndicError
      }

      "AdditionalInfo.docTypeInidc is OECD0" in {
        when(messageRefIdService.messageRefIdExists(any)(any)).thenReturn(Future.successful(false))
        when(reportingEntity.queryReportingEntityDataTin(any, any)(any))
          .thenReturn(EitherT.right(Future.successful(None)))
        when(reportingEntity.queryReportingEntityData(any)(any))
          .thenReturn(EitherT.right(Future.successful(Some(red))))

        val validFile = new File("test/resources/cbcr-additionalInfoOECD0.xml")
        val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
        result pipe errors should not contain MessageTypeIndicError
      }

      "AdditionalInfo.otherInfo is Empty" in {
        when(messageRefIdService.messageRefIdExists(any)(any)).thenReturn(Future.successful(false))
        when(reportingEntity.queryReportingEntityDataTin(any, any)(any))
          .thenReturn(EitherT.right(Future.successful(None)))
        when(reportingEntity.queryReportingEntityData(any)(any))
          .thenReturn(EitherT.right(Future.successful(Some(red))))

        val validFile = new File("test/resources/cbcr-additionalInfoOECD0.xml")
        val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
        result pipe errors should contain(OtherInfoEmpty)
      }

      "ReportingEntity.ReportingStartDate is after ReportingPeriod EndDate " in {
        val validFile = new File("test/resources/cbcr-invalidReportingDates1.xml")
        val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
        val err = result pipe errors
        err should contain(StartDateAfterEndDate)
        err should contain(AllReportingdatesInFuture)
      }

      "ReportingEntity.ReportingStartDate is in Future " in {
        val validFile = new File("test/resources/cbcr-invalidReportingDates2.xml")
        val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
        val err = result pipe errors
        err should contain(EndDateSameAsReportingPeriod)
        err should contain(StartDateNotBefore01012016)
      }

      "MessageTypeIndic is blank and AdditionalInfo.docTypeInidc is OECD0" in {
        val validFile = new File("test/resources/cbcr-additionalInfoOECD0-2.xml")
        val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
        result pipe errors should not contain MessageTypeIndicError
      }

      "MessageTypeIndic is blank and CBCReports.docTypeIndic is OECD0" in {
        val validFile = new File("test/resources/cbcr-cbcReportsOECD0-2.xml")
        val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
        result pipe errors should contain(MessageTypeIndicBlank)
      }

      "MessageTypeIndic is CBC402 and the DocTypeIndic's are invalid" when {
        "CBCReports.docTypeIndic isn't OECD2 or OECD3" in {
          val validFile = new File("test/resources/cbcr-messageTypeIndic.xml")
          when(reportingEntity.queryReportingEntityDataModel(any)(any))
            .thenReturn(EitherT.right(Future.successful(Some(redmFalse))))
          when(messageRefIdService.messageRefIdExists(any)(any)).thenReturn(Future.successful(false))
          when(reportingEntity.queryReportingEntityDataTin(any, any)(any))
            .thenReturn(EitherT.right(Future.successful(None)))
          when(reportingEntity.queryReportingEntityData(any)(any))
            .thenReturn(EitherT.right(Future.successful(Some(red))))

          val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
          result pipe errors should contain(MessageTypeIndicError)
        }

        "CBCReports[any].docTypeIndic isn't OECD2 or OECD3" in {
          val validFile = new File("test/resources/cbcr-messageTypeIndicM.xml")
          when(reportingEntity.queryReportingEntityDataModel(any)(any))
            .thenReturn(EitherT.right(Future.successful(Some(redmFalse))))
          val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
          result pipe errors should contain(MessageTypeIndicError)
        }

        "AdditionalInfo.docTypeIndic isn't OECD2 or OECD3" in {
          val validFile = new File("test/resources/cbcr-messageTypeIndic2.xml")
          when(messageRefIdService.messageRefIdExists(any)(any)).thenReturn(Future.successful(false))
          when(reportingEntity.queryReportingEntityDataTin(any, any)(any))
            .thenReturn(EitherT.right(Future.successful(None)))
          when(reportingEntity.queryReportingEntityData(any)(any))
            .thenReturn(EitherT.right(Future.successful(Some(red))))
          val result =
            await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))(120.seconds)
          result pipe errors should contain(MessageTypeIndicError)
        }

        "ReportingEntity.docTypeIndic isn't OECD2 or OECD3 or OECD0" in {
          val validFile = new File("test/resources/cbcr-messageTypeIndic3.xml")
          val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
          result pipe errors should contain(MessageTypeIndicError)
        }
      }

      "MessageTypeIndic is not provided" in {
        val invalidFile = new File("test/resources/cbcr-noMessageTypeIndic.xml")
        when(reportingEntity.queryReportingEntityDataModel(any)(any))
          .thenReturn(EitherT.right(Future.successful(Some(redmFalse))))
        val result = await(validator.validateBusinessRules(invalidFile, filename, Some(enrol), Some(Organisation)))
        result pipe errors should contain(MessageTypeIndicBlank)
      }

      "MessageTypeIndic is provided but invalid" in {
        val invalidFile = new File("test/resources/cbcr-InvalidMessageTypeIndic.xml")
        when(reportingEntity.queryReportingEntityDataModel(any)(any))
          .thenReturn(EitherT.right(Future.successful(Some(redmFalse))))
        val result = await(validator.validateBusinessRules(invalidFile, filename, Some(enrol), Some(Organisation)))
        result pipe errors should contain(MessageTypeIndicInvalid)
      }

      "when a corrRefId is present but refers to an anyunknownany docRefId" in {
        val validFile = new File("test/resources/cbcr-withCorrRefId.xml")
        when(docRefIdService.queryDocRefId(eqTo(docRefId1))(any)).thenReturn(Future.successful(DoesNotExist))
        when(docRefIdService.queryDocRefId(eqTo(docRefId2))(any)).thenReturn(Future.successful(DoesNotExist))
        when(docRefIdService.queryDocRefId(eqTo(docRefId3))(any)).thenReturn(Future.successful(DoesNotExist))
        when(docRefIdService.queryDocRefId(eqTo(docRefId4))(any)).thenReturn(Future.successful(DoesNotExist))

        when(docRefIdService.queryDocRefId(eqTo(corrDocRefId1))(any)).thenReturn(Future.successful(Valid))
        when(docRefIdService.queryDocRefId(eqTo(corrDocRefId2))(any)).thenReturn(Future.successful(Valid))
        when(docRefIdService.queryDocRefId(eqTo(corrDocRefId3))(any)).thenReturn(Future.successful(Valid))
        when(docRefIdService.queryDocRefId(eqTo(corrDocRefId4))(any)).thenReturn(Future.successful(DoesNotExist))

        when(reportingEntity.queryReportingEntityDataModel(any)(any))
          .thenReturn(EitherT.right(Future.successful(Some(redmFalse))))

        val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
        result pipe errors should contain(CorrDocRefIdUnknownRecord)
      }

      "when a corrRefId is present but refers to an anyinvalidany docRefId" in {
        val validFile = new File("test/resources/cbcr-withCorrRefId.xml")

        when(docRefIdService.queryDocRefId(eqTo(corrDocRefId1))(any)).thenReturn(Future.successful(Invalid))
        when(docRefIdService.queryDocRefId(eqTo(corrDocRefId2))(any)).thenReturn(Future.successful(Valid))
        when(docRefIdService.queryDocRefId(eqTo(corrDocRefId3))(any)).thenReturn(Future.successful(Valid))
        when(docRefIdService.queryDocRefId(eqTo(corrDocRefId4))(any)).thenReturn(Future.successful(Valid))

        when(reportingEntity.queryReportingEntityDataModel(any)(any))
          .thenReturn(EitherT.right(Future.successful(Some(redmFalse))))

        val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
        result pipe errors should contain(CorrDocRefIdInvalidRecord)
      }

      "when a CBCReports docRefId is a duplicate within the file" in {
        val validFile = new File("test/resources/cbcr-valid-dup.xml")
        when(messageRefIdService.messageRefIdExists(any)(any)).thenReturn(Future.successful(false))
        when(docRefIdService.queryDocRefId(any)(any)).thenReturn(Future.successful(DoesNotExist))

        val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
        result pipe errors should contain(DocRefIdDuplicate)
      }

      "when a AdditionalInfo docRefId is a duplicate within the file" in {
        val validFile = new File("test/resources/cbcr-valid-additional-dup.xml")
        when(messageRefIdService.messageRefIdExists(any)(any)).thenReturn(Future.successful(false))
        when(docRefIdService.queryDocRefId(any)(any)).thenReturn(Future.successful(DoesNotExist))

        val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
        result pipe errors should contain(DocRefIdDuplicate)
      }

      "when a docRefId is a duplicate" in {
        val validFile = new File("test/resources/cbcr-valid.xml")
        when(docRefIdService.queryDocRefId(eqTo(docRefId1))(any)).thenReturn(Future.successful(Valid))
        when(docRefIdService.queryDocRefId(eqTo(docRefId2))(any)).thenReturn(Future.successful(DoesNotExist))
        when(docRefIdService.queryDocRefId(eqTo(docRefId3))(any)).thenReturn(Future.successful(DoesNotExist))

        val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
        result pipe errors shouldBe List(DocRefIdDuplicate)
      }

      "when a docRefId is a duplicate but the duplicate is in an Unchanged ReportingEntity section" in {
        val validFile = new File("test/resources/cbcr-valid-dup-re-unchanged.xml")
        when(docRefIdService.queryDocRefId(eqTo(docRefId1))(any)).thenReturn(Future.successful(Valid))
        when(docRefIdService.queryDocRefId(eqTo(docRefId2))(any)).thenReturn(Future.successful(DoesNotExist))
        when(docRefIdService.queryDocRefId(eqTo(corrDocRefId2))(any)).thenReturn(Future.successful(Valid))
        when(docRefIdService.queryDocRefId(eqTo(docRefId3))(any)).thenReturn(Future.successful(DoesNotExist))
        when(docRefIdService.queryDocRefId(eqTo(corrDocRefId3))(any)).thenReturn(Future.successful(Valid))
        when(reportingEntity.queryReportingEntityDataDocRefId(any)(any))
          .thenReturn(EitherT.right(Future.successful(Some(red))))
        when(reportingEntity.queryReportingEntityData(any)(any)).thenReturn(EitherT.right(Future.successful(Some(red))))
        when(reportingEntity.queryReportingEntityDataModel(any)(any))
          .thenReturn(EitherT.right(Future.successful(Some(redmFalse))))

        val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))

        result.fold(
          _ => fail("No errors should be generated"),
          _ => ()
        )
      }

      "when the DocType is OECD1 but there are CorrDocRefIds defined" in {
        when(docRefIdService.queryDocRefId(eqTo(docRefId1))(any)).thenReturn(Future.successful(DoesNotExist))
        when(docRefIdService.queryDocRefId(eqTo(docRefId2))(any)).thenReturn(Future.successful(DoesNotExist))
        when(docRefIdService.queryDocRefId(eqTo(docRefId3))(any)).thenReturn(Future.successful(DoesNotExist))
        when(reportingEntity.queryReportingEntityDataModel(any)(any))
          .thenReturn(EitherT.right(Future.successful(Some(redmFalse))))

        val validFile1 = new File("test/resources/cbcr-OECD1-with-CorrDocRefIds1.xml")
        await(validator.validateBusinessRules(validFile1, filename, Some(enrol), Some(Organisation))) pipe
          errors should contain(CorrDocRefIdNotNeeded)
        val validFile2 = new File("test/resources/cbcr-OECD1-with-CorrDocRefIds2.xml")
        await(validator.validateBusinessRules(validFile2, filename, Some(enrol), Some(Organisation))) pipe
          errors should contain(CorrDocRefIdNotNeeded)
        val validFile3 = new File("test/resources/cbcr-OECD1-with-CorrDocRefIds3.xml")
        await(validator.validateBusinessRules(validFile3, filename, Some(enrol), Some(Organisation))) pipe
          errors should contain(CorrDocRefIdNotNeeded)
        val validFile4 = new File("test/resources/cbcr-OECD1-with-CorrDocRefIds4.xml")
        await(validator.validateBusinessRules(validFile4, filename, Some(enrol), Some(Organisation))) pipe
          errors should contain(CorrDocRefIdNotNeeded)
      }

      "when the DocType is OECD[23] but there are no CorrDocRefIds defined" in {
        val validFile = new File("test/resources/cbcr-OECD2-with-NoCorrDocRefIds.xml")
        val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
        result pipe errors should contain(CorrDocRefIdMissing)
      }

      "when the messageTypeIndic is CBC401 but ADD or ENT doctypeIndic is not OECD1" in {
        when(reportingEntity.queryReportingEntityDataByCbcId(any, any)(any))
          .thenReturn(EitherT.right(Future.successful(None)))
        val validFile = new File("test/resources/cbcr-OECD2-Incompatible-messageTypes.xml")
        val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
        result pipe errors should contain(MessageTypeIndicDocTypeIncompatible)
      }

      "when the messageTypeIndic is CBC401 but REP doctypeIndic is OECD1 or OECD0" in {
        val validFile = new File("test/resources/cbcr-OECD0[1]-valid.xml")
        val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))

        result.fold(
          errors => fail(s"errors generated: $errors"),
          _ => ()
        )

        val validFile2 = new File("test/resources/cbcr-OECD0[1]-valid2.xml")
        val result2 = await(validator.validateBusinessRules(validFile2, filename, Some(enrol), Some(Organisation)))

        result2.fold(
          errors => fail(s"errors generated: $errors"),
          _ => ()
        )
      }

      "when the messageTypeIndic is CBC401 but REP doctypeIndic is not OECD1 or OECD0" in {
        val validFile = new File("test/resources/cbcr-OECD0[1]-invalid.xml")
        val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
        result pipe errors should contain(IncompatibleOECDTypes)
      }

      "when the messageTypeIndic is CBC401 and REP doctypeIndic OECD0 but is not a known docrefid" in {
        val validFile = new File("test/resources/cbcr-OECD0[1]-invalid1.xml")
        when(messageRefIdService.messageRefIdExists(any)(any)).thenReturn(Future.successful(false))
        when(reportingEntity.queryReportingEntityDataTin(any, any)(any))
          .thenReturn(EitherT.right(Future.successful(None)))
        when(reportingEntity.queryReportingEntityDataDocRefId(any)(any))
          .thenReturn(EitherT.right(Future.successful(None)))
        when(reportingEntity.queryReportingEntityDataModel(any)(any))
          .thenReturn(EitherT.right(Future.successful(Some(redmFalse))))
        val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
        result pipe errors should contain(ResentDataIsUnknownError)
      }

      "when there are a mixture of OECD1 and OECD[23] docTypeIndics" in {
        val validFile = new File("test/resources/cbcr-docTypeIndicMixture.xml")

        val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
        result pipe errors should contain(IncompatibleOECDTypes)
      }

      "when there are invalid docRefIds" in {
        val validFile = new File("test/resources/cbcr-withInvalidDocRefId.xml")
        val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
        result pipe errors should contain(InvalidDocRefId)
      }

      "when there are invalid corrDocRefIds" in {
        val validFile = new File("test/resources/cbcr-withInvalidCorrDocRefId.xml")
        when(docRefIdService.queryDocRefId(eqTo(corrDocRefId1))(any)).thenReturn(Future.successful(Valid))
        when(docRefIdService.queryDocRefId(eqTo(corrDocRefId2))(any)).thenReturn(Future.successful(Valid))
        when(docRefIdService.queryDocRefId(eqTo(corrDocRefId3))(any)).thenReturn(Future.successful(Valid))

        val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
        result pipe errors should contain(InvalidCorrDocRefId)
      }

      "when the CBC_OECD version is invalid" in {
        val validFile = new File("test/resources/cbcr-withInvalidCBC-OECDVersion.xml")
        val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
        result pipe errors should contain(CbcOecdVersionError)
      }

      "when the XML Encoding value is NOT UTF-8" in {
        val validFile = new File("test/resources/cbcr-withInvalidXmlEncodingValue.xml")
        val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
        result pipe errors should contain(XmlEncodingError)
      }

      "when the DocRefId refers to the wrong parent group element" in {
        val validFile = new File("test/resources/cbcr-invalid-docrefid-PGE.xml")
        val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
        result pipe errors should contain(DocRefIdInvalidParentGroupElement)
      }

      "when the CorrDocRefId refers to the wrong parent group element" in {
        val validFile = new File("test/resources/cbcr-invalid-corrdocrefid-PGE.xml")
        val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
        result pipe errors should contain(CorrDocRefIdInvalidParentGroupElement)
      }

      "when the same CorrDocRefId is used multiple times" in {
        val validFile = new File("test/resources/cbcr-withCorrRefIdDup.xml")
        when(docRefIdService.queryDocRefId(eqTo(docRefId1))(any)).thenReturn(Future.successful(DoesNotExist))
        when(docRefIdService.queryDocRefId(eqTo(docRefId2))(any)).thenReturn(Future.successful(DoesNotExist))
        when(docRefIdService.queryDocRefId(eqTo(docRefId3))(any)).thenReturn(Future.successful(DoesNotExist))
        when(docRefIdService.queryDocRefId(eqTo(docRefId4))(any)).thenReturn(Future.successful(DoesNotExist))

        when(docRefIdService.queryDocRefId(eqTo(corrDocRefId1))(any)).thenReturn(Future.successful(Valid))
        when(docRefIdService.queryDocRefId(eqTo(corrDocRefId2))(any)).thenReturn(Future.successful(Valid))
        when(docRefIdService.queryDocRefId(eqTo(corrDocRefId3))(any)).thenReturn(Future.successful(Valid))
        when(docRefIdService.queryDocRefId(eqTo(corrDocRefId4))(any)).thenReturn(Future.successful(DoesNotExist))

        when(reportingEntity.queryReportingEntityDataModel(any)(any))
          .thenReturn(EitherT.right(Future.successful(Some(redmFalse))))

        val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
        result pipe errors should contain(CorrDocRefIdDuplicate)
      }

      "when the FilingType == CBC701" when {
        "the TIN field is not a valid UTR" in {
          val validFile = new File("test/resources/cbcr-CBC701-badTIN.xml")
          val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
          result pipe errors should contain(InvalidXMLError("xmlValidationError.InvalidTIN"))
        }

        "the @issuedBy attribute of the TIN is not 'GB' " in {
          val validFile = new File("test/resources/cbcr-CBC701-badTINAttribute.xml")
          val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
          result pipe errors should contain(InvalidXMLError("xmlValidationError.TINIssuedBy"))
        }
      }

      "when the FilingType == CBC703" when {
        "the TIN field is not a valid UTR" in {
          val validFile = new File("test/resources/cbcr-CBC703-badTIN.xml")
          val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
          result pipe errors should contain(InvalidXMLError("xmlValidationError.InvalidTIN"))
        }

        "the @issuedBy attribute of the TIN is not 'GB' " in {
          when(reportingEntity.queryReportingEntityDataByCbcId(any, any)(any))
            .thenReturn(EitherT.right(Future.successful(None)))
          val validFile = new File("test/resources/cbcr-CBC703-badTINAttribute.xml")
          val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
          result pipe errors should contain(InvalidXMLError("xmlValidationError.TINIssuedBy"))
        }
      }

      "when the FilingType == CBC704" when {
        "the TIN field is not a valid UTR" in {
          val validFile = new File("test/resources/cbcr-CBC704-badTIN.xml")
          val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
          result pipe errors should contain(InvalidXMLError("xmlValidationError.InvalidTIN"))
        }

        "the @issuedBy attribute of the TIN is not 'GB' " in {
          when(reportingEntity.queryReportingEntityDataByCbcId(any, any)(any))
            .thenReturn(EitherT.right(Future.successful(None)))
          val validFile = new File("test/resources/cbcr-CBC704-badTINAttribute.xml")
          val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
          result pipe errors should contain(InvalidXMLError("xmlValidationError.TINIssuedBy"))
        }
      }

      "when the FilingType == CBC702" when {
        "the TIN field is unrestricted" in {
          val validFile = new File("test/resources/cbcr-CBC702-badTIN.xml")
          val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
          result.fold(
            errors => fail(s"CBC702 should handle non UTR in TIN field: ${errors.toList.mkString("\n")}"),
            _ => ()
          )
        }

        "the @issuedBy attribute of the TIN is unrestricted" in {
          when(reportingEntity.queryReportingEntityDataByCbcId(any, any)(any))
            .thenReturn(EitherT.right(Future.successful(None)))
          val validFile = new File("test/resources/cbcr-CBC702-badTINAttribute.xml")
          val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))

          result.fold(
            errors => fail(s"CBC703 should handle non GB issuedBy field: ${errors.toList.mkString("\n")}"),
            _ => ()
          )
        }
      }

      "return the KeyXmlInfo when everything is fine and were using a NON GB TIN for a 702 submission" in {
        when(reportingEntity.queryReportingEntityDataByCbcId(any, any)(any))
          .thenReturn(EitherT.right(Future.successful(None)))

        val validFile = new File("test/resources/cbcr-valid-nonGBTINInDocRefId.xml")
        val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))

        result.fold(
          errors => fail(s"Error were generated: $errors"),
          x => x.cbcReport.size shouldBe 4
        )
      }

      "return the KeyXmlInfo when everything is fine" in {
        when(reportingEntity.queryReportingEntityDataByCbcId(any, any)(any))
          .thenReturn(EitherT.right(Future.successful(None)))

        val validFile = new File("test/resources/cbcr-valid.xml")
        val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))

        result.fold(
          errors => fail(s"Error were generated: $errors"),
          x => x.cbcReport.size shouldBe 4
        )
      }

      "should not create an error" when {
        "Should not fail when utf-8 is lowercase" in {
          val validFile = new File("test/resources/lower-case-utf8-pre-amble.xml")
          val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))

          result.fold(
            _ => fail("Should not fail when utf-8 is lowercase"),
            _ => ()
          )
        }
      }

      "the submission contains a correction" when {
        "the original submission was created > 3 years ago" in {
          when(creationDateService.isDateValid(any)(any)).thenReturn(Future.successful(DateOld))
          when(reportingEntity.queryReportingEntityDataModel(any)(any))
            .thenReturn(EitherT.right(Future.successful(Some(redmFalse))))
          when(docRefIdService.queryDocRefId(any)(any)).thenReturn(Future.successful(Valid))
          val validFile = new File("test/resources/cbcr-withCorrRefId.xml")
          val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
          result pipe errors should contain(CorrectedFileTooOld)
        }

        "the original submission's date is missing" in {
          when(creationDateService.isDateValid(any)(any)).thenReturn(Future.successful(DateMissing))
          when(reportingEntity.queryReportingEntityDataModel(any)(any))
            .thenReturn(EitherT.right(Future.successful(Some(redmFalse))))
          when(docRefIdService.queryDocRefId(any)(any)).thenReturn(Future.successful(Valid))
          val validFile = new File("test/resources/cbcr-withCorrRefId.xml")
          val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
          result pipe errors should contain(CorrectedFileDateMissing)
        }

        "the original submission was < 3 years ago" in {
          when(reportingEntity.queryReportingEntityDataByCbcId(any, any)(any))
            .thenReturn(EitherT.right(Future.successful(None)))

          when(docRefIdService.queryDocRefId(eqTo(docRefId6))(any)).thenReturn(Future.successful(DoesNotExist))
          when(docRefIdService.queryDocRefId(eqTo(docRefId7))(any)).thenReturn(Future.successful(DoesNotExist))
          when(docRefIdService.queryDocRefId(eqTo(docRefId8))(any)).thenReturn(Future.successful(DoesNotExist))
          when(docRefIdService.queryDocRefId(eqTo(docRefId9))(any)).thenReturn(Future.successful(DoesNotExist))

          when(docRefIdService.queryDocRefId(eqTo(corrDocRefId1))(any)).thenReturn(Future.successful(Valid))
          when(docRefIdService.queryDocRefId(eqTo(corrDocRefId2))(any)).thenReturn(Future.successful(Valid))
          when(docRefIdService.queryDocRefId(eqTo(corrDocRefId3))(any)).thenReturn(Future.successful(Valid))
          when(docRefIdService.queryDocRefId(eqTo(corrDocRefId5))(any)).thenReturn(Future.successful(Valid))

          when(reportingEntity.queryReportingEntityDataModel(any)(any))
            .thenReturn(EitherT.right(Future.successful(Some(redmFalse))))

          when(creationDateService.isDateValid(any)(any)).thenReturn(Future.successful(DateCorrect))
          val validFile = new File("test/resources/cbcr-withCorrRefId.xml")
          val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))

          result.fold(
            errors => fail(s"Error were generated: $errors"),
            _ => ()
          )
        }
      }

      "the CorrMessageRefID not in MessageSpec or DocSpec" in {
        val validFile = new File("test/resources/cbcr-valid.xml")
        when(docRefIdService.queryDocRefId(any)(any)).thenReturn(Future.successful(DoesNotExist))

        when(reportingEntity.queryReportingEntityDataModel(any)(any))
          .thenReturn(EitherT.right(Future.successful(Some(redmFalse))))

        when(messageRefIdService.messageRefIdExists(any)(any)).thenReturn(Future.successful(false))

        val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))

        result.fold(
          errors => fail(s"Error were generated: $errors"),
          _ => ()
        )
      }

      "the CorrMessageRefID included in MessageSpec" in {
        val validFile = new File("test/resources/cbcr-invalidCorrMessageRefIdInMessageSpec.xml")
        val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
        result pipe errors should contain(CorrMessageRefIdNotAllowedInMessageSpec)
      }

      "the CorrMessageRefID included in ReportingEntity DocSpec" in {
        val validFile = new File("test/resources/cbcr-invalidCorrMessageRefIdInReportingEntity.xml")
        val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
        result pipe errors should contain(CorrMessageRefIdNotAllowedInDocSpec)
      }

      "the CorrMessageRefID included in CbcReports DocSpec" in {
        val validFile = new File("test/resources/cbcr-invalidCorrMessageRefIdInCbcReports.xml")
        val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
        result pipe errors should contain(CorrMessageRefIdNotAllowedInDocSpec)
      }

      "the CorrMessageRefID included in AdditionalInfo DocSpec" in {
        val validFile = new File("test/resources/cbcr-invalidCorrMessageRefIdInAdditionalInfo.xml")
        val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
        result pipe errors should contain(CorrMessageRefIdNotAllowedInDocSpec)
      }

      "the CorrMessageRefID included in AdditionalInfo DocSpec and CbcReports DocSpec and ReportingEntity DocSpec" in {
        val validFile = new File("test/resources/cbcr-invalidCorrMessageRefIdInAllDocSpec.xml")
        val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
        result pipe errors should contain(CorrMessageRefIdNotAllowedInDocSpec)
      }

      "the CorrMessageRefID included in both MessageSpec and DocSpec" in {
        val validFile = new File("test/resources/cbcr-invalidCorrMessageRefIdInMsgSpecDocSpec.xml")

        val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))

        result shouldBe Validated.Invalid(
          NonEmptyList(
            CorrMessageRefIdNotAllowedInMessageSpec,
            List(CorrMessageRefIdNotAllowedInDocSpec, StartDateNotBefore01012016)
          )
        )
      }

      "the reporting period of a correction does not match the reporting period of original submission" in {
        val validFile = new File("test/resources/cbcr-withCorrRefId.xml")
        when(docRefIdService.queryDocRefId(eqTo(corrDocRefId3))(any)).thenReturn(Future.successful(Valid))
        when(reportingEntity.queryReportingEntityData(any)(any))
          .thenReturn(EitherT.right(Future.successful(Some(redReportPeriod))))

        val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
        result pipe errors should contain(CorrectedFileDateMissing)
      }

      "the reporting period of a correction matches the reporting period of original submission" in {
        val validFile = new File("test/resources/cbcr-withCorrRefId.xml")
        when(docRefIdService.queryDocRefId(eqTo(corrDocRefId1))(any)).thenReturn(Future.successful(Valid))
        when(docRefIdService.queryDocRefId(eqTo(corrDocRefId2))(any)).thenReturn(Future.successful(Valid))
        when(docRefIdService.queryDocRefId(eqTo(corrDocRefId3))(any)).thenReturn(Future.successful(Valid))
        when(docRefIdService.queryDocRefId(eqTo(corrDocRefId5))(any)).thenReturn(Future.successful(Valid))
        when(reportingEntity.queryReportingEntityData(any)(any)).thenReturn(
          EitherT.right(
            Future.successful(
              Some(red.copy(reportingPeriod = Some(LocalDate.of(2016, 3, 31))))
            )
          )
        )
        val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))

        result.fold(
          errors => fail(s"Error were generated: $errors"),
          _ => ()
        )
      }

      "the original submission di not persist a reporting period and so reportingEntity).thenReturn(None" in {
        val validFile = new File("test/resources/cbcr-withCorrRefId.xml")
        when(docRefIdService.queryDocRefId(eqTo(corrDocRefId3))(any)).thenReturn(Future.successful(Valid))
        when(reportingEntity.queryReportingEntityData(any)(any)).thenReturn(EitherT.right(Future.successful(Some(red))))

        val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))

        result.fold(
          errors => fail(s"Error were generated: $errors"),
          _ => ()
        )
      }

      "the submission corrects AdditionalIno" when {
        "the original submission only persisted the 1st AdditionalInfo DRI but the submission corrects a subsequent AdditionalInfo section" in {
          when(creationDateService.isDateValid(any)(any)).thenReturn(Future.successful(DateError))
          when(messageRefIdService.messageRefIdExists(any)(any)).thenReturn(Future.successful(false))
          when(reportingEntity.queryReportingEntityDataModel(any)(any))
            .thenReturn(EitherT.right(Future.successful(Some(redmTrue))))
          when(docRefIdService.queryDocRefId(eqTo(docRefId3))(any)).thenReturn(Future.successful(DoesNotExist))
          when(docRefIdService.queryDocRefId(eqTo(corrDocRefId3))(any)).thenReturn(Future.successful(DoesNotExist))
          when(reportingEntity.queryReportingEntityData(any)(any))
            .thenReturn(EitherT.right(Future.successful(Some(red))))
          when(reportingEntity.queryReportingEntityDataTin(any, any)(any))
            .thenReturn(EitherT.right(Future.successful(None)))

          val validFile = new File("test/resources/cbcr-withAddInfoCorrRefId-invalid.xml")
          val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
          result pipe errors should contain(AdditionalInfoDRINotFound(actualDocRefId2.toString, corrDocRefId3.toString))
        }

        "the original submission only persisted the 1st AdditionalInfo DRI when no ReportingEntity is present" in {
          when(creationDateService.isDateValid(any)(any)).thenReturn(Future.successful(DateCorrect))
          when(messageRefIdService.messageRefIdExists(any)(any)).thenReturn(Future.successful(false))
          when(reportingEntity.queryReportingEntityDataModel(any)(any))
            .thenReturn(EitherT.right(Future.successful(Some(redmTrue))))
          when(docRefIdService.queryDocRefId(eqTo(docRefId3))(any)).thenReturn(Future.successful(DoesNotExist))
          when(docRefIdService.queryDocRefId(eqTo(corrDocRefId3))(any)).thenReturn(Future.successful(Valid))
          when(reportingEntity.queryReportingEntityData(any)(any))
            .thenReturn(EitherT.right(Future.successful(Some(red))))
          val validFile = new File("test/resources/cbcr-withAddInfoCorrRefId-invalid.xml")
          val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
          result pipe errors should contain(ReportingEntityElementMissing)
        }

        "the original submission only persisted the 1st AdditionalInfo DRI and the submission corrects that AdditionalInfo section" in {
          when(creationDateService.isDateValid(any)(any)).thenReturn(Future.successful(DateCorrect))
          when(messageRefIdService.messageRefIdExists(any)(any)).thenReturn(Future.successful(false))
          when(reportingEntity.queryReportingEntityDataModel(any)(any))
            .thenReturn(EitherT.right(Future.successful(Some(redmTrue))))
          when(docRefIdService.queryDocRefId(eqTo(docRefId3))(any)).thenReturn(Future.successful(DoesNotExist))
          when(docRefIdService.queryDocRefId(eqTo(corrDocRefId3))(any)).thenReturn(Future.successful(Valid))
          when(docRefIdService.queryDocRefId(eqTo(docRefId1))(any)).thenReturn(Future.successful(Valid))
          when(reportingEntity.queryReportingEntityData(any)(any))
            .thenReturn(EitherT.right(Future.successful(Some(red))))
          when(reportingEntity.queryReportingEntityDataDocRefId(any)(any))
            .thenReturn(EitherT.right(Future.successful(Some(red))))
          val validFile = new File("test/resources/cbcr-withAddInfoCorrRefId.xml")
          val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))

          result.fold(
            errors => fail(s"Error were generated: $errors"),
            _ => ()
          )
        }

        "the original submission persisted ALL AdditionalInfo DRI and no ReportingEntity is present" in {
          when(creationDateService.isDateValid(any)(any)).thenReturn(Future.successful(DateCorrect))
          when(messageRefIdService.messageRefIdExists(any)(any)).thenReturn(Future.successful(false))
          when(reportingEntity.queryReportingEntityDataModel(any)(any))
            .thenReturn(EitherT.right(Future.successful(Some(redmFalse))))
          when(docRefIdService.queryDocRefId(eqTo(docRefId3))(any)).thenReturn(Future.successful(DoesNotExist))
          when(docRefIdService.queryDocRefId(eqTo(corrDocRefId3))(any)).thenReturn(Future.successful(Valid))
          when(reportingEntity.queryReportingEntityData(any)(any))
            .thenReturn(EitherT.right(Future.successful(Some(red))))
          val validFile = new File("test/resources/cbcr-withAddInfoCorrRefId-invalid.xml")
          val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
          result pipe errors should contain(ReportingEntityElementMissing)
        }

        "the original submission persisted ALL AdditionalInfo DRI and the submission correctly corrects one AdditionalInfo section" in {
          when(creationDateService.isDateValid(any)(any)).thenReturn(Future.successful(DateCorrect))
          when(messageRefIdService.messageRefIdExists(any)(any)).thenReturn(Future.successful(false))
          when(reportingEntity.queryReportingEntityDataModel(any)(any))
            .thenReturn(EitherT.right(Future.successful(Some(redmFalse))))
          when(docRefIdService.queryDocRefId(eqTo(docRefId3))(any)).thenReturn(Future.successful(DoesNotExist))
          when(docRefIdService.queryDocRefId(eqTo(corrDocRefId3))(any)).thenReturn(Future.successful(Valid))
          when(reportingEntity.queryReportingEntityData(any)(any))
            .thenReturn(EitherT.right(Future.successful(Some(red))))
          val validFile = new File("test/resources/cbcr-withAddInfoCorrRefId.xml")
          val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))

          result.fold(
            errors => fail(s"Error were generated: $errors"),
            _ => ()
          )
        }

        "the original submission persisted ALL AdditionalInfo DRI but the submission attempts to corrects a none-existent AdditionalInfo section" in {
          when(creationDateService.isDateValid(any)(any)).thenReturn(Future.successful(DateCorrect))
          when(messageRefIdService.messageRefIdExists(any)(any)).thenReturn(Future.successful(false))
          when(reportingEntity.queryReportingEntityDataModel(any)(any))
            .thenReturn(EitherT.right(Future.successful(Some(redmFalse))))
          when(docRefIdService.queryDocRefId(eqTo(docRefId3))(any)).thenReturn(Future.successful(DoesNotExist))
          when(docRefIdService.queryDocRefId(eqTo(corrDocRefId3))(any)).thenReturn(Future.successful(DoesNotExist))
          when(reportingEntity.queryReportingEntityData(any)(any))
            .thenReturn(EitherT.right(Future.successful(Some(red))))
          val validFile = new File("test/resources/cbcr-withAddInfoCorrRefId-invalid.xml")
          val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
          result pipe errors should contain(CorrDocRefIdUnknownRecord)
        }

        "the original submission persisted ALL AdditionalInfo DRI but the submission attempts to corrects a previously corrected AdditionalInfo section" in {
          when(creationDateService.isDateValid(any)(any)).thenReturn(Future.successful(DateCorrect))
          when(messageRefIdService.messageRefIdExists(any)(any)).thenReturn(Future.successful(false))
          when(reportingEntity.queryReportingEntityDataModel(any)(any))
            .thenReturn(EitherT.right(Future.successful(Some(redmFalse))))
          when(docRefIdService.queryDocRefId(eqTo(docRefId3))(any)).thenReturn(Future.successful(DoesNotExist))
          when(docRefIdService.queryDocRefId(eqTo(corrDocRefId3))(any)).thenReturn(Future.successful(Invalid))
          when(reportingEntity.queryReportingEntityData(any)(any))
            .thenReturn(EitherT.right(Future.successful(Some(red))))
          val validFile = new File("test/resources/cbcr-withAddInfoCorrRefId-invalid.xml")
          val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
          result pipe errors should contain(CorrDocRefIdInvalidRecord)
        }
      }
    }

    "throw an error when the user partially changes the currency code in a correction" in {
      val firstOriginalReportingEntityDri =
        DocRefId("GB2016RGXLCBC0100000056CBC40120170311T090000X1_7000000002OECD1ENT1").get
      val firstOriginalCbcReportsDri =
        DocRefId("GB2016RGXLCBC0100000056CBC40120170311T090000X1_7000000002OECD1REP1").get
      val secondOriginalCbcReportsDri =
        DocRefId("GB2016RGXLCBC0100000056CBC40120170311T090000X1_7000000002OECD1REP2").get
      val firstOriginalAdditionalInfoDri =
        DocRefId("GB2016RGXLCBC0100000056CBC40120170311T090000X1_7000000002OECD1ADD1").get

      val reportEntityData = ReportingEntityData(
        NonEmptyList.of(firstOriginalCbcReportsDri, secondOriginalCbcReportsDri),
        List(firstOriginalAdditionalInfoDri),
        firstOriginalReportingEntityDri,
        TIN("7000000002", "GB"),
        UltimateParentEntity("someone"),
        CBC703,
        Some(LocalDate.now()),
        Some(LocalDate.of(2016, 3, 31)),
        Some("GBP"),
        Some(EntityReportingPeriod(LocalDate.parse("2016-01-02"), LocalDate.parse("2016-03-31")))
      )
      val reportEntityDataModel = ReportingEntityDataModel(
        NonEmptyList.of(firstOriginalCbcReportsDri, secondOriginalCbcReportsDri),
        List(firstOriginalAdditionalInfoDri),
        firstOriginalReportingEntityDri,
        TIN("7000000002", "GB"),
        UltimateParentEntity("someone"),
        CBC703,
        Some(LocalDate.now()),
        Some(LocalDate.of(2016, 3, 31)),
        oldModel = false,
        Some("GBP"),
        Some(EntityReportingPeriod(LocalDate.parse("2016-01-02"), LocalDate.parse("2016-03-31")))
      )

      when(messageRefIdService.messageRefIdExists(any)(any)).thenReturn(Future.successful(false))
      when(docRefIdService.queryDocRefId(any)(any)).thenReturn(Future.successful(Valid))
      when(reportingEntity.queryReportingEntityDataTin(any, any)(any))
        .thenReturn(EitherT.right(Future.successful(Some(reportEntityData))))
      when(reportingEntity.queryReportingEntityDataModel(any)(any))
        .thenReturn(EitherT.right(Future.successful(Some(reportEntityDataModel))))
      when(reportingEntity.queryReportingEntityData(any)(any))
        .thenReturn(EitherT.right(Future.successful(Some(reportEntityData))))
      val partiallyCorrectedCurrency =
        new File("test/resources/cbcr-with-partially-corrected-currency.xml")

      val fullFile = new File("test/resources/cbcr-with-fully-corrected-currency.xml")
      await(validator.validateBusinessRules(partiallyCorrectedCurrency, filename, Some(enrol), Some(Organisation))) pipe
        errors should contain(PartiallyCorrectedCurrency)
      await(validator.validateBusinessRules(fullFile, filename, Some(enrol), Some(Organisation))) pipe
        errors shouldNot contain(PartiallyCorrectedCurrency)
    }

    "throw an error when the user partially deletes a file" in {
      val firstOriginalReportingEntityDri =
        DocRefId("GB2017RGXLCBC0100000056CBC40120180311T090000X2017_7000000002OECD1ENT1").get
      val firstOriginalCbcReportsDri =
        DocRefId("GB2017RGXLCBC0100000056CBC40120180311T090000X2017_7000000002OECD1REP1").get
      val secondOriginalCbcReportsDri =
        DocRefId("GB2017RGXLCBC0100000056CBC40120180311T090000X2017_7000000002OECD1REP2").get
      val firstOriginalAdditionalInfoDri =
        DocRefId("GB2017RGXLCBC0100000056CBC40120180311T090000X2017_7000000002OECD1ADD1").get

      val reportEntityData = ReportingEntityData(
        NonEmptyList.of(firstOriginalCbcReportsDri, secondOriginalCbcReportsDri),
        List(firstOriginalAdditionalInfoDri),
        firstOriginalReportingEntityDri,
        TIN("7000000002", "GB"),
        UltimateParentEntity("someone"),
        CBC703,
        Some(LocalDate.now()),
        Some(LocalDate.of(2017, 3, 31)),
        Some("USD"),
        Some(EntityReportingPeriod(LocalDate.parse("2017-01-02"), LocalDate.parse("2017-03-31")))
      )
      val reportEntityDataModel = ReportingEntityDataModel(
        NonEmptyList.of(firstOriginalCbcReportsDri, secondOriginalCbcReportsDri),
        List(firstOriginalAdditionalInfoDri),
        firstOriginalReportingEntityDri,
        TIN("7000000002", "GB"),
        UltimateParentEntity("someone"),
        CBC703,
        Some(LocalDate.now()),
        Some(LocalDate.of(2017, 3, 31)),
        oldModel = false,
        Some("USD"),
        Some(EntityReportingPeriod(LocalDate.parse("2017-01-02"), LocalDate.parse("2017-03-31")))
      )

      when(messageRefIdService.messageRefIdExists(any)(any)).thenReturn(Future.successful(false))
      when(docRefIdService.queryDocRefId(any)(any)).thenReturn(Future.successful(Valid))
      when(
        docRefIdService.queryDocRefId(
          eqTo(DocRefId("GB2017RGXLCBC0100000056CBC40220180311T090000X2018_7000000002OECD3ENTDeletion").get)
        )(any)
      ).thenReturn(
        Future
          .successful(DoesNotExist)
      )
      when(
        docRefIdService.queryDocRefId(
          eqTo(DocRefId("GB2017RGXLCBC0100000056CBC40220180311T090000X2018_7000000002OECD3REP1Deletion").get)
        )(any)
      ).thenReturn(
        Future
          .successful(DoesNotExist)
      )
      when(
        docRefIdService.queryDocRefId(
          eqTo(DocRefId("GB2017RGXLCBC0100000056CBC40220180311T090000X2018_7000000002OECD3ADDDeletion").get)
        )(any)
      ).thenReturn(
        Future
          .successful(DoesNotExist)
      )
      when(reportingEntity.queryReportingEntityDataTin(any, any)(any))
        .thenReturn(EitherT.right(Future.successful(Some(reportEntityData))))
      when(reportingEntity.queryReportingEntityDataModel(any)(any))
        .thenReturn(EitherT.right(Future.successful(Some(reportEntityDataModel))))
      when(reportingEntity.queryReportingEntityData(any)(any))
        .thenReturn(EitherT.right(Future.successful(Some(reportEntityData))))
      val partialDeletionFile = new File("test/resources/cbcr-partial-deletion.xml")
      val anotherPartialDeletion = new File("test/resources/cbcr-inconsistent-OECD3.xml")

      val fullDeletion = new File("test/resources/cbcr-full-deletion.xml")
      val filenameOrig = "GB2017RGXLCBC0100000056CBC40120180311T090000X2018.xml"
      val filenameSecond = "GB2017RGXLCBC0100000056CBC40120180311T090000X2018Second.xml"
      val filenameThird = "GB2017RGXLCBC0100000056CBC40120180311T090000X2018Third.xml"
      await(validator.validateBusinessRules(partialDeletionFile, filenameOrig, Some(enrol), Some(Organisation))) pipe
        errors should contain(PartialDeletion)
      await(
        validator.validateBusinessRules(anotherPartialDeletion, filenameSecond, Some(enrol), Some(Organisation))
      ) pipe
        errors should contain(PartialDeletion)
      await(validator.validateBusinessRules(fullDeletion, filenameThird, Some(enrol), Some(Organisation))) pipe
        errors shouldNot contain(PartialDeletion)
    }

    "validate dates overlapping should return invalid if dates are overlapping" in {
      when(reportingEntity.queryReportingEntityDatesOverlapping(any, any)(any))
        .thenReturn(EitherT.right(Future.successful(Some(DatesOverlap(true)))))
      when(messageRefIdService.messageRefIdExists(any)(any)).thenReturn(Future.successful(false))
      when(reportingEntity.queryReportingEntityDataTin(any, any)(any))
        .thenReturn(EitherT.right(Future.successful(None)))

      val validFile = new File("test/resources/cbcr-valid-2.xml")
      val filename = "GB2019RGXLCBC0100000056CBC40120201101T090000Xvalid2.xml"

      val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
      result pipe errors should contain(DatesOverlapInvalid)
    }

    "validate dates overlapping should return valid if dates are not overlapping" in {
      when(reportingEntity.queryReportingEntityDatesOverlapping(any, any)(any))
        .thenReturn(EitherT.right(Future.successful(Some(DatesOverlap(false)))))
      when(messageRefIdService.messageRefIdExists(any)(any)).thenReturn(Future.successful(false))
      when(reportingEntity.queryReportingEntityDataTin(any, any)(any))
        .thenReturn(EitherT.right(Future.successful(None)))

      val validFile = new File("test/resources/cbcr-valid-2.xml")
      val filename = "GB2019RGXLCBC0100000056CBC40120201101T090000Xvalid2.xml"

      val result = await(validator.validateBusinessRules(validFile, filename, Some(enrol), Some(Organisation)))
      result pipe errors should not contain DatesOverlapInvalid
    }
  }
}
