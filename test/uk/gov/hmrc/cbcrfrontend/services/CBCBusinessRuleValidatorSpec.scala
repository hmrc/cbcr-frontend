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

import java.io.File
import java.time.{LocalDate, LocalDateTime}

import cats.data.EitherT
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.play.http.HeaderCarrier
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import cats.instances.future._
import uk.gov.hmrc.cbcrfrontend.model.DocRefIdResponses.{DoesNotExist, Invalid, Valid}
import org.mockito.Matchers.{eq => EQ, _}
import uk.gov.hmrc.emailaddress.EmailAddress
import play.api.Configuration

/**
  * Created by max on 24/05/17.
  */
class CBCBusinessRuleValidatorSpec extends UnitSpec with MockitoSugar{


  val messageRefIdService = mock[MessageRefIdService]
  val docRefIdService = mock[DocRefIdService]
  val subscriptionDataService = mock[SubscriptionDataService]
  val reportingEntity = mock[ReportingEntityDataService]
  val configuration = mock[Configuration]
  val runMode = mock[RunMode]


  val docRefId1 = DocRefId("GB2016RGXLCBC0100000056CBC40120170311T090000X_7000000002OECD1ENT").getOrElse(fail("bad docrefid"))
  val docRefId2 = DocRefId("GB2016RGXLCBC0100000056CBC40120170311T090000X_7000000002OECD1REP").getOrElse(fail("bad docrefid"))
  val docRefId3 = DocRefId("GB2016RGXLCBC0100000056CBC40120170311T090000X_7000000002OECD1ADD").getOrElse(fail("bad docrefid"))
  val docRefId4 = DocRefId("GB2016RGXLCBC0100000056CBC40120170311T090000X_7000000002OECD1REP2").getOrElse(fail("bad docrefid"))

  val corrDocRefId1 = DocRefId("GB2016RGXLCBC0100000056CBC40120170311T090000X_7000000002OECD1ENTC").getOrElse(fail("bad docrefid"))
  val corrDocRefId2 = DocRefId("GB2016RGXLCBC0100000056CBC40120170311T090000X_7000000002OECD1REPC").getOrElse(fail("bad docrefid"))
  val corrDocRefId3 = DocRefId("GB2016RGXLCBC0100000056CBC40120170311T090000X_7000000002OECD1ADDC").getOrElse(fail("bad docrefid"))
  val corrDocRefId4 = DocRefId("GB2016RGXLCBC0100000056CBC40120170311T090000X_7000000002OECD1REP2C").getOrElse(fail("bad docrefid"))

  val schemaVer: String = "1.0"

  when(docRefIdService.queryDocRefId(any())(any())) thenReturn Future.successful(DoesNotExist)
  when(subscriptionDataService.retrieveSubscriptionData(any())(any(),any())) thenReturn EitherT.pure[Future,CBCErrors,Option[SubscriptionDetails]](Some(submissionData))
  when(runMode.env) thenReturn "Dev"
  when(configuration.getString(s"${runMode.env}.oecd-schema-version")) thenReturn Future.successful(Some(schemaVer))

  implicit val hc = HeaderCarrier()
  val extract = new XmlInfoExtract()
  implicit def fileToXml(f:File) : RawXMLInfo = extract.extract(f)

  val cbcId = CBCId.create(56).toOption
  val filename = "GB2016RGXLCBC0100000056CBC40120170311T090000X.xml"
  val filenamePB = "GB2016RGXVCBC0000000056CBC40120170311T090000X.xml"

  val submissionData = SubscriptionDetails(
    BusinessPartnerRecord("SAFEID",Some(OrganisationResponse("blagh")),EtmpAddress("Line1",None,None,None,Some("TF3 XFE"),"GB")),
    SubscriberContact("Brian","Lastname", "phonenum",EmailAddress("test@test.com")),cbcId,Utr("7000000002")
  )


  val docRefId="GB2016RGXVCBC0000000056CBC40120170311T090000X_7000000002OECD1"
  val xmlinfo = XMLInfo(
    MessageSpec(
      MessageRefID("GB2016RGXVCBC0000000056CBC40120170311T090000X").getOrElse(fail("waaaaa")),
      "GB",
      CBCId.create(99).getOrElse(fail("booo")),
      LocalDateTime.now(),
      LocalDate.parse("2017-01-30"),
      None
    ),
    None,
    List(CbcReports(DocSpec(OECD1,DocRefId(docRefId + "ENT").get,None))),
    Some(AdditionalInfo(DocSpec(OECD1,DocRefId(docRefId + "ADD").get,None)))
  )

  val validator = new CBCBusinessRuleValidator(messageRefIdService,docRefIdService,subscriptionDataService,reportingEntity, configuration,runMode)
  "The CBCBusinessRuleValidator" should {
    "return the correct error" when {
      "messageRefId is empty" in {
        val missingMessageRefID = new File("test/resources/cbcr-invalid-empty-messageRefID.xml")
        val result = Await.result(validator.validateBusinessRules(missingMessageRefID, filename), 5.seconds)

        result.fold(
          errors => errors.toList should contain(MessageRefIDMissing),
          _ => fail("No MessageRefIDMissing error generated")
        )
      }
      "messageRefId is null" in {
        val nullMessageRefID = new File("test/resources/cbcr-invalid-null-messageRefID.xml")
        val result = Await.result(validator.validateBusinessRules(nullMessageRefID, filename), 5.seconds)

        result.fold(
          errors => errors.toList should contain(MessageRefIDMissing),
          _ => fail("No MessageRefIDMissing error generated")
        )
      }
      "messageRefId format is wrong" in {
        val invalidMessageRefID = new File("test/resources/cbcr-invalid-invalid-messageRefID.xml")
        val result = Await.result(validator.validateBusinessRules(invalidMessageRefID, filename), 5.seconds)

        result.fold(
          errors => errors.toList should contain(MessageRefIDFormatError),
          _ => fail("No MessageRefIDFormatError error generated")
        )
      }
      "messageRefId contains a CBCId that doesnt match the CBCId in the SendingEntityIN field" in {
        when(messageRefIdService.messageRefIdExists(any())(any())) thenReturn Future.successful(false)
        val invalidMessageRefID = new File("test/resources/cbcr-invalid-cbcId-messageRefID.xml")
        val result = Await.result(validator.validateBusinessRules(invalidMessageRefID, filename), 5.seconds)

        result.fold(
          errors => errors.toList should contain(MessageRefIDCBCIdMismatch),
          _ => fail("No MessageRefIDCBCIdMismatch error generated")
        )
      }
      "messageRefId contains a Reporting Year that doesn't match the year in the ReportingPeriod field" in {
        when(messageRefIdService.messageRefIdExists(any())(any())) thenReturn Future.successful(false)
        val invalidMessageRefID = new File("test/resources/cbcr-invalid-reportingYear-messageRefID.xml")
        val result = Await.result(validator.validateBusinessRules(invalidMessageRefID, filename), 5.seconds)

        result.fold(
          errors => errors.toList should contain(MessageRefIDReportingPeriodMismatch),
          _ => fail("No MessageRefIDReportingPeriodMismatch error generated")
        )
      }
      "messageRefId contains a creation timestamp that isn't valid" in {
        when(messageRefIdService.messageRefIdExists(any())(any())) thenReturn Future.successful(false)
        val invalidMessageRefID = new File("test/resources/cbcr-invalid-creationTimestamp-messageRefID.xml")
        val result = Await.result(validator.validateBusinessRules(invalidMessageRefID, filename), 5.seconds)

        result.fold(
          errors => errors.toList should contain(MessageRefIDTimestampError),
          _ => fail("No MessageRefIDTimestampError error generated")
        )
      }
      "messageRefId has been seen before" in {
        when(messageRefIdService.messageRefIdExists(any())(any())) thenReturn Future.successful(true)
        val validFile = new File("test/resources/cbcr-valid.xml")
        val result = Await.result(validator.validateBusinessRules(validFile, filename), 5.seconds)

        result.fold(
          errors => errors.toList should contain(MessageRefIDDuplicate),
          _ => fail("No MessageRefIdDuplicate error generated")
        )
      }
      "test data is present" when {
        "the xml file has a single CBCReports element" in {
          val validFile = new File("test/resources/cbcr-testData.xml")
          val result = Await.result(validator.validateBusinessRules(validFile, filename), 5.seconds)

          result.fold(
            errors => errors.toList should contain(TestDataError),
            _ => fail("No TestDataError generated")
          )
        }
        "the xml file has a multiple CBCReports elements" in {
          val validFile = new File("test/resources/cbcr-testDataM.xml")
          val result = Await.result(validator.validateBusinessRules(validFile, filename), 5.seconds)

          result.fold(
            errors => errors.toList should contain(TestDataError),
            _ => fail("No TestDataError generated")
          )
        }
      }

      "SendingEntityIn is using a private beta CBCId" in {
        when(messageRefIdService.messageRefIdExists(any())(any())) thenReturn Future.successful(false)
        val validFile = new File("test/resources/cbcr-privatebeta.xml")
        val result = Await.result(validator.validateBusinessRules(validFile, filenamePB), 5.seconds)

        result.fold(
          errors => errors.head shouldBe PrivateBetaCBCIdError,
          _ => fail("No TestDataError generated")
        )
      }

      "SendingEntityIn does not match any CBCId in the database" in {
        when(messageRefIdService.messageRefIdExists(any())(any())) thenReturn Future.successful(false)
        when(subscriptionDataService.retrieveSubscriptionData(any())(any(), any())) thenReturn EitherT.pure[Future, CBCErrors, Option[SubscriptionDetails]](None)
        val validFile = new File("test/resources/cbcr-valid.xml")
        val result = Await.result(validator.validateBusinessRules(validFile, filename), 5.seconds)

        result.fold(
          errors => errors.head shouldBe SendingEntityError,
          _ => fail("No TestDataError generated")
        )
      }

      "ReceivingCountry does not equal GB" in {
        when(subscriptionDataService.retrieveSubscriptionData(any())(any(), any())) thenReturn EitherT.pure[Future, CBCErrors, Option[SubscriptionDetails]](Some(submissionData))
        val validFile = new File("test/resources/cbcr-invalidReceivingCountry.xml")
        val result = Await.result(validator.validateBusinessRules(validFile, filename), 5.seconds)

        result.fold(
          errors => errors.head shouldBe ReceivingCountryError,
          _ => fail("No TestDataError generated")
        )
      }

      "Filename does not match MessageRefId" in {
        val validFile = new File("test/resources/cbcr-valid.xml")
        val invalidFilename = "INVALID" + filename
        val result = Await.result(validator.validateBusinessRules(validFile, invalidFilename), 5.seconds)

        result.fold(
          errors => errors.head shouldBe FileNameError(invalidFilename, filename),
          _ => fail("No FileNameError generated")
        )
      }

      "ReportingEntity is missing" in {
        when(reportingEntity.queryReportingEntityData(any())(any())) thenReturn EitherT.pure[Future,CBCErrors,Option[ReportingEntityData]](None)

        val result = Await.result(validator.recoverReportingEntity(xmlinfo), 5.seconds)

        result.fold(
          errors => errors.toList should contain(OriginalSubmissionNotFound),
          _ => fail("No InvalidXMLError generated")
        )
      }
      "MessageTypeIndic is CBC402 and the DocTypeIndic's are invalid" when {
        "CBCReports.docTypeIndic isn't OECD2 or OECD3" in {
          val validFile = new File("test/resources/cbcr-messageTypeIndic.xml")
          val result = Await.result(validator.validateBusinessRules(validFile, filename), 5.seconds)

          result.fold(
            errors => errors.toList should contain(MessageTypeIndicError),
            _ => fail("No InvalidXMLError generated")
          )
        }
        "CBCReports[*].docTypeIndic isn't OECD2 or OECD3" in {
          val validFile = new File("test/resources/cbcr-messageTypeIndicM.xml")
          val result = Await.result(validator.validateBusinessRules(validFile, filename), 5.seconds)

          result.fold(
            errors => errors.toList should contain(MessageTypeIndicError),
            _ => fail("No InvalidXMLError generated")
          )
        }
        "AdditionalInfo.docTypeIndic isn't OECD2 or OECD3" in {
          val validFile = new File("test/resources/cbcr-messageTypeIndic2.xml")
          val result = Await.result(validator.validateBusinessRules(validFile, filename), 5.seconds)

          result.fold(
            errors => errors.toList should contain(MessageTypeIndicError),
            _ => fail("No InvalidXMLError generated")
          )
        }
        "ReportingEntity.docTypeIndic isn't OECD2 or OECD3 or OECD0" in {
          val validFile = new File("test/resources/cbcr-messageTypeIndic3.xml")
          val result = Await.result(validator.validateBusinessRules(validFile, filename), 5.seconds)

          result.fold(
            errors => errors.toList should contain(MessageTypeIndicError),
            _ => fail("No InvalidXMLError generated")
          )

        }
      }

      "MessageTypeIndic is not provided" in {
        val validFile = new File("test/resources/cbcr-noMessageTypeIndic.xml")
        val result = Await.result(validator.validateBusinessRules(validFile, filename), 5.seconds)

        result.fold(
          errors => errors.toList shouldNot contain(MessageTypeIndicError),
          _ => ()
        )

      }

      "File is not a valid xml file" in {
        val validFile = new File("test/resources/actually_a_jpg.xml")
        val result = Await.result(validator.validateBusinessRules(validFile, filename), 5.seconds)

        result.fold(
          _ => (),
          _ => fail("No InvalidXMLError generated")
        )

      }
      "when a corrRefId is present but refers to an *unknown* docRefId" in {

        val validFile = new File("test/resources/cbcr-withCorrRefId.xml")
        when(docRefIdService.queryDocRefId(EQ(docRefId1))(any())) thenReturn Future.successful(DoesNotExist)
        when(docRefIdService.queryDocRefId(EQ(docRefId2))(any())) thenReturn Future.successful(DoesNotExist)
        when(docRefIdService.queryDocRefId(EQ(docRefId3))(any())) thenReturn Future.successful(DoesNotExist)
        when(docRefIdService.queryDocRefId(EQ(docRefId4))(any())) thenReturn Future.successful(DoesNotExist)

        when(docRefIdService.queryDocRefId(EQ(corrDocRefId1))(any())) thenReturn Future.successful(Valid)
        when(docRefIdService.queryDocRefId(EQ(corrDocRefId2))(any())) thenReturn Future.successful(Valid)
        when(docRefIdService.queryDocRefId(EQ(corrDocRefId3))(any())) thenReturn Future.successful(Valid)
        when(docRefIdService.queryDocRefId(EQ(corrDocRefId4))(any())) thenReturn Future.successful(DoesNotExist)

        val result = Await.result(validator.validateBusinessRules(validFile, filename), 5.seconds)

        result.fold(
          errors => errors.head shouldBe CorrDocRefIdUnknownRecord,
          _ => fail("No InvalidXMLError generated")
        )
      }
      "when a corrRefId is present but refers to an *invalid* docRefId" in {
        val validFile = new File("test/resources/cbcr-withCorrRefId.xml")

        when(docRefIdService.queryDocRefId(EQ(corrDocRefId1))(any())) thenReturn Future.successful(Invalid)
        when(docRefIdService.queryDocRefId(EQ(corrDocRefId2))(any())) thenReturn Future.successful(Valid)
        when(docRefIdService.queryDocRefId(EQ(corrDocRefId3))(any())) thenReturn Future.successful(Valid)
        when(docRefIdService.queryDocRefId(EQ(corrDocRefId4))(any())) thenReturn Future.successful(Valid)

        val result = Await.result(validator.validateBusinessRules(validFile, filename), 5.seconds)

        result.fold(
          errors => errors.head shouldBe CorrDocRefIdInvalidRecord,
          _ => fail("No InvalidXMLError generated")
        )

      }
      "when a docRefId is a duplicate within the file" in {
        val validFile = new File("test/resources/cbcr-valid-dup.xml")
        when(messageRefIdService.messageRefIdExists(any())(any())) thenReturn Future.successful(false)
        when(docRefIdService.queryDocRefId(any())(any())) thenReturn Future.successful(DoesNotExist)

        val result = Await.result(validator.validateBusinessRules(validFile, filename), 5.seconds)

        result.fold(
          errors => errors.toList should contain(DocRefIdDuplicate),
          _ => fail("No InvalidXMLError generated")
        )
      }
      "when a docRefId is a duplicate" in {
        val validFile = new File("test/resources/cbcr-valid.xml")
        when(docRefIdService.queryDocRefId(EQ(docRefId1))(any())) thenReturn Future.successful(Valid)
        when(docRefIdService.queryDocRefId(EQ(docRefId2))(any())) thenReturn Future.successful(DoesNotExist)
        when(docRefIdService.queryDocRefId(EQ(docRefId3))(any())) thenReturn Future.successful(DoesNotExist)

        val result = Await.result(validator.validateBusinessRules(validFile, filename), 5.seconds)

        result.fold(
          errors => errors.head shouldBe DocRefIdDuplicate,
          _ => fail("No InvalidXMLError generated")
        )
      }
      "when a docRefId is a duplicate but the duplicate is in an Unchanged ReportingEntity section" in {
        val validFile = new File("test/resources/cbcr-valid-dup-re-unchanged.xml")

        when(docRefIdService.queryDocRefId(EQ(docRefId1))(any())) thenReturn Future.successful(Valid)
        when(docRefIdService.queryDocRefId(EQ(docRefId2))(any())) thenReturn Future.successful(DoesNotExist)
        when(docRefIdService.queryDocRefId(EQ(corrDocRefId2))(any())) thenReturn Future.successful(Valid)
        when(docRefIdService.queryDocRefId(EQ(docRefId3))(any())) thenReturn Future.successful(DoesNotExist)
        when(docRefIdService.queryDocRefId(EQ(corrDocRefId3))(any())) thenReturn Future.successful(Valid)


        val result = Await.result(validator.validateBusinessRules(validFile, filename), 5.seconds)

        result.fold(
          _ => fail("No errors should be generated"),
          _ =>  ()
        )
      }
      "when the DocType is OECD1 but there are CorrDocRefIds defined" in {
        when(docRefIdService.queryDocRefId(EQ(docRefId1))(any())) thenReturn Future.successful(DoesNotExist)
        when(docRefIdService.queryDocRefId(EQ(docRefId2))(any())) thenReturn Future.successful(DoesNotExist)
        when(docRefIdService.queryDocRefId(EQ(docRefId3))(any())) thenReturn Future.successful(DoesNotExist)


        val validFile1 = new File("test/resources/cbcr-OECD1-with-CorrDocRefIds1.xml")
        Await.result(validator.validateBusinessRules(validFile1, filename), 5.seconds).fold(
          errors => errors.toList should contain(CorrDocRefIdNotNeeded),
          _ => fail("No InvalidXMLError generated")
        )
        val validFile2 = new File("test/resources/cbcr-OECD1-with-CorrDocRefIds2.xml")
        Await.result(validator.validateBusinessRules(validFile2, filename), 5.seconds).fold(
          errors => errors.toList should contain(CorrDocRefIdNotNeeded),
          _ => fail("No InvalidXMLError generated")
        )
        val validFile3 = new File("test/resources/cbcr-OECD1-with-CorrDocRefIds3.xml")
        Await.result(validator.validateBusinessRules(validFile3, filename), 5.seconds).fold(
          errors => errors.toList should contain(CorrDocRefIdNotNeeded),
          _ => fail("No InvalidXMLError generated")
        )
        val validFile4 = new File("test/resources/cbcr-OECD1-with-CorrDocRefIds4.xml")
        Await.result(validator.validateBusinessRules(validFile4, filename), 5.seconds).fold(
          errors => errors.toList should contain(CorrDocRefIdNotNeeded),
          _ => fail("No InvalidXMLError generated")
        )

      }
      "when the DocType is OECD[23] but there are no CorrDocRefIds defined" in {
        val validFile = new File("test/resources/cbcr-OECD2-with-NoCorrDocRefIds.xml")
        val result = Await.result(validator.validateBusinessRules(validFile, filename), 5.seconds)

        result.fold(
          errors => errors.toList should contain(CorrDocRefIdMissing),
          _ => fail("No InvalidXMLError generated")
        )
      }
      "when the messageTypeIndic is CBC401 but doctypeIndic is not OECD1" in {
        val validFile = new File("test/resources/cbcr-OECD2-Incompatible-messageTypes.xml")
        val result = Await.result(validator.validateBusinessRules(validFile, filename), 5.seconds)

        result.fold(
          errors => errors.toList should contain(MessageTypeIndicDocTypeIncompatible),
          _ => fail("No InvalidXMLError generated")
        )

      }
      "when there are a mixture of OECD1 and OECD[23] docTypeIndics" in {
        val validFile = new File("test/resources/cbcr-docTypeIndicMixture.xml")

        val result = Await.result(validator.validateBusinessRules(validFile, filename), 5.seconds)

        result.fold(
          errors => errors.toList should contain(IncompatibleOECDTypes),
          _ => fail("No InvalidXMLError generated")
        )

      }

      "when there are invalid docRefIds" in {
        val validFile = new File("test/resources/cbcr-withInvalidDocRefId.xml")
        val result = Await.result(validator.validateBusinessRules(validFile, filename), 5.seconds)

        result.fold(
          errors => errors.toList should contain(InvalidDocRefId),
          _ => fail("No InvalidXMLError generated")
        )

      }

      "when there are invalid corrDocRefIds" in {
        val validFile = new File("test/resources/cbcr-withInvalidCorrDocRefId.xml")
        when(docRefIdService.queryDocRefId(EQ(corrDocRefId1))(any())) thenReturn Future.successful(Valid)
        when(docRefIdService.queryDocRefId(EQ(corrDocRefId2))(any())) thenReturn Future.successful(Valid)
        when(docRefIdService.queryDocRefId(EQ(corrDocRefId3))(any())) thenReturn Future.successful(Valid)

        val result = Await.result(validator.validateBusinessRules(validFile, filename), 5.seconds)

        result.fold(
          errors => errors.toList should contain(InvalidCorrDocRefId),
          _ => fail("No InvalidXMLError generated")
        )
      }

      "when the CBC_OECD version is invalid" in {
        val validFile = new File("test/resources/cbcr-withInvalidCBC-OECDVersion.xml")
        val result = Await.result(validator.validateBusinessRules(validFile, filename), 5.seconds)

        result.fold(
          errors => errors.toList should contain(CbcOecdVersionError),
          _ => fail("No InvalidXMLError generated")
        )

      }

      "when the XML Encoding value is NOT UTF-8" in {
        val validFile = new File("test/resources/cbcr-withInvalidXmlEncodingValue.xml")
        val result = Await.result(validator.validateBusinessRules(validFile, filename), 5.seconds)

        result.fold(
          errors => errors.toList should contain(XmlEncodingError),
          _ => fail("No InvalidXMLError generated")
        )

      }

      "when the DocRefId refers to the wrong parent group element" in {
        val validFile = new File("test/resources/cbcr-invalid-docrefid-PGE.xml")
        val result = Await.result(validator.validateBusinessRules(validFile, filename), 5.seconds)

        result.fold(
          errors => errors.toList should contain(DocRefIdInvalidParentGroupElement),
          _ => fail("No InvalidXMLError generated")
        )

      }
      "when the CorrDocRefId refers to the wrong parent group element" in {
        val validFile = new File("test/resources/cbcr-invalid-corrdocrefid-PGE.xml")
        val result = Await.result(validator.validateBusinessRules(validFile, filename), 5.seconds)

        result.fold(
          errors => errors.toList should contain(CorrDocRefIdInvalidParentGroupElement),
          _ => fail("No InvalidXMLError generated")
        )

      }
      "when the FilingType == CBC701" when {
        "the TIN field is not a valid UTR" in {
          val validFile = new File("test/resources/cbcr-CBC701-badTIN.xml")
          val result = Await.result(validator.validateBusinessRules(validFile, filename), 5.seconds)

          result.fold(
            errors => errors.toList should contain(
              InvalidXMLError("ReportingEntity.Entity.TIN must be a valid UTR for filings issued in 'GB'")
            ),
            _ => fail("No InvalidXMLError generated for CBC701 invalid TIN check")
          )

        }
        "the @issuedBy attribute of the TIN is not 'GB' " in {
          val validFile = new File("test/resources/cbcr-CBC701-badTINAttribute.xml")
          val result = Await.result(validator.validateBusinessRules(validFile, filename), 5.seconds)

          result.fold(
            errors => errors.toList should contain(InvalidXMLError("ReportingEntity.Entity.TIN@issuedBy must be 'GB' for local or primary filings")),
            _ => fail("No InvalidXMLError generated for CBC701 invalid TIN issuedBy check")
          )

        }
      }
      "when the FilingType == CBC703" when {
        "the TIN field is not a valid UTR" in {
          val validFile = new File("test/resources/cbcr-CBC703-badTIN.xml")
          val result = Await.result(validator.validateBusinessRules(validFile, filename), 5.seconds)

          result.fold(
            errors => errors.toList should contain(InvalidXMLError("ReportingEntity.Entity.TIN must be a valid UTR for filings issued in 'GB'")),
            _ => fail("No InvalidXMLError generated for CBC703 invalid TIN check")
          )

        }
        "the @issuedBy attribute of the TIN is not 'GB' " in {
          val validFile = new File("test/resources/cbcr-CBC703-badTINAttribute.xml")
          val result = Await.result(validator.validateBusinessRules(validFile, filename), 5.seconds)

          result.fold(
            errors => errors.toList should contain(InvalidXMLError("ReportingEntity.Entity.TIN@issuedBy must be 'GB' for local or primary filings")),
            _ => fail("No InvalidXMLError generated for CBC703 invalid TIN issuedBy check")
          )

        }

      }

      "when the FilingType == CBC702" when {
        "the TIN field is unrestricted" in {
          val validFile = new File("test/resources/cbcr-CBC702-badTIN.xml")
          val result = Await.result(validator.validateBusinessRules(validFile, filename), 5.seconds)

          result.fold(
            errors => fail(s"CBC702 should handle non UTR in TIN field: ${errors.toList.mkString("\n")}"),
            _      => ()
          )

        }
        "the @issuedBy attribute of the TIN is unrestricted" in {
          val validFile = new File("test/resources/cbcr-CBC702-badTINAttribute.xml")
          val result = Await.result(validator.validateBusinessRules(validFile, filename), 5.seconds)

          result.fold(
            errors => fail(s"CBC703 should handle non GB issuedBy field: ${errors.toList.mkString("\n")}"),
            _      => ()
          )

        }
      }

      "return the KeyXmlInfo when everything is fine and were using a NON GB TIN for a 702 submission" in {
        val validFile = new File("test/resources/cbcr-valid-nonGBTINInDocRefId.xml")
        val result = Await.result(validator.validateBusinessRules(validFile, filename), 5.seconds)


        result.fold(
          errors => fail(s"Error were generated: $errors"),
          x => x.cbcReport.size shouldBe 4
        )
      }

      "return the KeyXmlInfo when everything is fine" in {
        val validFile = new File("test/resources/cbcr-valid.xml")
        val result = Await.result(validator.validateBusinessRules(validFile, filename), 5.seconds)


        result.fold(
          errors => fail(s"Error were generated: $errors"),
          x => x.cbcReport.size shouldBe 4
        )
      }

      "should not create an error" when {
        "Should not fail when utf-8 is lowercase" in {

          val validFile = new File("test/resources/lower-case-utf8-pre-amble.xml")
          val result = Await.result(validator.validateBusinessRules(validFile, filename), 5.seconds)

          result.fold(
            _ => fail("Should not fail when utf-8 is lowercase"),
            _ => ()
          )
        }
      }
    }
  }
}
