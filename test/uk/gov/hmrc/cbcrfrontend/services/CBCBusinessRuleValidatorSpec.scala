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

import java.io.File

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


  val docRefId1 = DocRefId("GB2016RGXVCBC0000000056CBC40120170311T090000X_7000000002OECD1ENT").getOrElse(fail("bad docrefid"))
  val docRefId2 = DocRefId("GB2016RGXVCBC0000000056CBC40120170311T090000X_7000000002OECD1REP").getOrElse(fail("bad docrefid"))
  val docRefId3 = DocRefId("GB2016RGXVCBC0000000056CBC40120170311T090000X_7000000002OECD1ADD").getOrElse(fail("bad docrefid"))

  val corrDocRefId1 = DocRefId("GB2016RGXVCBC0000000056CBC40120170311T090000X_7000000002OECD1ENTC").getOrElse(fail("bad docrefid"))
  val corrDocRefId2 = DocRefId("GB2016RGXVCBC0000000056CBC40120170311T090000X_7000000002OECD1REPC").getOrElse(fail("bad docrefid"))
  val corrDocRefId3 = DocRefId("GB2016RGXVCBC0000000056CBC40120170311T090000X_7000000002OECD1ADDC").getOrElse(fail("bad docrefid"))

  val schemaVer: String = "1.0"

  when(docRefIdService.queryDocRefId(any())(any())) thenReturn Future.successful(DoesNotExist)
  when(subscriptionDataService.retrieveSubscriptionData(any())(any(),any())) thenReturn EitherT.pure[Future,CBCErrors,Option[SubscriptionDetails]](Some(submissionData))
  when(configuration.getString("oecd-schema-version")) thenReturn Future.successful(Some(schemaVer))

  implicit val hc = HeaderCarrier()
  val extract = new XmlInfoExtract()
  implicit def fileToXml(f:File) : RawXMLInfo = extract.extract(f)

  val cbcId = CBCId.create(56).toOption
  val filename = "GB2016RGXVCBC0000000056CBC40120170311T090000X.xml"

  val submissionData = SubscriptionDetails(
    BusinessPartnerRecord("SAFEID",Some(OrganisationResponse("blagh")),EtmpAddress("Line1",None,None,None,Some("TF3 XFE"),"GB")),
    SubscriberContact("Brian","Lastname", "phonenum",EmailAddress("test@test.com")),cbcId,Utr("7000000002")
  )

  val validator = new CBCBusinessRuleValidator(messageRefIdService,docRefIdService,subscriptionDataService,reportingEntity, configuration)
  "The CBCBusinessRuleValidator" should {
    "return the correct error" when {
      "messageRefId is empty and return the correct message and errorcode" in {
        val missingMessageRefID = new File("test/resources/cbcr-invalid-empty-messageRefID.xml")
        val result = Await.result(validator.validateBusinessRules(missingMessageRefID, filename).value, 5.seconds)

        result.fold(
          errors => errors.toList should contain(MessageRefIDMissing),
          _ => fail("No MessageRefIDMissing error generated")
        )
      }
      "messageRefId is null and return the correct message and errorcode" in {
        val nullMessageRefID = new File("test/resources/cbcr-invalid-null-messageRefID.xml")
        val result = Await.result(validator.validateBusinessRules(nullMessageRefID, filename).value, 5.seconds)

        result.fold(
          errors => errors.toList should contain(MessageRefIDMissing),
          _ => fail("No MessageRefIDMissing error generated")
        )
      }
      "messageRefId format is wrong" in {
        val invalidMessageRefID = new File("test/resources/cbcr-invalid-invalid-messageRefID.xml")
        val result = Await.result(validator.validateBusinessRules(invalidMessageRefID, filename).value, 5.seconds)

        result.fold(
          errors => errors.toList should contain(MessageRefIDFormatError),
          _ => fail("No MessageRefIDFormatError error generated")
        )
      }
      "messageRefId contains a CBCId that doesnt match the CBCId in the SendingEntityIN field" in {
        when(messageRefIdService.messageRefIdExists(any())(any())) thenReturn Future.successful(false)
        val invalidMessageRefID = new File("test/resources/cbcr-invalid-cbcId-messageRefID.xml")
        val result = Await.result(validator.validateBusinessRules(invalidMessageRefID, filename).value, 5.seconds)

        result.fold(
          errors => errors.toList should contain(MessageRefIDCBCIdMismatch),
          _ => fail("No MessageRefIDCBCIdMismatch error generated")
        )
      }
      "messageRefId contains a Reporting Year that doesn't match the year in the ReportingPeriod field" in {
        when(messageRefIdService.messageRefIdExists(any())(any())) thenReturn Future.successful(false)
        val invalidMessageRefID = new File("test/resources/cbcr-invalid-reportingYear-messageRefID.xml")
        val result = Await.result(validator.validateBusinessRules(invalidMessageRefID, filename).value, 5.seconds)

        result.fold(
          errors => errors.toList should contain(MessageRefIDReportingPeriodMismatch),
          _ => fail("No MessageRefIDReportingPeriodMismatch error generated")
        )
      }
      "messageRefId contains a creation timestamp that isn't valid" in {
        when(messageRefIdService.messageRefIdExists(any())(any())) thenReturn Future.successful(false)
        val invalidMessageRefID = new File("test/resources/cbcr-invalid-creationTimestamp-messageRefID.xml")
        val result = Await.result(validator.validateBusinessRules(invalidMessageRefID, filename).value, 5.seconds)

        result.fold(
          errors => errors.toList should contain(MessageRefIDTimestampError),
          _ => fail("No MessageRefIDTimestampError error generated")
        )
      }
      "messageRefId has been seen before" in {
        when(messageRefIdService.messageRefIdExists(any())(any())) thenReturn Future.successful(true)
        val validFile = new File("test/resources/cbcr-valid.xml")
        val result = Await.result(validator.validateBusinessRules(validFile, filename).value, 5.seconds)

        result.fold(
          errors => errors.toList should contain(MessageRefIDDuplicate),
          _ => fail("No MessageRefIdDuplicate error generated")
        )
      }
      "test data is present" in {
        val validFile = new File("test/resources/cbcr-testData.xml")
        val result = Await.result(validator.validateBusinessRules(validFile, filename).value, 5.seconds)

        result.fold(
          errors => errors.head shouldBe TestDataError,
          _ => fail("No TestDataError generated")
        )
      }

      "SendingEntityIn does not match any CBCId in the database" in {
        when(messageRefIdService.messageRefIdExists(any())(any())) thenReturn Future.successful(false)
        when(subscriptionDataService.retrieveSubscriptionData(any())(any(),any())) thenReturn EitherT.pure[Future,CBCErrors,Option[SubscriptionDetails]](None)
        val validFile = new File("test/resources/cbcr-valid.xml")
        val result = Await.result(validator.validateBusinessRules(validFile, filename).value, 5.seconds)

        result.fold(
          errors => errors.head shouldBe SendingEntityError,
          _ => fail("No TestDataError generated")
        )
      }

      "ReceivingCountry does not equal GB" in {
        when(subscriptionDataService.retrieveSubscriptionData(any())(any(),any())) thenReturn EitherT.pure[Future,CBCErrors,Option[SubscriptionDetails]](Some(submissionData))
        val validFile = new File("test/resources/cbcr-invalidReceivingCountry.xml")
        val result = Await.result(validator.validateBusinessRules(validFile, filename).value, 5.seconds)

        result.fold(
          errors => errors.head shouldBe ReceivingCountryError,
          _ => fail("No TestDataError generated")
        )
      }

      "Filename does not match MessageRefId" in {
        val validFile = new File("test/resources/cbcr-valid.xml")
        val result = Await.result(validator.validateBusinessRules(validFile, "INVALID" + filename).value, 5.seconds)

        result.fold(
          errors => errors.head shouldBe FileNameError,
          _ => fail("No FileNameError generated")
        )
      }

      //TODO: In a when CBCR-435 is done, we'll need to mock a call to the backend to retrieve missing ReportingEntity data
      "ReportingEntity is missing" in {
        val validFile = new File("test/resources/cbcr-noReportingEntity.xml")
        val result = Await.result(validator.validateBusinessRules(validFile, filename).value, 5.seconds)

        result.fold(
          errors => errors.toList should contain(OriginalSubmissionNotFound),
          _      => fail("No InvalidXMLError generated")
        )
      }
      "MessageTypeIndic is CBC402 and the DocTypeIndic's are invalid" when {
        "CBCReports.docTypeIndic isn't OECD2 or OECD3" in{
          val validFile = new File("test/resources/cbcr-messageTypeIndic.xml")
          val result = Await.result(validator.validateBusinessRules(validFile, filename).value, 5.seconds)

          result.fold(
            errors => errors.head shouldBe MessageTypeIndicError,
            _ => fail("No InvalidXMLError generated")
          )
        }
        "AdditionalInfo.docTypeIndic isn't OECD2 or OECD3" in{
          val validFile = new File("test/resources/cbcr-messageTypeIndic2.xml")
          val result = Await.result(validator.validateBusinessRules(validFile, filename).value, 5.seconds)

          result.fold(
            errors => errors.head shouldBe MessageTypeIndicError,
            _ => fail("No InvalidXMLError generated")
          )
        }
        "ReportingEntity.docTypeIndic isn't OECD2 or OECD3 or OECD0" in{
          val validFile = new File("test/resources/cbcr-messageTypeIndic3.xml")
          val result = Await.result(validator.validateBusinessRules(validFile, filename).value, 5.seconds)

          result.fold(
            errors => errors.head shouldBe MessageTypeIndicError,
            _ => fail("No InvalidXMLError generated")
          )

          when(docRefIdService.queryDocRefId(EQ(docRefId1))(any())) thenReturn Future.successful(DocRefIdResponses.DoesNotExist)
          when(docRefIdService.queryDocRefId(EQ(docRefId2))(any())) thenReturn Future.successful(DocRefIdResponses.DoesNotExist)
          when(docRefIdService.queryDocRefId(EQ(docRefId3))(any())) thenReturn Future.successful(DocRefIdResponses.DoesNotExist)
          when(docRefIdService.queryDocRefId(EQ(corrDocRefId1))(any())) thenReturn Future.successful(DocRefIdResponses.Valid)
          when(docRefIdService.queryDocRefId(EQ(corrDocRefId2))(any())) thenReturn Future.successful(DocRefIdResponses.Valid)
          when(docRefIdService.queryDocRefId(EQ(corrDocRefId3))(any())) thenReturn Future.successful(DocRefIdResponses.Valid)

          val validFile2 = new File("test/resources/cbcr-messageTypeIndic4.xml")
          val result2 = Await.result(validator.validateBusinessRules(validFile2, filename).value, 5.seconds)

          result2.left.map( errors => fail(s"errors generated: $errors"))

        }
      }

      "File is not a valid xml file" in {
        val validFile = new File("test/resources/actually_a_jpg.xml")
        val result = Await.result(validator.validateBusinessRules(validFile, filename).value, 5.seconds)

        result.fold(
          errors => (),
          _ => fail("No InvalidXMLError generated")
        )
      }
      "when a corrRefId is present but refers to an unknown docRefId" in {

        val validFile = new File("test/resources/cbcr-withCorrRefId.xml")
        when(docRefIdService.queryDocRefId(EQ(docRefId1))(any())) thenReturn Future.successful(DoesNotExist)
        when(docRefIdService.queryDocRefId(EQ(docRefId2))(any())) thenReturn Future.successful(DoesNotExist)
        when(docRefIdService.queryDocRefId(EQ(docRefId3))(any())) thenReturn Future.successful(DoesNotExist)

        when(docRefIdService.queryDocRefId(EQ(corrDocRefId1))(any())) thenReturn Future.successful(DoesNotExist)
        when(docRefIdService.queryDocRefId(EQ(corrDocRefId2))(any())) thenReturn Future.successful(DoesNotExist)
        when(docRefIdService.queryDocRefId(EQ(corrDocRefId3))(any())) thenReturn Future.successful(DoesNotExist)

        val result = Await.result(validator.validateBusinessRules(validFile, filename).value, 5.seconds)

        result.fold(
          errors => errors.head shouldBe CorrDocRefIdUnknownRecord,
          _ => fail("No InvalidXMLError generated")
        )
      }
      "when a corrRefId is present but refers to an invalid docRefId" in {
        val validFile = new File("test/resources/cbcr-withCorrRefId.xml")

        when(docRefIdService.queryDocRefId(EQ(corrDocRefId1))(any())) thenReturn Future.successful(Invalid)
        when(docRefIdService.queryDocRefId(EQ(corrDocRefId2))(any())) thenReturn Future.successful(Valid)
        when(docRefIdService.queryDocRefId(EQ(corrDocRefId3))(any())) thenReturn Future.successful(Valid)

        val result = Await.result(validator.validateBusinessRules(validFile, filename).value, 5.seconds)

        result.fold(
          errors => errors.head shouldBe CorrDocRefIdInvalidRecord,
          _ => fail("No InvalidXMLError generated")
        )

      }
      "when a docRefId is a duplicate within the file" in  {
        val validFile = new File("test/resources/cbcr-valid-dup.xml")
        when(docRefIdService.queryDocRefId(EQ(docRefId1))(any())) thenReturn Future.successful(DoesNotExist)
        when(docRefIdService.queryDocRefId(EQ(docRefId2))(any())) thenReturn Future.successful(DoesNotExist)
        when(docRefIdService.queryDocRefId(EQ(docRefId3))(any())) thenReturn Future.successful(DoesNotExist)

        val result = Await.result(validator.validateBusinessRules(validFile, filename).value, 5.seconds)

        result.fold(
          errors => errors.head shouldBe DocRefIdDuplicate,
          _ => fail("No InvalidXMLError generated")
        )
      }
      "when a docRefId is a duplicate" in  {
        val validFile = new File("test/resources/cbcr-valid.xml")
        when(docRefIdService.queryDocRefId(EQ(docRefId1))(any())) thenReturn Future.successful(Valid)
        when(docRefIdService.queryDocRefId(EQ(docRefId2))(any())) thenReturn Future.successful(DoesNotExist)
        when(docRefIdService.queryDocRefId(EQ(docRefId3))(any())) thenReturn Future.successful(DoesNotExist)

        val result = Await.result(validator.validateBusinessRules(validFile, filename).value, 5.seconds)

        result.fold(
          errors => errors.head shouldBe DocRefIdDuplicate,
          _ => fail("No InvalidXMLError generated")
        )
      }
      "when the DocType is OECD1 but there are CorrDocRefIds defined" in {
        when(docRefIdService.queryDocRefId(EQ(docRefId1))(any())) thenReturn Future.successful(DoesNotExist)
        when(docRefIdService.queryDocRefId(EQ(docRefId2))(any())) thenReturn Future.successful(DoesNotExist)
        when(docRefIdService.queryDocRefId(EQ(docRefId3))(any())) thenReturn Future.successful(DoesNotExist)


        val validFile1 = new File("test/resources/cbcr-OECD1-with-CorrDocRefIds1.xml")
        Await.result(validator.validateBusinessRules(validFile1, filename).value, 5.seconds).fold(
          errors => errors.head shouldBe CorrDocRefIdNotNeeded,
          _ => fail("No InvalidXMLError generated")
        )
        val validFile2 = new File("test/resources/cbcr-OECD1-with-CorrDocRefIds2.xml")
        Await.result(validator.validateBusinessRules(validFile2, filename).value, 5.seconds).fold(
          errors => errors.head shouldBe CorrDocRefIdNotNeeded,
          _ => fail("No InvalidXMLError generated")
        )
        val validFile3 = new File("test/resources/cbcr-OECD1-with-CorrDocRefIds3.xml")
        Await.result(validator.validateBusinessRules(validFile3, filename).value, 5.seconds).fold(
          errors => errors.head shouldBe CorrDocRefIdNotNeeded,
          _ => fail("No InvalidXMLError generated")
        )

      }
      "when the DocType is OECD[23] but there are no CorrDocRefIds defined" in {
        val validFile = new File("test/resources/cbcr-OECD2-with-NoCorrDocRefIds.xml")
        val result = Await.result(validator.validateBusinessRules(validFile, filename).value, 5.seconds)

        result.fold(
          errors => errors.toList should contain(CorrDocRefIdMissing),
          _ => fail("No InvalidXMLError generated")
        )
      }
      "when the messageTypeIndic is CBC401 but doctypeIndic is not OECD1" in {
        val validFile = new File("test/resources/cbcr-OECD2-Incompatible-messageTypes.xml")
        val result = Await.result(validator.validateBusinessRules(validFile, filename).value, 5.seconds)

        result.fold(
          errors => errors.toList should contain(MessageTypeIndicDocTypeIncompatible),
          _ => fail("No InvalidXMLError generated")
        )

      }
      "when there are a mixture of OECD1 and OECD[23] docTypeIndics" in {
        val validFile = new File("test/resources/cbcr-docTypeIndicMixture.xml")

        val result = Await.result(validator.validateBusinessRules(validFile, filename).value, 5.seconds)

        result.fold(
          errors => errors.toList should contain(IncompatibleOECDTypes),
          _ => fail("No InvalidXMLError generated")
        )

      }

      "when there are invalid docRefIds" in {
        val validFile = new File("test/resources/cbcr-withInvalidDocRefId.xml")
        val result = Await.result(validator.validateBusinessRules(validFile, filename).value, 5.seconds)

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

        val result = Await.result(validator.validateBusinessRules(validFile, filename).value, 5.seconds)

        result.fold(
          errors => errors.toList should contain(InvalidCorrDocRefId),
          _ => fail("No InvalidXMLError generated")
        )
      }

      "when the CBC_OECD version is invalid" in {
        val validFile = new File("test/resources/cbcr-withInvalidCBC-OECDVersion.xml")
        val result = Await.result(validator.validateBusinessRules(validFile, filename).value, 5.seconds)

        result.fold(
          errors => errors.toList should contain(CbcOecdVersionError),
          _ => fail("No InvalidXMLError generated")
        )

      }

      "when the XML Encoding value is NOT UTF-8" in {
        val validFile = new File("test/resources/cbcr-withInvalidXmlEncodingValue.xml")
        val result = Await.result(validator.validateBusinessRules(validFile, filename).value, 5.seconds)

        result.fold(
          errors => errors.toList should contain(XmlEncodingError),
          _ => fail("No InvalidXMLError generated")
        )

      }

      "when the DocRefId refers to the wrong parent group element" in {
        val validFile = new File("test/resources/cbcr-invalid-docrefid-PGE.xml")
        val result = Await.result(validator.validateBusinessRules(validFile, filename).value, 5.seconds)

        result.fold(
          errors => errors.toList should contain(DocRefIdInvalidParentGroupElement),
          _ => fail("No InvalidXMLError generated")
        )

      }
      "when the CorrDocRefId refers to the wrong parent group element" in {
        val validFile = new File("test/resources/cbcr-invalid-corrdocrefid-PGE.xml")
        val result = Await.result(validator.validateBusinessRules(validFile, filename).value, 5.seconds)

        result.fold(
          errors => errors.toList should contain(CorrDocRefIdInvalidParentGroupElement),
          _ => fail("No InvalidXMLError generated")
        )

      }

    }
    "return the KeyXmlInfo when everything is fine" in {
      val validFile = new File("test/resources/cbcr-valid.xml")
      val result = Await.result(validator.validateBusinessRules(validFile,filename).value, 5.seconds)

      result.fold(
        errors => fail(s"Error were generated: $errors"),
        _      => ()
      )
    }

  }

}
