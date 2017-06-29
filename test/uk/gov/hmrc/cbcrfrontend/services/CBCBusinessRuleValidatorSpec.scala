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

import org.mockito.Matchers._
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.play.http.HeaderCarrier
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import cats.instances.future._
/**
  * Created by max on 24/05/17.
  */
class CBCBusinessRuleValidatorSpec extends UnitSpec with MockitoSugar{


  val messageRefIdService = mock[MessageRefIdService]
  implicit val hc = HeaderCarrier()
  val extract = new XmlInfoExtract()
  implicit def fileToXml(f:File) : RawXMLInfo = extract.extract(f)

  val cbcId = CBCId.create(56).getOrElse(fail("failed to generate CBCId"))
  val filename = "GB2016RGXVCBC0000000056CBC40120170311T090000X.xml"

  val validator = new CBCBusinessRuleValidator(messageRefIdService)
  "The CBCBusinessRuleValidator" should {
    "return the correct error" when {
      "messageRefId is empty and return the correct message and errorcode" in {
        val missingMessageRefID = new File("test/resources/cbcr-invalid-empty-messageRefID.xml")
        val result = Await.result(validator.validateBusinessRules(missingMessageRefID, cbcId, filename).value, 5.seconds)

        result.fold(
          errors => errors.toList should contain(MessageRefIDMissing),
          _ => fail("No MessageRefIDMissing error generated")
        )
      }
      "messageRefId is null and return the correct message and errorcode" in {
        val nullMessageRefID = new File("test/resources/cbcr-invalid-null-messageRefID.xml")
        val result = Await.result(validator.validateBusinessRules(nullMessageRefID, cbcId, filename).value, 5.seconds)

        result.fold(
          errors => errors.toList should contain(MessageRefIDMissing),
          _ => fail("No MessageRefIDMissing error generated")
        )
      }
      "messageRefId format is wrong" in {
        val invalidMessageRefID = new File("test/resources/cbcr-invalid-invalid-messageRefID.xml")
        val result = Await.result(validator.validateBusinessRules(invalidMessageRefID, cbcId, filename).value, 5.seconds)

        result.fold(
          errors => errors.toList should contain(MessageRefIDFormatError),
          _ => fail("No MessageRefIDFormatError error generated")
        )
      }
      "messageRefId contains a CBCId that doesnt match the CBCId in the SendingEntityIN field" in {
        when(messageRefIdService.messageRefIdExists(any())(any())) thenReturn Future.successful(false)
        val invalidMessageRefID = new File("test/resources/cbcr-invalid-cbcId-messageRefID.xml")
        val result = Await.result(validator.validateBusinessRules(invalidMessageRefID, cbcId, filename).value, 5.seconds)

        result.fold(
          errors => errors.toList should contain(MessageRefIDCBCIdMismatch),
          _ => fail("No MessageRefIDCBCIdMismatch error generated")
        )
      }
      "messageRefId contains a Reporting Year that doesn't match the year in the ReportingPeriod field" in {
        when(messageRefIdService.messageRefIdExists(any())(any())) thenReturn Future.successful(false)
        val invalidMessageRefID = new File("test/resources/cbcr-invalid-reportingYear-messageRefID.xml")
        val result = Await.result(validator.validateBusinessRules(invalidMessageRefID, cbcId, filename).value, 5.seconds)

        result.fold(
          errors => errors.toList should contain(MessageRefIDReportingPeriodMismatch),
          _ => fail("No MessageRefIDReportingPeriodMismatch error generated")
        )
      }
      "messageRefId contains a creation timestamp that isn't valid" in {
        when(messageRefIdService.messageRefIdExists(any())(any())) thenReturn Future.successful(false)
        val invalidMessageRefID = new File("test/resources/cbcr-invalid-creationTimestamp-messageRefID.xml")
        val result = Await.result(validator.validateBusinessRules(invalidMessageRefID, cbcId, filename).value, 5.seconds)

        result.fold(
          errors => errors.toList should contain(MessageRefIDTimestampError),
          _ => fail("No MessageRefIDTimestampError error generated")
        )
      }
      "messageRefId has been seen before" in {
        when(messageRefIdService.messageRefIdExists(any())(any())) thenReturn Future.successful(true)
        val validFile = new File("test/resources/cbcr-valid.xml")
        val result = Await.result(validator.validateBusinessRules(validFile, cbcId, filename).value, 5.seconds)

        result.fold(
          errors => errors.toList should contain(MessageRefIDDuplicate),
          _ => fail("No MessageRefIdDuplicate error generated")
        )
      }
      "test data is present" in {
        val validFile = new File("test/resources/cbcr-testData.xml")
        val result = Await.result(validator.validateBusinessRules(validFile, cbcId, filename).value, 5.seconds)

        result.fold(
          errors => errors.head shouldBe TestDataError,
          _ => fail("No TestDataError generated")
        )
      }

      "SendingEntityIn does not match the CBCId we provided" in {
        when(messageRefIdService.messageRefIdExists(any())(any())) thenReturn Future.successful(false)
        val validFile = new File("test/resources/cbcr-valid.xml")
        val result = Await.result(validator.validateBusinessRules(validFile, CBCId.create(57).getOrElse(fail("cant gen cbcid")), filename).value, 5.seconds)

        result.fold(
          errors => errors.head shouldBe SendingEntityError,
          _ => fail("No TestDataError generated")
        )
      }

      "ReceivingCountry does not equal GB" in {
        val validFile = new File("test/resources/cbcr-invalidReceivingCountry.xml")
        val result = Await.result(validator.validateBusinessRules(validFile, cbcId, filename).value, 5.seconds)

        result.fold(
          errors => errors.head shouldBe ReceivingCountryError,
          _ => fail("No TestDataError generated")
        )
      }

      "Filename does not match MessageRefId" in {
        val validFile = new File("test/resources/cbcr-valid.xml")
        val result = Await.result(validator.validateBusinessRules(validFile, cbcId, "INVALID" + filename).value, 5.seconds)

        result.fold(
          errors => errors.head shouldBe FileNameError,
          _ => fail("No FileNameError generated")
        )
      }

      "ReportingRole is missing" in {
        val validFile = new File("test/resources/cbcr-noReportingEntity.xml")
        val result = Await.result(validator.validateBusinessRules(validFile, cbcId, filename).value, 5.seconds)

        result.fold(
          errors => errors.head shouldBe InvalidXMLError("ReportingEntity.ReportingRole not found or invalid"),
          _ => fail("No InvalidXMLError generated")
        )
      }
      "MessageTypeIndic is CBC402 and the DocTypeIndic's are invalid" when {
        "CBCReports.docTypeIndic isn't OECD2 or OECD3" in{
          val validFile = new File("test/resources/cbcr-messageTypeIndic.xml")
          val result = Await.result(validator.validateBusinessRules(validFile, cbcId, filename).value, 5.seconds)

          result.fold(
            errors => errors.head shouldBe MessageTypeIndicError,
            _ => fail("No InvalidXMLError generated")
          )
        }
        "AdditionalInfo.docTypeIndic isn't OECD2 or OECD3" in{
          val validFile = new File("test/resources/cbcr-messageTypeIndic2.xml")
          val result = Await.result(validator.validateBusinessRules(validFile, cbcId, filename).value, 5.seconds)

          result.fold(
            errors => errors.head shouldBe MessageTypeIndicError,
            _ => fail("No InvalidXMLError generated")
          )
        }
        "ReportingEntity.docTypeIndic isn't OECD2 or OECD3 or OECD0" in{
          val validFile = new File("test/resources/cbcr-messageTypeIndic3.xml")
          val result = Await.result(validator.validateBusinessRules(validFile, cbcId, filename).value, 5.seconds)

          result.fold(
            errors => errors.head shouldBe MessageTypeIndicError,
            _ => fail("No InvalidXMLError generated")
          )
        }
      }

      "File is not a valid xml file" in {
        val validFile = new File("test/resources/actually_a_jpg.xml")
        val result = Await.result(validator.validateBusinessRules(validFile, cbcId, filename).value, 5.seconds)

        result.fold(
          errors => (),
          _ => fail("No InvalidXMLError generated")
        )
      }
    }
    "return the KeyXmlInfo when everything is fine" in {
      val validFile = new File("test/resources/cbcr-valid.xml")
      val result = Await.result(validator.validateBusinessRules(validFile,cbcId,filename).value, 5.seconds)

      result.fold(
        errors => fail(s"Error were generated: $errors"),
        _      => ()
      )
    }
  }

}
