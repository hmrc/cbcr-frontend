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

import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Failure, Success, Try}

class CBCXMLValidatorSpec extends FlatSpec with Matchers {

  val validXmlFile: File = loadFile("cbcr-valid.xml")
  val invalidXmlFile: File = loadFile("cbcr-invalid.xml")
  val invalidMultipleXmlFile: File = loadFile("cbcr-invalid-multiple-errors.xml")
  val fatal:File = loadFile("fatal.xml")

  val ERROR_MESSAGE = "cvc-datatype-valid.1.2.1: '2016-11-01 15:00' is not a valid value for 'dateTime'."

  private def loadFile(filename: String) = {
    Try {
      new File(s"test/resources/${filename}")
    } match {
      case Success(file) => {
        if(file.exists()) file
        else fail(s"File not found: ${filename}")
      }
      case Failure(e) => fail(e)
    }
  }

  "An Xml Validator" should "not return any error for a valid file" in {
    CBCRXMLValidator.validateSchema(validXmlFile).isValid shouldBe true
  }

  it should "return an error if the file is invalid and a single error" in {
    val validate = CBCRXMLValidator.validateSchema(invalidXmlFile)
    validate.isValid shouldBe false
    validate.toEither.isLeft shouldBe true
    validate.toEither.fold(a => {
      assert(a.hasErrors)
      assert(a.errorsCollection.size == 2)
    }, f => f.deleteOnExit())
  }

  it should "return multiple errors if the file is invalid and has multiple errors" in {
    val validate = CBCRXMLValidator.validateSchema(invalidMultipleXmlFile)
    validate.isValid shouldBe false
    validate.toEither.isLeft shouldBe true
    validate.toEither.fold(a => {
      assert(a.hasErrors)
      assert(a.errorsCollection.size == 40)
    }, f => f.deleteOnExit())
  }

  it should "not throw errors if the validator encounters a fatal error" in {
    CBCRXMLValidator.validateSchema(fatal).fold(
      (xmlErrors: XmlErrorHandler) => println("At least I didn't throw an exception"),
      (file: File) => fail("Should have failed!")
    )
  }


}
