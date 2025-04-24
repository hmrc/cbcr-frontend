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

import com.typesafe.config.ConfigFactory
import org.codehaus.stax2.validation.{XMLValidationSchema, XMLValidationSchemaFactory}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.mockito.MockitoSugar
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Configuration

import java.io.File

class CBCXMLValidatorSpec extends AnyWordSpec with Matchers with GuiceOneAppPerSuite with MockitoSugar {

  private def loadFile(filename: String) = new File(s"test/resources/$filename")

  private val validXmlFile = loadFile("cbcr-valid.xml")
  private val invalidXmlFile = loadFile("cbcr-invalid.xml")
  private val invalidMultipleXmlFile = loadFile("cbcr-invalid-multiple-errors.xml")
  private val invalidMultipleXmlFile2 = loadFile("cbcr-invalid-multiple-errors2.xml")
  private val fatal = loadFile("fatal.xml")
  private val configuration = new Configuration(ConfigFactory.load("application.conf"))

  private val schemaVer = configuration
    .getOptional[String]("Prod.oecd-schema-version")
    .getOrElse(s"Prod.oecd-schema-version does not exist")
  private val xmlValidationSchemaFactory: XMLValidationSchemaFactory =
    XMLValidationSchemaFactory.newInstance(XMLValidationSchema.SCHEMA_ID_W3C_SCHEMA)
  private val schemaFile = new File(s"conf/schema/$schemaVer/CbcXML_v$schemaVer.xsd")
  private val validator = new CBCRXMLValidator(xmlValidationSchemaFactory.createSchema(schemaFile))

  "An Xml Validator" should {
    "not return any error for a valid file" in {
      validator.validateSchema(validXmlFile).hasErrors shouldBe false
      validator.validateSchema(validXmlFile).hasFatalErrors shouldBe false
      validator.validateSchema(validXmlFile).hasWarnings shouldBe false
    }

    "return an error if the file is invalid and a single error" in {
      val validate = validator.validateSchema(invalidXmlFile)
      validate.hasErrors shouldBe true
      validate.errorsCollection.size shouldBe 5
    }

    "return multiple errors if the file is invalid and has multiple errors" in {
      val validate = validator.validateSchema(invalidMultipleXmlFile)
      validate.hasErrors shouldBe true
      validate.errorsCollection.size shouldBe 20
    }

    "not throw errors if the validator encounters a fatal error" in {
      val validate = validator.validateSchema(fatal)
      validate.hasFatalErrors shouldBe true
    }

    "stop collecting errors after the configurable limit is reached" in {
      val validate = validator.validateSchema(invalidMultipleXmlFile2)
      validate.hasFatalErrors shouldBe true
      validate.hasErrors shouldBe true
      validate.errorsCollection.size shouldBe 100
    }
  }
}
