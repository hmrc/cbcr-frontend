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

import org.codehaus.stax2.validation.{XMLValidationSchema, XMLValidationSchemaFactory}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.play.guice.GuiceOneAppPerSuite

import java.io.File

class GuiceConfigSpec extends AnyFlatSpec with WithConfigFakeApplication with Matchers with GuiceOneAppPerSuite {

  override def configFile: String = "fakeConfig.conf"

  private val validXmlFile = new File(s"test/resources/cbcr-valid.xml")
  private val propertyName = "fake-oecd-schema-version"

  private val xmlValidationSchemaFactory =
    XMLValidationSchemaFactory.newInstance(XMLValidationSchema.SCHEMA_ID_W3C_SCHEMA)
  private val schemaFile = new File(s"conf/schema/${getString(propertyName)}/CbcXML_v${getString(propertyName)}.xsd")
  private val validator = new CBCRXMLValidator(xmlValidationSchemaFactory.createSchema(schemaFile))

  "A WithConfigFakeApplication " should " be able to retrieve a property defined in the fake config file" in {
    val propertyName = "fake-oecd-schema-version"
    getString(propertyName) should not equal s"$propertyName does not exist"
    getString(propertyName) shouldBe "2.0"
  }

  it should "not return a value when the property does not exist" in {
    val propertyName = "non-existant-property"
    getString(propertyName) shouldBe s"$propertyName does not exist"
  }

  "An Xml Validator" should "not return any error for a valid file" in {
    validator.validateSchema(validXmlFile).hasErrors shouldBe false
    validator.validateSchema(validXmlFile).hasFatalErrors shouldBe false
    validator.validateSchema(validXmlFile).hasWarnings shouldBe false
  }

}
