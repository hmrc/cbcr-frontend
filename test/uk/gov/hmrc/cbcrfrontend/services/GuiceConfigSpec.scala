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

import akka.actor.ActorSystem
import org.scalatest.{FlatSpec, Matchers}
import com.kenshoo.play.metrics.PlayModule
import org.codehaus.stax2.validation.{XMLValidationSchema, XMLValidationSchemaFactory}
import org.scalatestplus.play.OneAppPerSuite
import play.api.Environment



class GuiceConfigSpec extends FlatSpec with WithConfigFakeApplication with Matchers with OneAppPerSuite{

  override def configFile: String = "fakeConfig.conf"

  implicit val env = app.injector.instanceOf[Environment]
  implicit val as = app.injector.instanceOf[ActorSystem]

  val validXmlFile            = new File(s"test/resources/cbcr-valid.xml")
  val propertyName            = "fake-oecd-schema-version"

  val xmlValidationSchemaFactory: XMLValidationSchemaFactory =
    XMLValidationSchemaFactory.newInstance(XMLValidationSchema.SCHEMA_ID_W3C_SCHEMA)
  val schemaFile: File = new File(s"conf/schema/${getString(propertyName)}/CbcXML_v${getString(propertyName)}.xsd")
  val validator = new CBCRXMLValidator(env, xmlValidationSchemaFactory.createSchema(schemaFile))


  "A WithConfigFakeApplication " should " be able to retrieve a property defined in the fake config file" in {
    val propertyName = "fake-oecd-schema-version"
    getString(propertyName) should not equal s"${propertyName} does not exist"
    getString(propertyName) shouldBe "1.0.1"
  }

  it should "not return a value when the property does not exist" in {
    val propertyName = "non-existant-property"
    getString(propertyName) shouldBe s"${propertyName} does not exist"
  }

  "An Xml Validator" should "not return any error for a valid file" in {
      validator.validateSchema(validXmlFile).hasErrors shouldBe false
      validator.validateSchema(validXmlFile).hasFatalErrors shouldBe false
      validator.validateSchema(validXmlFile).hasWarnings shouldBe false
  }

}
