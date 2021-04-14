/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.cbcrfrontend

import java.io.File

import com.google.inject.AbstractModule
import org.codehaus.stax2.validation.{XMLValidationSchema, XMLValidationSchemaFactory}
import play.api.{Configuration, Environment, Logger, Mode}
import uk.gov.hmrc.cbcrfrontend.services.RunMode

class GuiceModule(environment: Environment, configuration: Configuration) extends AbstractModule {

  val mode: Mode = environment.mode
  val runModeConfiguration: Configuration = configuration

  lazy val logger: Logger = Logger(this.getClass)

  override def configure(): Unit =
    bind(classOf[XMLValidationSchema]).toInstance {
      val runMode: RunMode = new RunMode(configuration)
      val env = runMode.env
      val path = s"$env.oecd-schema-version"
      val schemaVer: String = "2.0"
      val xmlValidationSchemaFactory: XMLValidationSchemaFactory =
        XMLValidationSchemaFactory.newInstance(XMLValidationSchema.SCHEMA_ID_W3C_SCHEMA)
      val schemaFile: File = new File(s"conf/schema/$schemaVer/CbcXML_v$schemaVer.xsd")
      xmlValidationSchemaFactory.createSchema(schemaFile)
    }
}
