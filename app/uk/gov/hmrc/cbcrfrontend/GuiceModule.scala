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

package uk.gov.hmrc.cbcrfrontend

import com.google.inject.AbstractModule
import org.codehaus.stax2.validation.{XMLValidationSchema, XMLValidationSchemaFactory}
import play.api.{Configuration, Environment}
import uk.gov.hmrc.cbcrfrontend.util.ConfigurationOps.ConfigurationOps

import java.io.File
import java.time.Clock

class GuiceModule( environment: Environment, configuration: Configuration) extends AbstractModule {
  override def configure(): Unit = {
    bind(classOf[XMLValidationSchema]).toInstance {
      val path = "Prod.oecd-schema-version"
      val schemaVer: String = configuration.load[String](path)
      val xmlValidationSchemaFactory = XMLValidationSchemaFactory.newInstance(XMLValidationSchema.SCHEMA_ID_W3C_SCHEMA)
      val schemaFile: File = new File(s"conf/schema/$schemaVer/CbcXML_v$schemaVer.xsd")
      xmlValidationSchemaFactory.createSchema(schemaFile)
    }
    bind(classOf[Clock]).toInstance(Clock.systemDefaultZone())
  }
}
