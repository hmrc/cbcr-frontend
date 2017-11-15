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

package uk.gov.hmrc.cbcrfrontend

import java.io.File

import com.google.inject.AbstractModule
import org.codehaus.stax2.validation.{XMLValidationSchema, XMLValidationSchemaFactory}
import play.api.{Configuration, Environment, Logger}
import uk.gov.hmrc.cbcrfrontend.auth.{SecuredActions, SecuredActionsImpl}
import uk.gov.hmrc.cbcrfrontend.connectors.BPRKnownFactsConnector
import uk.gov.hmrc.cbcrfrontend.services.RunMode
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.frontend.auth.connectors.AuthConnector
import uk.gov.hmrc.play.http.{HttpGet, HttpPost}

class GuiceModule(environment: Environment,
                  configuration: Configuration) extends AbstractModule with ServicesConfig {
  Logger.warn("In Guice Module")


  override def configure(): Unit = {
    bind(classOf[HttpPost]).toInstance(WSHttp)
    bind(classOf[HttpGet]).toInstance(WSHttp)
    bind(classOf[AuthConnector]).to(classOf[FrontendAuthConnector])
    bind(classOf[SecuredActions]).to(classOf[SecuredActionsImpl])
    bind(classOf[BPRKnownFactsConnector])
    bind(classOf[XMLValidationSchema]).toInstance{
      Logger.error("At beginning")
      val env2 = environment.mode
      Logger.error(s"val env2: $env2")
      val runMode: RunMode = new RunMode(configuration)
      Logger.error("val runMOde")
      val env = runMode.env
      Logger.error(s"val env: $env")
      val path = s"$env.oecd-schema-version"
      Logger.error(s"val path: $path")
      val schemaVer: String = configuration.getString(path).getOrElse {
        Logger.error(s"Failed to find $path in config")
        throw new Exception(s"Missing configuration ${env}.oecd-schema-version")
      }
      val xmlValidationSchemaFactory: XMLValidationSchemaFactory =
        XMLValidationSchemaFactory.newInstance(XMLValidationSchema.SCHEMA_ID_W3C_SCHEMA)
      Logger.error("val xmlValidationSchemaFactory")
      val schemaFile: File = new File(s"conf/schema/${schemaVer}/CbcXML_v${schemaVer}.xsd")
      xmlValidationSchemaFactory.createSchema(schemaFile)
    }
  }
}