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
import play.api.{Configuration, Environment}
import uk.gov.hmrc.cbcrfrontend.auth.{SecuredActions, SecuredActionsImpl}
import uk.gov.hmrc.cbcrfrontend.connectors.BPRKnownFactsConnector
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.frontend.auth.connectors.AuthConnector
import uk.gov.hmrc.play.http.{HttpGet, HttpPost}
import uk.gov.hmrc.cbcrfrontend.services.RunMode

class GuiceModule(environment: Environment,
                  configuration: Configuration) extends AbstractModule with ServicesConfig {

  val runMode = new RunMode(configuration)
  val schemaVer: String = {
    if (runMode.env == "Dev")
      configuration.getString(s"Dev.oecd-schema-version").getOrElse(throw new Exception(s"Missing configuration Dev.oecd-schema-version"))
    else
      configuration.getString(s"Prod.oecd-schema-version").getOrElse(throw new Exception(s"Missing configuration Prod.oecd-schema-version"))
  }


  override def configure(): Unit = {
    val xmlValidationSchemaFactory: XMLValidationSchemaFactory =
      XMLValidationSchemaFactory.newInstance(XMLValidationSchema.SCHEMA_ID_W3C_SCHEMA)
    val schemaFile: File = new File(s"conf/schema/${schemaVer}/CbcXML_v${schemaVer}.xsd")

    bind(classOf[HttpPost]).toInstance(WSHttp)
    bind(classOf[HttpGet]).toInstance(WSHttp)
    bind(classOf[AuthConnector]).to(classOf[FrontendAuthConnector])
    bind(classOf[SecuredActions]).to(classOf[SecuredActionsImpl])
    bind(classOf[BPRKnownFactsConnector])
    bind(classOf[XMLValidationSchema]).toInstance{
      xmlValidationSchemaFactory.createSchema(schemaFile)
    }
  }
}