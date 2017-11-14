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
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.frontend.auth.connectors.AuthConnector
import uk.gov.hmrc.play.http.{HttpGet, HttpPost}
import uk.gov.hmrc.cbcrfrontend.services.RunMode
import play.api.Mode._

class GuiceModule(environment: Environment,
                  configuration: Configuration) extends AbstractModule with ServicesConfig {

//  Logger.info(s"environment.mode: ${environment.mode}")
//  val runMode = new RunMode(configuration)
//  val bollox: String = environment.mode match {
//    case Prod => configuration.getString(s"Prod.oecd-schema-version").getOrElse(throw new Exception(s"Missing configuration Prod.oecd-schema-version"))
//    case _      => configuration.getString(s"Dev.oecd-schema-version").getOrElse(throw new Exception(s"Missing configuration Dev.oecd-schema-version"))
//    }

val env2:String = "Dev"
//Logger.info(s"env = ${env.toString}")

  override def configure(): Unit = {

    bind(classOf[HttpPost]).toInstance(WSHttp)
    bind(classOf[HttpGet]).toInstance(WSHttp)
    bind(classOf[AuthConnector]).to(classOf[FrontendAuthConnector])
    bind(classOf[SecuredActions]).to(classOf[SecuredActionsImpl])
    bind(classOf[BPRKnownFactsConnector])

    bind(classOf[XMLValidationSchema]).toInstance {
      val schemaVerDev: String = configuration.getString("Dev.oecd-schema-version").getOrElse("")
      val schemaVerProd: String = configuration.getString("Prod.oecd-schema-version").getOrElse("")
      val xmlValidationSchemaFactory: XMLValidationSchemaFactory =
        XMLValidationSchemaFactory.newInstance(XMLValidationSchema.SCHEMA_ID_W3C_SCHEMA)
      val schemaFile = env2 match {
        case "Prod" => new File(s"conf/schema/${schemaVerProd}/CbcXML_v${schemaVerProd}.xsd")
        case _ => new File(s"conf/schema/${schemaVerDev}/CbcXML_v${schemaVerDev}.xsd")
      }
//      val schemaFile: File = new File(s"conf/schema/${schemaVer}/CbcXML_v${schemaVer}.xsd")
      xmlValidationSchemaFactory.createSchema(schemaFile)
    }
  }
}