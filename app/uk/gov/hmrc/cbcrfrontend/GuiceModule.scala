/*
 * Copyright 2018 HM Revenue & Customs
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
import play.api.Mode.Mode
import play.api.i18n.{DefaultMessagesApi, MessagesApi}
import play.api.{Configuration, Environment, Logger}
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cbcrfrontend.services.RunMode
import uk.gov.hmrc.http._
import uk.gov.hmrc.http.hooks.HttpHook
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.http.ws._

class GuiceModule(environment: Environment,
                  configuration: Configuration) extends AbstractModule with ServicesConfig {


  override def configure(): Unit = {

    bind(classOf[MessagesApi]).to(classOf[DefaultMessagesApi])

//    bind(classOf[HttpPost]).toInstance(new HttpPost with WSPost {
//      override val hooks: Seq[HttpHook] = NoneRequired
//    })
//    bind(classOf[HttpGet]).toInstance(new HttpGet with WSGet {
//      override val hooks: Seq[HttpHook] = NoneRequired
//    })
//    bind(classOf[HttpPut]).toInstance(new HttpPut with WSPut {
//      override val hooks: Seq[HttpHook] = NoneRequired
//    })
//    bind(classOf[HttpDelete]).toInstance(new HttpDelete with WSDelete {
//      override val hooks: Seq[HttpHook] = NoneRequired
//    })
//    bind(classOf[AuthConnector]).to(classOf[FrontendAuthConnector])
//    bind(classOf[BPRKnownFactsConnector])
    bind(classOf[XMLValidationSchema]).toInstance{
      val runMode: RunMode = new RunMode(configuration)
      val env = runMode.env
      val path = s"$env.oecd-schema-version"
      val schemaVer: String = configuration.getString(path).getOrElse {
        Logger.error(s"Failed to find $path in config")
        throw new Exception(s"Missing configuration $env.oecd-schema-version")
      }
      val xmlValidationSchemaFactory: XMLValidationSchemaFactory =
        XMLValidationSchemaFactory.newInstance(XMLValidationSchema.SCHEMA_ID_W3C_SCHEMA)
      val schemaFile: File = new File(s"conf/schema/$schemaVer/CbcXML_v$schemaVer.xsd")
      xmlValidationSchemaFactory.createSchema(schemaFile)
    }

  }

  override protected def mode: Mode = environment.mode

  override protected def runModeConfiguration: Configuration = configuration

}
