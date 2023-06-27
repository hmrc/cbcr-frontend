/*
 * Copyright 2023 HM Revenue & Customs
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
import org.scalatest.{BeforeAndAfterAll, Suite}
import play.api._
import play.api.inject.guice.{GuiceApplicationBuilder, GuiceableModule}
import play.api.test.Helpers._

trait WithConfigFakeApplication extends BeforeAndAfterAll {
  this: Suite =>

  def configFile: String

  private lazy val application = new GuiceApplicationBuilder()
    .loadConfig(new Configuration(ConfigFactory.load(configFile)))
    .bindings(bindModules: _*)
    .build()

  def bindModules: Seq[GuiceableModule] = Seq()

  override def beforeAll(): Unit = {
    super.beforeAll()
    Play.start(application)
  }

  override def afterAll(): Unit = {
    super.afterAll()
    Play.stop(application)
  }

  def getString(tag: String): String =
    application.configuration.getOptional[String](tag).getOrElse(tag + " does not exist")

  def evaluateUsingPlay[T](block: => T): T =
    running(application) {
      block
    }

}
