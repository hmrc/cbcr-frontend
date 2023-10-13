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

package uk.gov.hmrc.cbcrfrontend.util

import play.api.libs.json._

import scala.util.Try

sealed trait FeatureSwitch {
  def name: String

  def enabled: Boolean
}

sealed case class BooleanFeatureSwitch(name: String, enabled: Boolean) extends FeatureSwitch

object FeatureSwitch {

  def forName(name: String): FeatureSwitch = {
    val value = System.getProperty(systemPropertyName(name))
    BooleanFeatureSwitch(name, Try(value.toBoolean).toOption.getOrElse(false))
  }

  def apply(name: String, enabled: Boolean): FeatureSwitch = BooleanFeatureSwitch(name, enabled)

  def enable(switch: FeatureSwitch): FeatureSwitch = setProp(switch.name, "true")

  def disable(switch: FeatureSwitch): FeatureSwitch = setProp(switch.name, "false")

  private def setProp(name: String, value: String) = {
    sys.props += ((systemPropertyName(name), value))
    forName(name)
  }

  private def systemPropertyName(name: String) = name

  implicit val format: OFormat[BooleanFeatureSwitch] = Json.format[BooleanFeatureSwitch]
}

object CbcrSwitches {
  private val LANGUAGE_TOGGLE_SWITCH = "enableLanguageSwitching"
  def enableLanguageSwitching: FeatureSwitch =
    FeatureSwitch.forName(LANGUAGE_TOGGLE_SWITCH)
}
