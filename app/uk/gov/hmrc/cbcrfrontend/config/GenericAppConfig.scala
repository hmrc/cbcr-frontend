/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.cbcrfrontend.config

import akka.actor.ActorSystem
import play.api.Mode.Mode
import play.api.{Configuration, Play}
import uk.gov.hmrc.play.config.RunMode

trait GenericAppConfig extends RunMode {

  def mode: Mode = Play.current.mode

  def runModeConfiguration: Configuration = Play.current.configuration

  def appNameConfiguration: Configuration = runModeConfiguration

  def actorSystem: ActorSystem = Play.current.actorSystem

}
