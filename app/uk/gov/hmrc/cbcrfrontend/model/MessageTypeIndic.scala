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

package uk.gov.hmrc.cbcrfrontend.model

sealed trait MessageTypeIndic

case object CBC401 extends MessageTypeIndic
case object CBC402 extends MessageTypeIndic

object MessageTypeIndic{
  def parseFrom(s:String):Option[MessageTypeIndic] = s.toLowerCase.trim match{
    case "cbc401" => Some(CBC401)
    case "cbc402" => Some(CBC402)
    case _        => None
  }
}

