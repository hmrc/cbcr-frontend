/*
 * Copyright 2020 HM Revenue & Customs
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

sealed trait FieldName

case object FullName extends FieldName {
  override def toString: String = "fullName"
}
case object ContactPhone extends FieldName {
  override def toString: String = "contactPhone"
}
case object ContactEmail extends FieldName {
  override def toString: String = "email"
}

object FieldName {
  implicit class CaseInsensitiveRegex(sc: StringContext) {
    def ci = ("(?i)" + sc.parts.mkString).r
  }

  def fromString(s: String): Option[FieldName] = s match {
    case ci"fullname"     => Some(FullName)
    case ci"contactphone" => Some(ContactPhone)
    case ci"email"        => Some(ContactEmail)
    case _                => None
  }

}
