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

import play.api.libs.json._
import uk.gov.hmrc.emailaddress.EmailAddress
import uk.gov.hmrc.emailaddress.PlayJsonFormats._

case class SubmitterInfo(fullName: String,
                         agencyBusinessName: Option[AgencyBusinessName],
                         contactPhone: String,
                         email:EmailAddress,
                         affinityGroup: Option[AffinityGroup]
                        )


object SubmitterInfo {
  implicit val format = new Format[SubmitterInfo] {
    override def reads(json: JsValue) = json match {
      case JsObject(m) =>
        val result = for {
        fullName <- m.get("fullName").flatMap(_.asOpt[String])
        abn      <- m.get("agencyBusinessName").map(_.asOpt[String])
        cp       <- m.get("contactPhone").flatMap(_.asOpt[String])
        email    <- m.get("email").flatMap(_.asOpt[EmailAddress])
        ag       <- m.get("affinityGroup").map(_.asOpt[String])
      } yield JsSuccess(SubmitterInfo(fullName,abn.map(AgencyBusinessName(_)),cp,email,ag.map(AffinityGroup(_))))
        result.getOrElse(JsError(s"Unable to serialise $json as a  SubmitterInfo"))
      case _ => JsError(s"Unable to serialise $json as a  SubmitterInfo")

    }

    override def writes(s: SubmitterInfo) = Json.obj(
      "fullName" -> s.fullName,
      "agencyBusinessName" -> s.agencyBusinessName.map(_.name),
      "contactPhone" -> s.contactPhone,
      "email" -> s.email,
      "affinityGroup" -> s.affinityGroup.map(_.affinityGroup)
    )
  }
}