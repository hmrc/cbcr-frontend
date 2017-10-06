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

import play.api.libs.json.{JsValue, Json, Writes}
import uk.gov.hmrc.emailaddress.EmailAddress
import cats.syntax.show._

/**
  * Created by max on 11/05/17.
  */
case class FileInfo(id:FileId,
                    envelopeId: EnvelopeId,
                    status:String,
                    name:String,
                    contentType:String,
                    length: BigDecimal,
                    created: String)
object FileInfo{
  implicit val format = Json.format[FileInfo]
}

case class SubmissionInfo(gwCredId:String,
                          cbcId:CBCId,
                          bpSafeId:String,
                          hash:Hash,
                          ofdsRegime:String,
                          utr:Utr,
                          filingType:FilingType,
                          ultimateParentEntity:UltimateParentEntity)
object SubmissionInfo{
  implicit val format = Json.format[SubmissionInfo]
}

case class SubmissionMetaData(submissionInfo:SubmissionInfo,
                              submitterInfo:SubmitterInfo,
                              fileInfo:FileInfo)
object SubmissionMetaData {
  implicit val writes = new Writes[SubmissionMetaData] {
    override def writes(o: SubmissionMetaData): JsValue = Json.obj(
      "fileInfo" -> Json.obj(
        "id" -> o.fileInfo.id,
        "envelopeId" -> o.fileInfo.envelopeId,
        "status" -> o.fileInfo.status,
        "name" -> o.fileInfo.name,
        "contentType" -> o.fileInfo.contentType,
        "length" -> o.fileInfo.length,
        "created" -> o.fileInfo.created
      ),
      "submissionInfo" -> Json.obj(
        "gwCredId" -> o.submissionInfo.gwCredId,
        "cbcId" -> o.submissionInfo.cbcId,
        "bpSafeId" -> o.submissionInfo.bpSafeId,
        "hash" -> o.submissionInfo.hash.value,
        "ofdsRegime" -> o.submissionInfo.ofdsRegime,
        "utr" -> o.submissionInfo.utr.value,
        "filingType" -> o.submissionInfo.filingType.value.show,
        "ultimateParentEntity" -> o.submissionInfo.ultimateParentEntity.ultimateParentEntity
      ),
      "submitterInfo" -> Json.obj(
        "fullName" -> o.submitterInfo.fullName,
        "agencyBusinessName" -> o.submitterInfo.agencyBusinessName.map(_.name),
        "email" -> o.submitterInfo.email.value,
        "affinityGroup" -> o.submitterInfo.affinityGroup.map(_.affinityGroup)
      )
    )
  }
}
