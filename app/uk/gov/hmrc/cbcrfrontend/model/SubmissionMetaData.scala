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

import play.api.libs.json.Json
import uk.gov.hmrc.emailaddress.EmailAddress

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
                          ultimateParentEntity:UPE,
                          filingCapacity:FilingCapacity)
object SubmissionInfo{
  implicit val format = Json.format[SubmissionInfo]
}

case class SubmitterInfo(fullName:String,
                         agencyBusinessName:String,
                         jobRole:String,
                         contactPhone:String,
                         email:EmailAddress)

object SubmitterInfo{
  import uk.gov.hmrc.emailaddress.PlayJsonFormats._
  implicit val format = Json.format[SubmitterInfo]
}

case class SubmissionMetaData(submissionInfo:SubmissionInfo,
                              submitterInfo:SubmitterInfo,
                              fileInfo:FileInfo)
object SubmissionMetaData {
  implicit val format = Json.format[SubmissionMetaData]
}
