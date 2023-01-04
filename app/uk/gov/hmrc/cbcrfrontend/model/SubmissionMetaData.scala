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

package uk.gov.hmrc.cbcrfrontend.model

import play.api.libs.json._ // JSON library
import play.api.libs.json.Reads._ // Custom validation helpers
import cats.syntax.show._

/**
  * Created by max on 11/05/17.
  */
case class FileInfo(
  id: FileId,
  envelopeId: EnvelopeId,
  status: String,
  name: String,
  contentType: String,
  length: BigDecimal,
  created: String)
object FileInfo {
  implicit val format = Json.format[FileInfo]
}

case class SubmissionInfo(
  gwCredId: String,
  cbcId: CBCId,
  bpSafeId: String,
  hash: Hash,
  ofdsRegime: String,
  tin: TIN,
  filingType: FilingType,
  ultimateParentEntity: UltimateParentEntity)
object SubmissionInfo {
  implicit val format = new Format[SubmissionInfo] {
    override def reads(json: JsValue) = json match {
      case JsObject(m) =>
        for {
          gwCredId   <- m.get("gwCredId").fold[JsResult[String]](JsError("gwCredId not found"))(_.validate[String])
          cbcId      <- m.get("cbcId").fold[JsResult[CBCId]](JsError("cbcId not found"))(_.validate[CBCId])
          bpSafeId   <- m.get("bpSafeId").fold[JsResult[String]](JsError("bpSafeId not found"))(_.validate[String])
          hash       <- m.get("hash").fold[JsResult[Hash]](JsError("hash not found"))(_.validate[String].map(Hash(_)))
          ofdsRegime <- m.get("ofdsRegime").fold[JsResult[String]](JsError("ofdsRegime not found"))(_.validate[String])
          tin        <- m.get("tin").fold[JsResult[String]](JsError("TIN not found"))(_.validate[String])
          fts        <- m.get("filingType").fold[JsResult[String]](JsError("FilingType not found"))(_.validate[String])
          ft <- ReportingRole
                 .parseFromString(fts)
                 .fold[JsResult[FilingType]](JsError(s"FilingType invalid: $fts"))(r => JsSuccess(FilingType(r)))
          upe <- m.get("ultimateParentEntity")
                  .fold[JsResult[String]](JsError("UPE not found"))(_.validate[String])
                  .map(UltimateParentEntity(_))
        } yield SubmissionInfo(gwCredId, cbcId, bpSafeId, hash, ofdsRegime, TIN(tin, ""), ft, upe)
      case _ => JsError(s"Unable to parse $json as SubmissionInfo")
    }

    override def writes(o: SubmissionInfo) = Json.obj(
      "gwCredId"             -> o.gwCredId,
      "cbcId"                -> o.cbcId,
      "bpSafeId"             -> o.bpSafeId,
      "hash"                 -> o.hash.value,
      "ofdsRegime"           -> o.ofdsRegime,
      "tin"                  -> o.tin.value,
      "filingType"           -> o.filingType.value.show,
      "ultimateParentEntity" -> o.ultimateParentEntity.ultimateParentEntity
    )
  }
}

case class SubmissionMetaData(submissionInfo: SubmissionInfo, submitterInfo: SubmitterInfo, fileInfo: FileInfo)
object SubmissionMetaData {

  implicit val format = Json.format[SubmissionMetaData]
}
