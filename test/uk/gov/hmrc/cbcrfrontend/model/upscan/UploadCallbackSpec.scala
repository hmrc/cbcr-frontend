/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.cbcrfrontend.model.upscan

import base.SpecBase
import play.api.libs.json.Json

import java.time.Instant

class UploadCallbackSpec extends SpecBase {

  "Call back body" - {
    "must marshall correctly when upload is finished" in {
      val json =
        """
          |{
          |   "fileStatus": "READY",
          |   "reference": "ref",
          |   "downloadUrl": "http://test.com",
          |   "uploadDetails": {
          |                 "uploadTimestamp": 1591464117,
          |                 "checksum": "",
          |                 "fileMimeType": "",
          |                 "fileName": ""
          |               }
          |}""".stripMargin

      val expectedResult = ReadyCallbackBody(
        Reference("ref"),
        "http://test.com",
        UploadDetails(Instant.ofEpochMilli(1591464117), "", "", "")
      )

      Json.parse(json).as[CallbackBody] shouldBe expectedResult
    }

    "must marshall correctly when upload has failed" in {
      val json =
        """
          |{
          |   "fileStatus": "FAILED",
          |   "reference": "ref",
          |   "failureDetails": {
          |     "failureReason": "",
          |     "message": ""
          |   }
          |}""".stripMargin

      val expectedResult = FailedCallbackBody(
        Reference("ref"),
        ErrorDetails("", "")
      )

      Json.parse(json).as[CallbackBody] shouldBe expectedResult
    }
  }
}
