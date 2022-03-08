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

import org.scalatest.{EitherValues, FreeSpec, Matchers}
import play.api.mvc.PathBindable

class UploadIdSpec extends FreeSpec with Matchers with EitherValues {

  "UploadId" - {
    "must bind from url" in {
      val pathBindable = implicitly[PathBindable[UploadId]]
      val uploadId = UploadId("12")

      val bind: Either[String, UploadId] = pathBindable.bind("uploadId", "12")
      bind.right.value shouldBe uploadId
    }

    "unbind to path value" in {
      val pathBindable = implicitly[PathBindable[UploadId]]
      val uploadId = UploadId("12")

      val bindValue = pathBindable.unbind("uploadId", uploadId)
      bindValue shouldBe "12"
    }
  }
}
