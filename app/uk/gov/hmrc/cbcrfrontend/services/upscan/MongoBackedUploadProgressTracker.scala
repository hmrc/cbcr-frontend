/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.cbcrfrontend.services.upscan

import uk.gov.hmrc.cbcrfrontend.model.upscan.{Reference, UploadId, UploadStatus}

import javax.inject.Singleton
import scala.concurrent.Future

@Singleton
class MongoBackedUploadProgressTracker() extends UploadProgressTracker {
  def requestUpload(uploadId: UploadId, fileReference: Reference): Future[Unit] =
    Future.successful()

  override def registerUploadResult(reference: Reference, uploadStatus: UploadStatus): Future[Unit] =
    Future.successful()

  override def getUploadResult(id: UploadId): Future[Option[Unit]] =
    Future.successful(Option())
}
