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

package uk.gov.hmrc.cbcrfrontend.model

object DocRefIdResponses {

  sealed trait DocRefIdResponses extends Product with Serializable

  sealed trait DocRefIdSaveResponse extends DocRefIdResponses

  case object Ok extends DocRefIdSaveResponse
  case object AlreadyExists extends DocRefIdSaveResponse
  case object Failed extends DocRefIdSaveResponse

  sealed trait DocRefIdQueryResponse extends DocRefIdResponses

  case object Valid extends DocRefIdQueryResponse
  case object Invalid extends DocRefIdQueryResponse
  case object DoesNotExist extends DocRefIdQueryResponse

}
