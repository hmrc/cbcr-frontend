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

package uk.gov.hmrc.cbcrfrontend.views

import javax.inject.Inject

class Views @Inject() (
  val start: uk.gov.hmrc.cbcrfrontend.views.html.start,
  val errorTemplate: uk.gov.hmrc.cbcrfrontend.views.html.error_template,
  val errorContactDetails: uk.gov.hmrc.cbcrfrontend.views.html.error_contact_details,
  val notAuthorisedIndividual: uk.gov.hmrc.cbcrfrontend.views.html.not_authorised_individual,
  val notAuthorisedAssistant: uk.gov.hmrc.cbcrfrontend.views.html.not_authorised_assistant,
  val notRegistered: uk.gov.hmrc.cbcrfrontend.views.html.submission.notRegistered,
  val submitInfoUltimateParentEntity: uk.gov.hmrc.cbcrfrontend.views.html.submission.submitInfoUltimateParentEntity,
  val utrCheck: uk.gov.hmrc.cbcrfrontend.views.html.submission.utrCheck,
  val submitterInfo: uk.gov.hmrc.cbcrfrontend.views.html.submission.submitterInfo,
  val submitSummary: uk.gov.hmrc.cbcrfrontend.views.html.submission.submitSummary,
  val enterCompanyName: uk.gov.hmrc.cbcrfrontend.views.html.submission.enterCompanyName,
  val submitSuccessReceipt: uk.gov.hmrc.cbcrfrontend.views.html.submission.submitSuccessReceipt,
  val unregisteredGGAccount: uk.gov.hmrc.cbcrfrontend.views.html.submission.unregisteredGGAccount,
  val chooseFile: uk.gov.hmrc.cbcrfrontend.views.html.submission.fileupload.chooseFile,
  val fileUploadProgress: uk.gov.hmrc.cbcrfrontend.views.html.submission.fileupload.fileUploadProgress,
  val fileUploadResult: uk.gov.hmrc.cbcrfrontend.views.html.submission.fileupload.fileUploadResult,
  val fileUploadError: uk.gov.hmrc.cbcrfrontend.views.html.submission.fileupload.fileUploadError,
  val enterCBCId: uk.gov.hmrc.cbcrfrontend.views.html.submission.enterCBCId,
  val subscribeSuccessCbcId: uk.gov.hmrc.cbcrfrontend.views.html.subscription.subscribeSuccessCbcId,
  val contactInfoSubscriber: uk.gov.hmrc.cbcrfrontend.views.html.subscription.contactInfoSubscriber,
  val notAuthorised: uk.gov.hmrc.cbcrfrontend.views.html.subscription.notAuthorised,
  val alreadySubscribed: uk.gov.hmrc.cbcrfrontend.views.html.subscription.alreadySubscribed,
  val subscribeMatchFound: uk.gov.hmrc.cbcrfrontend.views.html.subscription.subscribeMatchFound,
  val contactDetailsUpdated: uk.gov.hmrc.cbcrfrontend.views.html.update.contactDetailsUpdated,
  val updateContactInfoSubscriber: uk.gov.hmrc.cbcrfrontend.views.html.update.updateContactInfoSubscriber,
  val sessionExpired: uk.gov.hmrc.cbcrfrontend.views.html.shared.sessionExpired,
  val enterKnownFacts: uk.gov.hmrc.cbcrfrontend.views.html.shared.enterKnownFacts,
  val knownFactsNotFound: uk.gov.hmrc.cbcrfrontend.views.html.shared.knownFactsNotFound,
  val notAuthorizedEnhancement: uk.gov.hmrc.cbcrfrontend.views.html.notAuthorisedEnhancement
)
