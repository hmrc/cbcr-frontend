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

package uk.gov.hmrc.cbcrfrontend.views

import javax.inject.Inject
import uk.gov.hmrc.cbcrfrontend.views.html._

class Views @Inject()(
  val start: start,
  val errorTemplate: error_template,
  val notAuthorisedIndividual: not_authorised_individual,
  val notAuthorisedAssistant: not_authorised_assistant,
  val notRegistered: submission.notRegistered,
  val submitInfoUltimateParentEntity: submission.submitInfoUltimateParentEntity,
  val utrCheck: submission.utrCheck,
  val submitterInfo: submission.submitterInfo,
  val submitSummary: submission.submitSummary,
  val enterCompanyName: submission.enterCompanyName,
  val submitSuccessReceipt: submission.submitSuccessReceipt,
  val enterCBCId: submission.enterCBCId,
  val subscribeSuccessCbcId: subscription.subscribeSuccessCbcId,
  val contactInfoSubscriber: subscription.contactInfoSubscriber,
  val notAuthorised: subscription.notAuthorised,
  val alreadySubscribed: subscription.alreadySubscribed,
  val subscribeMatchFound: subscription.subscribeMatchFound,
  val contactDetailsUpdated: update.contactDetailsUpdated,
  val updateContactInfoSubscriber: update.updateContactInfoSubscriber,
  val sessionExpired: shared.sessionExpired,
  val enterKnownFacts: shared.enterKnownFacts
)
