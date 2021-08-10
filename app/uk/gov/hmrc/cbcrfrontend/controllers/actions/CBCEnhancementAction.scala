/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.cbcrfrontend.controllers.actions

import play.api.mvc.Results.Redirect
import play.api.mvc.{ActionBuilderImpl, BodyParsers, Request, Result}
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.views._

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class CBCEnhancementAction @Inject()(
  appConfig: FrontendAppConfig,
  parser: BodyParsers.Default,
  views: Views
)(implicit ec: ExecutionContext)
    extends ActionBuilderImpl(parser) {
  override def invokeBlock[A](request: Request[A], block: (Request[A]) => Future[Result]): Future[Result] =
    if (appConfig.cbcEnhancementFeature) {
      block(request)
    } else {
      Future.successful(
        Redirect(uk.gov.hmrc.cbcrfrontend.controllers.routes.CBCEnhancementsController.enhancementUnavailable))
    }
}
