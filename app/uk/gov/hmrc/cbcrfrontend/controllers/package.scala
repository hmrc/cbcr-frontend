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
package uk.gov.hmrc.cbcrfrontend

import cats.data.EitherT
import cats.instances.all._
import play.api.mvc.{AnyContent, Request, Result}
import uk.gov.hmrc.cbcrfrontend.model.CBCErrors
import uk.gov.hmrc.play.frontend.auth.AuthContext

import scala.concurrent.{ExecutionContext, Future}

package object controllers {
  type AsyncUserRequest = AuthContext => Request[AnyContent] => Future[Result]
  type UserRequest = AuthContext => Request[AnyContent] => Result
  //todo refactor everything to use these
  def pure[A](a:A)(implicit ec:ExecutionContext) = EitherT.pure[Future,CBCErrors,A](a)
  def right[A](a:Future[A])(implicit ec:ExecutionContext) = EitherT.right[Future,CBCErrors,A](a)
  def fromEither[A,B](e:Either[A,B])(implicit ec:ExecutionContext)  = EitherT.fromEither[Future](e)
}
