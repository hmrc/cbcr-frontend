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

package uk.gov.hmrc.cbcrfrontend

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

import cats.data.EitherT
import uk.gov.hmrc.cbcrfrontend.model.{CBCErrors, UnexpectedState}

import scala.concurrent.{ExecutionContext, Future}

package object core {

  type ServiceResponse[A] = EitherT[Future, CBCErrors, A]
  type CBCErrorOr[A] = Either[CBCErrors, A]

  def fromFutureOptA[A](fa: Future[CBCErrorOr[A]]): ServiceResponse[A] =
    EitherT[Future, CBCErrors, A](fa)

  def fromFutureA[A](fa: Future[A])(implicit ec: ExecutionContext): ServiceResponse[A] =
    EitherT[Future, CBCErrors, A](fa.map(Right(_)))

  def fromOptA[A](oa: CBCErrorOr[A]): ServiceResponse[A] =
    EitherT[Future, CBCErrors, A](Future.successful(oa))

  def fromFutureOptionA[A](fo: Future[Option[A]])(invalid: => UnexpectedState)(
    implicit ec: ExecutionContext): ServiceResponse[A] = {
    val futureA = fo.map {
      case Some(a) => Right(a)
      case None    => Left(invalid)
    }
    EitherT[Future, CBCErrors, A](futureA)
  }
}
