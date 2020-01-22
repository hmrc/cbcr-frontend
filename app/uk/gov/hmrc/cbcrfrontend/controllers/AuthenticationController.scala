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

package uk.gov.hmrc.cbcrfrontend.controllers

import java.util.Base64

import org.mindrot.jbcrypt.BCrypt
import play.api.mvc.Security.AuthenticatedBuilder
import play.api.mvc._

import scala.concurrent.ExecutionContext
import scala.util.Try

case class Creds(username: String, password: String) {
  def check(providedUsername: String, providedPassword: String): Boolean =
    (providedUsername == username) && BCrypt.checkpw(providedPassword, password)

}

case class AuthenticationController(credentials: Creds)(
  implicit executionContext: ExecutionContext,
  defaultParser: BodyParser[AnyContent])
    extends AuthenticatedBuilder[String](
      AuthenticationController.extractCredentials(credentials),
      defaultParser,
      AuthenticationController.onUnauthorised)

object AuthenticationController {
  def extractCredentials(storedCredentials: Creds): RequestHeader => Option[String] = { header =>
    for {
      authHeader <- header.headers.get("Authorization")
      encoded    <- authHeader.split(" ").drop(1).headOption
      (username, password) <- Try {
                               val authInfo = new String(Base64.getDecoder.decode(encoded)).split(":").toList
                               (authInfo(0), authInfo(1))
                             }.toOption
      authenticatedUsername <- if (storedCredentials.check(username, password)) Some(username) else None
    } yield authenticatedUsername

  }

  def onUnauthorised: RequestHeader => Result = { header =>
    Results.Unauthorized("Authentication failed").withHeaders("WWW-Authenticate" -> """Basic realm="Secured"""")
  }
}
