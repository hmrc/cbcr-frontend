/*
 * Copyright 2024 HM Revenue & Customs
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

package uk.gov.hmrc

import _root_.play.api.Logger
import _root_.play.api.i18n.Messages
import _root_.play.api.libs.json.Reads
import _root_.play.api.mvc.Results._
import _root_.play.api.mvc._
import cats.data.ValidatedNel
import cats.implicits.catsSyntaxTuple10Semigroupal
import cats.instances.future._
import cats.syntax.show._
import cats.{Applicative, Functor}
import uk.gov.hmrc.auth.core.Enrolment
import uk.gov.hmrc.auth.core.retrieve.{Credentials, Retrieval, SimpleRetrieval}
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.controllers._
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.repositories.CBCSessionCache
import uk.gov.hmrc.cbcrfrontend.views.html.{error_template, not_authorised_individual}
import uk.gov.hmrc.http.HeaderCarrier

import java.io.{File, FileInputStream}
import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions

package object cbcrfrontend {

  lazy val logger: Logger = Logger(this.getClass)

  //  class ErrorRedirect @Ine
  val cbcEnrolment: Retrieval[Option[CBCEnrolment]] = SimpleRetrieval(
    "allEnrolments",
    Reads.set[Enrolment].map { e =>
      for {
        value <- e.find(_.key == "HMRC-CBC-ORG")
        id    <- value.identifiers.find(_.key == "cbcId")
        utr   <- value.identifiers.find(_.key == "UTR")
        cbcId <- CBCId(id.value)
      } yield CBCEnrolment(cbcId, Utr(utr.value))
    }
  )

  private type ValidResult[A] = ValidatedNel[CBCErrors, A]
  private type CacheResult[A] = ValidatedNel[ExpiredSession, A]
  type FutureValidResult[A] = Future[ValidResult[A]]
  type FutureCacheResult[A] = Future[CacheResult[A]]
  type ValidBusinessResult[A] = ValidatedNel[BusinessRuleErrors, A]
  type FutureValidBusinessResult[A] = Future[ValidBusinessResult[A]]

  implicit def applicativeInstance(implicit ec: ExecutionContext): Applicative[FutureValidBusinessResult] =
    Applicative[Future] compose Applicative[ValidBusinessResult]
  implicit def applicativeInstance2(implicit ec: ExecutionContext): Applicative[FutureCacheResult] =
    Applicative[Future] compose Applicative[CacheResult]
  implicit def functorInstance(implicit ec: ExecutionContext): Functor[FutureValidBusinessResult] =
    Functor[Future] compose Functor[ValidBusinessResult]

  implicit def toTheFuture[A](a: ValidBusinessResult[A]): FutureValidBusinessResult[A] = Future.successful(a)
  implicit def resultFuture(r: Result): Future[Result] = Future.successful(r)

  def errorRedirect(
    error: CBCErrors,
    notAuthorisedIndividual: not_authorised_individual,
    errorTemplate: error_template
  )(implicit request: Request[_], msgs: Messages, feConfig: FrontendAppConfig): Result = {
    logger.error(error.show)
    error match {
      case ExpiredSession(_) => Redirect(routes.SharedController.sessionExpired)
      case UnexpectedState(error, _) if error.equals("Individuals are not permitted to use this service") =>
        Forbidden(
          notAuthorisedIndividual()
        )
      case _ =>
        InternalServerError(
          errorTemplate("Internal Server Error", "Internal Server Error", "Something went wrong")
        )
    }
  }

  implicit def utrToLeft(u: Utr): Either[Utr, CBCId] = Left[Utr, CBCId](u)
  implicit def cbcToRight(c: CBCId): Either[Utr, CBCId] = Right[Utr, CBCId](c)

  def sha256Hash(file: File): String = {
    val stream = new FileInputStream(file)
    try
      String.format(
        "%064x",
        new java.math.BigInteger(
          1,
          java.security.MessageDigest
            .getInstance("SHA-256")
            .digest(
              org.apache.commons.io.IOUtils.toByteArray(stream)
            )
        )
      )
    finally
      stream.close()
  }

  def generateMetadataFile(cache: CBCSessionCache, creds: Credentials)(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[ValidatedNel[ExpiredSession, SubmissionMetaData]] =
    (
      cache.read[BusinessPartnerRecord].toValidatedNel: FutureCacheResult[BusinessPartnerRecord],
      cache.read[TIN].toValidatedNel: FutureCacheResult[TIN],
      cache.read[Hash].toValidatedNel: FutureCacheResult[Hash],
      cache.read[CBCId].toValidatedNel: FutureCacheResult[CBCId],
      cache.read[FileId].toValidatedNel: FutureCacheResult[FileId],
      cache.read[EnvelopeId].toValidatedNel: FutureCacheResult[EnvelopeId],
      cache.read[SubmitterInfo].toValidatedNel: FutureCacheResult[SubmitterInfo],
      cache.read[FilingType].toValidatedNel: FutureCacheResult[FilingType],
      cache.read[UltimateParentEntity].toValidatedNel: FutureCacheResult[UltimateParentEntity],
      cache.read[FileMetadata].toValidatedNel: FutureCacheResult[FileMetadata]
    ).mapN { (record, tin, hash, id, fileId, envelopeId, info, filingType, upe, metadata) =>
      SubmissionMetaData(
        SubmissionInfo(
          gwCredId = creds.toString,
          cbcId = id,
          bpSafeId = record.safeId,
          hash = hash,
          ofdsRegime = "cbc",
          tin = tin,
          filingType = filingType,
          ultimateParentEntity = upe
        ),
        info,
        FileInfo(
          fileId,
          envelopeId,
          metadata.status,
          metadata.name,
          metadata.contentType,
          metadata.length,
          metadata.created
        )
      )
    }
}
