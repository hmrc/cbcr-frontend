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

package uk.gov.hmrc

import java.io.{File, FileInputStream, InputStream}
import java.nio.file.Files

import cats.data.{EitherT, OptionT, ValidatedNel}
import cats.instances.future._
import cats.syntax.option._
import cats.syntax.cartesian._
import cats.syntax.show._
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.services.CBCSessionCache
import uk.gov.hmrc.play.frontend.auth.AuthContext
import uk.gov.hmrc.play.frontend.auth.connectors.AuthConnector
import uk.gov.hmrc.play.http.HeaderCarrier
import _root_.play.api.mvc._
import _root_.play.api.mvc.Results._
import _root_.play.api.Logger

import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.runtime.universe._


package object cbcrfrontend {

  implicit def resultFuture(r:Result):Future[Result] = Future.successful(r)

  def errorRedirect(error:CBCErrors)(implicit request:Request[_]): Result = {
    Logger.error(error.show)
    InternalServerError(FrontendGlobal.internalServerErrorTemplate)
  }

  def affinityGroupToUserType(a: AffinityGroup): Either[CBCErrors, UserType] = {
    a.affinityGroup.toLowerCase.trim match {
      case "agent"        => Right(Agent)
      case "organisation" => Right(Organisation)
      case "individual"   => Right(Individual)
      case other          => Left(UnexpectedState(s"Unknown affinity group: $other"))
    }
  }

  implicit def utrToLeft(u:Utr): Either[Utr, CBCId] = Left[Utr,CBCId](u)
  implicit def cbcToRight(c:CBCId): Either[Utr, CBCId] = Right[Utr,CBCId](c)

  def getUserType(ac: AuthContext)(implicit cache: CBCSessionCache, sec: AuthConnector, hc: HeaderCarrier, ec: ExecutionContext): ServiceResponse[UserType] =
    EitherT(OptionT(cache.read[AffinityGroup])
      .getOrElseF {
        sec.getUserDetails[AffinityGroup](ac)
          .flatMap(ag => cache.save[AffinityGroup](ag)
            .map(_ => ag))
      }
      .map(affinityGroupToUserType)
    )

  def getUserGGId(ac:AuthContext)(implicit cache:CBCSessionCache, sec:AuthConnector,hc:HeaderCarrier,ec:ExecutionContext) : Future[GGId] =
    OptionT(cache.read[GGId])
      .getOrElseF{
        sec.getUserDetails[GGId](ac)
          .flatMap(ag => cache.save[GGId](ag)
            .map(_ => ag))
      }

  def sha256Hash(file: File): String =
    String.format("%064x", new java.math.BigInteger(1, java.security.MessageDigest.getInstance("SHA-256").digest(
      org.apache.commons.io.IOUtils.toByteArray(new FileInputStream(file))
    )))

  def generateMetadataFile(cache: CBCSessionCache, authContext: AuthContext)(implicit hc: HeaderCarrier, ec: ExecutionContext, sec:AuthConnector): Future[ValidatedNel[String, SubmissionMetaData]] = {

    def errors[T: TypeTag](v: Option[T]): ValidatedNel[String, T] =
      v.toValidNel(s"Could not find data for ${typeOf[T].toString} in cache")

    for {
      gatewayId <- getUserGGId(authContext)(cache,sec,hc,ec)
      bpr <- cache.read[BusinessPartnerRecord]
      utr <- cache.read[Utr]
      hash <- cache.read[Hash]
      cbcId <- cache.read[CBCId]
      fileId <- cache.read[FileId]
      envelopeId <- cache.read[EnvelopeId]
      submitterInfo <- cache.read[SubmitterInfo]
      filingType <- cache.read[FilingType]
      upe <- cache.read[UltimateParentEntity]
      fileMetadata <- cache.read[FileMetadata]
    } yield {
      (errors(bpr) |@| errors(utr) |@| errors(hash) |@| errors(cbcId) |@| errors(fileId) |@|
        errors(envelopeId) |@| errors(submitterInfo) |@| errors(filingType) |@|
        errors(upe) |@| errors(fileMetadata)
        ).map { (record, utr, hash, id, fileId, envelopeId, info, filingType, upe, metadata) =>

        SubmissionMetaData(
          SubmissionInfo(
            gwCredId = gatewayId.authProviderId,
            cbcId = id,
            bpSafeId = record.safeId,
            hash = hash,
            ofdsRegime = "cbc",
            utr = utr,
            filingType = filingType,
            ultimateParentEntity = upe
          ),
          info,
          FileInfo(fileId, envelopeId, metadata.status, metadata.name, metadata.contentType, metadata.length, metadata.created)
        )
      }
    }
  }


}