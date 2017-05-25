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

import java.io.File
import java.nio.file.Files

import scala.reflect.runtime.universe._
import cats.data.{EitherT, OptionT, ValidatedNel}
import cats.instances.future._
import cats.syntax.option._
import cats.syntax.cartesian._
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.exceptions.UnexpectedState
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.services.CBCSessionCache
import uk.gov.hmrc.play.frontend.auth.AuthContext
import uk.gov.hmrc.play.frontend.auth.connectors.AuthConnector
import uk.gov.hmrc.play.http.HeaderCarrier
import scala.concurrent.{ExecutionContext, Future}

package object cbcrfrontend {

  def affinityGroupToUserType(a: AffinityGroup): Either[UnexpectedState, UserType] = a.affinityGroup.toLowerCase.trim match {
    case "agent" => Right(Agent)
    case "organisation" => Right(Organisation)
    case other => Left(UnexpectedState(s"Unknown affinity group: $other"))
  }

  def getUserType(ac: AuthContext)(implicit cache: CBCSessionCache, sec: AuthConnector, hc: HeaderCarrier, ec: ExecutionContext): ServiceResponse[UserType] =
    EitherT(OptionT(cache.read[AffinityGroup])
      .getOrElseF {
        sec.getUserDetails[AffinityGroup](ac)
          .flatMap(ag => cache.save[AffinityGroup](ag)
            .map(_ => ag))
      }
      .map(affinityGroupToUserType)
    )

  def sha256Hash(file: File): String =
    String.format("%064x", new java.math.BigInteger(1, java.security.MessageDigest.getInstance("SHA-256").digest(Files.readAllBytes(file.toPath))))


  def generateMetadataFile(gatewayId: String, cache: CBCSessionCache)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[ValidatedNel[String, SubmissionMetaData]] = {

    def errors[T: TypeTag](v: Option[T]): ValidatedNel[String, T] =
      v.toValidNel(s"Could not find data for ${typeOf[T].toString} in cache")

    for {
      bpr <- cache.read[BusinessPartnerRecord]
      utr <- cache.read[Utr]
      hash <- cache.read[Hash]
      cbcId <- cache.read[CBCId]
      fileId <- cache.read[FileId]
      envelopeId <- cache.read[EnvelopeId]
      submitterInfo <- cache.read[SubmitterInfo]
      filingType <- cache.read[FilingType]
      upe <- cache.read[UltimateParentEntity]
      filingCapacity <- cache.read[FilingCapacity]
      fileMetadata <- cache.read[FileMetadata]
    } yield {
      (errors(bpr) |@| errors(utr) |@| errors(hash) |@| errors(cbcId) |@| errors(fileId) |@|
        errors(envelopeId) |@| errors(submitterInfo) |@| errors(filingType) |@|
        errors(upe) |@| errors(filingCapacity) |@| errors(fileMetadata)
        ).map { (record, utr, hash, id, fileId, envelopeId, info, filingType, upe, capacity, metadata) =>

        SubmissionMetaData(
          SubmissionInfo(
            gwCredId = gatewayId,
            cbcId = id,
            bpSafeId = record.safeId,
            hash = hash,
            ofdsRegime = "cbc",
            utr = utr,
            filingType = filingType,
            ultimateParentEntity = upe,
            filingCapacity = capacity
          ),
          info,
          FileInfo(fileId, envelopeId, metadata.status, metadata.name, metadata.contentType, metadata.length, metadata.created)
        )
      }
    }
  }

  import scala.xml.XML

  def getKeyXMLFileInfo(file: File): KeyXMLFileInfo = {

      val xmlFile = XML.loadFile(file)
      KeyXMLFileInfo(
        (xmlFile \ "MessageSpec" \ "MessageRefId").text,
        (xmlFile \ "MessageSpec" \ "ReportingPeriod").text,
        (xmlFile \ "MessageSpec" \ "Timestamp").text)
    }
}