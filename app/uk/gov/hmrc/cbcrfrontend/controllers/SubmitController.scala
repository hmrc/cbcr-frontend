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

package uk.gov.hmrc.cbcrfrontend.controllers

import javax.inject.{Inject, Singleton}

import cats.data._
import cats.instances.all._
import cats.syntax.all._
import play.api.Logger
import uk.gov.hmrc.cbcrfrontend.auth.SecuredActions
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.services.{CBCSessionCache, FileUploadService}
import uk.gov.hmrc.cbcrfrontend.typesclasses.{CbcrsUrl, FusFeUrl, FusUrl, ServiceUrl}
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.frontend.controller.FrontendController
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.runtime.universe._
import scala.util.control.NonFatal

@Singleton
class SubmitController @Inject() (val cache:CBCSessionCache, val sec:SecuredActions, val fus:FileUploadService)(implicit val ec:ExecutionContext) extends FrontendController with ServicesConfig{

  implicit lazy val fusUrl   = new ServiceUrl[FusUrl] { val url = baseUrl("file-upload")}
  implicit lazy val fusFeUrl = new ServiceUrl[FusFeUrl] { val url = baseUrl("file-upload-frontend")}
  implicit lazy val cbcrsUrl = new ServiceUrl[CbcrsUrl] { val url = baseUrl("cbcr")}

  def errors[T:TypeTag](v:Option[T]) : ValidatedNel[String,T] =
    v.toValidNel(s"Could not find data for ${typeOf[T].toString} in cache")

  def generateMetadataFile(gatewayId:String)(implicit hc:HeaderCarrier): Future[ValidatedNel[String,SubmissionMetaData]] = {
    for {
      bpr <- cache.read[BusinessPartnerRecord]
      utr <- cache.read[Utr]
      hash <- cache.read[Hash]
      cbcId <- cache.read[CBCId]
      fileId <- cache.read[FileId]
      envelopeId <- cache.read[EnvelopeId]
      submitterInfo <- cache.read[SubmitterInfo]
      filingType <- cache.read[FilingType]
      upe <- cache.read[UPE]
      filingCapacity <- cache.read[FilingCapacity]
      fileMetadata <- cache.read[FileMetadata]
    } yield {
      (errors(bpr) |@| errors(utr) |@| errors(hash) |@| errors(cbcId) |@| errors(fileId) |@|
        errors(envelopeId) |@| errors(submitterInfo) |@| errors(filingType) |@|
        errors(upe) |@| errors(filingCapacity) |@| errors(fileMetadata)
        ).map { (record, utr, hash,id, fileId, envelopeId, info, filingType, upe, capacity, metadata) =>

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

  def confirm = sec.AsyncAuthenticatedAction { authContext => implicit request =>
    generateMetadataFile(authContext.user.userId).flatMap {
      _.fold(
        errors => {
          Logger.error(errors.toList.mkString(", "))
          Future.successful(InternalServerError(errors.toList.mkString(", ")))
        },
        data   => fus.uploadMetadataAndRoute(data).fold(
          errors => {
            Logger.error(errors.errorMsg)
            InternalServerError
          },
          _      => Redirect(routes.FileUpload.submitSuccessReceipt())
        )
      )
    }.recover{
      case NonFatal(e) =>
        Logger.error(e.getMessage,e)
        InternalServerError
    }
  }

}
