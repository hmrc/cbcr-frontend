/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.cbcrfrontend.controllers.test

import cats.data.OptionT
import javax.inject.{Inject, Singleton}
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.libs.json._
import play.api.mvc.Action
import play.api.{Configuration, Environment, Logger}
import uk.gov.hmrc.auth.core.{AuthConnector, AuthorisedFunctions}
import uk.gov.hmrc.cbcrfrontend.config.FrontendAppConfig
import uk.gov.hmrc.cbcrfrontend.connectors.test.TestCBCRConnector
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.cbcrfrontend.services.{CBCSessionCache, FileUploadService}
import uk.gov.hmrc.http.{HttpException, NotFoundException}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import cats.instances.all._
import play.api.libs.json.Json

import scala.concurrent.{ExecutionContext, Future}


@Singleton
class TestCBCRController @Inject()(val authConnector:AuthConnector,
                                   val testCBCRConnector: TestCBCRConnector,
                                   val env:Environment,
                                   val fileUploadService:FileUploadService,
                                   val messagesApi:MessagesApi)
                                  (implicit ec: ExecutionContext, cache:CBCSessionCache, feConfig:FrontendAppConfig, val config:Configuration)extends FrontendController with AuthorisedFunctions with I18nSupport{

  def insertSubscriptionData(cbcId: String, utr: String) = Action.async{ implicit request =>
    authorised() {
      testCBCRConnector.insertSubscriptionData(defaultSubscriptionData(cbcId, utr)).map(_ => Ok("Data inserted"))
    }
  }

  def defaultSubscriptionData(cbcId: String, utr: String): JsValue =
    Json.parse(

      s"""
         |{
         |   "businessPartnerRecord":{
         |      "safeId":"XP0000100099577",
         |      "organisation":{
         |         "organisationName":"S A 1"
         |      },
         |      "address":{
         |         "addressLine1":"Delivery DAy",
         |         "addressLine2":"Making changes",
         |         "addressLine3":"Do Not Deliver Town",
         |         "addressLine4":"Do Not Deliver County",
         |         "postalCode":"TF3 4ER",
         |         "countryCode":"GB"
         |      }
         |   },
         |   "subscriberContact":{
         |      "firstName":"first name first",
         |      "lastName":"last name last",
         |      "phoneNumber":"1234577",
         |      "email":"vbla@email.com"
         |   },
         |   "cbcId":"$cbcId",
         |   "utr":"$utr"
         |}
       """.stripMargin
    )

  def deleteSubscription(utr: String) = Action.async{ implicit request =>
    authorised() {
      testCBCRConnector.deleteSubscription(utr).map(_ => Ok("Record with the specific UTR deleted"))
    }
  }

  def deleteSingleDocRefId(docRefId: String) = Action.async{ implicit request =>
    authorised() {
      testCBCRConnector.deleteSingleDocRefId(docRefId).map(_ => Ok("DocRefId has been deleted"))
    }
  }

  def deleteReportingEntityData(docRefId:String) = Action.async{implicit request =>
    authorised() {
      testCBCRConnector.deleteReportingEntityData(docRefId).map(_ => Ok("Reporting entity data deleted")).recover{
        case _:NotFoundException => Ok("Reporting entity data deleted")
      }
    }
  }

  def dropReportingEntityDataCollection() = Action.async{implicit request =>
    authorised() {
      testCBCRConnector.dropReportingEntityDataCollection.map(_ => Ok("Reporting entity data collection dropped")).recover{
        case _:NotFoundException => Ok("Reporting entity data collection dropped")
      }
    }
  }

  def deleteSingleMessageRefId(messageRefId: String) = Action.async{ implicit request =>
    authorised() {
      testCBCRConnector.deleteSingleMessageRefId(messageRefId).map(_ => Ok("MessageRefId has been deleted"))
    }
  }

  def updateReportingEntityCreationDate(docRefId:String, createDate: String) = Action.async{implicit request =>
    authorised() {
      testCBCRConnector.updateReportingEntityCreationDate(docRefId, createDate).map{s =>
        s.status match {
          case OK           => Ok("Reporting entity createDate updated")
          case NOT_MODIFIED => Ok("Reporting entity createDate NOT updated")
          case _            => Ok("Something went wrong")
        }
      }.recover{
        case _:NotFoundException => Ok("Reporting entity not found")
      }
    }
  }

  def deleteReportingEntityCreationDate(docRefId:String) = Action.async{implicit request =>
    authorised() {
      testCBCRConnector.deleteReportingEntityCreationDate(docRefId).map{s =>
        s.status match {
          case OK           => Ok("Reporting entity createDate deleted")
          case NOT_MODIFIED => Ok("Reporting entity createDate NOT deleted")
          case _            => Ok("Something went wrong")
        }
      }.recover{
        case _:NotFoundException => Ok("Reporting entity not found")
      }
    }
  }

  def confirmReportingEntityCreationDate(docRefId:String, createDate: String) = Action.async{implicit request =>
    authorised() {
      testCBCRConnector.confirmReportingEntityCreationDate(docRefId, createDate).map{s =>
        s.status match {
          case OK           => Ok("Reporting entity createDate correct")
          case NOT_FOUND    => Ok("Reporting entity createDate NOT correct")
          case _            => Ok("Something went wrong")
        }
      }.recover{
        case _:NotFoundException => Ok("Reporting entity not found")
      }
    }
  }

  def deleteReportingEntityReportingPeriod(docRefId:String) = Action.async{implicit request =>
    authorised() {
      testCBCRConnector.deleteReportingEntityReportingPeriod(docRefId).map{s =>
        s.status match {
          case OK           => Ok("Reporting entity reportingPeriod deleted")
          case NOT_MODIFIED => Ok("Reporting entity reportingPeriod NOT deleted")
          case _            => Ok("Something went wrong")
        }
      }.recover{
        case _:NotFoundException => Ok("Reporting entity not found")
      }
    }
  }

  def retrieveBusinessRuleValidationErrors() = Action.async{ implicit request =>
    authorised() {
      OptionT(cache.readOption[AllBusinessRuleErrors]).map(x => fileUploadService.errorsToString(x.errors)).fold (
        NoContent
      ) { errors: String =>
        Ok(errors)
      }
    }
  }

  def retrieveSchemaValidationErrors() = Action.async{ implicit request =>
    authorised() {
      OptionT(cache.readOption[XMLErrors]).map(x => x.errors.mkString ).fold (
        NoContent
      ) { errors: String =>
        Ok(errors)
      }
    }
  }

  def validateNumberOfCbcIdForUtr(utr: String) = Action.async{implicit request =>
    authorised() {
      testCBCRConnector.checkNumberOfCbcIdForUtr(utr).map{s =>
        s.status match {
          case OK           => Ok(s"The total number of cbc id for given utr is: ${s.json}")
          case NOT_FOUND    => Ok("Subscription data not found for the given utr")
          case _            => Ok("Something went wrong")
        }
      }.recover{
        case _:NotFoundException => Ok("Subscription data not found")
      }
    }
  }

}
