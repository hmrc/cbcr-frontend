/*
 * Copyright 2018 HM Revenue & Customs
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

import javax.inject.{Inject, Singleton}

import play.api.libs.json._
import uk.gov.hmrc.cbcrfrontend.auth.SecuredActions
import uk.gov.hmrc.cbcrfrontend.connectors.test.TestCBCRConnector
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.frontend.controller.FrontendController

@Singleton
class TestCBCRController @Inject()(val sec: SecuredActions) extends FrontendController with ServicesConfig {

  def insertSubscriptionData(cbcId: String, utr: String) = sec.AsyncAuthenticatedAction() { _ =>
    implicit request =>
        TestCBCRConnector.insertSubscriptionData(defaultSubscriptionData(cbcId, utr)).map(_ => Ok("Data inserted"))
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
         |   "cbcId":"${cbcId}",
         |   "utr":"${utr}"
         |}
       """.stripMargin
    )

  def deleteSubscription(utr: String) = sec.AsyncAuthenticatedAction() { _ =>
    implicit request =>
      TestCBCRConnector.deleteSubscription(utr).map(_ => Ok("Record with the specific UTR deleted"))
  }

  def deleteSingleDocRefId(docRefId: String) = sec.AsyncAuthenticatedAction() { _ =>
    implicit request =>
      TestCBCRConnector.deleteSingleDocRefId(docRefId).map(_ => Ok("DocRefId has been deleted"))
  }
}
