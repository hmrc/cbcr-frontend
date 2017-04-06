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

package uk.gov.hmrc.cbcrfrontend.model

import play.api.libs.json.Json

/**
  * Created by max on 05/04/17.
  */

case class EtmpAddress(addressLine1: String,
                       addressLine2: String,
                       addressLine3: Option[String],
                       addressLine4: Option[String],
                       postalCode: Option[String],
                       countryCode: String)

object EtmpAddress {
  implicit val formats = Json.format[EtmpAddress]
}



case class Individual(firstName: String, lastName: String, dateOfBirth: Option[String])

object Individual {
  implicit val formats = Json.format[Individual]
}

case class Organisation(organisationName: String, organisationType: String)

object Organisation {
  implicit val formats = Json.format[Organisation]
}

case class OrganisationResponse(organisationName: String, isAGroup: Option[Boolean], organisationType: Option[String])

object OrganisationResponse {
  implicit val formats = Json.format[OrganisationResponse]
}

case class FindBusinessData(acknowledgementReference: String,
                            utr: String,
                            requiresNameMatch: Boolean = false,
                            isAnAgent: Boolean = false,
                            individual: Option[Individual],
                            organisation: Option[Organisation])

object FindBusinessData {
  implicit val formats = Json.format[FindBusinessData]
}

case class FindBusinessDataResponse(isAnIndividual: Boolean,
                                    agentReferenceNumber: Option[String],
                                    sapNumber: Option[String],
                                    safeId: String,
                                    address: EtmpAddress,
                                    organisation: Option[OrganisationResponse] = None,
                                    individual: Option[Individual] = None)

object FindBusinessDataResponse {
  implicit val format = Json.format[FindBusinessDataResponse]
}

