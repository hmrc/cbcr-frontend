package uk.gov.hmrc.cbcrfrontend.model

import play.api.libs.json.Json

/**
  * Created by max on 15/05/17.
  */

sealed trait UserType
case object Company extends UserType
case object Agent extends UserType

case class AffinityGroup(affinityGroup: String)
object AffinityGroup{
  implicit val format = Json.format[AffinityGroup]
}
