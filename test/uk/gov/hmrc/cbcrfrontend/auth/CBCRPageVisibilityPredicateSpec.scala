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

package uk.gov.hmrc.cbcrfrontend.auth

import org.mockito.Matchers.{any, eq => EQ}
import org.mockito.Mockito.when
import org.scalatest.concurrent.ScalaFutures
import org.scalatestplus.play.OneAppPerSuite
import uk.gov.hmrc.cbcrfrontend.controllers._
import uk.gov.hmrc.cbcrfrontend.model.AffinityGroup
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class CBCRPageVisibilityPredicateSpec  extends UnitSpec with ScalaFutures with OneAppPerSuite with CSRFTest with FakeAuthConnector with TestPredicate {



  "A CBCRPageVisibilityPredicate " should {
    "should return true if the Organisation is an admin" in {

      when(cache.readOption[AffinityGroup](EQ(AffinityGroup.format),any(),any())) thenReturn Future.successful(Some(AffinityGroup("Organisation", Some("admin"))))

      val result = await(new CBCRPageVisibilityPredicate()(authConnector, cache, ec).apply(authContext, request))
      result.isVisible shouldBe true
    }

    "should return false if the Organisation is not an admin" in {

      when(cache.readOption[AffinityGroup](EQ(AffinityGroup.format),any(),any())) thenReturn Future.successful(Some(AffinityGroup("Organisation", None)))

      val result = await(new CBCRPageVisibilityPredicate()(authConnector, cache, ec).apply(authContext, request))
      result.isVisible shouldBe false
    }
  }




}

