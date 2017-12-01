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

package uk.gov.hmrc.cbcrfrontend.auth

import akka.stream.Materializer
import org.mockito.Matchers.any
import org.mockito.Mockito.when
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mock.MockitoSugar
import org.scalatestplus.play.OneAppPerSuite
import play.api.mvc.{Action, Controller}
import play.api.test.FakeRequest
import uk.gov.hmrc.cbcrfrontend.controllers.{AsyncUserRequest, CSRFTest, FakeAuthConnector}
import uk.gov.hmrc.cbcrfrontend.model.AffinityGroup
import uk.gov.hmrc.cbcrfrontend.services.{CBCSessionCache, WithConfigFakeApplication}
import uk.gov.hmrc.play.frontend.auth.connectors.AuthConnector
import uk.gov.hmrc.play.frontend.auth.connectors.domain.Accounts
import uk.gov.hmrc.play.frontend.auth.connectors.domain.ConfidenceLevel.L500
import uk.gov.hmrc.play.frontend.auth.connectors.domain.CredentialStrength.Strong
import uk.gov.hmrc.play.frontend.auth._
import uk.gov.hmrc.play.test.UnitSpec
import org.mockito.Matchers.{eq => EQ}

import scala.concurrent.{ExecutionContext, Future}

class CBCRPageVisibilityPredicateSpec  extends UnitSpec with ScalaFutures with OneAppPerSuite with CSRFTest with FakeAuthConnector with TestPredicate {



  "A CBCRPageVisibilityPredicate " should {
    "should return true if the Organisation is an admin" in {

      when(cache.read[AffinityGroup](EQ(AffinityGroup.format),any(),any())) thenReturn Future.successful(Some(AffinityGroup("Organisation", Some("admin"))))

      val result = await(new CBCRPageVisibilityPredicate()(authConnector, cache, ec).apply(authContext, request))
      result.isVisible shouldBe true
    }

    "should return false if the Organisation is not an admin" in {

      when(cache.read[AffinityGroup](EQ(AffinityGroup.format),any(),any())) thenReturn Future.successful(Some(AffinityGroup("Organisation", None)))

      val result = await(new CBCRPageVisibilityPredicate()(authConnector, cache, ec).apply(authContext, request))
      result.isVisible shouldBe false
    }
  }




}

