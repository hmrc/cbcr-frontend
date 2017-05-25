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

import org.scalatest.{FlatSpec, Matchers}
import uk.gov.hmrc.cbcrfrontend.model.KeyXMLFileInfo


class CbcrFrontendPackageSpec extends FlatSpec with Matchers {

  it should "return KeyXMLInfo from the input XML file"  in {
    val keyXMLInfo = cbcrfrontend.getKeyXMLFileInfo(new File(s"test/resources/cbcr-valid.xml"))
    keyXMLInfo should be (KeyXMLFileInfo("String_MessageRefId", "2016-03-31", "2016-11-01T15:00:00"))
  }
}
