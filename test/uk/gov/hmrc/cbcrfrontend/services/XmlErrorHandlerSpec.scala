/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.cbcrfrontend.services

import javax.xml.stream.Location

import org.apache.xerces.stax.ImmutableLocation
import org.codehaus.stax2.validation.{XMLValidationException, XMLValidationProblem}
import org.scalatest.{FlatSpec, Matchers}

import scala.xml.SAXParseException

class XmlErrorHandlerSpec extends FlatSpec with Matchers {

  def addException(errorMessage: String, line: Int, column: Int, severity: Int) =
    new XMLValidationProblem(new ImmutableLocation(0, column, line, "", ""), errorMessage, severity)

  val warnings: List[XMLValidationProblem] =
    List(addException("WarningOne", 5, 2, 1), addException("WarningTwo", 13, 14, 1))
  val errors: List[XMLValidationProblem] = List(addException("ErrorOne", 5, 2, 2), addException("ErrorTwo", 13, 14, 2))
  val fatalErrors: List[XMLValidationProblem] = List(addException("FatalErrorOne", 5, 2, 3))

  "An XmlErrorHandler" should "report multiple errors and multiple warnings" in {
    val xmlErorHandler = new XmlErrorHandler

    warnings.foreach(spe => xmlErorHandler.reportProblem(spe))
    errors.foreach(spe => xmlErorHandler.reportProblem(spe))
    fatalErrors.foreach(spe => xmlErorHandler.reportProblem(spe))

    xmlErorHandler.hasErrors shouldBe true
    xmlErorHandler.errorsCollection.size shouldBe errors.size

    for (i <- errors.indices) {
      val message = s"${errors(i).getMessage} on line ${errors(i).getLocation.getLineNumber}"
      assert(message == xmlErorHandler.errorsCollection(i))
    }

    xmlErorHandler.hasWarnings shouldBe true
    xmlErorHandler.warningsCollection.size shouldBe warnings.size

    for (i <- warnings.indices) {
      val message = s"${warnings(i).getMessage} on line ${warnings(i).getLocation.getLineNumber}"
      assert(message == xmlErorHandler.warningsCollection(i))
    }

    xmlErorHandler.hasFatalErrors shouldBe true
    xmlErorHandler.fatalErrorsCollection.size shouldBe fatalErrors.size

    for (i <- fatalErrors.indices) {
      val message = s"${fatalErrors(i).getMessage} on line ${fatalErrors(i).getLocation.getLineNumber}"
      assert(message == xmlErorHandler.fatalErrorsCollection(i))
    }
  }

}
