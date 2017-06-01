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

package uk.gov.hmrc.cbcrfrontend.services

import org.scalatest.{FlatSpec, Matchers}

import scala.xml.SAXParseException


class XmlErrorHandlerSpec  extends FlatSpec with Matchers {

  def addException(errorMessage: String, line: Int, column: Int) = new SAXParseException(errorMessage, "PublicId", "SystemId", line, column)

  val errors: List[SAXParseException] = List(addException("ErrorOne", 5, 2), addException("ErrorTwo", 13, 14))
  val warnings: List[SAXParseException] = List(addException("WarningOne", 5, 2), addException("WarningTwo", 13, 14))


  "An XmlErrorHandler" should "report multiple errors and multiple warnings" in {
    val xmlErorHandler =  new XmlErrorHandler

    errors.foreach(spe => xmlErorHandler.error(spe))
    warnings.foreach(spe => xmlErorHandler.warning(spe))

    xmlErorHandler.hasErrors shouldBe true
    xmlErorHandler.errorsCollection.size shouldBe errors.size

    val errorsMap = errors.map{spe => (s"${spe.getLineNumber.toString}:${spe.getColumnNumber}" , spe)}.toMap

    for(i <- errors.indices) {
      val message = s"Error at line number: ${errors(i).getLineNumber}, ${errors(i).getMessage}"
      assert(message == xmlErorHandler.errorsCollection(i))
    }

    xmlErorHandler.hasWarnings shouldBe true
    xmlErorHandler.warningsCollection.size shouldBe warnings.size

    for(i <- warnings.indices) {
      val message = s"Warning at line number: ${warnings(i).getLineNumber}, ${warnings(i).getMessage}"
      assert(message == xmlErorHandler.warningsCollection(i))
    }

  }

}
