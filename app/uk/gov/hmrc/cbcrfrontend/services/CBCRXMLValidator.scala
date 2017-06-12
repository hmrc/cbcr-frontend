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

import java.io.File
import javax.xml.transform.stream.StreamSource
import javax.xml.validation.SchemaFactory

import cats.data.Validated
import org.xml.sax.{ErrorHandler, SAXParseException}

import scala.collection.mutable.ListBuffer


class CBCRXMLValidator {

  val schemaLang = "http://www.w3.org/2001/XMLSchema"

  def validateSchema(in: File): Validated[XmlErrorHandler, File] = {

    val validatorFactory = SchemaFactory.newInstance(schemaLang)
    val validator = validatorFactory.newSchema(new StreamSource(new File("conf/schema/CbcXML_v1.0.xsd"))).newValidator()
    val xmlErrorHandler = new XmlErrorHandler
    validator.setErrorHandler(xmlErrorHandler)

    try {
      validator.validate(new StreamSource(in))

      if(xmlErrorHandler.hasErrors) Validated.Invalid(xmlErrorHandler)
      else Validated.Valid(in)

    } catch {
      case _:SAXParseException => Validated.Invalid(xmlErrorHandler)
    }

  }

}

class XmlErrorHandler extends ErrorHandler {

  def hasErrors: Boolean = errorsCollection.nonEmpty
  def hasWarnings: Boolean = warningsCollection.nonEmpty

  private var errorsListBuffer: ListBuffer[String] = new ListBuffer[String]()
  private var warningsListBuffer: ListBuffer[String] = new ListBuffer[String]()

  def errorsCollection: List[String] = errorsListBuffer.toList
  def warningsCollection: List[String] = warningsListBuffer.toList


  private def addNewError(exception: SAXParseException) = {
    errorsListBuffer += s"Error at line number: ${exception.getLineNumber}, ${exception.getMessage}"
  }

  override def fatalError(exception: SAXParseException): Unit = addNewError(exception)

  override def error(exception: SAXParseException): Unit = addNewError(exception)

  override def warning(exception: SAXParseException): Unit =
    warningsListBuffer += s"Warning at line number: ${exception.getLineNumber}, ${exception.getMessage}"

}

object CBCRXMLValidator extends CBCRXMLValidator

