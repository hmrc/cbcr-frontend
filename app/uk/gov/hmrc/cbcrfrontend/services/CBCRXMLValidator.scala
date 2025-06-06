/*
 * Copyright 2024 HM Revenue & Customs
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

import com.ctc.wstx.exc.WstxException
import org.codehaus.stax2.validation._
import org.codehaus.stax2.{XMLInputFactory2, XMLStreamReader2}
import play.api.Logger

import java.io.File
import javax.inject.Inject
import javax.xml.stream.XMLInputFactory
import scala.collection.mutable.ListBuffer
import scala.util.control.Exception.nonFatalCatch

class CBCRXMLValidator @Inject() (xmlValidationSchema: XMLValidationSchema) {

  lazy val logger: Logger = Logger(this.getClass)

  private val xmlInputFactory2 = XMLInputFactory.newInstance.asInstanceOf[XMLInputFactory2]
  xmlInputFactory2.setProperty(XMLInputFactory.SUPPORT_DTD, false)
  xmlInputFactory2.setProperty("javax.xml.stream.isSupportingExternalEntities", false)

  def validateSchema(input: File): XmlErrorHandler = {
    val xmlErrorHandler = new XmlErrorHandler()

    try {
      val xmlStreamReader: XMLStreamReader2 = xmlInputFactory2.createXMLStreamReader(input)
      try {
        xmlStreamReader.setValidationProblemHandler(xmlErrorHandler)
        xmlStreamReader.validateAgainst(xmlValidationSchema)
        while (xmlStreamReader.hasNext)
          xmlStreamReader.next
      } finally xmlStreamReader.closeCompletely()
    } catch {
      case e: WstxException =>
        xmlErrorHandler.reportProblem(
          new XMLValidationProblem(e.getLocation, e.getMessage, XMLValidationProblem.SEVERITY_FATAL)
        )
      case ErrorLimitExceededException =>
        logger.warn(s"Errors exceeding the ${xmlErrorHandler.errorMessageLimit} encountered, validation aborting.")
    }

    xmlErrorHandler
  }
}

class XmlErrorHandler extends ValidationProblemHandler {

  // How many errors should be handle before giving up?
  val errorMessageLimit = 100

  override def reportProblem(problem: XMLValidationProblem): Unit = captureError(problem)

  private val errorsListBuffer: ListBuffer[String] = new ListBuffer[String]()
  private val warningsListBuffer: ListBuffer[String] = new ListBuffer[String]()
  private val fatalErrorsListBuffer: ListBuffer[String] = new ListBuffer[String]()

  def hasErrors: Boolean = errorsCollection.nonEmpty
  def hasFatalErrors: Boolean = fatalErrorsCollection.nonEmpty
  def hasWarnings: Boolean = warningsCollection.nonEmpty

  def errorsCollection: List[String] = errorsListBuffer.toList
  def warningsCollection: List[String] = warningsListBuffer.toList
  def fatalErrorsCollection: List[String] = fatalErrorsListBuffer.toList

  private def captureError(problem: XMLValidationProblem) = {

    val listBuffer: ListBuffer[String] = problem.getSeverity match {
      case XMLValidationProblem.SEVERITY_WARNING => warningsListBuffer
      case XMLValidationProblem.SEVERITY_ERROR   => errorsListBuffer
      case XMLValidationProblem.SEVERITY_FATAL   => fatalErrorsListBuffer
    }

    if (listBuffer.size < errorMessageLimit) {
      val lineNumber = nonFatalCatch opt problem.getLocation.getLineNumber
      listBuffer += lineNumber.fold(s"${problem.getMessage}")(line => s"${problem.getMessage} on line $line")
    } else {
      fatalErrorsListBuffer += s"Number of errors exceeding limit ($errorMessageLimit), aborting validation.."
      throw ErrorLimitExceededException
    }
  }
}

case object ErrorLimitExceededException extends Throwable
