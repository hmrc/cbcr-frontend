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

import java.io.{File, InputStream}
import javax.inject.Inject
import javax.xml.stream.XMLInputFactory

import akka.actor.ActorSystem
import com.ctc.wstx.exc.WstxException
import org.codehaus.stax2.{XMLInputFactory2, XMLStreamReader2}
import org.codehaus.stax2.validation._
import play.api.{Environment, Logger}
import uk.gov.hmrc.cbcrfrontend.FrontendAppConfig

import scala.collection.mutable.ListBuffer
import scala.util.control.Exception.nonFatalCatch


class CBCRXMLValidator @Inject()(env:Environment, xmlValidationSchema: XMLValidationSchema)(implicit system:ActorSystem) {


  val xmlInputFactory2: XMLInputFactory2 = XMLInputFactory.newInstance.asInstanceOf[XMLInputFactory2]

  def validateSchema(input: File): XmlErrorHandler = {
    val xmlErrorHandler = new XmlErrorHandler()

    try {
      val xmlStreamReader: XMLStreamReader2  = xmlInputFactory2.createXMLStreamReader(input)
      xmlStreamReader.setValidationProblemHandler(xmlErrorHandler)
      xmlStreamReader.validateAgainst(xmlValidationSchema)
      while ( xmlStreamReader.hasNext ) { xmlStreamReader.next }
    } catch {
      case e:WstxException =>
        xmlErrorHandler.reportProblem(new XMLValidationProblem(e.getLocation,e.getMessage,XMLValidationProblem.SEVERITY_FATAL))
      case ErrorLimitExceededException =>
        Logger.warn(s"Errors exceeding the ${xmlErrorHandler.errorMessageLimit} encountered, validation aborting.")
    }

    xmlErrorHandler

  }
}

class XmlErrorHandler() extends ValidationProblemHandler{

  // How many errors should be handle before giving up?
  val errorMessageLimit = 100

  override def reportProblem(problem: XMLValidationProblem): Unit = captureError(problem)

  private val errorsListBuffer:      ListBuffer[String] = new ListBuffer[String]()
  private val warningsListBuffer:    ListBuffer[String] = new ListBuffer[String]()
  private val fatalErrorsListBuffer: ListBuffer[String] = new ListBuffer[String]()

  def hasErrors:      Boolean = errorsCollection.nonEmpty
  def hasFatalErrors: Boolean = fatalErrorsCollection.nonEmpty
  def hasWarnings:    Boolean = warningsCollection.nonEmpty

  def errorsCollection:      List[String] = errorsListBuffer.toList
  def warningsCollection:    List[String] = warningsListBuffer.toList
  def fatalErrorsCollection: List[String] = fatalErrorsListBuffer.toList

  private def captureError(problem:XMLValidationProblem) = {

    var listBuffer: ListBuffer[String] = problem.getSeverity match {
      case XMLValidationProblem.SEVERITY_WARNING => warningsListBuffer
      case XMLValidationProblem.SEVERITY_ERROR   => errorsListBuffer
      case XMLValidationProblem.SEVERITY_FATAL   => fatalErrorsListBuffer
    }

    if(listBuffer.size < errorMessageLimit) {
      val lineNumber = nonFatalCatch opt problem.getLocation.getLineNumber
      listBuffer += lineNumber.fold(s"${problem.getMessage}")(line => s"${problem.getMessage} on line $line")
    } else {
      fatalErrorsListBuffer += s"Number of errors exceeding limit ($errorMessageLimit), aborting validation.."
      throw ErrorLimitExceededException
    }

  }

}


case object ErrorLimitExceededException extends Throwable

