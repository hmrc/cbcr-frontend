package uk.gov.hmrc.cbcrfrontend.xmlvalidator

import org.scalatest.{FlatSpec, Matchers}

import scala.xml.SAXParseException


class XmlErrorHandlerSpec  extends FlatSpec with Matchers {

  def addException(errorMessage: String, line: Int, column: Int) = new SAXParseException(errorMessage, "PublicId", "SystemId", line, column)

  val errors: List[SAXParseException] = List(addException("ErrorOne", 5, 2), addException("ErrorTwo", 13, 14))
  val warnings: List[SAXParseException] = List(addException("WarningOne", 5, 2), addException("WarningTwo", 13, 14))


  "An XmlErrorHandler" should "report multiple errors and multiple warnings" in {
    val xmlErorHandler =  XmlErorHandler

    errors.foreach(spe => xmlErorHandler.error(spe))
    warnings.foreach(spe => xmlErorHandler.warning(spe))

    xmlErorHandler.hasErrors shouldBe true
    xmlErorHandler.errorsCollection.size shouldBe errors.size

    val errorsMap = errors.map{spe => (s"${spe.getLineNumber.toString}:${spe.getColumnNumber}" , spe)}.toMap

    for(i <- 0 until errors.size) {
      val message = s"Error at position ${errors(i).getLineNumber}:${errors(i).getColumnNumber} ${errors(i).getMessage}"
      assert(message == xmlErorHandler.errorsCollection(i))
    }

    xmlErorHandler.hasWarnings shouldBe true
    xmlErorHandler.warningsCollection.size shouldBe warnings.size

    for(i <- 0 until warnings.size) {
      val message = s"Warning at position ${warnings(i).getLineNumber}:${warnings(i).getColumnNumber} ${warnings(i).getMessage}"
      assert(message == xmlErorHandler.warningsCollection(i))
    }

  }

}
