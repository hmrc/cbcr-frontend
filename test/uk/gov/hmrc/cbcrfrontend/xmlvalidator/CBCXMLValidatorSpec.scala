package uk.gov.hmrc.cbcrfrontend.xmlvalidator

import java.io.File

import org.scalatest.{FlatSpec, Matchers}

import scala.util.Try

class CBCXMLValidatorSpec extends FlatSpec with Matchers {


  "An Xml Validator" should "not return any error for a valid file" in {

    Try[File] {
      new File("./")
    }

    CBCRXMLValidator.validate(file)
  }
}
