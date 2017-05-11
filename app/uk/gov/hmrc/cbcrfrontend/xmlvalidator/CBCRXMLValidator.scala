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

package uk.gov.hmrc.cbcrfrontend.xmlvalidator

import javax.xml.transform.stream.StreamSource
import javax.xml.validation.SchemaFactory
import java.io.File
import org.xml.sax.SAXParseException

import cats.data.Validated


class CBCRXMLValidator {

  def validate(in: File): Validated[SAXParseException,File] = {

      val schemaLang = "http://www.w3.org/2001/XMLSchema"
      val validator = SchemaFactory.newInstance(schemaLang).
        newSchema(new StreamSource(new File("conf/schema/CbcXML_v1.0.xsd"))).newValidator()

      Validated.catchOnly[SAXParseException](
        validator.validate(new StreamSource(in))).map { _ =>
        in
      }
  }

}

object CBCRXMLValidator extends CBCRXMLValidator
