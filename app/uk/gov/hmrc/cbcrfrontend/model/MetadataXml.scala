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

package uk.gov.hmrc.cbcrfrontend.model

import org.joda.time.DateTime
import uk.gov.hmrc.cbcrfrontend.typesclasses.Attribute

import scala.xml.Elem

object MetadataXml {

  val xmlDec = """<?xml version="1.0" encoding="UTF-8" standalone="no"?>"""

  private def createMetadata(mne: String, revenue: Long): Elem = {

    val attributes = List(
      createAttribute("MNE", mne),
      createAttribute("Revenue", revenue)
    )
    <metadata></metadata>.copy(child = attributes)
  }

  private def createHeader(mne: String, reconciliationId: String): Elem = {
    <header>
      <title>{ mne }</title>
      <format>xml</format>
      <mime_type>application/xml</mime_type>
      <store>true</store>
      <source>dfs</source>
      <target>DMS</target>
      <reconciliation_id>{ reconciliationId }</reconciliation_id>
    </header>
  }

  private def createDocument(elems: List[Elem]): Elem = {
    <documents xmlns="http://govtalk.gov.uk/hmrc/gis/content/1">
      { <document></document>.copy(child = elems) }
    </documents>
  }

  def getXml(mne: String, revenue: Long, reconciliationId: String): Elem = {
    val body = List(
      createHeader(mne, reconciliationId),
      createMetadata(mne, revenue)
    )

    createDocument(body)
  }

  private def createAttribute[T: Attribute](name: String, value: T): Elem =
    createAttribute(name, List(value))

  private def createAttribute[T: Attribute](name: String, values: List[T]): Elem = {
    implicitly[Attribute[T]].attribute(name, values)
  }
}
