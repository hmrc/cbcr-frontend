/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.cbcrfrontend.util

import cats.implicits.toShowOps
import play.api.i18n.Messages
import play.api.libs.Files.SingletonTemporaryFileCreator
import uk.gov.hmrc.cbcrfrontend.model.ValidationErrors

import java.io.{File, PrintWriter}

object ErrorUtil {

  def errorsToList(e: List[ValidationErrors])(implicit messages: Messages): List[String] =
    e.map(x => x.show.split(" ").map(x => messages(x)).map(_.toString).mkString(" "))

  def errorsToMap(e: List[ValidationErrors])(implicit messages: Messages): Map[String, String] =
    errorsToList(e).foldLeft(Map[String, String]()) { (m, t) =>
      m + ("error_" + (m.size + 1).toString -> t)
    }

  def errorsToString(e: List[ValidationErrors])(implicit messages: Messages): String =
    errorsToList(e).map(_.toString).mkString("\r\n")

  def errorsToFile(e: List[ValidationErrors], name: String)(implicit messages: Messages): File = {
    val b = SingletonTemporaryFileCreator.create(name, ".txt")
    val writer = new PrintWriter(b.path.toFile)
    writer.write(errorsToString(e))
    writer.flush()
    writer.close()
    b.path.toFile
  }

}
