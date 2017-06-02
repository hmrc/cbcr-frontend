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
import javax.inject.{Inject, Singleton}

import cats.data.{Validated, ValidatedNel}
import cats.syntax.all._
import cats.instances.all._
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import play.api.Logger
import uk.gov.hmrc.cbcrfrontend.model._
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.xml.pull._
import uk.gov.hmrc.cbcrfrontend._


@Singleton
class CBCBusinessRuleValidator @Inject() (messageRefService:MessageRefIdService)(implicit ec:ExecutionContext) {

  private val dateFmt = DateTimeFormat.forPattern("YYYYMMDD'T'hhmmss")
  private val dateRegex = """\d{8}T\d{6}"""

  // strip the ^ and $ characters from the cbcRegex
  private val cbcRegex: String = CBCId.cbcRegex.init.tail
  private val messageRefIDRegex = ("""GB(\d{4})\w{2}(""" + cbcRegex + """)CBC40[1,2](""" + dateRegex + """)\w{1,56}""").r


  def validateBusinessRules(in:File, cBCId: CBCId)(implicit hc:HeaderCarrier) : Future[ValidatedNel[BusinessRuleErrors,KeyXMLFileInfo]] = {
    val stream = new XMLEventReader(scala.io.Source.fromFile(in)).toStream
    messageRefIDCheck(stream).map{ messageRefIdVal  =>
      val otherRules = (
          validateTestDataPresent(stream).toValidatedNel |@|
          validateReceivingCountry(stream).toValidatedNel |@|
          validateSendingEntity(stream,cBCId).toValidatedNel |@|
          validateFileName(in,stream).toValidatedNel
        ).map((_,_,_,_) => ())

      otherRules *> messageRefIdVal

    }
  }

  @tailrec
  private def findElementText(tagName:String,text:Option[String],input:Stream[XMLEvent]) : Option[String] = {
    input match {
      case Stream.Empty                                                                => None
      case EvElemStart(_, value, _, _) #:: EvText(refId) #:: _
        if value.equalsIgnoreCase(tagName) &&
          text.forall(t => refId.toLowerCase.matches("(?i)"+t.toLowerCase))            => Some(refId)
      case _ #:: t                                                                     => findElementText(tagName,text,t)
    }
  }

  private def validateFileName(file:File,in:Stream[XMLEvent]) : Validated[BusinessRuleErrors,Unit] = {
    val withoutExtension = file.getName.split("""\.""").headOption
    findElementText("MessageRefId", withoutExtension, in).fold[Validated[BusinessRuleErrors, Unit]](FileNameError.invalid)(_ => ().valid)
  }

  private def validateSendingEntity(in:Stream[XMLEvent],cbcId:CBCId) : Validated[BusinessRuleErrors,Unit] =
    findElementText("SendingEntityIN",None,in).fold[Validated[SendingEntityError.type,Unit]](SendingEntityError.invalid){s =>
      if(s.equalsIgnoreCase(cbcId.value)){ ().valid } else { SendingEntityError.invalid }
    }

  private def validateReceivingCountry(in:Stream[XMLEvent]) : Validated[BusinessRuleErrors,Unit] =
    findElementText("ReceivingCountry",None,in).fold[Validated[ReceivingCountryError.type,Unit]](ReceivingCountryError.invalid){r =>
      if(r.equalsIgnoreCase("GB")){ ().valid } else { ReceivingCountryError.invalid }
    }

  private def validateTestDataPresent(in:Stream[XMLEvent]) : Validated[BusinessRuleErrors,Unit] =
    if(findElementText("DocTypeIndic",Some("OECD1[123]"),in).isDefined) TestDataError.invalid
    else ().valid

  // MessageRefIDChecks
  private def validateCBCId(in:Stream[XMLEvent], cbcId:String) : Validated[MessageRefIDError,Unit] =
    if(findElementText("SendingEntityIN",None,in).contains(cbcId)) { ().valid}
    else MessageRefIDCBCIdMismatch.invalid

  private def validateReportingPeriod(in:Stream[XMLEvent], year:String) : Validated[MessageRefIDError,String] = {
    val rp = findElementText("ReportingPeriod",None,in)
    rp.fold[Validated[MessageRefIDError,String]](MessageRefIDReportingPeriodMismatch.invalid){date =>
      if(date.startsWith(year)) { date.valid }
      else { MessageRefIDReportingPeriodMismatch.invalid }
    }
  }

  private def validateDateStamp(dateTime:String) : Validated[MessageRefIDError,Unit] =
    Validated.catchNonFatal(DateTime.parse(dateTime,dateFmt)).bimap(
      _ => MessageRefIDTimestampError,
      _ => ()
    )

  private def isADuplicate(msgRefId:String)(implicit hc:HeaderCarrier) : Future[Validated[MessageRefIDError,Unit]] =
    messageRefService.messageRefIdExists(msgRefId).map(result =>
      if(result) MessageRefIDDuplicate.invalid else ().valid
    )

  private def messageRefIDCheck(in:Stream[XMLEvent])(implicit hc:HeaderCarrier) : Future[ValidatedNel[MessageRefIDError, KeyXMLFileInfo]] = {
    findElementText("MessageRefId",None,in) .fold[Future[ValidatedNel[MessageRefIDError, KeyXMLFileInfo]]](
      MessageRefIDMissing.invalidNel[KeyXMLFileInfo].pure[Future])(
      {
        case invalidMsgRefId if invalidMsgRefId.equalsIgnoreCase("null") || invalidMsgRefId.isEmpty =>
          MessageRefIDMissing.invalidNel[KeyXMLFileInfo].pure[Future]
        case msgRefId@messageRefIDRegex(reportingPeriod, cbcId, date) =>
          isADuplicate(msgRefId).map(dup =>
            (dup.toValidatedNel |@|
              validateCBCId(in, cbcId).toValidatedNel |@|
              validateReportingPeriod(in, reportingPeriod).toValidatedNel |@|
              validateDateStamp(date).toValidatedNel |@|
              findElementText("Timestamp",None,in).toValidNel(MessageRefIDTimestampError)
              ).map((_, _, rp, _, ts) => KeyXMLFileInfo(msgRefId,rp,ts))
          )
        case _ => MessageRefIDFormatError.invalidNel[KeyXMLFileInfo].pure[Future]
      }

    )

  }

}
