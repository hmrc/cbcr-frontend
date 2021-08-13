package uk.gov.hmrc.cbcrfrontend.services

import akka.actor.ActorSystem
import base.SpecBase
import cats.data.EitherT
import cats.data.Validated.Valid
import cats.instances.future._
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.when
import play.api.Environment
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import uk.gov.hmrc.cbcrfrontend.connectors.UpscanConnector
import uk.gov.hmrc.cbcrfrontend.core.ServiceResponse
import uk.gov.hmrc.cbcrfrontend.model._

import java.io.File
import java.time.{LocalDate, LocalDateTime}
import scala.concurrent.{ExecutionContext, Future}

class FileValidationServiceSpec extends SpecBase {

  implicit val ec: ExecutionContext = app.injector.instanceOf[ExecutionContext]
  implicit val env = app.injector.instanceOf[Environment]
  implicit val as = app.injector.instanceOf[ActorSystem]

  val mockCBCRXMLValidator: CBCRXMLValidator = mock[CBCRXMLValidator]
  val mockCBCBusinessRuleValidator: CBCBusinessRuleValidator = mock[CBCBusinessRuleValidator]
  val mockXmlInfoExtract: XmlInfoExtract = mock[XmlInfoExtract]
  val mockUpscanConnector: UpscanConnector = mock[UpscanConnector]
  val mockAuditService: AuditService = mock[AuditService]
  val mockFile: File = mock[File]

  override def guiceApplicationBuilder(): GuiceApplicationBuilder = {
    super.guiceApplicationBuilder().overrides(
      bind[CBCBusinessRuleValidator].toInstance(mockCBCBusinessRuleValidator),
      bind[CBCRXMLValidator].toInstance(mockCBCRXMLValidator),
      bind[XmlInfoExtract].toInstance(mockXmlInfoExtract),
      bind[UpscanConnector].toInstance(mockUpscanConnector),
      bind[AuditService].toInstance(mockAuditService)
    )
  }

  val fileValidationService: FileValidationService = app.injector.instanceOf[FileValidationService]

  def right[A](a: Future[A]): ServiceResponse[A] = EitherT.right[Future, CBCErrors, A](a)
  def left[A](s: String): ServiceResponse[A] = EitherT.left[Future, CBCErrors, A](UnexpectedState(s))
  def pure[A](a: A): ServiceResponse[A] = EitherT.pure[Future, CBCErrors, A](a)

  val docRefId = "GB2016RGXVCBC0000000056CBC40120170311T090000X_7000000002OECD1"
  val xmlinfo = XMLInfo(
    MessageSpec(
      MessageRefID("GB2016RGXVCBC0000000056CBC40120170311T090000X").getOrElse(fail("waaaaa")),
      "GB",
      CBCId.create(99).getOrElse(fail("booo")),
      LocalDateTime.now(),
      LocalDate.parse("2017-01-30"),
      None,
      None
    ),
    Some(
      ReportingEntity(
        CBC701,
        DocSpec(OECD1, DocRefId(docRefId + "REP").get, None, None),
        TIN("7000000002", "gb"),
        "name",
        None,
        EntityReportingPeriod(LocalDate.parse("2016-03-31"), LocalDate.parse("2017-03-30"))
      )),
    List(CbcReports(DocSpec(OECD1, DocRefId(docRefId + "ENT").get, None, None))),
    List(AdditionalInfo(DocSpec(OECD1, DocRefId(docRefId + "ADD").get, None, None), "Some Other Info")),
    Some(LocalDate.now()),
    List.empty[String],
    List.empty[String]
  )
  val completeXmlInfo = CompleteXMLInfo(
    xmlinfo,
    ReportingEntity(
      CBC701,
      DocSpec(OECD1, DocRefId(docRefId + "REP").get, None, None),
      TIN("7000000002", "gb"),
      "name",
      None,
      EntityReportingPeriod(LocalDate.parse("2016-03-31"), LocalDate.parse("2017-03-30"))
    )
  )

  "FileValidationService" - {
    "must validate xml schema and business rules and return FileValidationSuccess" in  {

      when(fileValidationService.getFile(any(), any())(any(), any())).thenReturn(right(mockFile))
      when(mockCBCRXMLValidator.validateSchema(any[File]())) thenReturn new XmlErrorHandler()
      when(mockCBCBusinessRuleValidator.validateBusinessRules(any(), any(), any(), any())(any())) thenReturn Future
        .successful(Valid(xmlinfo))
      when(mockCBCBusinessRuleValidator.recoverReportingEntity(any())(any())) thenReturn Future.successful(
        Valid(completeXmlInfo))

    }
  }
}
