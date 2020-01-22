import play.core.PlayVersion
import play.sbt.PlayImport._
import sbt._

object FrontendBuild extends Build with MicroService {

  val appName = "cbcr-frontend"

  override lazy val appDependencies: Seq[ModuleID] = compile ++ test()

  val compile = Seq(
    ws,
    "uk.gov.hmrc"              %% "bootstrap-play-26"   %           "1.3.0",
    "uk.gov.hmrc"              %% "govuk-template"      %           "5.48.0-play-26",
    "uk.gov.hmrc"              %% "play-ui"             %           "8.7.0-play-26",
    "uk.gov.hmrc"              %% "emailaddress"        %           "3.4.0",
    "uk.gov.hmrc"              %% "domain"              %           "5.6.0-play-26",
    "uk.gov.hmrc"              %% "http-caching-client" %           "9.0.0-play-26",
    "org.typelevel"            %% "cats"                %           "0.9.0",
    "com.github.kxbmap"        %% "configs"             %           "0.4.4",
    "com.scalawilliam"         %% "xs4s"                %           "0.5",
    "org.codehaus.woodstox"    % "woodstox-core-asl"    %           "4.4.1",
    "msv"                      % "msv"                  %           "20050913",
    "com.sun.xml"              % "relaxngDatatype"      %           "1.0",
    "com.sun.msv.datatype.xsd" % "xsdlib"               %           "2013.2",
    "commons-io"               % "commons-io"           %           "2.5",
    "org.mindrot"              % "jbcrypt"              %           "0.4"

  )

  def test(scope: String = "test") = Seq(
    "org.pegdown" % "pegdown" % "1.6.0" % scope,
    "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.2" % scope,
    "org.mockito" % "mockito-core" % "2.28.2" % scope
  )

}
