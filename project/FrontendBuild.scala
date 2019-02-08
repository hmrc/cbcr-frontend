import play.core.PlayVersion
import play.sbt.PlayImport._
import sbt._

object FrontendBuild extends Build with MicroService {

  val appName = "cbcr-frontend"

  override lazy val appDependencies: Seq[ModuleID] = compile ++ test()

  val compile = Seq(
    ws,
    "uk.gov.hmrc"              %% "bootstrap-play-25"   %           "4.8.0",
    "uk.gov.hmrc"              %% "govuk-template"      %           "5.28.0-play-25",
    "uk.gov.hmrc"              %% "play-ui"             %           "7.31.0-play-25",
    "uk.gov.hmrc"              %% "emailaddress"        %           "3.2.0",
    "uk.gov.hmrc"              %% "domain"              %           "5.3.0",
    "uk.gov.hmrc"              %% "http-caching-client" %           "8.0.0",
    "org.typelevel"            %% "cats"                %           "0.9.0",
    "org.typelevel"            %% "cats-core"           %           "0.9.0",
    "com.github.kxbmap"        %% "configs"             %           "0.4.4",
    "com.scalawilliam"         %% "xs4s"                %           "0.3",
    "org.codehaus.woodstox"    % "woodstox-core-asl"    %           "4.4.1",
    "msv"                      % "msv"                  %           "20050913",
    "com.sun.xml"              % "relaxngDatatype"      %           "1.0",
    "com.sun.msv.datatype.xsd" % "xsdlib"               %           "2013.2",
    "commons-io"               % "commons-io"           %           "2.5",
    "uk.gov.hmrc"              %% "government-gateway-domain"  %    "1.28.0"
  )

  def test(scope: String = "test") = Seq(
    "uk.gov.hmrc" %% "hmrctest" % "3.4.0-play-25" % scope,
//    "org.scalatest" %% "scalatest" % "2.2.6" % scope,
    "org.pegdown" % "pegdown" % "1.6.0" % scope,
    "org.jsoup" % "jsoup" % "1.8.1" % scope,
    "com.typesafe.play" %% "play-test" % PlayVersion.current % scope,
    "org.scalatestplus.play" %% "scalatestplus-play" % "2.0.1" % scope,
    "org.mockito" % "mockito-core" % "1.9.0" % scope
  )

}
