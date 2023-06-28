import play.sbt.PlayImport.ws
import sbt.*

object AppDependencies {
  val hmrcBootstrapVersion = "5.25.0"
  val mockitoScalaVersion = "1.17.12"

  val compile: Seq[ModuleID] = Seq(
    ws,
    "uk.gov.hmrc"              %% "bootstrap-frontend-play-28" % "5.25.0",
    "uk.gov.hmrc"              %% "play-frontend-hmrc"         % "5.0.0-play-28",
    "uk.gov.hmrc"              %% "emailaddress"               % "3.7.0",
    "uk.gov.hmrc"              %% "domain"                     % "8.1.0-play-28",
    "uk.gov.hmrc"              %% "http-caching-client"        % "9.6.0-play-28",
    "org.typelevel"            %% "cats-core"                  % "1.0.0",
    "com.github.kxbmap"        %% "configs"                    % "0.6.0",
    "com.scalawilliam"         %% "xs4s"                       % "0.5",
    "commons-io"               % "commons-io"                  % "2.6",
    "org.mindrot"              % "jbcrypt"                     % "0.4",
    "com.sun.msv.datatype.xsd" % "xsdlib"                      % "2013.2",
    "msv"                      % "msv"                         % "20050913",
  )

  def test(scope: String = "test"): Seq[ModuleID] = Seq(
    "uk.gov.hmrc" %% "bootstrap-test-play-28" % hmrcBootstrapVersion % scope,
    "org.mockito" %% "mockito-scala"          % mockitoScalaVersion  % scope,
    "org.mockito" %% "mockito-scala-cats"     % mockitoScalaVersion  % scope
  )
}
