import play.sbt.PlayImport.ws
import sbt.*

object AppDependencies {
  val hmrcBootstrapVersion = "8.5.0"
  val mockitoScalaVersion = "1.17.31"
  val hmrcMongoVersion = "1.4.0"
  val playVersion = "play-30"

  val compile: Seq[ModuleID] = Seq(
    ws,
    "uk.gov.hmrc"             %% s"bootstrap-frontend-$playVersion" % hmrcBootstrapVersion,
    "uk.gov.hmrc"             %% s"play-frontend-hmrc-$playVersion" % hmrcBootstrapVersion,
    "uk.gov.hmrc.mongo"       %% s"hmrc-mongo-$playVersion"         % hmrcMongoVersion,
    "uk.gov.hmrc"             %% "emailaddress"                     % "3.7.0",
    "uk.gov.hmrc"             %% s"domain-$playVersion"             % "9.0.0",
    "org.typelevel"           %% "cats-core"                        % "2.0.0",
    "org.codehaus.woodstox"    % "stax2-api"                        % "3.1.4",
    "org.codehaus.woodstox"    % "woodstox-core-asl"                % "4.4.1",
    "commons-io"               % "commons-io"                       % "2.6",
    "org.mindrot"              % "jbcrypt"                          % "0.4",
    "com.sun.msv.datatype.xsd" % "xsdlib"                           % "2013.2",
    "msv"                      % "msv"                              % "20050913"
  )

  def test(scope: String = "test"): Seq[ModuleID] = Seq(
    "uk.gov.hmrc"           %% s"bootstrap-test-$playVersion"  % hmrcBootstrapVersion % scope,
    "uk.gov.hmrc.mongo"     %% s"hmrc-mongo-test-$playVersion" % hmrcMongoVersion     % scope,
    "org.mockito"           %% "mockito-scala"                 % mockitoScalaVersion  % scope,
    "org.mockito"           %% "mockito-scala-cats"            % mockitoScalaVersion  % scope,
    "com.github.tomakehurst" % "wiremock"                      % "3.0.0-beta-7"       % scope,
    "xerces"                 % "xercesImpl"                    % "2.12.0"             % scope
  )
}
