import play.sbt.PlayImport.ws
import sbt.*

object AppDependencies {
  val hmrcBootstrapVersion = "8.5.0"
  val mockitoScalaVersion = "1.17.37"
  val hmrcMongoVersion = "2.3.0"
  val playVersion = "play-30"

  val compile: Seq[ModuleID] = Seq(
    ws,
    "uk.gov.hmrc"             %% s"bootstrap-frontend-$playVersion" % hmrcBootstrapVersion,
    "uk.gov.hmrc"             %% s"play-frontend-hmrc-$playVersion" % hmrcBootstrapVersion,
    "uk.gov.hmrc.mongo"       %% s"hmrc-mongo-$playVersion"         % hmrcMongoVersion,
    "uk.gov.hmrc"             %% s"domain-$playVersion"             % "10.0.0",
    "org.typelevel"           %% "cats-core"                        % "2.12.0",
    "org.codehaus.woodstox"    % "stax2-api"                        % "4.2.2",
    "org.codehaus.woodstox"    % "woodstox-core-asl"                % "4.4.1",
    "commons-io"               % "commons-io"                       % "2.16.1",
    "org.mindrot"              % "jbcrypt"                          % "0.4",
    "com.sun.msv.datatype.xsd" % "xsdlib"                           % "2013.2",
    "msv"                      % "msv"                              % "20050913"
  )

  def test(scope: String = "test"): Seq[ModuleID] = Seq(
    "uk.gov.hmrc"           %% s"bootstrap-test-$playVersion"  % hmrcBootstrapVersion % scope,
    "uk.gov.hmrc.mongo"     %% s"hmrc-mongo-test-$playVersion" % hmrcMongoVersion     % scope,
    "org.mockito"           %% "mockito-scala"                 % mockitoScalaVersion  % scope,
    "org.mockito"           %% "mockito-scala-cats"            % mockitoScalaVersion  % scope,
    "xerces"                 % "xercesImpl"                    % "2.12.2"             % scope
  )
}
