import play.sbt.PlayImport.ws
import sbt.*

object AppDependencies {
  val hmrcBootstrapVersion = "5.25.0"

  val compile = Seq(
    ws,
    "uk.gov.hmrc"              %% "bootstrap-frontend-play-28" % "5.25.0",
    "uk.gov.hmrc"              %% "play-frontend-hmrc"         % "0.94.0-play-28",
    "uk.gov.hmrc"              %% "emailaddress"               % "3.7.0",
    "uk.gov.hmrc"              %% "domain"                     % "8.1.0-play-28",
    "uk.gov.hmrc"              %% "http-caching-client"        % "9.6.0-play-28",
    "org.typelevel"            %% "cats"                       % "0.9.0",
    "com.github.kxbmap"        %% "configs"                    % "0.6.0",
    "com.scalawilliam"         %% "xs4s"                       % "0.5",
    "org.codehaus.woodstox"    % "woodstox-core-asl"           % "4.4.1",
    "msv"                      % "msv"                         % "20050913",
    "com.sun.xml"              % "relaxngDatatype"             % "1.0",
    "com.sun.msv.datatype.xsd" % "xsdlib"                      % "2013.2",
    "commons-io"               % "commons-io"                  % "2.6",
    "org.mindrot"              % "jbcrypt"                     % "0.4"
  )

  def test(scope: String = "test") = Seq(
    "org.pegdown"            % "pegdown"             % "1.6.0"  % scope,
    "org.scalatestplus.play" %% "scalatestplus-play" % "5.0.0"  % scope,
    "org.mockito"            % "mockito-core"        % "3.11.0" % scope
  )
}
