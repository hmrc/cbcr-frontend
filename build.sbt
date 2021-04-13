import sbt.Keys._
import sbt.Tests.{Group, SubProcess}
import sbt._
import play.routes.compiler.InjectedRoutesGenerator
import play.sbt.PlayImport.PlayKeys.playDefaultPort
import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin._
import com.lucidchart.sbt.scalafmt.ScalafmtCorePlugin.autoImport._
import uk.gov.hmrc._
import DefaultBuildSettings._
import uk.gov.hmrc.{SbtArtifactory, SbtAutoBuildPlugin}
import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin
import uk.gov.hmrc.versioning.SbtGitVersioning
import play.sbt.routes.RoutesKeys.routesGenerator
import uk.gov.hmrc.versioning.SbtGitVersioning.autoImport.majorVersion
import play.sbt.PlayImport._

val appName = "cbcr-frontend"

resolvers += Resolver.bintrayRepo("hmrc", "releases")

lazy val appDependencies: Seq[ModuleID] = compile ++ test()

val compile = Seq(
  ws,
  "uk.gov.hmrc"              %% "bootstrap-frontend-play-26"      % "4.2.0",
  "uk.gov.hmrc"              %% "govuk-template"      % "5.65.0-play-26",
  "uk.gov.hmrc"              %% "play-ui"             % "8.15.0-play-26",
  "uk.gov.hmrc"              %% "emailaddress"        % "3.5.0",
  "uk.gov.hmrc"              %% "domain"              % "5.11.0-play-26",
  "uk.gov.hmrc"              %% "http-caching-client" % "9.3.0-play-26",
  "uk.gov.hmrc"              %% "http-verbs"          % "13.3.0",
  "uk.gov.hmrc"              %% "http-core"           % "2.5.0",
  "org.typelevel"            %% "cats"                % "0.9.0",
  "com.github.kxbmap"        %% "configs"             % "0.4.4",
  "com.scalawilliam"         %% "xs4s"                % "0.5",
  "org.codehaus.woodstox"    % "woodstox-core-asl"    % "4.4.1",
  "msv"                      % "msv"                  % "20050913",
  "com.sun.xml"              % "relaxngDatatype"      % "1.0",
  "com.sun.msv.datatype.xsd" % "xsdlib"               % "2013.2",
  "commons-io"               % "commons-io"           % "2.6",
  "org.mindrot"              % "jbcrypt"              % "0.4"
)

def test(scope: String = "test") = Seq(
  "org.pegdown"            % "pegdown"             % "1.6.0"  % scope,
  "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.2"  % scope,
  "org.mockito"            % "mockito-core"        % "3.2.4" % scope
)

lazy val plugins: Seq[Plugins] = Seq.empty
lazy val playSettings: Seq[Setting[_]] = Seq.empty

lazy val excludedPackages = Seq(
  "<empty>",
  "Reverse*",
  "models/.data/..*",
  "view.*",
  ".*standardError*.*",
  ".*govuk_wrapper*.*",
  ".*main_template*.*",
  "uk.gov.hmrc.BuildInfo",
  "app.*",
  "prod.*",
  "config.*",
  "testOnlyDoNotUseInAppConf.*",
  "testOnly.*",
  "uk.gov.hmrc.cbcr.controllers.test",
  "test",
  "uk.gov.hmrc.cbcrfrontend.connectors.test",
  "uk.gov.hmrc.cbcrfrontend.controllers.test",
  "uk.gov.hmrc.cbcrfrontend.controllers.AdminController",
  "uk.gov.hmrc.cbcrfrontend.controllers.AdminDocRefId",
  "uk.gov.hmrc.cbcrfrontend.views.*",
  "uk.gov.hmrc.cbcrfrontend.connectors.BPRKnownFactsConnector.*",
  "uk.gov.hmrc.cbcrfrontend.connectors.CBCRBackendConnector.*",
  "uk.gov.hmrc.cbcrfrontend.connectors.TaxEnrolmentsConnector.*",
  "uk.gov.hmrc.cbcrfrontend.typesclasses",
  "uk.gov.hmrc.cbcrfrontend.core",
  "uk.gov.hmrc.cbcrfrontend.model"
)

lazy val scoverageSettings = {
  import scoverage._
  Seq(
    ScoverageKeys.coverageExcludedPackages := excludedPackages.mkString(";"),
    ScoverageKeys.coverageMinimum := 80,
    ScoverageKeys.coverageFailOnMinimum := false,
    ScoverageKeys.coverageHighlighting := true
  )
}
libraryDependencies ++= Seq(
  compilerPlugin("com.github.ghik" % "silencer-plugin" % "1.7.1" cross CrossVersion.full),
  "com.github.ghik" % "silencer-lib" % "1.7.1" % Provided cross CrossVersion.full
)
lazy val microservice =
  Project(appName, file("."))
    .enablePlugins(Seq(
      play.sbt.PlayScala,
      SbtAutoBuildPlugin,
      SbtGitVersioning,
      SbtDistributablesPlugin,
      SbtArtifactory) ++ plugins: _*)
    .settings(playSettings ++ scoverageSettings: _*)
    .settings(scalaSettings: _*)
    .settings(playDefaultPort := 9696)
    .settings(publishingSettings: _*)
    .settings(majorVersion := 1)
    .settings(defaultSettings(): _*)
    .settings(
      scalaVersion := "2.12.11",
      libraryDependencies ++= appDependencies,
      retrieveManaged := true,
      evictionWarningOptions in update := EvictionWarningOptions.default.withWarnScalaVersionEviction(false),
      routesGenerator := InjectedRoutesGenerator,
      scalafmtOnCompile in Compile := true,
      scalafmtOnCompile in Test := true
    )
    .configs(IntegrationTest)
    .settings(inConfig(IntegrationTest)(Defaults.itSettings): _*)
    .settings(
      Keys.fork in IntegrationTest := false,
      unmanagedSourceDirectories in IntegrationTest := (baseDirectory in IntegrationTest)(base => Seq(base / "it")).value,
      addTestReportOption(IntegrationTest, "int-test-reports"),
      testGrouping in IntegrationTest := oneForkedJvmPerTest((definedTests in IntegrationTest).value),
      parallelExecution in IntegrationTest := false,
      scalafmtOnCompile in IntegrationTest := true
    )
    .settings(scalacOptions ++= List(
      // Warn if an import selector is not referenced.
      "-P:silencer:pathFilters=routes;html"
    ))
    .settings(resolvers ++= Seq(
      "hmrc-releases" at "https://artefacts.tax.service.gov.uk/artifactory/hmrc-releases/",
      Resolver.jcenterRepo
    ))
    .disablePlugins(JUnitXmlReportPlugin) //Required to prevent https://github.com/scalatest/scalatest/issues/1427

def oneForkedJvmPerTest(tests: Seq[TestDefinition]) =
  tests.map { test =>
    new Group(test.name, Seq(test), SubProcess(ForkOptions().withRunJVMOptions(Vector(s"-Dtest.name=${test.name}"))))
  }

