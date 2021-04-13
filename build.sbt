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
  "uk.gov.hmrc"              %% "govuk-template"      % "5.55.0-play-26",
  "uk.gov.hmrc"              %% "play-ui"             % "8.15.0-play-26",
  "uk.gov.hmrc"              %% "emailaddress"        % "3.5.0",
  "uk.gov.hmrc"              %% "domain"              % "5.9.0-play-26",
  "uk.gov.hmrc"              %% "http-caching-client" % "9.1.0-play-26",
  "uk.gov.hmrc"               %% "http-verbs-play-26" % "11.5.0",
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
      "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
      "-encoding", "utf-8",                // Specify character encoding used by source files.
      "-explaintypes",                     // Explain type errors in more detail.
      "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.
      "-unchecked",                        // Enable additional warnings where generated code depends on assumptions.
      "-Xcheckinit",                       // Wrap field accessors to throw an exception on uninitialized access.
      "-Xfuture",                          // Turn on future language features.
      "-Xlint:adapted-args",               // Warn if an argument list is modified to match the receiver.
      "-Xlint:by-name-right-associative",  // By-name parameter of right associative operator.
      "-Xlint:delayedinit-select",         // Selecting member of DelayedInit.
      "-Xlint:doc-detached",               // A Scaladoc comment appears to be detached from its element.
      "-Xlint:inaccessible",               // Warn about inaccessible types in method signatures.
      "-Xlint:infer-any",                  // Warn when a type argument is inferred to be `Any`.
      "-Xlint:missing-interpolator",       // A string literal appears to be missing an interpolator id.
      "-Xlint:nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
      "-Xlint:nullary-unit",               // Warn when nullary methods return Unit.
      "-Xlint:option-implicit",            // Option.apply used implicit view.
      "-Xlint:package-object-classes",     // Class or object defined in package object.
      "-Xlint:poly-implicit-overload",     // Parameterized overloaded implicit methods are not visible as view bounds.
      "-Xlint:private-shadow",             // A private field (or class parameter) shadows a superclass field.
      "-Xlint:stars-align",                // Pattern sequence wildcard must align with sequence component.
      "-Xlint:type-parameter-shadow",      // A local type parameter shadows a type already in scope.
      "-Xlint:unsound-match",              // Pattern match may not be typesafe.
      "-Yno-adapted-args",                 // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
      "-Ywarn-inaccessible",               // Warn about inaccessible types in method signatures.
      "-Ywarn-infer-any",                  // Warn when a type argument is inferred to be `Any`.
      "-Ywarn-nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
      "-Ywarn-nullary-unit",               // Warn when nullary methods return Unit.
      "-Ywarn-numeric-widen",              // Warn when numerics are widened.
      "-Ywarn-unused",                     // Warn if an import selector is not referenced.
      "-P:silencer:lineContentFilters=^\\[@]",// Avoid '^\\w' warnings for Twirl template
      "-Ywarn-value-discard"               // Warn when non-Unit expression results are unused.
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

