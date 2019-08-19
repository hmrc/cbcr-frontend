import sbt.Keys._
import sbt.Tests.{Group, SubProcess}
import sbt._
import play.routes.compiler.{InjectedRoutesGenerator, StaticRoutesGenerator}
import play.sbt.PlayImport.PlayKeys.playDefaultPort
import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin._


trait MicroService {

  import uk.gov.hmrc._
  import DefaultBuildSettings._
  import uk.gov.hmrc.{SbtBuildInfo, ShellPrompt, SbtAutoBuildPlugin, SbtArtifactory}
  import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin
  import uk.gov.hmrc.versioning.SbtGitVersioning
  import play.sbt.routes.RoutesKeys.routesGenerator
  import uk.gov.hmrc.versioning.SbtGitVersioning.autoImport.majorVersion


  import TestPhases.oneForkedJvmPerTest

  val appName: String

  lazy val appDependencies : Seq[ModuleID] = ???
  lazy val plugins : Seq[Plugins] = Seq.empty
  lazy val playSettings : Seq[Setting[_]] = Seq.empty

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

  lazy val microservice = Project(appName, file("."))
    .enablePlugins(Seq(play.sbt.PlayScala,SbtAutoBuildPlugin, SbtGitVersioning, SbtDistributablesPlugin, SbtArtifactory) ++ plugins : _*)
    .settings(playSettings ++ scoverageSettings : _*)
    .settings(scalaSettings: _*)
    .settings(playDefaultPort := 9696)
    .settings(publishingSettings: _*)
    .settings(majorVersion := 1 )
    .settings(defaultSettings(): _*)
    .settings(
      scalaVersion := "2.11.11",
      libraryDependencies ++= appDependencies,
      retrieveManaged := true,
      evictionWarningOptions in update := EvictionWarningOptions.default.withWarnScalaVersionEviction(false),
      routesGenerator := InjectedRoutesGenerator
    )
    .configs(IntegrationTest)
    .settings(inConfig(IntegrationTest)(Defaults.itSettings): _*)
    .settings(
      Keys.fork in IntegrationTest := false,
      unmanagedSourceDirectories in IntegrationTest := (baseDirectory in IntegrationTest)(base => Seq(base / "it")).value,
      addTestReportOption(IntegrationTest, "int-test-reports"),
      testGrouping in IntegrationTest := oneForkedJvmPerTest((definedTests in IntegrationTest).value),
      parallelExecution in IntegrationTest := false)
      .settings(resolvers ++= Seq(
        "hmrc-releases" at "https://artefacts.tax.service.gov.uk/artifactory/hmrc-releases/",
        Resolver.jcenterRepo
      ))
}

private object TestPhases {

  def oneForkedJvmPerTest(tests: Seq[TestDefinition]) =
    tests map {
      test => new Group(test.name, Seq(test), SubProcess(ForkOptions(runJVMOptions = Seq("-Dtest.name=" + test.name))))
    }
}
