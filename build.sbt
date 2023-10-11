import sbt.Keys.*
import sbt.*
import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin
import uk.gov.hmrc.versioning.SbtGitVersioning.autoImport.majorVersion
import play.sbt.PlayImport.*

val appName = "cbcr-frontend"

val silencerVersion = "1.7.13"

lazy val microservice = Project(appName, file("."))
  .enablePlugins(play.sbt.PlayScala, SbtDistributablesPlugin)
  .disablePlugins(JUnitXmlReportPlugin) //Required to prevent https://github.com/scalatest/scalatest/issues/1427
  .settings(onLoadMessage := "")
  .settings(
    majorVersion := 1,
    scalaVersion := "2.13.11",
    libraryDependencies ++= AppDependencies.compile ++ AppDependencies.test(),
    // ***************
    // Use the silencer plugin to suppress warnings
    scalacOptions ++= Seq(
      "-P:silencer:pathFilters=routes;views"
    ),
    libraryDependencies ++= Seq(
      compilerPlugin("com.github.ghik" % "silencer-plugin" % silencerVersion cross CrossVersion.full),
      "com.github.ghik" % "silencer-lib" % silencerVersion % Provided cross CrossVersion.full
    )
    // ***************
  )
  .settings(
    PlayKeys.playDefaultPort := 9696
  )
  .settings(resolvers += Resolver.jcenterRepo)
  .settings(CodeCoverageSettings.settings *)
  // Disable default sbt Test options (might change with new versions of bootstrap)
  .settings(Test / testOptions -= Tests.Argument("-o", "-u", "target/test-reports", "-h", "target/test-reports/html-report"))
  // Suppress successful events in Scalatest in standard output (-o)
  // Options described here: https://www.scalatest.org/user_guide/using_scalatest_with_sbt
  .settings(Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oNCHPQR", "-u", "target/test-reports", "-h", "target/test-reports/html-report"))

TwirlKeys.templateImports ++= Seq(
  "uk.gov.hmrc.govukfrontend.views.html.components._",
  "uk.gov.hmrc.hmrcfrontend.views.html.components._",
  "uk.gov.hmrc.hmrcfrontend.views.html.helpers._"
)

libraryDependencySchemes += "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always
