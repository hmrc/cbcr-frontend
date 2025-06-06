import play.sbt.PlayImport.*
import sbt.*
import sbt.Keys.*
import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin
import uk.gov.hmrc.versioning.SbtGitVersioning.autoImport.majorVersion

val appName = "cbcr-frontend"

lazy val microservice = Project(appName, file("."))
  .enablePlugins(play.sbt.PlayScala, SbtDistributablesPlugin)
  .disablePlugins(JUnitXmlReportPlugin) // Required to prevent https://github.com/scalatest/scalatest/issues/1427
  .settings(onLoadMessage := "")
  .settings(scalafmtOnCompile := true)
  .settings(
    majorVersion := 1,
    scalaVersion := "3.3.5",
    libraryDependencies ++= AppDependencies.compile ++ AppDependencies.test()
  )
  .settings(
    scalacOptions += "-Wconf:src=routes/.*:s",
    scalacOptions += "-Wconf:cat=unused-imports&src=html/.*:s",
    scalacOptions := Seq("-explain")
  )
  .settings(PlayKeys.playDefaultPort := 9696)
  .settings(CodeCoverageSettings.settings*)
  // Disable default sbt Test options (might change with new versions of bootstrap)
  .settings(
    Test / testOptions -= Tests
      .Argument("-o", "-u", "target/test-reports", "-h", "target/test-reports/html-report")
  )
  // Suppress successful events in Scalatest in standard output (-o)
  // Options described here: https://www.scalatest.org/user_guide/using_scalatest_with_sbt
  .settings(
    Test / testOptions += Tests.Argument(
      TestFrameworks.ScalaTest,
      "-oNCHPQR",
      "-u",
      "target/test-reports",
      "-h",
      "target/test-reports/html-report"
    )
  )

TwirlKeys.templateImports ++= Seq(
  "uk.gov.hmrc.govukfrontend.views.html.components._",
  "uk.gov.hmrc.hmrcfrontend.views.html.components._",
  "uk.gov.hmrc.hmrcfrontend.views.html.helpers._"
)

libraryDependencySchemes += "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always
