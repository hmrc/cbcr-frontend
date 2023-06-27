import sbt.Keys.*
import sbt.*
import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin
import uk.gov.hmrc.versioning.SbtGitVersioning.autoImport.majorVersion
import play.sbt.PlayImport.*

val appName = "cbcr-frontend"

val silencerVersion = "1.7.7"

lazy val microservice = Project(appName, file("."))
  .enablePlugins(play.sbt.PlayScala, SbtDistributablesPlugin)
  .settings(
    majorVersion := 1,
    scalaVersion := "2.12.13",
    libraryDependencies ++= AppDependencies.compile ++ AppDependencies.test(),
    // ***************
    // Use the silencer plugin to suppress warnings
    scalacOptions ++= Seq(
      "-P:silencer:pathFilters=routes;views",
      "-Ypartial-unification"
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

TwirlKeys.templateImports ++= Seq(
  "uk.gov.hmrc.govukfrontend.views.html.components._",
  "uk.gov.hmrc.hmrcfrontend.views.html.components._",
  "uk.gov.hmrc.hmrcfrontend.views.html.helpers._"
)
