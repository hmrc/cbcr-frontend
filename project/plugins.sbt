resolvers += Resolver.url("HMRC Sbt Plugin Releases", url("https://dl.bintray.com/hmrc/sbt-plugin-releases"))(Resolver.ivyStylePatterns)
resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/"

addSbtPlugin("uk.gov.hmrc" % "sbt-auto-build" % "2.5.0")

addSbtPlugin("uk.gov.hmrc" % "sbt-git-versioning" % "2.1.0")

addSbtPlugin("uk.gov.hmrc" % "sbt-artifactory" % "1.0.0")

addSbtPlugin("uk.gov.hmrc" % "sbt-distributables" % "2.0.0")

addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.6.23")

addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.2.24")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.1")

addSbtPlugin("uk.gov.hmrc" % "sbt-service-manager" % "0.4.0")

addSbtPlugin("com.lucidchart" % "sbt-scalafmt" % "1.16")