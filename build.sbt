import Dependencies._

lazy val scala213 = "2.13.8"
lazy val scala212 = "2.12.15"
lazy val scala211 = "2.11.12"
lazy val scala312 = "3.1.2"
lazy val supportedScalaVersions = List(scala213, scala212, scala211, scala312)

ThisBuild / scalaVersion     := scala213
ThisBuild / version          := "0.0.1"
ThisBuild / organization     := "io.github.marchliu"
ThisBuild / organizationName := "Mars Liu<mars.liu@outlook.com>"
ThisBuild / organizationHomepage := Some(url("https://marchliu.github.io/"))

lazy val root = (project in file("."))
  .settings(
    name := "commons-lang3-bridge",
    libraryDependencies ++= Seq(
      "org.apache.commons" % "commons-lang3" % "3.12.0" % "provided",
      scalaTest % Test
    )
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
def scalacOptionsVersion(scalaVersion: String): Seq[String] = {
  Seq(
    "-unchecked",
    "-deprecation",
    "-Xlint",
    "-Xfatal-warnings",
    "-Ywarn-dead-code",
    "-encoding", "UTF-8"
  ) ++(CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, scalaMajor)) if scalaMajor == 11 => Seq("-Xexperimental")
    case _ => Nil
  })
}

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/MarchLiu/common-lang3-bridge"),
    "scm:git@github.com:MarchLiu/common-lang3-bridge.git"
  )
)
ThisBuild / developers := List(
  Developer(
    id = "Mars Liu",
    name = "Liu Xin",
    email = "mars.liu@outlook.com",
    url = url("https://marchliu.github.io/")
  )
)

ThisBuild / description := "Scala Bridge For Apache Commons Lang3 Library"
ThisBuild / licenses := List("MIT" -> new URL("https://github.com/MarchLiu/commons-lang3-bridge/blob/master/LICENSE"))
ThisBuild / homepage := Some(url("https://github.com/MarchLiu/jaskell-core"))

// Remove all additional repository other than Maven Central from POM
ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
ThisBuild / publishMavenStyle := true

val appSettings = Seq(
  scalacOptions := scalacOptionsVersion(scalaVersion.value)
)