import sbt._
import sbt.Keys._
import ProjectKeys._

import djx.sbt.depts.output.Djx314DeptsPlugin.autoImport.scalaV

object CommonSettings {

  // See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
  private def scalacOptionsVersion(scalaVersion: Option[(Long, Long)]): Seq[String] = {
    val common = Seq(
      "-unchecked",
      "-deprecation",
      // "-Xfatal-warnings",
      "-encoding",
      "UTF-8"
    )
    val compat = scalaVersion match {
      case Some((2, scalaMajor)) if scalaMajor == 11 => Seq("-Xexperimental", "-Xlint", "-Ywarn-dead-code")
      case _                                         => Seq.empty
    }
    common ++ compat
  }

  private def genDirectory(sourceFile: File, c: String, parVersion: Option[(Long, Long)]): Seq[File] = {
    val common = sourceFile / c / "scala"
    val compat = parVersion match {
      case Some((2, 12)) =>
        Seq(
          sourceFile / c / "scala-2",
          sourceFile / c / "scala-2.12",
          sourceFile / c / "scala-2.12-2.13"
        )
      case Some((2, 13)) =>
        Seq(sourceFile / c / "scala-2", sourceFile / c / "scala-2.12-2.13", sourceFile / c / "scala-2.13")
      case Some((2, _)) => Seq(sourceFile / c / "scala-2")
      case Some((3, _)) => Seq(sourceFile / c / "scala-3")
      case _            => Seq.empty
    }
    common +: compat
  }

  val pushSettings = Seq(
    version              := "0.1.0",
    organization         := "net.scalax",
    organizationName     := "Scala Workers",
    organizationHomepage := Some(url("https://github.com/scala-workers")),
    scmInfo := Some(
      ScmInfo(
        url("https://github.com/scala-workers/commons-lang3-bridge"),
        "scm:git@github.com:scala-workers/commons-lang3-bridge.git"
      )
    ),
    developers := List(
      Developer(
        id = "Mars Liu",
        name = "Liu Xin",
        email = "mars.liu@outlook.com",
        url = url("https://marchliu.github.io/")
      ),
      Developer(
        id = "djx314",
        name = "djx314",
        email = "djx314@sina.cn",
        url = url("https://github.com/djx314")
      )
    ),
    description          := "Scala Bridge For Apache Commons Lang3 Library",
    licenses             := List("Apache" -> new URL("https://github.com/scala-workers/commons-lang3-bridge/blob/master/LICENSE")),
    homepage             := Some(url("https://github.com/scala-workers/commons-lang3-bridge")),
    pomIncludeRepository := { _ => false }, // Remove all additional repository other than Maven Central from POM
    publishTo            := localStaging.value,
    publishMavenStyle    := true
  )

  private val commonSetting = Seq(
    parVersion                                                   := CrossVersion.partialVersion(scalaVersion.value),
    scalacOptions                                                := scalacOptionsVersion(parVersion.value),
    org.scalafmt.sbt.ScalafmtPlugin.autoImport.scalafmtOnCompile := true,
    Compile / unmanagedSourceDirectories ++= {
      val addToMain    = genDirectory(sourceDirectory.value, "main", parVersion.value)
      val addToCodeGen = genDirectory(sourceDirectory.value, "codegen", parVersion.value)
      addToMain ++: addToCodeGen
    },
    Test / unmanagedSourceDirectories ++= genDirectory(sourceDirectory.value, "test", parVersion.value)
  )

  val commonProjectSettings  = pushSettings ++ commonSetting
  val codegenProjectSettings = commonSetting

}
