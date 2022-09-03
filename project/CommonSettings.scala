import sbt._
import sbt.Keys._
import ProjectKeys._

object scalaVersions {
  val v211    = "2.11.12"
  val v212    = "2.12.15"
  val v213    = "2.13.8"
  val v312    = "3.1.3"
  val v320    = "3.2.0"
  val current = v213
}

object CommonSettings {

  // See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
  private def scalacOptionsVersion(scalaVersion: Option[(Long, Long)]): Seq[String] = {
    val common = Seq(
      "-unchecked",
      "-deprecation",
      "-Xlint",
      // "-Xfatal-warnings",
      "-Ywarn-dead-code",
      "-encoding",
      "UTF-8"
    )
    val compat = scalaVersion match {
      case Some((2, scalaMajor)) if scalaMajor == 11 => Seq("-Xexperimental")
      case Some((3, scalaMajor))                     => Seq("-Ykind-projector")
      case _                                         => Nil
    }
    common ++ compat
  }

  private def genDirectory(sourceFile: File, c: String, parVersion: Option[(Long, Long)]): Seq[File] = {
    val common = sourceFile / c / "scala"
    val compat = parVersion match {
      case Some((2, 11)) =>
        Seq(sourceFile / c / "scala-2", sourceFile / c / "scala-2.11", sourceFile / c / "scala-2.11-2.12")
      case Some((2, 12)) =>
        Seq(
          sourceFile / c / "scala-2",
          sourceFile / c / "scala-2.11-2.12",
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

  val supportedScalaVersions = Seq(scalaVersions.v213, scalaVersions.v212, scalaVersions.v211, scalaVersions.v312)

  val pushSettings = Seq(
    version              := "0.0.1",
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
    description := "Scala Bridge For Apache Commons Lang3 Library",
    licenses    := List("MIT" -> new URL("https://github.com/scala-workers/commons-lang3-bridge/blob/master/LICENSE")),
    homepage    := Some(url("https://github.com/scala-workers/commons-lang3-brdge")),
    // Remove all additional repository other than Maven Central from POM
    pomIncludeRepository := { _ => false },
    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
      else Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
    publishMavenStyle := true
  )

  private val commonSetting = Seq(
    parVersion                                                   := CrossVersion.partialVersion(scalaVersion.value),
    scalaVersion                                                 := scalaVersions.current,
    scalacOptions                                                := scalacOptionsVersion(parVersion.value),
    org.scalafmt.sbt.ScalafmtPlugin.autoImport.scalafmtOnCompile := true,
    Compile / unmanagedSourceDirectories ++= {
      val addToMain    = genDirectory(sourceDirectory.value, "main", parVersion.value)
      val addToCodeGen = genDirectory(sourceDirectory.value, "codegen", parVersion.value)
      addToMain ++: addToCodeGen
    },
    Test / unmanagedSourceDirectories ++= genDirectory(sourceDirectory.value, "test", parVersion.value)
  )

  def addCompilerPlugins(v: String, mudules: ModuleID*): Seq[Def.Setting[Seq[ModuleID]]] = CrossVersion.partialVersion(v) match {
    case Some((2, _)) => mudules.map(addCompilerPlugin)
    case _            => Seq.empty
  }

  val commonProjectSettings  = pushSettings ++ commonSetting ++ Seq(crossScalaVersions := supportedScalaVersions)
  val codegenProjectSettings = commonSetting

}
