import sbt._
import sbt.Keys._

object Dependencies {

  object versions {
    val commonsLang3  = "3.12.0"
    val scalatest     = "3.2.13"
    val commonsIO     = "1.3.2"
    val kindProjector = "0.13.2"
  }

  val commonsLang3  = "org.apache.commons" % "commons-lang3"  % versions.commonsLang3
  val scalactic     = "org.scalactic"     %% "scalactic"      % versions.scalatest
  val scalatest     = "org.scalatest"     %% "scalatest"      % versions.scalatest
  val commonsIO     = "org.apache.commons" % "commons-io"     % versions.commonsIO
  val kindProjector = "org.typelevel"      % "kind-projector" % versions.kindProjector cross CrossVersion.full

}
