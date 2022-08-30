import sbt._
import sbt.Keys._

object ProjectKeys {

  val rootCodegenPath = settingKey[File]("Codegen root path")
  val runGen          = inputKey[Unit]("Execute codegen action")
  val preGen          = inputKey[Unit]("Execute prepare codegen action")
  val parVersion      = settingKey[Option[(Long, Long)]]("parVersion")

}
