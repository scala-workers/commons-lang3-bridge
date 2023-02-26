import sbt._
import sbt.Keys._

object ProjectKeys {
  val parVersion = settingKey[Option[(Long, Long)]]("parVersion")
}
