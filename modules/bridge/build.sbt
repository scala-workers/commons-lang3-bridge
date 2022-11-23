import ProjectKeys._

val depts = Lang3Depts

name := "commons-lang3-bridge"

libraryDependencies += depts.commonsLang3 % Provided
libraryDependencies += depts.scalactic
libraryDependencies += depts.scalatest % Test
libraryDependencies ++= depts.kindProjector.value

CommonSettings.commonProjectSettings
