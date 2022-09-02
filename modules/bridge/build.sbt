import ProjectKeys._

libraryDependencies += Dependencies.commonsLang3 % Provided
libraryDependencies += Dependencies.scalactic
libraryDependencies += Dependencies.scalatest % Test

CommonSettings.commonProjectSettings

libraryDependencies ++= {
  val v = CrossVersion.partialVersion(scalaVersion.value).map(_._1)
  if (v == Some(2))
    Seq(compilerPlugin(Dependencies.kindProjector))
  else Seq.empty
}
