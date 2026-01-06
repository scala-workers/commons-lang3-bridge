name := "commons-lang3-bridge"

crossScalaVersions := Seq(scalaV.v212, scalaV.v213, scalaV.v3)
scalaVersion       := scalaV.v212

libraryDependencies ++= libScalax.`commons-lang3`.value.map(_ % Provided)
libraryDependencies ++= libScalax.scalatest.value.map(_ % Test)
libraryDependencies ++= libScalax.`simple-adt`.value

CommonSettings.commonProjectSettings

useKindProjector
