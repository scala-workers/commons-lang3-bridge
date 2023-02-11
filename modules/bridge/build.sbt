name := "commons-lang3-bridge"

libraryDependencies ++= libScalax.`commons-lang3`.value.map(_ % Provided)
libraryDependencies ++= libScalax.scalatest.value.map(_ % Test)
libraryDependencies ++= libScalax.`kind-projector`.value
libraryDependencies ++= libScalax.`simple-adt`.value

CommonSettings.commonProjectSettings
