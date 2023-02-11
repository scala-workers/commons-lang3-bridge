name := "commons-lang3-bridge"

libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.12.0" % Provided
libraryDependencies ++= libScalax.scalatest.value.map(_ % Test)
libraryDependencies ++= libScalax.`kind-projector`.value
libraryDependencies ++= Seq("net.scalax.simple" %% "simple-adt-core" % "0.0.1-M3")

CommonSettings.commonProjectSettings
