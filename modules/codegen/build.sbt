import ProjectKeys._

libraryDependencies += Dependencies.commonsIO

CommonSettings.codegenProjectSettings
enablePlugins(SbtTwirl)

val commonScalaRunMainInputStr = settingKey[String]("commonScalaRunMainInputStr")
commonScalaRunMainInputStr := {
  val projectRoot = rootCodegenPath.value / "scala"
  projectRoot.getAbsoluteFile.toString
}

val commonScalaRunGen       = inputKey[Unit]("commonScalaRunGen")
val commonScalaRunMainClass = "commons.lang3.bridge.codegen.CommonScalaCodegenExec"
Compile / commonScalaRunGen := (Compile / runMain).inputTaskValue
  .partialInput(s" $commonScalaRunMainClass")
  .partialInput(s" ${commonScalaRunMainInputStr.value}")
  .evaluated

val scala2RunMainInputStr = settingKey[String]("scala2RunMainInputStr")
scala2RunMainInputStr := {
  val projectRoot = rootCodegenPath.value / "scala-2"
  projectRoot.getAbsoluteFile.toString
}

val scala2RunGen       = inputKey[Unit]("scala2RunGen")
val scala2RunMainClass = "commons.lang3.bridge.codegen.Scala2CodegenExec"
Compile / scala2RunGen := (Compile / runMain).inputTaskValue
  .partialInput(s" $scala2RunMainClass")
  .partialInput(s" ${scala2RunMainInputStr.value}")
  .evaluated

val scala3RunMainInputStr = settingKey[String]("scala3RunMainInputStr")
scala3RunMainInputStr := {
  val projectRoot = rootCodegenPath.value / "scala-3"
  projectRoot.getAbsoluteFile.toString
}

val scala3RunGen       = inputKey[Unit]("scala3RunGen")
val scala3RunMainClass = "commons.lang3.bridge.codegen.Scala3CodegenExec"
Compile / scala3RunGen := (Compile / runMain).inputTaskValue
  .partialInput(s" $scala3RunMainClass")
  .partialInput(s" ${scala3RunMainInputStr.value}")
  .evaluated

val preCodegenTask  = inputKey[Unit]("preCodegenTask")
val preGenMainClass = "commons.lang3.bridge.codegen.PreCodegen"
Compile / preCodegenTask := (Compile / runMain).inputTaskValue
  .partialInput(s" $preGenMainClass")
  .partialInput(s" ${rootCodegenPath.value.getAbsoluteFile.toString}")
  .evaluated

preGen := {
  (Compile / preCodegenTask).evaluated
}

runGen := {
  (Compile / commonScalaRunGen).evaluated
  (Compile / scala2RunGen).evaluated
  (Compile / scala3RunGen).evaluated
}
