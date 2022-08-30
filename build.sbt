import ProjectKeys._

val modulesFile = file(".") / "modules"
val brigeFile   = modulesFile / "bridge"

val bridge  = project in brigeFile
val codegen = project in modulesFile / "codegen"

codegen / rootCodegenPath := brigeFile / "src" / "codegen"

bridge / name  := "commons-lang3-bridge"
codegen / name := "commons-lang3-codegen"
