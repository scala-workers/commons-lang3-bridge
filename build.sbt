import ProjectKeys._

val `modules/file` = file(".").getCanonicalFile / "modules"
val `brige/file`   = `modules/file` / "bridge"

val bridge  = project in `brige/file`
val codegen = project in `modules/file` / "codegen"

codegen / rootCodegenPath := `brige/file` / "src" / "codegen"

bridge / name  := "commons-lang3-bridge"
codegen / name := "commons-lang3-codegen"

Global / onChangedBuildSource := ReloadOnSourceChanges
