val `modules/file` = file(".").getCanonicalFile / "modules"

val bridge = project in `modules/file` / "bridge"

bridge / name := "commons-lang3-bridge"

Global / onChangedBuildSource := ReloadOnSourceChanges
