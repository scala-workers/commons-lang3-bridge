val `root/file` = file(".").getCanonicalFile

djxScalafmtFile   := `root/file` / ".scalafmt_commons-lang3-bridge.conf"
djxBuildSbtFile   := `root/file` / "project" / "build.properties"
djxPluginsLigFile := `root/file` / "project" / "project" / "scalax-depts-plugins-lib.sbt"
