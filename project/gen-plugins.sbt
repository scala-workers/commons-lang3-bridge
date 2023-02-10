import java.io.PrintWriter
import java.nio.file.{Files, Path}
import java.util.stream.Collectors
import scala.jdk.CollectionConverters._
import scala.collection.compat._
import scala.util.Using

def genPluginString(str: String, pluginName: String): String = {
  val settingStr =
    str replaceAllLiterally ("""// start setting""", "addSetting {") replaceAllLiterally ("""// end setting""", "}") replaceAllLiterally ("""// scala code""", "") replaceAllLiterally ("""// start autoImport""", "object autoImport {") replaceAllLiterally ("""// end autoImport""",
    """
        |}
        |
        |import autoImport._
        |""".stripMargin)

  s"""
     |object `$pluginName` extends _root_.sbt.AutoPlugin {
     |
     |  import _root_.sbt._
     |  import _root_.sbt.Keys._
     |
     |  private var preSettings: Seq[Setting[_]] = Seq.empty
     |
     |  def addSetting(set: Setting[_]): Unit = preSettings = set +: preSettings
     |
     |  $settingStr
     |
     |}
     |""".stripMargin
}

def genPluginFromFile(f: Path): String = genPluginString(Files.readString(f), f.getFileName.toString.split('.')(0))

Compile / sourceGenerators += Def.task {
  val sourceDir = (Compile / sourceManaged).value / "PluginCollection.scala"

  val scriptFilesDir = file(".").getCanonicalFile / "scripts"
  val scriptFiles    = Files.list(scriptFilesDir.toPath).collect(Collectors.toList[Path]).asScala.to(List)

  val str = scriptFiles.map(genPluginFromFile)

  Files.createDirectories(sourceDir.toPath.getParent)
  Using.resource(new PrintWriter(sourceDir)) { p =>
    for (eachStr <- str) {
      p.println(eachStr)
      p.println()
    }
  }
  Seq(sourceDir)
}.taskValue
