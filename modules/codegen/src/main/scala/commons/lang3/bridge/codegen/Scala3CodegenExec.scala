package commons.lang3.bridge.codegen

import java.io.PrintWriter
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.util.Using

object Scala3CodegenExec {

  def main(arr: Array[String]): Unit = {
    val rootString = arr(0)
    val rootPath   = Paths.get(rootString)
    val writePath  = rootPath.resolve(Paths.get("commons", "lang3", "bridge", "impl"))
    Files.createDirectories(writePath)
    locally {
      val filePath = writePath.resolve("TypeMappingAlias.scala")
      Using.resource(new PrintWriter(filePath.toFile, StandardCharsets.UTF_8)) { writer =>
        val linerContent = commons.lang3.bridge.codegen.txt.TypeMappingAliasScala3().body
        writer.println(linerContent)
      }
    }
  }

}
