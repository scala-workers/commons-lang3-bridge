package commons.lang3.scala.codegen

import java.io.PrintWriter
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.util.Using

object CommonScalaCodegenExec {

  def main(arr: Array[String]): Unit = {
    val rootString = arr(0)
    val rootPath   = Paths.get(rootString)
    val writePath  = rootPath.resolve(Paths.get("commons", "lang3", "scala"))
    Files.createDirectories(writePath)
    locally {
      val filePath = writePath.resolve("TypeMappingImplicitOptsPoly1.scala")
      Using.resource(new PrintWriter(filePath.toFile, StandardCharsets.UTF_8)) { writer =>
        val linerContent = commons.lang3.scala.codegen.txt.TypeMappingImplicitOptsPoly1().body
        writer.println(linerContent)
      }
    }
    locally {
      val filePath = writePath.resolve("TypeMappingInnerHelper.scala")
      Using.resource(new PrintWriter(filePath.toFile, StandardCharsets.UTF_8)) { writer =>
        val linerContent = commons.lang3.scala.codegen.txt.TypeMappingInnerHelper().body
        writer.println(linerContent)
      }
    }
  }

}
