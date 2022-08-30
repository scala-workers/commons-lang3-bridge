package commons.lang3.scala.codegen

import org.apache.commons.io.FileUtils

import java.nio.file.Paths

object PreCodegen {

  def main(arr: Array[String]): Unit = {
    val rootString = arr(0)
    val rootPath   = Paths.get(rootString)
    FileUtils.deleteDirectory(rootPath.toFile)
  }

}
