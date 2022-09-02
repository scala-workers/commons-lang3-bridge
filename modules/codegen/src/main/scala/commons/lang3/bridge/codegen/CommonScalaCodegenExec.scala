package commons.lang3.bridge.codegen

import java.nio.file.Paths

object CommonScalaCodegenExec {

  def main(arr: Array[String]): Unit = {
    val rootString = arr(0)
    Paths.get(rootString)
  }

}
