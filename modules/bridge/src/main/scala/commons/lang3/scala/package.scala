package commons.lang3

import commons.lang3.scala.{TypeMappingInnerHelper => helper}

package object scala {

  import helper._

  type StrToOpt[T] = TypeMapping[T, (String, Option[String])]

  def strToOpt[T: StrToOpt](t: T): Option[String] = {
    val mapping = implicitly[StrToOpt[T]]
    mapping.ops(Option(_), identity).accept(t)
  }

}
