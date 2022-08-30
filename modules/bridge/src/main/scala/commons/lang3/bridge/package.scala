package commons.lang3

import commons.lang3.bridge.{TypeMappingInnerHelper => helper}

package object bridge {

  import helper._

  type StrToOpt[T] = TypeMapping[T, (String, Option[String])]

  def strToOpt[T: StrToOpt](t: T): Option[String] = {
    val mapping = implicitly[StrToOpt[T]]
    mapping.ops(t)(Option(_), identity)
  }

  type VarArgsOfPlain[T] = TypeMapping[Seq[T], (Seq[Char], Seq[CharSequence])]

  def uniformVarArgs[T: VarArgsOfPlain](arr: Seq[T]): Either[Seq[Char], Seq[CharSequence]] = {
    val mapping = implicitly[VarArgsOfPlain[T]]
    mapping.ops(arr)(Left(_), Right(_))
  }
}
