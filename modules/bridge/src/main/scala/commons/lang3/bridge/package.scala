package commons.lang3

import commons.lang3.bridge.{TypeMappingInnerHelper => helper}

package object bridge {

  import helper._

  type StrToOpt[T] = TypeMapping[T, (String, Option[String])]

  def strToOpt[T: StrToOpt](t: T): Option[String] = {
    val mapping = implicitly[StrToOpt[T]]
    mapping.ops(t)(Option(_), identity)
  }

  type VarArgsOfCharOrString[T] =
    TypeMapping[Seq[T], (Seq[Char], Seq[CharSequence], Seq[Option[Char]], Seq[Option[CharSequence]])]

  def uniformCharStringVarArgs[T: VarArgsOfCharOrString](arr: Seq[T]): Either[Seq[Char], Seq[CharSequence]] = {
    val mapping = implicitly[VarArgsOfCharOrString[T]]
    mapping.ops(arr)(
      Left(_),
      Right(_),
      { chars => Left(chars.filter(_.isDefined).map(_.get)) },
      { css => Right(css.map(_.orNull)) }
    )
  }

  type VarArgsOfString[T] =
    TypeMapping[Seq[T], (Seq[CharSequence], Seq[Option[CharSequence]])]

  def uniformStringVarArgs[T: VarArgsOfString](arr: Seq[T]): Seq[CharSequence] = {
    val mapping = implicitly[VarArgsOfString[T]]
    mapping.ops(arr)(
      identity,
      { css => css.map(_.orNull) }
    )
  }
}
