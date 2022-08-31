package commons.lang3

package object bridge {
  type StrToOpt[T] = TypeMapping[T, (String, Option[String])]
  type VarArgsOfCharOrString[U] =
    TypeMapping[Seq[U], (Seq[Char], Seq[CharSequence], Seq[Option[Char]], Seq[Option[CharSequence]])]

  type VarArgsOfChar[U] =
    TypeMapping[Seq[U], (Seq[Char], Seq[Option[Char]])]

  type StringOrVarArgsOfChar[U] =
    TypeMapping[Seq[U], (String, Option[String], Seq[Char], Seq[Option[Char]])]

  type VarArgsOfString[U] =
    TypeMapping[Seq[U], (Seq[CharSequence], Seq[Option[CharSequence]])]
}
