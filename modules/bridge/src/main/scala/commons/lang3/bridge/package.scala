package commons.lang3

package object bridge {
  type StrToOpt[T] = TypeMapping[T, (String, Option[String])]
  type VarArgsOfCharOrString[U] =
    TypeMapping[Seq[U], (Seq[Char], Seq[CharSequence], Seq[Option[Char]], Seq[Option[CharSequence]])]

  type VarArgsOfString[U] =
    TypeMapping[Seq[U], (Seq[CharSequence], Seq[Option[CharSequence]])]
}
