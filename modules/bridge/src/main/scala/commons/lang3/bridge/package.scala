package commons.lang3

package object bridge {
  type StrToOpt[T]       = TypeMapping[T, (String, Option[String])]
  type VarArgsOfPlain[U] = TypeMapping[Seq[U], (Seq[Char], Seq[CharSequence])]
}
