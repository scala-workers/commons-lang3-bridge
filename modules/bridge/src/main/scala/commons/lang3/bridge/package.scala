package commons.lang3

import java.nio.charset.Charset

package object bridge {
  type StrToOpt[T] = TypeMapping[T, (String, Option[String])]

  type CharToOpt[T] = TypeMapping[T, (Char, Option[Char])]

  type VarArgsOfCharOrString[U] =
    TypeMapping[Seq[U], (Seq[Char], Seq[CharSequence], Seq[Option[Char]], Seq[Option[CharSequence]])]

  type VarArgsOfChar[U] =
    TypeMapping[Seq[U], (Seq[Char], Seq[Option[Char]])]

  type StringOrVarArgsOfChar[U] =
    TypeMapping[Seq[U], (String, Option[String], Seq[Char], Seq[Option[Char]])]

  type VarArgsOfString[U] =
    TypeMapping[Seq[U], (Seq[CharSequence], Seq[Option[CharSequence]])]

  type CharsetOrName[T] =
    TypeMapping[T, (Charset, Option[Charset], String, Option[String])]

  type CharOrString[T] =
    TypeMapping[T, (Char, String, Option[String])]
}
