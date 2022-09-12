package commons.lang3

package object bridge {

  type VarArgsOfCharOrStrings[U] = TypeMapping[Seq[U], (Seq[Char], Seq[CharSequence], Seq[Option[Char]], Seq[Option[CharSequence]])]

  type VarArgsOfCharsOrString[U] = TypeMapping[Seq[U], (Seq[Char], Seq[Option[Char]], String, Option[String])]

  type VarArgsOfChar[U] = TypeMapping[Seq[U], (Seq[Char], Seq[Option[Char]])]

  type StringOrVarArgsOfChar[U] = TypeMapping[Seq[U], (String, Option[String], Seq[Char], Seq[Option[Char]])]

  type VarArgsOfString[U] = TypeMapping[Seq[U], (Seq[String], Seq[Option[String]])]

  type VarArgsOfCharSequence[U] = TypeMapping[Seq[U], (Seq[CharSequence], Seq[Option[CharSequence]])]

}
