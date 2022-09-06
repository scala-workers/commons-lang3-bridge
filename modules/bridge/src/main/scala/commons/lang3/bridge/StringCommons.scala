package commons.lang3.bridge

import commons.lang3.bridge.{TypeMappingInnerHelper => helper}
import org.apache.commons.lang3.{StringUtils => Strings}

import java.nio.charset.Charset
import java.util.Locale
import java.util.function.Supplier

private object privateUtils {
  import helper._

  @inline def getMapper[I, O](implicit map: SingleTypeMap[I, O]): SingleTypeMap[I, O] = map

  @FunctionalInterface
  trait SingleTypeMap[I, O] {
    protected def input(i: I): O
    def func: I => O = i => this.input(i)
  }

  object SingleTypeMap {
    implicit def toStrOptImplicit[U: TypeMapping[*, (String, Option[String])]]: SingleTypeMap[U, Option[String]] =
      strToOpt
    implicit def toCharSequenceOptImplicit[U: TypeMapping[*, (CharSequence, Option[CharSequence])]]
      : SingleTypeMap[U, Option[CharSequence]] = csToOpt
    implicit def seqOptionCharToSeqCharImplicit: SingleTypeMap[Seq[Option[Char]], Seq[Char]] = tranCharSeqOptFunc
    implicit def seqOptionCharSequenceToSeqCharSequenceImplicit: SingleTypeMap[Seq[Option[CharSequence]], Seq[CharSequence]] =
      tranCharSeqSeqOptFunc
  }

  implicit class funcToStringOptOrNull[T](val func: T => Option[String]) {
    def orNull: T => String = func.andThen(_.orNull)
  }

  implicit class funcToCharSequenceOptOrNull[T](val func: T => Option[CharSequence]) {
    def orNull: T => CharSequence = func.andThen(_.orNull)
  }

  private def strToOpt[U: TypeMapping[*, (String, Option[String])]](t: U): Option[String] = {
    val mapping = TypeMapping.getMapping[TypeMapping[*, (String, Option[String])], U]
    mapping.input(t).fold(Option(_), identity)
  }

  private def csToOpt[U: TypeMapping[*, (CharSequence, Option[CharSequence])]](t: U): Option[CharSequence] = {
    val mapping = TypeMapping.getMapping[TypeMapping[*, (CharSequence, Option[CharSequence])], U]
    mapping.input(t).fold(Option(_), identity)
  }

  private def tranCharSeqOptFunc(seq: Seq[Option[Char]]): Seq[Char] = seq.filter(_.isDefined).map(_.get)

  private def tranCharSeqSeqOptFunc(seq: Seq[Option[CharSequence]]): Seq[CharSequence] = seq.map(_.orNull)
}

/** TODO
  *
  * @author
  *   mars
  * @version 1.0.0
  * @since 2022/08/28
  *   21:04
  */
class StringCommons[T: TypeMapping[*, (String, Option[String])]](value: T) {

  import helper._
  import privateUtils._

  @inline private def strOpt: Option[String] = {
    val toStrOpt = getMapper[T, Option[String]].func
    toStrOpt(value)
  }
  @inline private def strOrNull: String = {
    val mapping = TypeMapping.getMapping[TypeMapping[*, (String, Option[String])], T]
    mapping.input(value).fold(identity, _.orNull)
  }

  /** * <p>Abbreviates a String using ellipses. This will turn "Now is the time for all good men" into "Now is the time for..."</p>
    *
    * {@code str} is less than or equal to {@code maxWidth}, return {@code str}.</li> <li>Else abbreviate it to {@code (substring(str, 0,
    * max-3) + "...")}.</li> <li>If {@code maxWidth} is less than {@code 4}, throw an {@code IllegalArgumentException}.</li> <li>In no case
    * will it return a String of length greater than {@code maxWidth}.</li> </ul>
    *
    * <pre>
    *
    * None.ops.abbreviate(*) = None
    *
    * None.abbreviate(4) = Some("")
    *
    * Some("abcdefg").ops.abbreviate(6) = Some("abc...")
    *
    * Some("abcdefg").ops.abbreviate(7) = Some("abcdefg")
    *
    * Some("abcdefg").ops.abbreviate(8) = Some("abcdefg")
    *
    * Some("abcdefg").ops.abbreviate(4) = Some("a...")
    *
    * Some("abcdefg").ops.abbreviate(3) = IllegalArgumentException
    *
    * </pre>
    *
    * @param maxWidth
    *   maximum length of result String, must be at least 4
    * @return
    *   abbreviated String, {@code None} if None input
    * @throws IllegalArgumentException
    *   if the width is too small
    */
  @inline def abbreviate(maxWidth: Int): Option[String] = Option(Strings.abbreviate(strOrNull, maxWidth))

  /** <p>Abbreviates a String using ellipses. This will turn "Now is the time for all good men" into "...is the time for..."</p>
    *
    * <p>Works like {@code Option(String).abbreviate(int)}, but allows you to specify a "left edge" offset. Note that this left edge is not
    * necessarily going to be the leftmost character in the result, or the first character following the ellipses, but it will appear
    * somewhere in the result.
    *
    * <p>In no case will it return a String of length greater than {@code maxWidth}.</p>
    *
    * <pre>
    *
    * None.ops.abbreviate(*, *) = None
    *
    * Some("").ops.abbreviate(0, 4) = Some("")
    *
    * Some("abcdefghijklmno").ops.abbreviate(-1, 10) = Some("abcdefg...")
    *
    * Some("abcdefghijklmno").ops.abbreviate(0, 10) = Some("abcdefg...")
    *
    * Some("abcdefghijklmno").ops.abbreviate(1, 10) = Some("abcdefg...")
    *
    * Some("abcdefghijklmno").ops.abbreviate(4, 10) = Some("abcdefg...")
    *
    * Some("abcdefghijklmno").ops.abbreviate(5, 10) = Some("...fghi...")
    *
    * Some("abcdefghijklmno").ops.abbreviate(6, 10) = Some("...ghij...")
    *
    * Some("abcdefghijklmno").ops.abbreviate(8, 10) = Some("...ijklmno")
    *
    * Some("abcdefghijklmno").ops.abbreviate(10, 10) = Some("...ijklmno")
    *
    * Some("abcdefghijklmno").ops.abbreviate(12, 10) = Some("...ijklmno")
    *
    * Some("abcdefghij").ops.abbreviate(0, 3) = IllegalArgumentException
    *
    * Some("abcdefghij").ops.abbreviate(5, 6) = IllegalArgumentException
    *
    * </pre>
    *
    * @param offset
    *   left edge of source String
    * @param maxWidth
    *   maximum length of result String, must be at least 4
    * @return
    *   abbreviated String Option, {@code None} if None input
    * @throws IllegalArgumentException
    *   if the width is too small
    */
  @inline def abbreviate(offset: Int, maxWidth: Int): Option[String] = Option(Strings.abbreviate(strOrNull, offset, maxWidth))

  /** <p>Abbreviates a String using another given String as replacement marker. This will turn "Now is the time for all good men" into "Now
    * is the time for..." if "..." was defined as the replacement marker.</p>
    *
    * <p>Specifically:</p> <ul> <li>If the number of characters in {@code str} is less than or equal to {@code maxWidth}, return {@code
    * str}.</li> <li>Else abbreviate it to {@code (substring(str, 0, max-abbrevMarker.length) + abbrevMarker)}.</li> <li>If {@code maxWidth}
    * is less than {@code abbrevMarker.length + 1}, throw an {@code IllegalArgumentException}.</li> <li>In no case will it return a String
    * of length greater than {@code maxWidth}.</li> </ul>
    *
    * <pre>
    *
    * None.ops.abbreviate(Some("..."), *) = None
    *
    * Some("abcdefg")ops..abbreviate(None, *) = Some("abcdefg")
    *
    * Some("")ops..abbreviate(Some("..."), 4) = Some("")
    *
    * Some("abcdefg").ops.abbreviate(Some("."), 5) = Some("abcd.")
    *
    * Some("abcdefg").ops.abbreviate(Some("."), 7) = Some("abcdefg")
    *
    * Some("abcdefg").ops.abbreviate(Some("."), 8) = Some("abcdefg")
    *
    * Some("abcdefg").ops.abbreviate(Some(".."), 4) = Some("ab..")
    *
    * Some("abcdefg").ops.abbreviate(Some(".."), 3) = Some("a..")
    *
    * Some("abcdefg").ops.abbreviate(Some(".."), 2) = IllegalArgumentException
    *
    * Some("abcdefg").abbreviate(Some("..."), 3) = IllegalArgumentException
    *
    * </pre>
    *
    * @param abbrevMarker
    *   the String used as replacement marker
    * @param maxWidth
    *   maximum length of result String, must be at least {@code abbrevMarker.length + 1}
    * @tparam Abb
    *   String or Option[String]
    * @return
    *   abbreviated String Option, {@code None} if None input
    * @throws IllegalArgumentException
    *   if the width is too small
    */
  def abbreviate[Abb: TypeMapping[*, (String, Option[String])]](abbrevMarker: Abb, maxWidth: Int): Option[String] = {
    val toStrOpt = getMapper[Abb, Option[String]].func.orNull
    Option(Strings.abbreviate(strOrNull, toStrOpt(abbrevMarker), maxWidth))
  }

  /** <p>Abbreviates a String using a given replacement marker. This will turn "Now is the time for all good men" into "...is the time
    * for..." if "..." was defined as the replacement marker.</p>
    *
    * <p>Works like {@code abbreviate(String, String, int)}, but allows you to specify a "left edge" offset. Note that this left edge is not
    * necessarily going to be the leftmost character in the result, or the first character following the replacement marker, but it will
    * appear somewhere in the result.
    *
    * <p>In no case will it return a String of length greater than {@code maxWidth}.</p>
    *
    * <pre>
    *
    * None.ops.abbreviate(None, *, *) = None
    *
    * Some("abcdefghijklmno").ops.abbreviate(None, *, *) = Some("abcdefghijklmno")
    *
    * Some("").ops.abbreviate("...", 0, 4) = Some("")
    *
    * Some("abcdefghijklmno").ops.abbreviate("---", -1, 10) = Some("abcdefg---")
    *
    * Some("abcdefghijklmno").ops.abbreviate(",", 0, 10) = Some("abcdefghi,")
    *
    * Some("abcdefghijklmno").ops.abbreviate(",", 1, 10) = Some("abcdefghi,")
    *
    * Some("abcdefghijklmno").ops.abbreviate(",", 2, 10) = Some("abcdefghi,")
    *
    * Some("abcdefghijklmno").ops.abbreviate("::", 4, 10) = Some("::efghij::")
    *
    * Some("abcdefghijklmno").ops.abbreviate("...", 6, 10) = Some("...ghij...")
    *
    * Some("abcdefghijklmno").ops.abbreviate("*", 9, 10) = Some("*ghijklmno")
    *
    * Some("abcdefghijklmno").ops.abbreviate("'", 10, 10) = Some("'ghijklmno")
    *
    * Some("abcdefghijklmno").ops.abbreviate("!", 12, 10) = Some("!ghijklmno")
    *
    * Some("abcdefghij").ops.abbreviate("abra", 0, 4) = IllegalArgumentException
    *
    * Some("abcdefghij").ops.abbreviate("...", 5, 6) = IllegalArgumentException
    *
    * </pre>
    *
    * @param abbrevMarker
    *   the String used as replacement marker
    * @param offset
    *   left edge of source String
    * @param maxWidth
    *   maximum length of result String, must be at least 4
    * @tparam Abb
    *   String or Option[String]
    * @return
    *   abbreviated String Option, {@code None} if None input
    * @throws IllegalArgumentException
    *   if the width is too small
    */
  def abbreviate[Abb: TypeMapping[*, (String, Option[String])]](abbrevMarker: Abb, offset: Int, maxWidth: Int): Option[String] = {
    val toStrOpt = getMapper[Abb, Option[String]].func.orNull
    Option(Strings.abbreviate(strOrNull, toStrOpt(abbrevMarker), offset, maxWidth))
  }

  /** <p>Abbreviates a String to the length passed, replacing the middle characters with the supplied replacement String.</p>
    *
    * <p>This abbreviation only occurs if the following criteria is met:</p> <ul> <li>Neither the String for abbreviation nor the
    * replacement String are null or empty </li> <li>The length to truncate to is less than the length of the supplied String</li> <li>The
    * length to truncate to is greater than 0</li> <li>The abbreviated String will have enough room for the length supplied replacement
    * String and the first and last characters of the supplied String for abbreviation</li> </ul> <p>Otherwise, the returned String will be
    * the same as the supplied String for abbreviation. </p>
    *
    * <pre>
    *
    * None.ops.abbreviateMiddle(None, 0) = None
    *
    * Some("abc").ops.abbreviateMiddle(None, 0) = Some("abc")
    *
    * Some("abc")ops..abbreviateMiddle(".", 0) = Some("abc")
    *
    * Some("abc").ops.abbreviateMiddle(".", 3) = Some("abc")
    *
    * Some("abcdef").ops.abbreviateMiddle(".", 4) = Some("ab.f")
    *
    * </pre>
    *
    * @param middle
    *   the String to replace the middle characters with, may be null
    * @param length
    *   the length to abbreviate {@code str} to.
    * @tparam M
    *   String or Option[String]
    * @return
    *   the abbreviated String if the above criteria is met, or the original String supplied for abbreviation.
    */
  def abbreviateMiddle[M: TypeMapping[*, (String, Option[String])]](middle: M, length: Int): Option[String] = {
    val toStrOpt = getMapper[M, Option[String]].func.orNull
    Option(Strings.abbreviateMiddle(strOrNull, toStrOpt(middle), length))
  }

  /** Appends the suffix to the end of the string if the string does not already end with the suffix.
    *
    * @param suffix
    *   The suffix to append to the end of the string.
    * @param suffixes
    *   Indicates whether the compare should ignore case.
    * @tparam S
    *   String Or Option
    * @return
    *   A new Option[String] if suffix was appended, the same string otherwise.
    */
  def appendIfMissing[S: TypeMapping[*, (String, Option[String])], SS: VarArgsOfCharSequence](suffix: S, suffixes: SS*): Option[String] = {
    val toStrOpt = getMapper[S, Option[String]].func.orNull

    if (suffixes == null) {
      return Option(Strings.appendIfMissing(strOrNull, toStrOpt(suffix)))
    }

    def mapping: VarArgsOfCharSequence[SS] = TypeMapping.getMapping[VarArgsOfCharSequence, SS]

    val sfs = mapping
      .input(suffixes)
      .fold(
        identity,
        oss => oss.map(_.orNull)
      )

    Option(Strings.appendIfMissing(strOrNull, toStrOpt(suffix), sfs: _*))
  }
  // helpers for method call without suffixes
  def appendIfMissing(suffix: CharSequence): Option[String]         = Option(Strings.appendIfMissing(strOrNull, suffix))
  def appendIfMissing(suffix: Option[CharSequence]): Option[String] = Option(Strings.appendIfMissing(strOrNull, suffix.orNull))

  /** Appends the suffix to the end of the string if the string does not already end with any of the suffixes.
    *
    * <pre>
    *
    * None.ops.appendIfMissing(None) = None
    *
    * Some("abc").ops.appendIfMissing(None) = Some("abc")
    *
    * Some("").ops.appendIfMissing("xyz") = Some("xyz")
    *
    * Some("abc").ops.appendIfMissing("xyz") = Some("abcxyz")
    *
    * Some("abcxyz").ops.appendIfMissing("xyz") = Some("abcxyz")
    *
    * Some("abcXYZ").ops.appendIfMissing("xyz") = Some("abcXYZxyz")
    *
    * </pre>
    *
    * <p>With additional suffixes,</p>
    *
    * <pre>
    *
    * None.ops.appendIfMissing(None, None) = None
    *
    * Some("abc").ops.appendIfMissing(null, null) = Some("abc")
    *
    * Some("").ops.appendIfMissing("xyz", null) = Some("xyz")
    *
    * Some("abc").ops.appendIfMissing(Some("xyz"), new CharSequence[]{null}) = Some("abcxyz")
    *
    * Some("abc").ops.appendIfMissing("xyz", "") = Some("abc")
    *
    * Some("abc").ops.appendIfMissing("xyz", "mno") = Some("abcxyz")
    *
    * Some("abcxyz").ops.appendIfMissing("xyz", "mno") = Some("abcxyz")
    *
    * Some("abcmno").ops.appendIfMissing("xyz", "mno") = Some("abcmno")
    *
    * Some("abcXYZ").ops.appendIfMissing("xyz", "mno") = Some("abcXYZxyz")
    *
    * Some("abcMNO").ops.appendIfMissing("xyz", "mno") = Some("abcMNOxyz")
    *
    * </pre>
    *
    * @param suffix
    *   The suffix to append to the end of the string.
    * @param suffixes
    *   Additional suffixes that are valid terminators.
    * @tparam S
    *   String Or Option[String]
    * @return
    *   A new String if suffix was appended, the same string otherwise.
    */
  def appendIfMissingIgnoreCase[S: TypeMapping[*, (String, Option[String])], SS: VarArgsOfCharSequence](
    suffix: S,
    suffixes: SS*
  ): Option[String] = {
    val toStrOpt = getMapper[S, Option[String]].func.orNull

    if (suffixes == null) {
      return Option(Strings.appendIfMissingIgnoreCase(strOrNull, toStrOpt(suffix)))
    }

    def mapping: VarArgsOfCharSequence[SS] = TypeMapping.getMapping[VarArgsOfCharSequence, SS]

    val sfs = mapping
      .input(suffixes)
      .fold(
        identity,
        oss => oss.map(_.orNull)
      )

    Option(Strings.appendIfMissingIgnoreCase(strOrNull, toStrOpt(suffix), sfs: _*))
  }

  // helpers for method call without suffixes
  def appendIfMissingIgnoreCase(suffix: CharSequence): Option[String] =
    Option(Strings.appendIfMissingIgnoreCase(strOrNull, suffix))

  def appendIfMissingIgnoreCase(suffix: Option[CharSequence]): Option[String] =
    Option(Strings.appendIfMissingIgnoreCase(strOrNull, suffix.orNull))

  /** <p>Capitalizes a String changing the first character to title case as per {@link Character# toTitleCase ( int )}. No other characters
    * are changed.</p>
    *
    * <p>For a word based algorithm, see {@link org.apache.commons.lang3.text.WordUtils# capitalize ( String )}. A {@code null} input String
    * returns {@code null}.</p>
    *
    * <pre> None.capitalize = None Some("").capitalize = Some("") Some("cat").capitalize = "Cat" Some("cAt") .capitalize = "CAt"
    * Some("'cat'").capitalize = "'cat'" </pre>
    *
    * @return
    *   the capitalized String, {@code null} if null String input
    * @see
    *   #uncapitalize(String)
    */
  @inline def capitalize: Option[String] = Option(Strings.capitalize(strOrNull))

  /** <p>Centers a String in a larger String of size {@code size} using the space character (' ').</p>
    *
    * <p>If the size is less than the String length, the original String is returned. A {@code null} String returns {@code null}. A negative
    * size is treated as zero.</p>
    *
    * <p>Equivalent to {@code center(str, size, " ")}.</p>
    *
    * <pre>
    *
    * None.ops.center(*) = None
    *
    * Some("").ops.center(4) = Some(" ")
    *
    * Some("ab").ops.center(-1) = Some("ab")
    *
    * Some("ab").ops.center(4) = Some(" ab ")
    *
    * Some("abcd").ops.center(2) = Some("abcd")
    *
    * Some("a").ops.center(4) = Some(" a ")
    *
    * </pre>
    *
    * @param size
    *   the int size of new String, negative treated as zero
    * @return
    *   centered Option[String], {@code None} if None input
    */
  @inline def center(size: Int): Option[String] = Option(Strings.center(strOrNull, size))

  /** <p>Centers a String in a larger String of size {@code size}. Uses a supplied character as the value to pad the String with.</p>
    *
    * <p>If the size is less than the String length, the String is returned. A {@code null} String returns {@code null}. A negative size is
    * treated as zero.</p>
    *
    * <pre>
    *
    * None.ops.center(*, *) = None
    *
    * Some("").ops.center(4, ' ') = Some(" ")
    *
    * Some("ab").ops.center(-1, ' ') = Some("ab")
    *
    * Some("ab").ops.center(4, ' ') = Some(" ab ")
    *
    * Some("abcd").ops.center(2, ' ') = Some("abcd")
    *
    * Some("a").ops.center(4, ' ') = Some(" a ")
    *
    * Some("a").ops.center(4, 'y') = Some("yayy")
    *
    * </pre>
    *
    * @param size
    *   the int size of new String, negative treated as zero
    * @param padChar
    *   the character to pad the new String with
    * @return
    *   centered String, {@code null} if null String input
    */
  @inline def center(size: Int, padChar: Char): Option[String] = Option(Strings.center(strOrNull, size, padChar))

  /** <p>Centers a String in a larger String of size {@code size}. Uses a supplied character as the value to pad the String with.</p>
    *
    * <p>If the size is less than the String length, the String is returned. A {@code null} String returns {@code null}. A negative size is
    * treated as zero.</p>
    *
    * <pre> None.center(*, *) = None Some("").center(4, " ") = Some(" ") Some("ab").center(-1, " ") = Some("ab") Some("ab").center(4, " ") =
    * Some(" ab ") Some("abcd").center(2, " ") = Some("abcd") Some("a").center(4, " ") = Some(" a ") Some("a").center(4, "yz") =
    * Some("yayz") Some("abc").center(7, None) = Some(" abc ") Some("abc").center(7, "") = Some(" abc ") </pre>
    *
    * @param size
    *   the int size of new String, negative treated as zero
    * @param padStr
    *   the String to pad the new String with, must not be null or empty
    * @tparam P
    *   String or Option[String]
    * @return
    *   centered String, {@code None} if None input
    */
  def center[P: TypeMapping[*, (String, Option[String])]](size: Int, padStr: P): Option[String] = {
    val toStrOpt = getMapper[P, Option[String]].func.orNull
    Option(Strings.center(strOrNull, size, toStrOpt(padStr)))
  }

  /** <p>Removes one newline from end of a String if it's there, otherwise leave it alone. A newline is &quot;{@code \n}&quot;, &quot;{@code
    * \r}&quot;, or &quot;{@code \r\n}&quot;.</p>
    *
    * <p>NOTE: This method changed in 2.0. It now more closely matches Perl chomp.</p>
    *
    * <pre>
    *
    * None.ops.chomp = None
    *
    * Some("").ops.chomp = Some("")
    *
    * Some("abc \r").ops.chomp = Some("abc ")
    *
    * Some("abc\n").ops.chomp = Some("abc")
    *
    * Some("abc\r\n").ops.chomp = Some("abc")
    *
    * Some("abc\r\n\r\n").ops.chomp = Some("abc\r\n")
    *
    * Some("abc\n\r").ops.chomp = Some("abc\n")
    *
    * Some("abc\n\rabc").ops.chomp = Some("abc\n\rabc")
    *
    * Some("\r").ops.chomp = Some("")
    *
    * Some("\n").ops.chomp = Some("")
    *
    * Some("\r\n").ops.chomp = Some("")
    *
    * </pre>
    *
    * @return
    *   Option[String] without newline, {@code None} if null String input
    */
  def chomp: Option[String] = Option(Strings.chomp(strOrNull))

  /** <p>Remove the last character from a String.</p>
    *
    * <p>If the String ends in {@code \r\n}, then remove both of them.</p>
    *
    * <pre>
    *
    * None.ops.chop = None
    *
    * Some("").ops.chop = Some("")
    *
    * Some("abc \r").ops.chop = Some("abc ")
    *
    * Some("abc\n").ops.chop = Some("abc")
    *
    * Some("abc\r\n").ops.chop = Some("abc")
    *
    * Some("abc").ops.chop = Some("ab")
    *
    * Some("abc\nabc").ops.chop = Some("abc\nab")
    *
    * Some("a").ops.chop = Some("")
    *
    * Some("\r").ops.chop = Some("")
    *
    * Some("\n").ops.chop = Some("")
    *
    * Some("\r\n").ops.chop = Some("")
    *
    * </pre>
    *
    * @return
    *   String without last character, {@code None} if None String input
    */
  def chop: Option[String] = Option(Strings.chop(strOrNull))

  /** <p>Compare two Strings lexicographically, as per {@link String# compareTo ( String )}, returning :</p> <ul> <li>{@code int = 0}, if
    * {@code str1} is equal to {@code str2} (or both {@code null})</li> <li>{@code int < 0}, if {@code str1} is less than {@code str2}</li>
    * <li>{@code int > 0}, if {@code str1} is greater than {@code str2}</li> </ul>
    *
    * <p>This is a {@code null} safe version of :</p> <blockquote><pre>str1.compareTo(str2)</pre></blockquote>
    *
    * <p>{@code null} value is considered less than non-{@code null} value. Two {@code null} references are considered equal.</p>
    *
    * <pre>
    *
    * None.ops.compare(null) = 0
    *
    * None.ops.compare("a") &lt; 0
    *
    * Some("a")).ops.compare(null) &gt; 0
    *
    * "abc".ops.compare("abc") = 0
    *
    * "a".ops.compare("b") &lt; 0
    *
    * "b".ops.compare("a") &gt; 0
    *
    * "a".ops.compare("B") &gt; 0
    *
    * "ab".ops.compare("abc") &lt; 0
    *
    * </pre>
    *
    * @param other
    *   the String to compare to
    * @tparam O
    *   String Or Option[String]
    * @return
    *   &lt; 0, 0, &gt; 0, if {@code this} is respectively less, equal or greater than {@code other}
    */
  def compare[O: TypeMapping[*, (String, Option[String])]](other: O): Int = {
    val toStrOpt = getMapper[O, Option[String]].func.orNull
    Strings.compare(strOrNull, toStrOpt(other))
  }

  /** <p>Compare two Strings lexicographically, as per {@link String# compareTo ( String )}, returning :</p> <ul> <li>{@code int = 0}, if
    * {@code str1} is equal to {@code str2} (or both {@code null})</li> <li>{@code int < 0}, if {@code str1} is less than {@code str2}</li>
    * <li>{@code int > 0}, if {@code str1} is greater than {@code str2}</li> </ul>
    *
    * <p>This is a {@code null} safe version of :</p> <blockquote><pre>str1.compareTo(str2)</pre></blockquote>
    *
    * <p>{@code null} inputs are handled according to the {@code nullIsLess} parameter. Two {@code null} references are considered
    * equal.</p>
    *
    * <pre>
    *
    * None.ops.compare(None, *) = 0
    *
    * None.ops.compare(Some("a"), true) &lt; 0
    *
    * None.ops.compare(Some("a"), false) &gt; 0
    *
    * Some("a").ops.compare(None, true) &gt; 0
    *
    * Some("a").ops.compare(None, false) &lt; 0
    *
    * Some("abc").ops.compare(Some("abc"), *) = 0
    *
    * Some("a").ops.compare(Some("b"), *) &lt; 0
    *
    * Some("b").ops.compare("a", *) &gt; 0
    *
    * Some("a").ops.compare("B", *) &gt; 0
    *
    * Some("ab").ops.compare("abc", *) &lt; 0
    *
    * </pre>
    *
    * @param other
    *   the String to compare to
    * @param nullIsNull
    *   whether consider {@code None} value less than non-{@code None} value
    * @tparam O
    *   String Or Option
    * @return
    *   &lt; 0, 0, &gt; 0, if {@code this} is respectively less, equal ou greater than {@code other}
    */
  def compare[O: TypeMapping[*, (String, Option[String])]](other: O, nullIsNull: Boolean): Int = {
    val toStrOpt = getMapper[O, Option[String]].func.orNull
    Strings.compare(strOrNull, toStrOpt(other), nullIsNull)
  }

  /** <p>Compare two Strings lexicographically, ignoring case differences, as per {@link String# compareToIgnoreCase ( String )}, returning
    * :</p> <ul> <li>{@code int = 0}, if {@code str1} is equal to {@code str2} (or both {@code null})</li> <li>{@code int < 0}, if {@code
    * str1} is less than {@code str2}</li> <li>{@code int > 0}, if {@code str1} is greater than {@code str2}</li> </ul>
    *
    * <p>This is a {@code null} safe version of :</p> <blockquote><pre>str1.compareToIgnoreCase(str2)</pre></blockquote>
    *
    * <p>{@code null} value is considered less than non-{@code null} value. Two {@code null} references are considered equal. Comparison is
    * case insensitive.</p>
    *
    * <pre>
    *
    * None.ops.compareToIgnoreCase(None) = 0
    *
    * None.ops.compareToIgnoreCase(None , "a") &lt; 0
    *
    * Some("a").ops.compareToIgnoreCase(None) &gt; 0
    *
    * Some("abc").ops.compareToIgnoreCase("abc") = 0
    *
    * Some("abc").ops.compareToIgnoreCase("ABC") = 0
    *
    * Some("a").ops.compareToIgnoreCase("b") &lt; 0
    *
    * Some("b").ops.compareToIgnoreCase("a") &gt; 0
    *
    * Some("a").ops.compareToIgnoreCase("B") &lt; 0
    *
    * Some("A").ops.compareToIgnoreCase("b") &lt; 0
    *
    * Some("ab").compareToIgnoreCase("ABC") &lt; 0
    *
    * </pre>
    *
    * @param other
    *   the String to compare to
    * @tparam O
    *   String or Option[String]
    * @return
    *   &lt; 0, 0, &gt; 0, if {@code this} is respectively less, equal ou greater than {@code other}, ignoring case differences.
    */
  def compareIgnoreCase[O: TypeMapping[*, (String, Option[String])]](other: O): Int = {
    val toStrOpt = getMapper[O, Option[String]].func.orNull
    Strings.compareIgnoreCase(strOrNull, toStrOpt(other))
  }

  /** <p>Compare two Strings lexicographically, ignoring case differences, as per {@link String# compareToIgnoreCase ( String )}, returning
    * :</p> <ul> <li>{@code int = 0}, if {@code str1} is equal to {@code str2} (or both {@code null})</li> <li>{@code int < 0}, if {@code
    * str1} is less than {@code str2}</li> <li>{@code int > 0}, if {@code str1} is greater than {@code str2}</li> </ul>
    *
    * <p>This is a {@code null} safe version of :</p> <blockquote><pre>str1.compareToIgnoreCase(str2)</pre></blockquote>
    *
    * <p>{@code None} inputs are handled according to the {@code nullIsLess} parameter. Two {@code null} references are considered equal.
    * Comparison is case insensitive.</p>
    *
    * <pre>
    *
    * None.ops.compareToIgnoreCase(null, *) = 0
    *
    * None.ops.compareToIgnoreCase("a", true) &lt; 0
    *
    * None.ops.compareToIgnoreCase("a", false) &gt; 0
    *
    * "a".ops.compareToIgnoreCase(null, true) &gt; 0
    *
    * Some("a").ops.compareToIgnoreCase(None, false) &lt; 0
    *
    * "abc".ops.compareToIgnoreCase(Some("abc"), *) = 0
    *
    * "abc".compareToIgnoreCase("ABC", *) = 0
    *
    * "a".ops.compareToIgnoreCase("b", *) &lt; 0
    *
    * StringUtils.compareIgnoreCase("b", "a", *) &gt; 0 StringUtils.compareIgnoreCase("a", "B", *) &lt; 0 StringUtils.compareIgnoreCase("A",
    * "b", *) &lt; 0 StringUtils.compareIgnoreCase("ab", "abc", *) &lt; 0 </pre>
    *
    * @param other
    *   the String to compare to
    * @param nullIsLess
    *   whether consider {@code None} value less than non-{@code None} value
    * @tparam O
    *   String or Option[String]
    * @return
    *   &lt; 0, 0, &gt; 0, if {@code this} is respectively less, equal ou greater than {@code other}, ignoring case differences.
    */
  def compareIgnoreCase[O: TypeMapping[*, (String, Option[String])]](other: O, nullIsLess: Boolean): Int = {
    val toStrOpt = getMapper[O, Option[String]].func.orNull
    Strings.compareIgnoreCase(strOrNull, toStrOpt(other), nullIsLess)
  }

  /** * <p>Checks if CharSequence contains a search CharSequence, handling {@code null}. This method uses {@link String# indexOf ( String )}
    * if possible.</p>
    *
    * <p>A {@code null} CharSequence will return {@code false}.</p>
    *
    * <pre>
    *
    * None.ops.contains(*) = false
    *
    * *.ops.contains(None) = false
    *
    * Some("").ops.contains("") = true
    *
    * Some("abc").ops.contains("") = true
    *
    * Some("abc").ops.contains("a") = true
    *
    * Some("abc").ops.contains("z") = false
    *
    * </pre>
    *
    * @param searchSeq
    *   the CharSequence to find, may be null
    * @tparam To
    *   String Or Option[String]
    * @return
    *   true if the CharSequence contains the search CharSequence,
    */
  def contains[To: TypeMapping[*, (String, Option[String])]](searchSeq: To): Boolean = {
    val toStrOpt = getMapper[To, Option[String]].func.orNull
    val str1     = toStrOpt(searchSeq)
    Strings.contains(strOrNull, str1)
  }

  /** <p>Checks if CharSequence contains a search character, handling {@code null}. This method uses {@link String# indexOf ( int )} if
    * possible.</p>
    *
    * <p>A {@code null} or empty ("") CharSequence will return {@code false}.</p>
    *
    * <pre>
    *
    * None.ops.contains(*) = false
    *
    * Some("").ops.contains(*) = false
    *
    * Some("abc").ops.contains('a') = true
    *
    * Some("abc").ops.contains('z') = false
    *
    * </pre>
    *
    * @param searchChar
    *   the character to find
    * @return
    *   true if the CharSequence contains the search character,
    */
  @inline def contains(searchChar: Char): Boolean = Strings.contains(strOrNull, searchChar)

  /** <p> Checks if the CharSequence contains any character or string in the given set of characters. </p>
    *
    * <p> A {@code None} CharSequence will return {@code false}. A {@code null} search CharSequence will return {@code false}. </p>
    *
    * <pre>
    *
    * * <pre>
    *
    * None.ops.containsAny(*) = false
    *
    * Some("").ops.containsAny(*) = false
    *
    * Option(*).ops.containsAny(None) = false
    *
    * Option(*).ops.containsAny([]) = false
    *
    * Some("zzabyycdxx").ops.containsAny(['z', 'a']) = true
    *
    * Some("zzabyycdxx").ops.containsAny(['b', 'y']) = true
    *
    * Some("zzabyycdxx").ops.containsAny(['z', 'y']) = true
    *
    * Some("aba").ops.containsAny(['z']) = false
    *
    * </pre>
    *
    * None.ops.containsAny(*) = false
    *
    * Some("").ops.containsAny(*) = false
    *
    * Option(*).ops.containsAny(None) = false
    *
    * Option(*).ops.containsAny("") = false
    *
    * Some("zzabyycdxx").ops.containAny("za") = true
    *
    * Some("zzabyycdxx").ops.containAny("by") = true
    *
    * Some("zzabyycdxx").ops.containAny("zy") = true
    *
    * Some("zzabyycdxx").containAny("\tx") = true
    *
    * Some("zzabyycdxx").containAny("$.#yF") = true
    *
    * Some("aba").containAny("z") = false
    *
    * </pre>
    *
    * @param searchChars
    *   the chars to search for, may be null
    * @tparam S
    *   String or Option[String]
    * @return
    *   the {@code true} if any of the chars are found, {@code false} if no match or null input
    */
  def containsAny[S: VarArgsOfCharOrString](searchArgs: S*): Boolean = {
    def dealWithSeqChar(chars: Seq[Char]): Boolean = Strings.containsAny(strOrNull, chars.toArray[Char]: _*)

    def dealWithSeqCharSequence(css: Seq[CharSequence]): Boolean =
      if (css.length == 1) {
        Strings.containsAny(strOrNull, css.head)
      } else {
        Strings.containsAny(strOrNull, css: _*)
      }

    def mapping             = TypeMapping.getMapping[VarArgsOfCharOrString, S]
    def charOptSeqMapper    = getMapper[Seq[Option[Char]], Seq[Char]].func
    def charSeqOptSeqMapper = getMapper[Seq[Option[CharSequence]], Seq[CharSequence]].func

    if (searchArgs == null) {
      Strings.containsAny(strOrNull, null)
    } else {
      mapping
        .input(searchArgs)
        .fold(
          dealWithSeqChar,
          dealWithSeqCharSequence,
          charOptSeqMapper.andThen(dealWithSeqChar),
          charSeqOptSeqMapper.andThen(dealWithSeqCharSequence)
        )
    }
  }

  /** <p> Checks if the CharSequence contains any of the CharSequences in the given array, ignoring case. </p>
    *
    * <p>
    *
    * A {@code null} {@code cs} CharSequence will return {@code false}.
    *
    * A {@code null} or zero length search array will return {@code false}.
    *
    * </p>
    *
    * <pre>
    *
    * None.containsAnyIgnoreCase(, *) = false
    *
    * "".ops.containsAnyIgnoreCase(*) = false
    *
    * Some(*).ops.containsAnyIgnoreCase(null) = false
    *
    * *.ops.containsAnyIgnoreCase([]) = false
    *
    * "abcd"ops.containsAnyIgnoreCase("ab", null) = true
    *
    * Some("abcd").ops.containsAnyIgnoreCase(Some("ab"), Some("cd")) = true
    *
    * "abc".ops.containsAnyIgnoreCase("d", "abc") = true
    *
    * "abc".ops.containsAnyIgnoreCase("D", "ABC") = true
    *
    * "ABC".ops.containsAnyIgnoreCase( "d", "abc") = true
    *
    * </pre> None.containsIgnoreCase(*) = false
    *
    * Option(*).ops.containsIgnoreCase(None) = false
    *
    * "".ops.containsIgnoreCase "") = true
    *
    * StringUtils.containsIgnoreCase("abc", "") = true
    *
    * StringUtils.containsIgnoreCase("abc", "a") = true
    *
    * StringUtils.containsIgnoreCase("abc", "z") = false
    *
    * StringUtils.containsIgnoreCase("abc", "A") = true
    *
    * StringUtils.containsIgnoreCase("abc", "Z") = false
    *
    * @param searchArgs
    *   The array of CharSequences to search for, may be null. Individual CharSequences may be null as well.
    * @param tt
    *   classTag
    * @tparam S
    *   String Or Option[String]
    * @return
    *   {@code true} if any of the search CharSequences are found, {@code false} otherwise
    */
  def containsAnyIgnoreCase[S: VarArgsOfCharSequence](searchArgs: S*): Boolean = {
    def dealWithSeqCharSeq(strs: Seq[CharSequence]) = Strings.containsAnyIgnoreCase(strOrNull, strs: _*)
    def mapping                                     = TypeMapping.getMapping[VarArgsOfCharSequence, S]
    def charSeqSeqOptMappper                        = getMapper[Seq[Option[CharSequence]], Seq[CharSequence]].func

    if (searchArgs == null) {
      Strings.equalsAnyIgnoreCase(strOrNull, null)
    } else {
      mapping
        .input(searchArgs)
        .fold(
          dealWithSeqCharSeq,
          charSeqSeqOptMappper.andThen(dealWithSeqCharSeq)
        )
    }
  }

  /** <p>Checks if CharSequence contains a search CharSequence irrespective of case, handling {@code null}. Case-insensitivity is defined as
    * by {@link String# equalsIgnoreCase ( String )}.
    *
    * <p>A {@code null} CharSequence will return {@code false}.</p>
    *
    * <pre>
    *
    * None.ops.containsIgnoreCase(*) = false
    *
    * Option(*).ops.containsIgnoreCase(null) = false
    *
    * Some("").ops.containsIgnoreCase("") = true
    *
    * "abc".ops.containsIgnoreCase(Some("")) = true
    *
    * StringUtils.containsIgnoreCase("abc", "a") = true
    *
    * StringUtils.containsIgnoreCase("abc", "z") = false
    *
    * StringUtils.containsIgnoreCase("abc", "A") = true
    *
    * StringUtils.containsIgnoreCase("abc", "Z") = false
    *
    * </pre>
    *
    * @param searchStr
    *   the CharSequence to find, may be null
    * @tparam S
    *   String Or Option[String]
    * @return
    *   true if the CharSequence contains the search CharSequence irrespective of case or false if not or {@code null} string input
    */
  def containsIgnoreCase[S: TypeMapping[*, (String, Option[String])]](searchStr: S): Boolean = {
    val toStrOrNull = getMapper[S, Option[String]].func.orNull
    Strings.containsIgnoreCase(strOrNull, toStrOrNull(searchStr))
  }

  /** <p> Checks that the CharSequence does not contain certain characters. </p>
    *
    * <p>
    *
    * A {@code None} CharSequence will return {@code true}. A {@code null} invalid character array will return {@code true}. An empty String
    * ("") always returns true.</p>
    *
    * <pre>
    *
    * None.ops.containsNone(*) = true
    *
    * Option(*).ops.containsNone(null) = true
    *
    * Some("").ops.containsNone(*) = true
    *
    * "ab".ops.containsNone("") = true
    *
    * Some("abab").ops.containsNone("xyz") = true
    *
    * "ab1".ops.containsNone("xyz") = true
    *
    * "abz".ops.containsNone("xyz") = false
    *
    * </pre>
    *
    * @param invalidChars
    *   a String of invalid chars, may be null
    * @return
    *   true if it contains none of the invalid chars, or is null
    */
  @inline def containsNone(invalidChars: String): Boolean = Strings.containsNone(strOrNull, invalidChars)

  /** <p> Checks that the CharSequence does not contain certain option characters. </p>
    *
    * <p>
    *
    * A {@code None} CharSequence will return {@code true}. A {@code None} invalid character array will return {@code true}. An empty String
    * (Some("")) always returns true.</p>
    *
    * <pre>
    *
    * None.ops.containsNone(*) = true
    *
    * Option(*).ops.containsNone(None) = true
    *
    * Some("").ops.containsNone(*) = true
    *
    * "ab".ops.containsNone(Some("")) = true
    *
    * Some("abab").ops.containsNone(Some("xyz")) = true
    *
    * "ab1".ops.containsNone(Some("xyz")) = true
    *
    * "abz".ops.containsNone(Some("xyz")) = false
    *
    * </pre>
    *
    * @param invalidChars
    *   a String of invalid chars, may be null
    * @return
    *   true if it contains none of the invalid chars, or is null
    */
  @inline def containsNone(invalidChars: Option[String]): Boolean = Strings.containsNone(strOrNull, invalidChars.orNull)

  /** <p>Checks that the CharSequence does not contain certain characters.</p>
    *
    * <p>A {@code null} CharSequence will return {@code true}. A {@code null} invalid character array will return {@code true}. An empty
    * CharSequence (length()=0) always returns true.</p>
    *
    * <pre>
    *
    * None.ops.containsNone(*) = true
    *
    * *.ops.containsNone(null) = true
    *
    * "".ops.containsNone(*) = true
    *
    * "ab".ops.containsNone('') = true
    *
    * "abab".ops.containsNone('xyz') = true
    *
    * "ab1".ops.containsNone('xyz') = true
    *
    * "abz".ops.containsNone('xyz') = false
    *
    * </pre>
    *
    * @param invalidChars
    *   an array of invalid chars, may be null
    * @tparam S
    *   var args of chars
    * @return
    *   true if it contains none of the invalid chars, or is null
    */
  def containsNone[I: VarArgsOfChar](invalidChars: I*): Boolean = {
    def dealWithSeqChar(chars: Seq[Char]): Boolean = Strings.containsNone(strOrNull, chars: _*)
    def charSeqMapper                              = getMapper[Seq[Option[Char]], Seq[Char]].func
    val mapping                                    = TypeMapping.getMapping[VarArgsOfChar, I]
    mapping.input(invalidChars).fold(dealWithSeqChar, charSeqMapper.andThen(dealWithSeqChar))
  }

  /** <p>Checks if the CharSequence contains only certain characters.</p>
    *
    * <p>A {@code null} CharSequence will return {@code false}. A {@code null} valid character String will return {@code false}. An empty
    * String (length()=0) always returns {@code true}.</p>
    *
    * <pre>
    *
    * None.ops.containsOnly(*) = false
    *
    * *.ops.containsOnly(null) = false
    *
    * "".ops.containsOnly(*) = true
    *
    * Some("ab").ops.containsOnly("") = false
    *
    * "abab".ops.containsOnly("abc") = true
    *
    * "ab1".ops.containsOnly("abc") = false
    *
    * "abz".ops.containsOnly("abc") = false
    *
    * </pre>
    *
    * @param validChars
    *   a String of valid chars, may be null
    * @return
    *   true if it only contains valid chars and is non-null
    */
  @inline def containsOnly(validChars: String): Boolean = Strings.containsOnly(strOrNull, validChars)

  /** <p>Checks if the CharSequence contains only certain option characters.</p>
    *
    * <p>A {@code None} CharSequence will return {@code false}. A {@code None} valid character String will return {@code false}. An empty
    * String (length()=0) always returns {@code true}.</p>
    *
    * <pre>
    *
    * None.ops.containsOnly(*) = false
    *
    * *.ops.containsOnly(None) = false
    *
    * "".ops.containsOnly(*) = true
    *
    * Some("ab").ops.containsOnly(Some("")) = false
    *
    * "abab".ops.containsOnly(Some("abc")) = true
    *
    * "ab1".ops.containsOnly(Some("abc")) = false
    *
    * "abz".ops.containsOnly(Some("abc")) = false
    *
    * </pre>
    *
    * @param validChars
    *   a Option String of valid chars, may be None
    * @return
    *   true if it only contains valid chars and is non-null
    */
  def containsOnly(validChars: Option[String]): Boolean = Strings.containsOnly(strOrNull, validChars.orNull)

  /** <p>Checks if the CharSequence contains only certain characters.</p>
    *
    * <p>A {@code null} CharSequence will return {@code false}. A {@code null} valid character array will return {@code false}. An empty
    * CharSequence (length()=0) always returns {@code true}.</p>
    *
    * <pre>
    *
    * None.ops.containsOnly(*) = false
    *
    * *.ops.containsOnly(null) = false
    *
    * Some("").containsOnly(*) = true
    *
    * "ab".ops.containsOnly('') = false
    *
    * Some("abab").ops.containsOnly('abc') = true
    *
    * "ab1".ops.containsOnly('abc') = false
    *
    * "abz".ops.containsOnly('abc') = false
    *
    * </pre>
    * @param valid
    *   an array of valid chars, may be null
    * @tparam V
    *   var args of char
    * @return
    *   true if it only contains valid chars and is non-null
    */
  def containsOnly[V: VarArgsOfChar](valid: V*): Boolean = {
    val mapping                           = TypeMapping.getMapping[VarArgsOfChar, V]
    val charSeqMapper                     = getMapper[Seq[Option[Char]], Seq[Char]].func
    def dealWithSeqChar(chars: Seq[Char]) = Strings.containsOnly(strOrNull, chars: _*)
    mapping.input(valid).fold(dealWithSeqChar, charSeqMapper.andThen(dealWithSeqChar))
  }

  /** <p>Check whether the given CharSequence contains any whitespace characters.</p>
    *
    * <p>Whitespace is defined by {@link Character# isWhitespace ( char )}.</p>
    *
    * @return
    *   if the CharSequence is not empty and contains at least 1 (breaking) whitespace character
    */
  def containsWhitespace: Boolean = Strings.containsWhitespace(strOrNull)

  def countMatches(ch: Char): Int = Strings.countMatches(strOrNull, ch)

  def countMatches[S: TypeMapping[*, (String, Option[String])]](sub: S): Int = {
    val mapper = getMapper[S, Option[String]].func.orNull
    val str1   = mapper(sub)
    Strings.countMatches(strOrNull, str1)
  }

  def defaultIfBlank[S: TypeMapping[*, (CharSequence, Option[CharSequence])]](defaultStr: S): CharSequence = {
    val mapper = getMapper[S, Option[CharSequence]].func.orNull
    val defStr = mapper(defaultStr)
    val result = Strings.defaultIfBlank(strOrNull, defStr)
    result
  }

  def defaultIfEmpty[S: TypeMapping[*, (CharSequence, Option[CharSequence])]](defaultStr: S): CharSequence = {
    val mapper = getMapper[S, Option[CharSequence]].func.orNull
    val str1   = mapper(defaultStr)
    val result = Strings.defaultIfEmpty(strOrNull, str1)
    result
  }

  def defaultString: String = Strings.defaultString(strOrNull)

  def defaultString[S: TypeMapping[*, (String, Option[String])]](defaultStr: S): String = {
    val mapper = getMapper[S, Option[String]].func.orNull
    val str1   = mapper(defaultStr)
    val result = Strings.defaultString(strOrNull, str1)
    result
  }

  def orDefault: Option[String] = Some(Strings.defaultString(strOrNull))

  def orDefault[S: TypeMapping[*, (String, Option[String])]](defaultStr: S): Option[String] = {
    val mapper = getMapper[S, Option[String]].func.orNull
    val str1   = mapper(defaultStr)
    val result = Strings.defaultString(strOrNull, str1)
    Option(result)
  }

  def deleteWhitespace(): Option[String] = strOpt.map(Strings.deleteWhitespace)

  def difference[S: TypeMapping[*, (String, Option[String])]](other: S): Option[String] = {
    val mapper = getMapper[S, Option[String]].func.orNull
    val str1   = mapper(other)
    val result = Strings.difference(strOrNull, str1)
    Option(result)
  }

  def endsWith[S: TypeMapping[*, (String, Option[String])]](suffix: S): Boolean = {
    val mapper = getMapper[S, Option[String]].func.orNull
    val str1   = mapper(suffix)
    Strings.endsWith(strOrNull, str1)
  }

  def endWithAny[S: VarArgsOfCharSequence](searchStrings: S*): Boolean = {
    def mapping                                    = TypeMapping.getMapping[VarArgsOfCharSequence, S]
    def mapperSeqString                            = getMapper[Seq[Option[CharSequence]], Seq[CharSequence]].func
    def dealWithSeqString(strs: Seq[CharSequence]) = Strings.endsWithAny(strOrNull, strs: _*)

    if (searchStrings == null) {
      Strings.endsWithAny(strOrNull, null)
    } else {
      mapping.input(searchStrings).fold(dealWithSeqString, mapperSeqString.andThen(dealWithSeqString))
    }
  }

  def equalsAnyIgnoreCase[S: VarArgsOfCharSequence](searchStrings: S*): Boolean = {
    def mapping                                    = TypeMapping.getMapping[VarArgsOfCharSequence, S]
    def mapperSeqString                            = getMapper[Seq[Option[CharSequence]], Seq[CharSequence]].func
    def dealWithSeqString(strs: Seq[CharSequence]) = Strings.equalsAnyIgnoreCase(strOrNull, strs: _*)

    if (searchStrings == null) {
      Strings.equalsAnyIgnoreCase(strOrNull, null)
    } else {
      mapping.input(searchStrings).fold(dealWithSeqString, mapperSeqString.andThen(dealWithSeqString))
    }
  }

  def equalsIgnoreCase[S: TypeMapping[*, (String, Option[String])]](other: S): Boolean = {
    val mapper = getMapper[S, Option[String]].func.orNull
    val str1   = mapper(other)
    Strings.equalsIgnoreCase(strOrNull, str1)
  }

  def getBytes[C: TypeMapping[*, (Charset, Option[Charset], String, Option[String])]](charset: C): Array[Byte] = {
    val mapping = TypeMapping.getMapping[TypeMapping[*, (Charset, Option[Charset], String, Option[String])], C]

    def dealWithCharsetOptFunc(c: Charset): Array[Byte] = Strings.getBytes(strOrNull, c)
    def dealWithStringOptFunc(c: String): Array[Byte]   = Strings.getBytes(strOrNull, c)
    def dealWithCharsetOpt                              = dealWithCharsetOptFunc _
    def dealWithStringOpt                               = dealWithStringOptFunc _

    mapping
      .input(charset)
      .fold(
        dealWithCharsetOpt,
        dealWithCharsetOpt.compose(_.orNull),
        dealWithStringOpt,
        dealWithStringOpt.compose(_.orNull)
      )
  }

  def getDigits: String = Strings.getDigits(strOrNull)

  def getIfBlank[S: TypeMapping[*, (CharSequence, Option[CharSequence])]](defaultSupplier: Supplier[S]): CharSequence = {
    if (defaultSupplier == null) {
      return Strings.getIfBlank(strOrNull, null)
    }
    val supplier: Supplier[CharSequence] = { () =>
      val mapper = getMapper[S, Option[CharSequence]].func.orNull
      mapper(defaultSupplier.get())
    }
    Strings.getIfBlank(strOrNull, supplier)
  }

  def getIfEmpty[S: TypeMapping[*, (CharSequence, Option[CharSequence])]](defaultSupplier: Supplier[S]): CharSequence = {
    if (defaultSupplier == null) {
      return Strings.getIfEmpty(strOrNull, null)
    }
    val supplier: Supplier[CharSequence] = { () =>
      val mapper = getMapper[S, Option[CharSequence]].func.orNull
      mapper(defaultSupplier.get())
    }
    Strings.getIfEmpty(strOrNull, supplier)
  }

  def indexOf[S: TypeMapping[*, (String, Option[String])]](searchSeq: S): Int = {
    val mapper = getMapper[S, Option[String]].func.orNull
    val str1   = mapper(searchSeq)
    Strings.indexOf(strOrNull, str1)
  }

  def indexOf(searchChar: Char): Int = Strings.indexOf(strOrNull, searchChar)

  def indexOfAny[S: VarArgsOfCharOrString](searchArgs: S*): Int = {
    def mapping     = TypeMapping.getMapping[VarArgsOfCharOrString, S]
    val mapping2    = mapping.asInstanceOf[TypeMapping[S, (Char, CharSequence, Option[Char], Option[SeqCharSequence])]]
    def indexOfNull = Strings.indexOfAny(strOrNull, null)

    def mapperSeqChar   = getMapper[Seq[Option[Char]], Seq[Char]].func
    def mapperSeqString = getMapper[Seq[Option[CharSequence]], Seq[CharSequence]].func

    def dealWithSeqChar(chars: Seq[Char])             = Strings.indexOfAny(strOrNull, chars: _*)
    def dealWithSeqString(strings: Seq[CharSequence]) = Strings.indexOfAny(strOrNull, strings: _*)
    def dealWithChar(char: Char)                      = Strings.indexOfAny(strOrNull, char)
    def dealWithString(string: CharSequence)          = Strings.indexOfAny(strOrNull, string)

    if (searchArgs == null) {
      indexOfNull
    } else if (searchArgs.length == 1) {
      mapping2
        .input(searchArgs.head)
        .fold(
          dealWithChar,
          dealWithString,
          opt => opt.map(dealWithChar).getOrElse(indexOfNull),
          opt => opt.map(dealWithString).getOrElse(indexOfNull)
        )
    } else {
      mapping
        .input(searchArgs)
        .fold(
          dealWithSeqChar,
          dealWithSeqString,
          mapperSeqChar.andThen(dealWithSeqChar),
          mapperSeqString.andThen(dealWithSeqString)
        )
    }
  }

  // 实现存疑，Tag，osc.head.get 可能抛异常
  def indexOfAnyBut[S: VarArgsOfChar](searchChars: S*): Int = {
    if (searchChars == null) {
      return Strings.indexOfAnyBut(strOrNull, null)
    }

    val mapping = TypeMapping.getMapping[VarArgsOfChar, S]
    mapping
      .input(searchChars)
      .fold(
        chars =>
          if (chars.length == 1) { Strings.indexOfAnyBut(strOrNull, chars.head) }
          else { Strings.indexOfAnyBut(strOrNull, chars: _*) },
        ocs =>
          if (ocs.length == 1) { Strings.indexOfAnyBut(strOrNull, ocs.head.get) }
          else { Strings.indexOfAnyBut(strOrNull, ocs.filter(_.isDefined).map(_.get): _*) }
      )
  }

  def indexOfDifference[S: TypeMapping[*, (String, Option[String])]](cs: S): Int = {
    val mapper = getMapper[S, Option[String]].func.orNull
    val str1   = mapper(cs)
    Strings.indexOfDifference(strOrNull, str1)
  }

  def indexOfIgnoreCase[S: TypeMapping[*, (String, Option[String])]](searchStr: S): Int = {
    val mapper = getMapper[S, Option[String]].func.orNull
    val str1   = mapper(searchStr)
    Strings.indexOfIgnoreCase(strOrNull, str1)
  }

  def indexOfIgnoreCase[S: TypeMapping[*, (String, Option[String])]](searchStr: S, startPos: Int): Int = {
    val mapper = getMapper[S, Option[String]].func.orNull
    val str1   = mapper(searchStr)
    Strings.indexOfIgnoreCase(strOrNull, str1, startPos)
  }

  def isAllLowerCase: Boolean = Strings.isAllLowerCase(strOrNull)

  def isAllUpperCase: Boolean = Strings.isAllUpperCase(strOrNull)

  def isAlpha: Boolean = Strings.isAlpha(strOrNull)

  def isAlphanumeric: Boolean = Strings.isAlphanumeric(strOrNull)

  def isAlphanumericSpace: Boolean = Strings.isAlphanumericSpace(strOrNull)

  def isAlphaSpace: Boolean = Strings.isAlphaSpace(strOrNull)

  def isAsciiPrintable: Boolean = Strings.isAsciiPrintable(strOrNull)

  def isBlank: Boolean = Strings.isBlank(strOrNull)

  def isEmpty: Boolean = Strings.isEmpty(strOrNull)

  def isMixedCase: Boolean = Strings.isMixedCase(strOrNull)

  def isNotBlank: Boolean = Strings.isNotBlank(strOrNull)

  def isNotEmpty: Boolean = Strings.isNotEmpty(strOrNull)

  def isNumeric: Boolean = Strings.isNumeric(strOrNull)

  def isNumericSpace: Boolean = Strings.isNumericSpace(strOrNull)

  def isWhitespace: Boolean = Strings.isWhitespace(strOrNull)

  def lastIndexOf[S: TypeMapping[*, (Char, String, Option[String])]](searchArg: S): Int = {
    val mapping = TypeMapping.getMapping[TypeMapping[*, (Char, String, Option[String])], S]
    mapping
      .input(searchArg)
      .fold(
        ch => Strings.lastIndexOf(strOrNull, ch),
        str => Strings.lastIndexOf(strOrNull, str),
        ostr => Strings.indexOf(strOrNull, ostr.orNull)
      )
  }

  def lastIndexOf[S: TypeMapping[*, (Char, String, Option[String])]](searchArg: S, startPos: Int): Int = {
    val mapping = TypeMapping.getMapping[TypeMapping[*, (Char, String, Option[String])], S]
    mapping
      .input(searchArg)
      .fold(
        ch => Strings.lastIndexOf(strOrNull, ch, startPos),
        str => Strings.lastIndexOf(strOrNull, str, startPos),
        ostr => Strings.indexOf(strOrNull, ostr.orNull, startPos)
      )
  }

  def lastIndexOfAny[S: VarArgsOfCharSequence](searchArgs: S*): Int = {
    def mapping      = TypeMapping.getMapping[VarArgsOfCharSequence, S]
    def seqOptMapper = getMapper[Seq[Option[CharSequence]], Seq[CharSequence]].func

    def dealWithCharSeqSeq(strs: Seq[CharSequence]) = Strings.lastIndexOfAny(strOrNull, strs: _*)

    def result = mapping.input(searchArgs).fold(dealWithCharSeqSeq, seqOptMapper.andThen(dealWithCharSeqSeq))

    if (searchArgs == null) {
      Strings.lastIndexOfAny(strOrNull, null)
    } else {
      result
    }
  }

  def lastIndexOfIgnoreCase[S: TypeMapping[*, (String, Option[String])]](searchStr: S): Int = {
    val mapper = getMapper[S, Option[String]].func.orNull
    val str1   = mapper(searchStr)
    Strings.lastIndexOfIgnoreCase(strOrNull, str1)
  }

  def lastIndexOfIgnoreCase[S: TypeMapping[*, (String, Option[String])]](searchStr: S, startPos: Int): Int = {
    val mapper = getMapper[S, Option[String]].func.orNull
    val str1   = mapper(searchStr)
    Strings.lastIndexOfIgnoreCase(strOrNull, str1, startPos)
  }

  def lastOrdinalIndexOf[S: TypeMapping[*, (String, Option[String])]](searchStr: S, ordinal: Int): Int = {
    val mapper = getMapper[S, Option[String]].func.orNull
    val str1   = mapper(searchStr)
    Strings.lastOrdinalIndexOf(strOrNull, str1, ordinal)
  }

  def left(len: Int): Option[String] = Option(Strings.left(strOrNull, len))

  def leftPad(size: Int): Option[String] = Option(Strings.leftPad(strOrNull, size))

  def leftPad(size: Int, padChar: Char): Option[String] = Option(Strings.leftPad(strOrNull, size, padChar))

  def leftPad[P: TypeMapping[*, (String, Option[String])]](size: Int, padStr: P): Option[String] = {
    val mapper = getMapper[P, Option[String]].func.orNull
    val ps     = mapper(padStr)
    Option(Strings.leftPad(strOrNull, size, ps))
  }

  def length: Int = Strings.length(strOrNull)

  def lowerCase: Option[String] = Option(Strings.lowerCase(strOrNull))

  def lowerCase(locale: Locale): Option[String] = Option(Strings.lowerCase(strOrNull, locale))

  def mid(pos: Int, len: Int): String = Strings.mid(strOrNull, pos, len)

  def normalizeSpace: Option[String] = Option(Strings.normalizeSpace(strOrNull))

  def ordinalIndexOf[S: TypeMapping[*, (String, Option[String])]](searchStr: S, ordinal: Int): Int = {
    val mapper = getMapper[S, Option[String]].func.orNull
    val str1   = mapper(searchStr)
    Strings.ordinalIndexOf(strOrNull, str1, ordinal)
  }

  def overlay[O: TypeMapping[*, (String, Option[String])]](overlay: O, start: Int, end: Int): Option[String] = {
    val mapper  = getMapper[O, Option[String]].func.orNull
    val str1    = mapper(overlay)
    val result2 = Strings.overlay(strOrNull, str1, start, end)
    Option(result2)
  }

  def prependIfMissing[P: TypeMapping[*, (CharSequence, Option[CharSequence])], Ps: VarArgsOfCharSequence](
    prefix: P,
    prefixes: Ps*
  ): Option[String] = {

    def prefixMapper = getMapper[P, Option[CharSequence]].func.orNull
    def prefixStr    = prefixMapper(prefix)

    def prefixesMapping                             = TypeMapping.getMapping[VarArgsOfCharSequence, Ps]
    def getCharSeqOptionMapper                      = getMapper[Seq[Option[CharSequence]], Seq[CharSequence]].func
    def dealWithCharSeqSeq(strs: Seq[CharSequence]) = Strings.prependIfMissing(strOrNull, prefixStr, strs: _*)

    def result = prefixesMapping.input(prefixes).fold(dealWithCharSeqSeq, getCharSeqOptionMapper.andThen(dealWithCharSeqSeq))

    if (prefixes == null) {
      Option(Strings.prependIfMissing(strOrNull, prefixStr, null))
    } else {
      Option(result)
    }
  }

  def prependIfMissing(prefix: CharSequence): Option[String] =
    Option(Strings.prependIfMissing(strOrNull, prefix))
  def prependIfMissing(prefix: Option[CharSequence]): Option[String] =
    Option(Strings.prependIfMissing(strOrNull, prefix.orNull))

  def prependIfMissingIgnoreCase[P: TypeMapping[*, (String, Option[String])], Ps: VarArgsOfCharSequence](
    prefix: P,
    prefixes: Ps*
  ): Option[String] = {
    def prefixMapper    = getMapper[P, Option[String]].func.orNull
    def prefixStr       = prefixMapper(prefix)
    def prefixesMapping = TypeMapping.getMapping[VarArgsOfCharSequence, Ps]

    def getCharSeqOptionMapper                      = getMapper[Seq[Option[CharSequence]], Seq[CharSequence]].func
    def dealWithCharSeqSeq(strs: Seq[CharSequence]) = Strings.prependIfMissingIgnoreCase(strOrNull, prefixStr, strs: _*)
    def result = prefixesMapping.input(prefixes).fold(dealWithCharSeqSeq, getCharSeqOptionMapper.andThen(dealWithCharSeqSeq))

    if (prefixes == null) {
      Option(Strings.prependIfMissingIgnoreCase(strOrNull, prefixStr, null))
    } else {
      Option(result)
    }
  }

  def remove(rmv: Char): Option[String] = Option(Strings.remove(strOrNull, rmv))

  def remove[R: TypeMapping[*, (String, Option[String])]](rmv: R): Option[String] = {
    val mapper = getMapper[R, Option[String]].func.orNull
    val rmvStr = mapper(rmv)
    Option(Strings.remove(strOrNull, rmvStr))
  }

  def removeEnd[R: TypeMapping[*, (String, Option[String])]](rmv: R): Option[String] = {
    val mapper = getMapper[R, Option[String]].func.orNull
    val rmvStr = mapper(rmv)
    Option(Strings.removeEnd(strOrNull, rmvStr))
  }

  def removeEndIgnoreCase[R: TypeMapping[*, (String, Option[String])]](rmv: R): Option[String] = {
    val mapper = getMapper[R, Option[String]].func.orNull
    val rmvStr = mapper(rmv)
    Option(Strings.removeEndIgnoreCase(strOrNull, rmvStr))
  }

  def removeIgnoreCase[R: TypeMapping[*, (String, Option[String])]](rmv: R): Option[String] = {
    val mapper = getMapper[R, Option[String]].func.orNull
    val rmvStr = mapper(rmv)
    Option(Strings.removeIgnoreCase(strOrNull, rmvStr))
  }

  def removeStart[R: TypeMapping[*, (String, Option[String])]](rmv: R): Option[String] = {
    val mapper = getMapper[R, Option[String]].func.orNull
    val rmvStr = mapper(rmv)
    Option(Strings.removeStart(strOrNull, rmvStr))
  }

  def removeStartIgnoreCase[R: TypeMapping[*, (String, Option[String])]](rmv: R): Option[String] = {
    val mapper = getMapper[R, Option[String]].func.orNull
    val rmvStr = mapper(rmv)
    Option(Strings.removeStartIgnoreCase(strOrNull, rmvStr))
  }

  def repeat(rep: Int): Option[String] = Option(Strings.repeat(strOrNull, rep))

  def repeat[S: TypeMapping[*, (String, Option[String])]](separator: S, repeat: Int): Option[String] = {
    val mapper = getMapper[S, Option[String]].func.orNull
    val sep    = mapper(separator)
    Option(Strings.repeat(strOrNull, sep, repeat))
  }

  def replace[S: TypeMapping[*, (String, Option[String])], R: TypeMapping[*, (String, Option[String])]](
    searchString: S,
    replacement: R
  ): Option[String] = {
    val searchMapper      = getMapper[S, Option[String]].func.orNull
    val replacementMapper = getMapper[R, Option[String]].func.orNull

    val sstr = searchMapper(searchString)
    val rstr = replacementMapper(replacement)

    Option(Strings.replace(strOrNull, sstr, rstr))
  }

  def replace[S: TypeMapping[*, (String, Option[String])], R: TypeMapping[*, (String, Option[String])]](
    searchString: S,
    replacement: R,
    max: Int
  ): Option[String] = {
    val searchMapper      = getMapper[S, Option[String]].func.orNull
    val replacementMapper = getMapper[R, Option[String]].func.orNull

    val sstr = searchMapper(searchString)
    val rstr = replacementMapper(replacement)

    Option(Strings.replace(strOrNull, sstr, rstr, max))
  }

  def replaceChars(searchChar: Char, replaceChar: Char): Option[String] =
    Option(Strings.replaceChars(strOrNull, searchChar, replaceChar))

  def replaceChars[S: TypeMapping[*, (String, Option[String])], R: TypeMapping[*, (String, Option[String])]](
    searchChars: S,
    replaceChars: R
  ): Option[String] = {
    val searchMapper      = getMapper[S, Option[String]].func.orNull
    val replacementMapper = getMapper[R, Option[String]].func.orNull

    val sstr = searchMapper(searchChars)
    val rstr = replacementMapper(replaceChars)

    Option(Strings.replaceChars(strOrNull, sstr, rstr))
  }

  def replaceEach(searchList: Array[String], replacementList: Array[String]): Option[String] =
    Option(Strings.replaceEach(strOrNull, searchList, replacementList))

  def replaceEachRepeatedly(searchList: Array[String], replacementList: Array[String]): Option[String] =
    Option(Strings.replaceEachRepeatedly(strOrNull, searchList, replacementList))

  def replaceIgnoreCase[S: TypeMapping[*, (String, Option[String])], R: TypeMapping[*, (String, Option[String])]](
    searchString: S,
    replacement: R,
    max: Int
  ): Option[String] = {
    val searchMapping      = getMapper[S, Option[String]].func.orNull
    val replacementMapping = getMapper[R, Option[String]].func.orNull

    val sstr = searchMapping(searchString)
    val rstr = replacementMapping(replacement)

    Option(Strings.replaceIgnoreCase(strOrNull, sstr, rstr, max))
  }

  def replaceOnce[S: TypeMapping[*, (String, Option[String])], R: TypeMapping[*, (String, Option[String])]](
    searchString: S,
    replacement: R
  ): Option[String] = {
    val searchMapping      = getMapper[S, Option[String]].func.orNull
    val replacementMapping = getMapper[R, Option[String]].func.orNull

    val sstr = searchMapping(searchString)
    val rstr = replacementMapping(replacement)

    Option(Strings.replaceOnce(strOrNull, sstr, rstr))
  }

  def reverse: Option[String] = Option(Strings.reverse(strOrNull))

  def reverseDelimited(separatorChar: Char): Option[String] =
    Option(Strings.reverseDelimited(strOrNull, separatorChar))

  def right(len: Int): Option[String] = Option(Strings.right(strOrNull, len))

  def rightPad(size: Int): Option[String] = Option(Strings.rightPad(strOrNull, size))

  def rightPad(size: Int, padChar: Char): Option[String] = Option(Strings.rightPad(strOrNull, size, padChar))

  def rightPad[P: TypeMapping[*, (String, Option[String])]](size: Int, padStr: P): Option[String] = {
    val mapper = getMapper[P, Option[String]].func.orNull
    val ps     = mapper(padStr)
    Option(Strings.rightPad(strOrNull, size, ps))
  }

  def rotate(shift: Int): Option[String] = Option(Strings.rotate(strOrNull, shift))

  def split: Option[Array[String]] = Option(Strings.split(strOrNull))

  def split(separatorChar: Char): Option[Array[String]] = Option(Strings.split(strOrNull, separatorChar))

  def split[S: TypeMapping[*, (String, Option[String])]](separatorChars: S): Option[Array[String]] = {
    val mapper = getMapper[S, Option[String]].func.orNull
    val sep    = mapper(separatorChars)
    Option(Strings.split(strOrNull, sep))
  }

  def split[S: TypeMapping[*, (String, Option[String])]](separatorChars: S, max: Int): Option[Array[String]] = {
    val mapper = getMapper[S, Option[String]].func.orNull
    val sep    = mapper(separatorChars)
    Option(Strings.split(strOrNull, sep, max))
  }

  def splitByCharacterType: Option[Array[String]] = Option(Strings.splitByCharacterType(strOrNull))

  def splitByCharacterTypeCamelCase: Option[Array[String]] =
    Option(Strings.splitByCharacterTypeCamelCase(strOrNull))

  def splitByWholeSeparator[S: TypeMapping[*, (String, Option[String])]](separatorChars: S): Option[Array[String]] = {
    val mapper = getMapper[S, Option[String]].func.orNull
    val sep    = mapper(separatorChars)
    Option(Strings.splitByWholeSeparator(strOrNull, sep))
  }

  def splitByWholeSeparator[S: TypeMapping[*, (String, Option[String])]](separatorChars: S, max: Int): Option[Array[String]] = {
    val mapper = getMapper[S, Option[String]].func.orNull
    val sep    = mapper(separatorChars)
    Option(Strings.splitByWholeSeparator(strOrNull, sep, max))
  }

  def splitByWholeSeparatorPreserveAllTokens[S: TypeMapping[*, (String, Option[String])]](separatorChars: S): Option[Array[String]] = {
    val mapper = getMapper[S, Option[String]].func.orNull
    val sep    = mapper(separatorChars)
    Option(Strings.splitByWholeSeparatorPreserveAllTokens(strOrNull, sep))
  }

  def splitByWholeSeparatorPreserveAllTokens[S: TypeMapping[*, (String, Option[String])]](
    separatorChars: S,
    max: Int
  ): Option[Array[String]] = {
    val mapper = getMapper[S, Option[String]].func.orNull
    val sep    = mapper(separatorChars)
    Option(Strings.splitByWholeSeparatorPreserveAllTokens(strOrNull, sep, max))
  }

  def splitPreserveAllTokens: Option[Array[String]] = Option(Strings.splitPreserveAllTokens(strOrNull))

  def splitPreserveAllTokens(separatorChar: Char): Option[Array[String]] =
    Option(Strings.splitPreserveAllTokens(strOrNull, separatorChar))

  def splitPreserveAllTokens[S: TypeMapping[*, (String, Option[String])]](separatorChars: S): Option[Array[String]] = {
    val mapper = getMapper[S, Option[String]].func.orNull
    val sep    = mapper(separatorChars)
    Option(Strings.splitPreserveAllTokens(strOrNull, sep))
  }

  def splitPreserveAllTokens[S: TypeMapping[*, (String, Option[String])]](separatorChars: S, max: Int): Option[Array[String]] = {
    val mapper = getMapper[S, Option[String]].func.orNull
    val sep    = mapper(separatorChars)
    Option(Strings.splitPreserveAllTokens(strOrNull, sep, max))
  }

  def startsWith[S: TypeMapping[*, (String, Option[String])]](prefix: S): Boolean = {
    val mapper = getMapper[S, Option[String]].func.orNull
    val pre    = mapper(prefix)
    Strings.startsWith(strOrNull, pre)
  }

  def startsWithAny[CS: VarArgsOfCharSequence](searchStrings: CS*): Boolean = {
    val mapping = TypeMapping.getMapping[VarArgsOfCharSequence, CS]
    val strs    = mapping.input(searchStrings).fold(identity, { css => css.map(_.orNull) })
    Strings.startsWithAny(strOrNull, strs: _*)
  }

  def startsWithIgnoreCase[P: TypeMapping[*, (String, Option[String])]](prefix: P): Boolean = {
    val mapper = getMapper[P, Option[String]].func.orNull
    val str    = mapper(prefix)
    Strings.startsWithIgnoreCase(strOrNull, str)
  }

  def strip: Option[String] = Option(Strings.strip(strOrNull))

  def strip[S: TypeMapping[*, (String, Option[String])]](stripChars: S): Option[String] = {
    val mapper = getMapper[S, Option[String]].func.orNull
    val chars  = mapper(stripChars)
    Option(Strings.strip(strOrNull, chars))
  }

  def stripAccents: Option[String] = Option(Strings.stripAccents(strOrNull))

  def stripEnd[S: TypeMapping[*, (String, Option[String])]](stripChars: S): Option[String] = {
    val mapper = getMapper[S, Option[String]].func.orNull
    val chars  = mapper(stripChars)
    Option(Strings.stripEnd(strOrNull, chars))
  }

  def stripStart[S: TypeMapping[*, (String, Option[String])]](stripChars: S): Option[String] = {
    val mapper = getMapper[S, Option[String]].func.orNull
    val chars  = mapper(stripChars)
    Option(Strings.stripStart(strOrNull, chars))
  }

  def stripToEmpty: String = Strings.stripToEmpty(strOrNull)

  def stripToNone: Option[String] = Option(Strings.stripToNull(strOrNull))

  def substring(start: Int): Option[String] = Option(Strings.substring(strOrNull, start))

  def substring(start: Int, end: Int): Option[String] = Option(Strings.substring(strOrNull, start, end))

  def substringAfter(separator: Char): Option[String] = Option(Strings.substringAfter(strOrNull, separator))

  def substringAfter[S: TypeMapping[*, (String, Option[String])]](separator: S): Option[String] = {
    val mapper = getMapper[S, Option[String]].func.orNull
    val sep    = mapper(separator)
    Option(Strings.substringAfter(strOrNull, sep))
  }

  def substringAfterLast(separator: Char): Option[String] = Option(Strings.substringAfterLast(strOrNull, separator))

  def substringAfterLast[S: TypeMapping[*, (String, Option[String])]](separator: S): Option[String] = {
    val mapper = getMapper[S, Option[String]].func.orNull
    val sep    = mapper(separator)
    Option(Strings.substringAfterLast(strOrNull, sep))
  }

  def substringBefore(separator: Char): Option[String] = Option(Strings.substringBefore(strOrNull, separator))

  def substringBefore[S: TypeMapping[*, (String, Option[String])]](separator: S): Option[String] = {
    val mapper = getMapper[S, Option[String]].func.orNull
    val sep    = mapper(separator)
    Option(Strings.substringBefore(strOrNull, sep))
  }

  def substringBeforeLast[S: TypeMapping[*, (String, Option[String])]](separator: S): Option[String] = {
    val mapper = getMapper[S, Option[String]].func.orNull
    val sep    = mapper(separator)
    Option(Strings.substringBeforeLast(strOrNull, sep))
  }

  def substringBetween[S: TypeMapping[*, (String, Option[String])]](tag: S): Option[String] = {
    val mapper = getMapper[S, Option[String]].func.orNull
    val t      = mapper(tag)
    Option(Strings.substringBetween(strOrNull, t))
  }

  def substringBetween[S: TypeMapping[*, (String, Option[String])]](open: S, close: S): Option[String] = {
    val mapper = getMapper[S, Option[String]].func.orNull
    val o      = mapper(open)
    val c      = mapper(close)
    Option(Strings.substringBetween(strOrNull, o, c))
  }

  def substringsBetween[S: TypeMapping[*, (String, Option[String])]](open: S, close: S): Option[Array[String]] = {
    val mapper = getMapper[S, Option[String]].func.orNull
    val o      = mapper(open)
    val c      = mapper(close)
    Option(Strings.substringsBetween(strOrNull, o, c))
  }

  def swapCase: Option[String] = Option(Strings.swapCase(strOrNull))

  def toCodePoints: Option[Array[Int]] = Option(Strings.toCodePoints(strOrNull))

  def toRootLowerCase: Option[String] = Option(Strings.toRootLowerCase(strOrNull))

  def toRootUpperCase: Option[String] = Option(Strings.toRootUpperCase(strOrNull))

  def trim: Option[String] = Option(Strings.trim(strOrNull))

  def trimToEmpty: Option[String] = Option(Strings.trimToEmpty(strOrNull))

  def trimToNull: Option[String] = Option(Strings.trimToNull(strOrNull))

  def truncate(maxWidth: Int): Option[String] = Option(Strings.truncate(strOrNull, maxWidth))

  def truncate(offset: Int, maxWidth: Int): Option[String] = Option(Strings.truncate(strOrNull, offset, maxWidth))

  def uncapitalize: Option[String] = Option(Strings.uncapitalize(strOrNull))

  def unwrap(wrapChar: Char): Option[String] = Option(Strings.unwrap(strOrNull, wrapChar))

  def unwrap[S: TypeMapping[*, (String, Option[String])]](wrapToken: S): Option[String] = {
    val mapper = getMapper[S, Option[String]].func.orNull
    val token  = mapper(wrapToken)
    Option(Strings.unwrap(strOrNull, token))
  }

  def upperCase: Option[String] = Option(Strings.upperCase(strOrNull))

  def upperCase(locale: Locale): Option[String] = Option(Strings.upperCase(strOrNull, locale))

  def wrap(wrapChar: Char): Option[String] = Option(Strings.wrap(strOrNull, wrapChar))

  def wrap[S: TypeMapping[*, (String, Option[String])]](wrapToken: S): Option[String] = {
    val mapper = getMapper[S, Option[String]].func.orNull
    val token  = mapper(wrapToken)
    Option(Strings.wrap(strOrNull, token))
  }

  def wrapIfMissing(wrapChar: Char): Option[String] = Option(Strings.wrapIfMissing(strOrNull, wrapChar))

  def wrapIfMissing[S: TypeMapping[*, (String, Option[String])]](wrapToken: S): Option[String] = {
    val mapper = getMapper[S, Option[String]].func.orNull
    val token  = mapper(wrapToken)
    Option(Strings.wrapIfMissing(strOrNull, token))
  }
}
