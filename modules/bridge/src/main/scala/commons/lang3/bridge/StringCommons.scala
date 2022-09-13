package commons.lang3.bridge

import commons.lang3.bridge.TypeMapping.alias._
import org.apache.commons.lang3.{StringUtils => Strings}

import java.nio.charset.Charset
import java.util.Locale
import java.util.function.Supplier

private object privateUtils {
  trait SingleTypeMapApply[U] {
    @inline def input[T](t: T)(implicit map: SingleTypeMap[T, U]): U = map.input(t)
  }
  object SingleTypeMapApply {
    private object value extends SingleTypeMapApply[Any]
    @inline def get[U]: SingleTypeMapApply[U] = value.asInstanceOf[SingleTypeMapApply[U]]
  }
  @inline def mapTo[O]: SingleTypeMapApply[O]                      = SingleTypeMapApply.get
  @inline val mapToStrOpt: SingleTypeMapApply[Option[String]]      = SingleTypeMapApply.get
  @inline val mapToCsOpt: SingleTypeMapApply[Option[CharSequence]] = SingleTypeMapApply.get

  @FunctionalInterface
  trait SingleTypeMap[I, O] {
    def input(i: I): O
  }

  object SingleTypeMap {
    implicit def toStrOptImplicit[U: TypeOptions2[*, String, Option[String]]]: SingleTypeMap[U, Option[String]] =
      strToOpt
    implicit def toCharSequenceOptImplicit[U: TypeOptions2[*, CharSequence, Option[CharSequence]]]: SingleTypeMap[U, Option[CharSequence]] =
      csToOpt
    implicit def seqOptionCharToSeqCharImplicit: SingleTypeMap[Seq[Option[Char]], Seq[Char]] = tranCharSeqOptFunc
    implicit def seqOptionCharSequenceToSeqCharSequenceImplicit: SingleTypeMap[Seq[Option[CharSequence]], Seq[CharSequence]] =
      tranCharSeqSeqOptFunc
  }

  private def strToOpt[U: TypeOptions2[*, String, Option[String]]](t: U): Option[String] = {
    val mapping = TypeMapping.getMapping[TypeOptions2[*, String, Option[String]]]
    mapping.input(t).fold(Option(_), identity)
  }

  private def csToOpt[U: TypeOptions2[*, CharSequence, Option[CharSequence]]](t: U): Option[CharSequence] = {
    val mapping = TypeMapping.getMapping[TypeOptions2[*, CharSequence, Option[CharSequence]]]
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
class StringCommons[T: TypeOptions2[*, String, Option[String]]](value: T) {
  import privateUtils._

  @inline private def strOpt: Option[String] = mapToStrOpt.input(value)
  @inline private def strOrNull: String = {
    val mapping = TypeMapping.getMapping[TypeOptions2[*, String, Option[String]]]
    mapping.input(value).fold(identity, _.orNull)
  }

  /** * <p>Abbreviates a String using ellipses. This will turn "Now is the time for all good men" into "Now is the time for..."</p>
    *
    * {@code str} is less than or equal to {@code maxWidth}, return {@code str}.</li> <li>Else abbreviate it to {@code (substring(str, 0,
    * max-3) + "...")}.</li> <li>If {@code maxWidth} is less than {@code 4}, throw an {@code IllegalArgumentException}.</li> <li>In no case
    * will it return a String of length greater than {@code maxWidth}.</li> </ul>
    *
    * {{{
    *
    * none.ops.abbreviate(*) = None
    * none.abbreviate(4) = Some("")
    * Some("abcdefg").ops.abbreviate(6) = Some("abc...")
    * Some("abcdefg").ops.abbreviate(7) = Some("abcdefg")
    * Some("abcdefg").ops.abbreviate(8) = Some("abcdefg")
    * Some("abcdefg").ops.abbreviate(4) = Some("a...")
    * Some("abcdefg").ops.abbreviate(3) = IllegalArgumentException
    *
    * }}}
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
    * {{{
    *
    * none.ops.abbreviate(*, *) = None
    * Some("").ops.abbreviate(0, 4) = Some("")
    * Some("abcdefghijklmno").ops.abbreviate(-1, 10) = Some("abcdefg...")
    * Some("abcdefghijklmno").ops.abbreviate(0, 10) = Some("abcdefg...")
    * Some("abcdefghijklmno").ops.abbreviate(1, 10) = Some("abcdefg...")
    * Some("abcdefghijklmno").ops.abbreviate(4, 10) = Some("abcdefg...")
    * Some("abcdefghijklmno").ops.abbreviate(5, 10) = Some("...fghi...")
    * Some("abcdefghijklmno").ops.abbreviate(6, 10) = Some("...ghij...")
    * Some("abcdefghijklmno").ops.abbreviate(8, 10) = Some("...ijklmno")
    * Some("abcdefghijklmno").ops.abbreviate(10, 10) = Some("...ijklmno")
    * Some("abcdefghijklmno").ops.abbreviate(12, 10) = Some("...ijklmno")
    * Some("abcdefghij").ops.abbreviate(0, 3) = IllegalArgumentException
    * Some("abcdefghij").ops.abbreviate(5, 6) = IllegalArgumentException
    *
    * }}}
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
    * {{{
    * none.ops.abbreviate(Some("..."), *) = None
    * Some("abcdefg")ops..abbreviate(None, *) = Some("abcdefg")
    * Some("")ops..abbreviate(Some("..."), 4) = Some("")
    * Some("abcdefg").ops.abbreviate(Some("."), 5) = Some("abcd.")
    * Some("abcdefg").ops.abbreviate(Some("."), 7) = Some("abcdefg")
    * Some("abcdefg").ops.abbreviate(Some("."), 8) = Some("abcdefg")
    * Some("abcdefg").ops.abbreviate(Some(".."), 4) = Some("ab..")
    * Some("abcdefg").ops.abbreviate(Some(".."), 3) = Some("a..")
    * Some("abcdefg").ops.abbreviate(Some(".."), 2) = IllegalArgumentException
    * Some("abcdefg").abbreviate(Some("..."), 3) = IllegalArgumentException
    *
    * }}}
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
  def abbreviate[Abb: TypeOptions2[*, String, Option[String]]](abbrevMarker: Abb, maxWidth: Int): Option[String] = {
    val abbrevMarkerOrNull = mapToStrOpt.input(abbrevMarker).orNull
    Option(Strings.abbreviate(strOrNull, abbrevMarkerOrNull, maxWidth))
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
    * {{{
    *
    * none.ops.abbreviate(None, *, *) = None
    * Some("abcdefghijklmno").ops.abbreviate(None, *, *) = Some("abcdefghijklmno")
    * Some("").ops.abbreviate("...", 0, 4) = Some("")
    * Some("abcdefghijklmno").ops.abbreviate("---", -1, 10) = Some("abcdefg---")
    * Some("abcdefghijklmno").ops.abbreviate(",", 0, 10) = Some("abcdefghi,")
    * Some("abcdefghijklmno").ops.abbreviate(",", 1, 10) = Some("abcdefghi,")
    * Some("abcdefghijklmno").ops.abbreviate(",", 2, 10) = Some("abcdefghi,")
    * Some("abcdefghijklmno").ops.abbreviate("::", 4, 10) = Some("::efghij::")
    * Some("abcdefghijklmno").ops.abbreviate("...", 6, 10) = Some("...ghij...")
    * Some("abcdefghijklmno").ops.abbreviate("*", 9, 10) = Some("*ghijklmno")
    * Some("abcdefghijklmno").ops.abbreviate("'", 10, 10) = Some("'ghijklmno")
    * Some("abcdefghijklmno").ops.abbreviate("!", 12, 10) = Some("!ghijklmno")
    * Some("abcdefghij").ops.abbreviate("abra", 0, 4) = IllegalArgumentException
    * Some("abcdefghij").ops.abbreviate("...", 5, 6) = IllegalArgumentException
    *
    * }}}
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
  def abbreviate[Abb: TypeOptions2[*, String, Option[String]]](abbrevMarker: Abb, offset: Int, maxWidth: Int): Option[String] = {
    val abbrevMarkerOrNull = mapToStrOpt.input(abbrevMarker).orNull
    Option(Strings.abbreviate(strOrNull, abbrevMarkerOrNull, offset, maxWidth))
  }

  /** <p>Abbreviates a String to the length passed, replacing the middle characters with the supplied replacement String.</p>
    *
    * <p>This abbreviation only occurs if the following criteria is met:</p> <ul> <li>Neither the String for abbreviation nor the
    * replacement String are null or empty </li> <li>The length to truncate to is less than the length of the supplied String</li> <li>The
    * length to truncate to is greater than 0</li> <li>The abbreviated String will have enough room for the length supplied replacement
    * String and the first and last characters of the supplied String for abbreviation</li> </ul> <p>Otherwise, the returned String will be
    * the same as the supplied String for abbreviation. </p>
    *
    * {{{
    *
    * none.ops.abbreviateMiddle(None, 0) = None
    * Some("abc").ops.abbreviateMiddle(None, 0) = Some("abc")
    * Some("abc")ops..abbreviateMiddle(".", 0) = Some("abc")
    * Some("abc").ops.abbreviateMiddle(".", 3) = Some("abc")
    * Some("abcdef").ops.abbreviateMiddle(".", 4) = Some("ab.f")
    *
    * }}}
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
  def abbreviateMiddle[M: TypeOptions2[*, String, Option[String]]](middle: M, length: Int): Option[String] = {
    val middleOrNull = mapToStrOpt.input(middle).orNull
    Option(Strings.abbreviateMiddle(strOrNull, middleOrNull, length))
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
  def appendIfMissing[S: TypeOptions2[*, String, Option[String]], SS: TypeOptions2F[Seq, *, Seq[CharSequence], Seq[Option[CharSequence]]]](
    suffix: S,
    suffixes: SS*
  ): Option[String] = {
    def suffixOrNull = mapToStrOpt.input(suffix).orNull
    def mapping      = TypeMapping.getMapping[TypeOptions2[*, Seq[CharSequence], Seq[Option[CharSequence]]]]

    if (suffixes == null) Option(Strings.appendIfMissing(strOrNull, suffixOrNull))
    else {
      val sfs = mapping.input(suffixes).fold(identity, oss => oss.map(_.orNull))
      Option(Strings.appendIfMissing(strOrNull, suffixOrNull, sfs: _*))
    }
  }
  // helpers for method call without suffixes
  def appendIfMissing(suffix: CharSequence): Option[String]         = Option(Strings.appendIfMissing(strOrNull, suffix))
  def appendIfMissing(suffix: Option[CharSequence]): Option[String] = Option(Strings.appendIfMissing(strOrNull, suffix.orNull))

  /** Appends the suffix to the end of the string if the string does not already end with any of the suffixes.
    *
    * {{{
    * none.ops.appendIfMissing(None) = None
    * Some("abc").ops.appendIfMissing(None) = Some("abc")
    * Some("").ops.appendIfMissing("xyz") = Some("xyz")
    * Some("abc").ops.appendIfMissing("xyz") = Some("abcxyz")
    * Some("abcxyz").ops.appendIfMissing("xyz") = Some("abcxyz")
    * Some("abcXYZ").ops.appendIfMissing("xyz") = Some("abcXYZxyz")
    *
    * }}}
    *
    * <p>With additional suffixes,</p>
    *
    * {{{
    *
    * none.ops.appendIfMissing(None, None) = None
    * Some("abc").ops.appendIfMissing(null, null) = Some("abc")
    * Some("").ops.appendIfMissing("xyz", null) = Some("xyz")
    * Some("abc").ops.appendIfMissing(Some("xyz"), new CharSequence[]{null}) = Some("abcxyz")
    * Some("abc").ops.appendIfMissing("xyz", "") = Some("abc")
    * Some("abc").ops.appendIfMissing("xyz", "mno") = Some("abcxyz")
    * Some("abcxyz").ops.appendIfMissing("xyz", "mno") = Some("abcxyz")
    * Some("abcmno").ops.appendIfMissing("xyz", "mno") = Some("abcmno")
    * Some("abcXYZ").ops.appendIfMissing("xyz", "mno") = Some("abcXYZxyz")
    * Some("abcMNO").ops.appendIfMissing("xyz", "mno") = Some("abcMNOxyz")
    *
    * }}}
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
  def appendIfMissingIgnoreCase[S: TypeOptions2[*, String, Option[String]], SS: TypeOptions2F[Seq, *, Seq[CharSequence], Seq[
    Option[CharSequence]
  ]]](
    suffix: S,
    suffixes: SS*
  ): Option[String] = {
    def suffixOrNull = mapToStrOpt.input(suffix).orNull
    def mapping      = TypeMapping.getMapping[TypeOptions2[*, Seq[CharSequence], Seq[Option[CharSequence]]]]

    if (suffixes == null) Option(Strings.appendIfMissingIgnoreCase(strOrNull, suffixOrNull))
    else {
      val sfs = mapping.input(suffixes).fold(identity, oss => oss.map(_.orNull))
      Option(Strings.appendIfMissingIgnoreCase(strOrNull, suffixOrNull, sfs: _*))
    }
  }

  // helpers for method call without suffixes
  def appendIfMissingIgnoreCase(suffix: CharSequence): Option[String] =
    Option(Strings.appendIfMissingIgnoreCase(strOrNull, suffix))

  def appendIfMissingIgnoreCase(suffix: Option[CharSequence]): Option[String] =
    Option(Strings.appendIfMissingIgnoreCase(strOrNull, suffix.orNull))

  /** <p>Capitalizes a String changing the first character to title case as per {@link Character# toTitleCase ( int )}. No other characters
    * are changed.</p>
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
    * {{{
    *
    * none.ops.center(*) = None
    * Some("").ops.center(4) = Some(" ")
    * Some("ab").ops.center(-1) = Some("ab")
    * Some("ab").ops.center(4) = Some(" ab ")
    * Some("abcd").ops.center(2) = Some("abcd")
    * Some("a").ops.center(4) = Some(" a ")
    *
    * }}}
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
    * {{{
    *
    * none.ops.center(*, *) = None
    * Some("").ops.center(4, ' ') = Some(" ")
    * Some("ab").ops.center(-1, ' ') = Some("ab")
    * Some("ab").ops.center(4, ' ') = Some(" ab ")
    * Some("abcd").ops.center(2, ' ') = Some("abcd")
    * Some("a").ops.center(4, ' ') = Some(" a ")
    * Some("a").ops.center(4, 'y') = Some("yayy")
    *
    * }}}
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
    * {{{
    * none.center(*, *) = None
    * Some("").center(4, " ") = Some(" ")
    * Some("ab").center(-1, " ") = Some("ab")
    * Some("ab").center(4, " ") = Some(" ab ")
    * Some("abcd").center(2, " ") = Some("abcd")
    * Some("a").center(4, " ") = Some(" a ")
    * Some("a").center(4, "yz") = Some("yayz")
    * Some("abc").center(7, None) = Some(" abc ")
    * Some("abc").center(7, "") = Some(" abc ")
    * }}}
    * @param size
    *   the int size of new String, negative treated as zero
    * @param padStr
    *   the String to pad the new String with, must not be null or empty
    * @tparam P
    *   String or Option[String]
    * @return
    *   centered String, {@code None} if None input
    */
  def center[P: TypeOptions2[*, String, Option[String]]](size: Int, padStr: P): Option[String] = {
    val padStrOrNull = mapToStrOpt.input(padStr).orNull
    Option(Strings.center(strOrNull, size, padStrOrNull))
  }

  /** <p>Removes one newline from end of a String if it's there, otherwise leave it alone. A newline is &quot;{@code \n}&quot;, &quot;{@code
    * \r}&quot;, or &quot;{@code \r\n}&quot;.</p>
    *
    * <p>NOTE: This method changed in 2.0. It now more closely matches Perl chomp.</p>
    *
    * {{{
    *
    * none.ops.chomp = None
    * Some("").ops.chomp = Some("")
    * Some("abc \r").ops.chomp = Some("abc ")
    * Some("abc\n").ops.chomp = Some("abc")
    * Some("abc\r\n").ops.chomp = Some("abc")
    * Some("abc\r\n\r\n").ops.chomp = Some("abc\r\n")
    * Some("abc\n\r").ops.chomp = Some("abc\n")
    * Some("abc\n\rabc").ops.chomp = Some("abc\n\rabc")
    * Some("\r").ops.chomp = Some("")
    * Some("\n").ops.chomp = Some("")
    * Some("\r\n").ops.chomp = Some("")
    *
    * }}}
    *
    * @return
    *   Option[String] without newline, {@code None} if null String input
    */
  def chomp: Option[String] = Option(Strings.chomp(strOrNull))

  /** <p>Remove the last character from a String.</p>
    *
    * <p>If the String ends in {@code \r\n}, then remove both of them.</p>
    *
    * {{{
    *
    * none.ops.chop = None
    * Some("").ops.chop = Some("")
    * Some("abc \r").ops.chop = Some("abc ")
    * Some("abc\n").ops.chop = Some("abc")
    * Some("abc\r\n").ops.chop = Some("abc")
    * Some("abc").ops.chop = Some("ab")
    * Some("abc\nabc").ops.chop = Some("abc\nab")
    * Some("a").ops.chop = Some("")
    * Some("\r").ops.chop = Some("")
    * Some("\n").ops.chop = Some("")
    * Some("\r\n").ops.chop = Some("")
    *
    * }}}
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
    * {{{
    * none.ops.compare(null) = 0
    * none.ops.compare("a") &lt; 0
    * Some("a")).ops.compare(null) &gt; 0
    * "abc".ops.compare("abc") = 0
    * "a".ops.compare("b") &lt; 0
    * "b".ops.compare("a") &gt; 0
    * "a".ops.compare("B") &gt; 0
    * "ab".ops.compare("abc") &lt; 0
    * }}}
    *
    * @param other
    *   the String to compare to
    * @tparam O
    *   String Or Option[String]
    * @return
    *   &lt; 0, 0, &gt; 0, if {@code this} is respectively less, equal or greater than {@code other}
    */
  def compare[O: TypeOptions2[*, String, Option[String]]](other: O): Int = {
    val otherOrNull = mapToStrOpt.input(other).orNull
    Strings.compare(strOrNull, otherOrNull)
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
    * {{{
    *
    * none.ops.compare(None, *) = 0
    * none.ops.compare(Some("a"), true) &lt; 0
    * none.ops.compare(Some("a"), false) &gt; 0
    * Some("a").ops.compare(None, true) &gt; 0
    * Some("a").ops.compare(None, false) &lt; 0
    * Some("abc").ops.compare(Some("abc"), *) = 0
    * Some("a").ops.compare(Some("b"), *) &lt; 0
    * Some("b").ops.compare("a", *) &gt; 0
    * Some("a").ops.compare("B", *) &gt; 0
    * Some("ab").ops.compare("abc", *) &lt; 0
    *
    * }}}
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
  def compare[O: TypeOptions2[*, String, Option[String]]](other: O, nullIsNull: Boolean): Int = {
    val otherOrNull = mapToStrOpt.input(other).orNull
    Strings.compare(strOrNull, otherOrNull, nullIsNull)
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
    * {{{
    *
    * none.ops.compareToIgnoreCase(None) = 0
    * none.ops.compareToIgnoreCase(None , "a") &lt; 0
    * Some("a").ops.compareToIgnoreCase(None) &gt; 0
    * Some("abc").ops.compareToIgnoreCase("abc") = 0
    * Some("abc").ops.compareToIgnoreCase("ABC") = 0
    * Some("a").ops.compareToIgnoreCase("b") &lt; 0
    * Some("b").ops.compareToIgnoreCase("a") &gt; 0
    * Some("a").ops.compareToIgnoreCase("B") &lt; 0
    * Some("A").ops.compareToIgnoreCase("b") &lt; 0
    * Some("ab").compareToIgnoreCase("ABC") &lt; 0
    *
    * }}}
    *
    * @param other
    *   the String to compare to
    * @tparam O
    *   String or Option[String]
    * @return
    *   &lt; 0, 0, &gt; 0, if {@code this} is respectively less, equal ou greater than {@code other}, ignoring case differences.
    */
  def compareIgnoreCase[O: TypeOptions2[*, String, Option[String]]](other: O): Int = {
    val otherOrNull = mapToStrOpt.input(other).orNull
    Strings.compareIgnoreCase(strOrNull, otherOrNull)
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
    * {{{
    *
    * none.ops.compareToIgnoreCase(null, *) = 0
    * none.ops.compareToIgnoreCase("a", true) > 0
    * none.ops.compareToIgnoreCase("a", false) > 0
    * "a".ops.compareToIgnoreCase(null, true) > 0
    * Some("a").ops.compareToIgnoreCase(None, false) < 0
    * "abc".ops.compareToIgnoreCase(Some("abc"), *) = 0
    * "abc".compareToIgnoreCase("ABC", *) = 0
    * "a".ops.compareToIgnoreCase("b", *) < 0
    *
    * }}}
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
  def compareIgnoreCase[O: TypeOptions2[*, String, Option[String]]](other: O, nullIsLess: Boolean): Int = {
    val otherOrNull = mapToStrOpt.input(other).orNull
    Strings.compareIgnoreCase(strOrNull, otherOrNull, nullIsLess)
  }

  /** * <p>Checks if CharSequence contains a search CharSequence, handling {@code null}. This method uses {@link String# indexOf ( String )}
    * if possible.</p>
    *
    * <p>A {@code null} CharSequence will return {@code false}.</p>
    *
    * {{{
    *
    * none.ops.contains(*) = false
    * *.ops.contains(None) = false
    * Some("").ops.contains("") = true
    * Some("abc").ops.contains("") = true
    * Some("abc").ops.contains("a") = true
    * Some("abc").ops.contains("z") = false
    *
    * }}}
    *
    * @param searchSeq
    *   the CharSequence to find, may be null
    * @tparam To
    *   String Or Option[String]
    * @return
    *   true if the CharSequence contains the search CharSequence,
    */
  def contains[To: TypeOptions2[*, String, Option[String]]](searchSeq: To): Boolean = {
    val str1 = mapToStrOpt.input(searchSeq).orNull
    Strings.contains(strOrNull, str1)
  }

  /** <p>Checks if CharSequence contains a search character, handling {@code null}. This method uses {@link String# indexOf ( int )} if
    * possible.</p>
    *
    * <p>A {@code null} or empty ("") CharSequence will return {@code false}.</p>
    *
    * {{{
    *
    * none.ops.contains(*) = false
    * Some("").ops.contains(*) = false
    * Some("abc").ops.contains('a') = true
    * Some("abc").ops.contains('z') = false
    *
    * }}}
    *
    * @param searchChar
    *   the character to find
    * @return
    *   true if the CharSequence contains the search character,
    */
  @inline def contains(searchChar: Char): Boolean = Strings.contains(strOrNull, searchChar)

  /** <p>Checks if CharSequence contains a search character, handling {@code null}. This method uses {@link String# indexOf ( int )} if
    * possible.</p>
    *
    * <p>A {@code null} or empty ("") CharSequence will return {@code false}.</p>
    *
    * {{{
    *
    * none.ops.contains(*) = false
    * Some("").ops.contains(*) = false
    * Some("abc").ops.contains('a') = true
    * Some("abc").ops.contains('z') = false
    *
    * }}}
    *
    * @param searchChar
    *   the character to find
    * @return
    *   true if the CharSequence contains the search character,
    */
  @inline def contains(searchChar: Int): Boolean = Strings.contains(strOrNull, searchChar)

  /** <p> Checks if the CharSequence contains any character or string in the given set of characters. </p>
    *
    * <p> A {@code None} CharSequence will return {@code false}. A {@code null} search CharSequence will return {@code false}. </p>
    *
    * {{{
    *
    * none.ops.containsAny(*) = false
    * Some("").ops.containsAny(*) = false
    * Option(*).ops.containsAny(None) = false
    * Option(*).ops.containsAny([]) = false
    * Some("zzabyycdxx").ops.containsAny(['z', 'a']) = true
    * Some("zzabyycdxx").ops.containsAny(['b', 'y']) = true
    * Some("zzabyycdxx").ops.containsAny(['z', 'y']) = true
    * Some("aba").ops.containsAny(['z']) = false
    *
    * }}}
    *
    * {{{
    *
    * none.ops.containsAny(*) = false
    * Some("").ops.containsAny(*) = false
    * Option(*).ops.containsAny(None) = false
    * Option(*).ops.containsAny("") = false
    * Some("zzabyycdxx").ops.containAny("za") = true
    * Some("zzabyycdxx").ops.containAny("by") = true
    * Some("zzabyycdxx").ops.containAny("zy") = true
    * Some("zzabyycdxx").containAny("\tx") = true
    * Some("zzabyycdxx").containAny("$.#yF") = true
    * Some("aba").containAny("z") = false
    *
    * }}}
    *
    * @param searchChars
    *   the chars to search for, may be null
    * @tparam S
    *   String or Option[String]
    * @return
    *   the {@code true} if any of the chars are found, {@code false} if no match or null input
    */
  def containsAny[S: TypeOptions4F[Seq, *, Seq[Char], Seq[CharSequence], Seq[Option[Char]], Seq[Option[CharSequence]]]](
    searchArgs: S*
  ): Boolean = {
    def dealWithSeqChar(chars: Seq[Char]): Boolean = Strings.containsAny(strOrNull, chars.toArray[Char]: _*)
    def mapping = TypeMapping.getMapping[TypeOptions4[*, Seq[Char], Seq[CharSequence], Seq[Option[Char]], Seq[Option[CharSequence]]]]

    def dealWithSeqCharSequence(css: Seq[CharSequence]): Boolean = if (css.length == 1) {
      Strings.containsAny(strOrNull, css.head)
    } else {
      Strings.containsAny(strOrNull, css: _*)
    }

    if (searchArgs == null) {
      Strings.containsAny(strOrNull, null)
    } else
      mapping
        .input(searchArgs)
        .fold(
          dealWithSeqChar,
          dealWithSeqCharSequence,
          s => dealWithSeqChar(mapTo[Seq[Char]].input(s)),
          s => dealWithSeqCharSequence(mapTo[Seq[CharSequence]].input(s))
        )
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
    * {{{
    *
    * none.containsAnyIgnoreCase(, *) = false
    * "".ops.containsAnyIgnoreCase(*) = false
    * Some(*).ops.containsAnyIgnoreCase(null) = false
    * *.ops.containsAnyIgnoreCase([]) = false
    * "abcd"ops.containsAnyIgnoreCase("ab", null) = true
    * Some("abcd").ops.containsAnyIgnoreCase(Some("ab"), Some("cd")) = true
    * "abc".ops.containsAnyIgnoreCase("d", "abc") = true
    * "abc".ops.containsAnyIgnoreCase("D", "ABC") = true
    * "ABC".ops.containsAnyIgnoreCase( "d", "abc") = true
    *  None.containsIgnoreCase(*) = false
    * Option(*).ops.containsIgnoreCase(None) = false
    * "".ops.containsIgnoreCase "") = true
    * StringUtils.containsIgnoreCase("abc", "") = true
    * StringUtils.containsIgnoreCase("abc", "a") = true
    * StringUtils.containsIgnoreCase("abc", "z") = false
    * StringUtils.containsIgnoreCase("abc", "A") = true
    * StringUtils.containsIgnoreCase("abc", "Z") = false
    * }}}
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
  def containsAnyIgnoreCase[S: TypeOptions2F[Seq, *, Seq[CharSequence], Seq[Option[CharSequence]]]](searchArgs: S*): Boolean = {
    def dealWithSeqCharSeq(strs: Seq[CharSequence]) = Strings.containsAnyIgnoreCase(strOrNull, strs: _*)
    def mapping                                     = TypeMapping.getMapping[TypeOptions2[*, Seq[CharSequence], Seq[Option[CharSequence]]]]

    if (searchArgs == null) {
      Strings.equalsAnyIgnoreCase(strOrNull, null)
    } else {
      mapping.input(searchArgs).fold(dealWithSeqCharSeq, s => dealWithSeqCharSeq(mapTo[Seq[CharSequence]].input(s)))
    }
  }

  /** <p>Checks if CharSequence contains a search CharSequence irrespective of case, handling {@code null}. Case-insensitivity is defined as
    * by {@link String# equalsIgnoreCase ( String )}.
    *
    * <p>A {@code null} CharSequence will return {@code false}.</p>
    *
    * {{{
    *
    * none.ops.containsIgnoreCase(*) = false
    * Option(*).ops.containsIgnoreCase(null) = false
    * Some("").ops.containsIgnoreCase("") = true
    * "abc".ops.containsIgnoreCase(Some("")) = true
    * StringUtils.containsIgnoreCase("abc", "a") = true
    * StringUtils.containsIgnoreCase("abc", "z") = false
    * StringUtils.containsIgnoreCase("abc", "A") = true
    * StringUtils.containsIgnoreCase("abc", "Z") = false
    *
    * }}}
    *
    * @param searchStr
    *   the CharSequence to find, may be null
    * @tparam S
    *   String Or Option[String]
    * @return
    *   true if the CharSequence contains the search CharSequence irrespective of case or false if not or {@code null} string input
    */
  def containsIgnoreCase[S: TypeOptions2[*, String, Option[String]]](searchStr: S): Boolean = {
    val searchStrOrNull = mapToStrOpt.input(searchStr).orNull
    Strings.containsIgnoreCase(strOrNull, searchStrOrNull)
  }

  /** <p> Checks that the CharSequence does not contain certain characters. </p>
    *
    * <p>
    *
    * A {@code None} CharSequence will return {@code true}. A {@code null} invalid character array will return {@code true}. An empty String
    * ("") always returns true.</p>
    *
    * {{{
    *
    * none.ops.containsNone(*) = true
    * Option(*).ops.containsNone(null) = true
    * Some("").ops.containsNone(*) = true
    * "ab".ops.containsNone("") = true
    * Some("abab").ops.containsNone("xyz") = true
    * "ab1".ops.containsNone("xyz") = true
    * "abz".ops.containsNone("xyz") = false
    *
    * }}}
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
    * {{{
    *
    * none.ops.containsNone(*) = true
    * Option(*).ops.containsNone(None) = true
    * Some("").ops.containsNone(*) = true
    * "ab".ops.containsNone(Some("")) = true
    * Some("abab").ops.containsNone(Some("xyz")) = true
    * "ab1".ops.containsNone(Some("xyz")) = true
    * "abz".ops.containsNone(Some("xyz")) = false
    *
    * }}}
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
    * {{{
    *
    * none.ops.containsNone(*) = true
    * *.ops.containsNone(null) = true
    * "".ops.containsNone(*) = true
    * "ab".ops.containsNone('') = true
    * "abab".ops.containsNone('xyz') = true
    * "ab1".ops.containsNone('xyz') = true
    * "abz".ops.containsNone('xyz') = false
    *
    * }}}
    *
    * @param invalidChars
    *   an array of invalid chars, may be null
    * @tparam S
    *   var args of chars
    * @return
    *   true if it contains none of the invalid chars, or is null
    */
  def containsNone[I: TypeOptions2F[Seq, *, Seq[Char], Seq[Option[Char]]]](invalidChars: I*): Boolean = {
    def dealWithSeqChar(chars: Seq[Char]): Boolean = Strings.containsNone(strOrNull, chars: _*)
    val mapping                                    = TypeMapping.getMapping[TypeOptions2[*, Seq[Char], Seq[Option[Char]]]]
    mapping.input(invalidChars).fold(dealWithSeqChar, s => dealWithSeqChar(mapTo[Seq[Char]].input(s)))
  }

  /** <p>Checks if the CharSequence contains only certain characters.</p>
    *
    * <p>A {@code null} CharSequence will return {@code false}. A {@code null} valid character String will return {@code false}. An empty
    * String (length()=0) always returns {@code true}.</p>
    *
    * {{{
    *
    * none.ops.containsOnly(*) = false
    * *.ops.containsOnly(null) = false
    * "".ops.containsOnly(*) = true
    * Some("ab").ops.containsOnly("") = false
    * "abab".ops.containsOnly("abc") = true
    * "ab1".ops.containsOnly("abc") = false
    * "abz".ops.containsOnly("abc") = false
    *
    * }}}
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
    * {{{
    *
    * none.ops.containsOnly(*) = false
    * *.ops.containsOnly(None) = false
    * "".ops.containsOnly(*) = true
    * Some("ab").ops.containsOnly(Some("")) = false
    * "abab".ops.containsOnly(Some("abc")) = true
    * "ab1".ops.containsOnly(Some("abc")) = false
    * "abz".ops.containsOnly(Some("abc")) = false
    *
    * }}}
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
    * {{{
    *
    * none.ops.containsOnly(*) = false
    * *.ops.containsOnly(null) = false
    * Some("").containsOnly(*) = true
    * "ab".ops.containsOnly('') = false
    * Some("abab").ops.containsOnly('abc') = true
    * "ab1".ops.containsOnly('abc') = false
    * "abz".ops.containsOnly('abc') = false
    *
    * }}}
    *
    * @param valid
    *   an array of valid chars, may be null
    * @tparam V
    *   var args of char
    * @return
    *   true if it only contains valid chars and is non-null
    */
  def containsOnly[V: TypeOptions2F[Seq, *, Seq[Char], Seq[Option[Char]]]](valid: V*): Boolean = {
    val mapping                           = TypeMapping.getMapping[TypeOptions2[*, Seq[Char], Seq[Option[Char]]]]
    def dealWithSeqChar(chars: Seq[Char]) = Strings.containsOnly(strOrNull, chars: _*)
    mapping.input(valid).fold(dealWithSeqChar, s => dealWithSeqChar(mapTo[Seq[Char]].input(s)))
  }

  /** <p>Check whether the given CharSequence contains any whitespace characters.</p>
    *
    * <p>Whitespace is defined by {@link Character# isWhitespace ( char )}.</p>
    *
    * @return
    *   if the CharSequence is not empty and contains at least 1 (breaking) whitespace character
    */
  def containsWhitespace: Boolean = Strings.containsWhitespace(strOrNull)

  /** <p>Counts how many times the char appears in the given string.</p>
    *
    * <p>A {@code null} or empty ("") String input returns {@code 0}.</p>
    *
    * {{{
    *
    * none.ops.countMatches(*) = 0
    * "".ops.countMatches(*) = 0
    * "abba".ops.countMatches(0) = 0
    * "abba".ops.countMatches('a') = 2
    * "abba".ops.countMatches('b') = 2
    * "abba".ops.countMatches('x') = 0
    *
    * }}}
    *
    * @param ch
    *   the char to count
    * @return
    *   the number of occurrences, 0 if the CharSequence is {@code null}
    */
  def countMatches(ch: Char): Int = Strings.countMatches(strOrNull, ch)

  /** <p>Counts how many times the substring appears in the larger string. Note that the code only counts non-overlapping matches.</p>
    *
    * <p>A {@code null} or empty ("") String input returns {@code 0}.</p>
    *
    * {{{
    *
    * none.ops.countMatches(*) = 0
    * "".ops.countMatches(*) = 0
    * "abba".ops.countMatches(null) = 0
    * "abba".ops.countMatches("") = 0
    * "abba".ops.countMatches("a") = 2
    * "abba".ops.countMatches("ab") = 1
    * "abba".ops.countMatches("xxx") = 0
    * "ababa".ops.countMatches("aba") = 1
    *
    * }}}
    *
    * @param str
    *   the CharSequence to check, may be null
    * @param sub
    *   the substring to count, may be null
    * @tparam S
    *   String or Option[String]
    * @return
    *   the number of occurrences, 0 if either CharSequence is {@code null}
    */
  def countMatches[S: TypeOptions2[*, String, Option[String]]](sub: S): Int = {
    val str1 = mapToStrOpt.input(sub).orNull
    Strings.countMatches(strOrNull, str1)
  }

  /** <p>Counts how many times the substring appears in the larger string. Note that the code only counts non-overlapping matches.</p>
    *
    * <p>A {@code null} or empty ("") String input returns {@code 0}.</p>
    *
    * {{{
    *
    * none.ops.countMatches(*) = 0
    * "".ops.countMatches(*) = 0
    * "abba".ops.countMatches(null) = 0
    * "abba".ops.countMatches("") = 0
    * "abba".ops.countMatches("a") = 2
    * "abba".ops.countMatches("ab") = 1
    * "abba".ops.countMatches("xxx") = 0
    * "ababa".ops.countMatches("aba") = 1
    *
    * }}}
    *
    * @param str
    *   the CharSequence to check, may be null
    * @param sub
    *   the substring to count, may be null
    * @tparam S
    *   String or Option[String]
    * @return
    *   the number of occurrences, 0 if either CharSequence is {@code null}
    */
  def defaultIfBlank[S: TypeOptions2[*, CharSequence, Option[CharSequence]]](defaultStr: S): CharSequence = {
    val defStr = mapTo[Option[CharSequence]].input(defaultStr).orNull
    val result = Strings.defaultIfBlank(strOrNull, defStr)
    result
  }

  /** Returns either the passed in CharSequence, or if the CharSequence is empty or {@code null}, the value of {@code defaultStr}.
    *
    * {{{
    * none.ops.defaultIfEmpty("NULL")  = "NULL"
    * "".ops.defaultIfEmpty("NULL")    = "NULL"
    * " ".ops.defaultIfEmpty("NULL")   = " "
    * "bat".ops.defaultIfEmpty("NULL") = "bat"
    * "".ops.defaultIfEmpty(null)      = null
    * }}}
    *
    * @param defaultStr
    *   the default CharSequence to return
    * @tparam S
    *   CharSequence or Option[CharSequence]
    * @return
    *   the passed in CharSequence, or the default
    */
  def defaultIfEmpty[S: TypeOptions2[*, CharSequence, Option[CharSequence]]](defaultStr: S): CharSequence = {
    val str1   = mapTo[Option[CharSequence]].input(defaultStr).orNull
    val result = Strings.defaultIfEmpty(strOrNull, str1)
    result
  }

  /** <p>Returns either the passed in String, or if the String is {@code null}, an empty String ("").</p>
    *
    * {{{
    * none.ops.defaultString  = ""
    * "".ops.defaultString    = ""
    * "bat".ops.defaultString = "bat"
    * }}}
    *
    * @return
    *   the passed in String, or the empty String if it
    */
  def defaultString: String = Strings.defaultString(strOrNull)

  /** <p>Returns either the passed in String, or if the String is {@code null}, the value of {@code defaultStr}.</p>
    *
    * {{{
    * none.ops.defaultString("NULL")  = "NULL"
    * "".ops.defaultString("NULL")    = ""
    * "bat".ops.defaultString("NULL") = "bat"
    * }}}
    *
    * @param defaultStr
    *   the default String to return
    * @tparam S
    *   String or Option[String]
    * @return
    *   the passed in String, or the default if it was {@code null}
    */
  def defaultString[S: TypeOptions2[*, String, Option[String]]](defaultStr: S): String = {
    val str1   = mapToStrOpt.input(defaultStr).orNull
    val result = Strings.defaultString(strOrNull, str1)
    result
  }

  /** <p>Deletes all whitespaces from a String as defined by {@link Character# isWhitespace ( char )}.</p>
    *
    * {{{
    * null.ops.deleteWhitespace()         = null
    * "".ops.deleteWhitespace()           = ""
    * "abc".ops.deleteWhitespace()        = "abc"
    * "   ab  c  ".ops.deleteWhitespace() = "abc"
    * }}}
    *
    * @return
    *   the String to delete whitespace from, may be null
    */
  def deleteWhitespace(): Option[String] = strOpt.map(Strings.deleteWhitespace)

  /** <p>Compares two Strings, and returns the portion where they differ. More precisely, return the remainder of the second String,
    * starting from where it's different from the first. This means that the difference between "abc" and "ab" is the empty String and not
    * "c". </p>
    *
    * <p>For example, {@code "i am a machine".ops.difference("i am a robot") -> "robot"}.</p>
    *
    * {{{
    * none.ops.difference(null) = null
    * "".ops.difference("") = ""
    * "".ops.difference("abc") = "abc"
    * "abc".ops.difference("") = ""
    * "abc".ops.difference("abc") = ""
    * "abc".ops.difference("ab") = ""
    * "ab".ops.difference("abxyz") = "xyz"
    * "abcde".ops.difference("abxyz") = "xyz"
    * "abcde".ops.difference("xyz") = "xyz"
    * }}}
    *
    * @param other
    *   the other String, may be null or Option
    * @tparam S
    *   String or Option[String]
    * @return
    *   the portion of str2 where it differs from str1; returns the empty String if they are equal
    */
  def difference[S: TypeOptions2[*, String, Option[String]]](other: S): Option[String] = {
    val str1   = mapToStrOpt.input(other).orNull
    val result = Strings.difference(strOrNull, str1)
    Option(result)
  }

  /** <p>Check if a CharSequence ends with a specified suffix.</p>
    *
    * <p>{@code null}s are handled without exceptions. Two {@code null} references are considered to be equal. The comparison is case
    * sensitive.</p>
    *
    * {{{
    * none.ops.endsWith(null)      = true
    * null.ops.endsWith("def")     = false
    * "abcdef".ops.endsWith(null)  = false
    * "abcdef".ops.endsWith("def") = true
    * "ABCDEF".ops.endsWith("def") = false
    * "ABCDEF".ops.endsWith("cde") = false
    * "ABCDEF".ops.endsWith("")    = true
    * }}}
    *
    * @param suffix
    *   the suffix to find, may be null
    * @tparam S
    *   String or Option[String]
    * @return
    *   if the CharSequence ends with the suffix, case sensitive, or both {@code null}
    */
  def endsWith[S: TypeOptions2[*, String, Option[String]]](suffix: S): Boolean = {
    val str1 = mapToStrOpt.input(suffix).orNull
    Strings.endsWith(strOrNull, str1)
  }

  /** <p>Check if a CharSequence ends with any of the provided case-sensitive suffixes.</p>
    *
    * {{{
    * none.ops.endsWithAny(null)      = false
    * null.ops.endsWithAny(new String[] {"abc"})  = false
    * "abcxyz".ops.endsWithAny(null)     = false
    * "abcxyz".ops.endsWithAny(new String[] {""}) = true
    * "abcxyz".ops.endsWithAny(new String[] {"xyz"}) = true
    * "abcxyz".ops.endsWithAny(new String[] {null, "xyz", "abc"}) = true
    * "abcXYZ".ops.endsWithAny("def", "XYZ") = true
    * "abcXYZ".ops.endsWithAny("def", "xyz") = false
    * }}}
    *
    * @param searchStrings
    *   the case-sensitive CharSequences to find, may be empty or contain {@code null}
    * @tparam S
    *   CharSequence or Option[CharSequence]
    * @return
    *   {@code true} if the input {@code sequence} is {@code null} AND no {@code searchStrings} are provided, or the input {@code sequence}
    *   ends in any of the provided case-sensitive {@code searchStrings}.
    */
  def endsWithAny[S: TypeOptions2F[Seq, *, Seq[CharSequence], Seq[Option[CharSequence]]]](searchStrings: S*): Boolean = {
    def mapping                                    = TypeMapping.getMapping[TypeOptions2[*, Seq[CharSequence], Seq[Option[CharSequence]]]]
    def dealWithSeqString(strs: Seq[CharSequence]) = Strings.endsWithAny(strOrNull, strs: _*)

    if (searchStrings == null)
      Strings.endsWithAny(strOrNull, null)
    else
      mapping.input(searchStrings).fold(dealWithSeqString, s => dealWithSeqString(mapTo[Seq[CharSequence]].input(s)))
  }

  /** <p>Case insensitive check if a CharSequence ends with a specified suffix.</p>
    *
    * <p>{@code null}s are handled without exceptions. Two {@code null} references are considered to be equal. The comparison is case
    * insensitive.</p>
    *
    * {{{
    * none.ops.endsWithIgnoreCase(null)      = true
    * null.ops.endsWithIgnoreCase("def")     = false
    * "abcdef".ops.endsWithIgnoreCase(null)  = false
    * "abcdef".ops.endsWithIgnoreCase("def") = true
    * "ABCDEF".ops.endsWithIgnoreCase("def") = true
    * "ABCDEF".ops.endsWithIgnoreCase("cde") = false
    * }}}
    *
    * @param suffix
    *   the suffix to find, may be null
    * @tparam S
    *   CharSequence Or Option[CharSequence]
    * @return
    *   {@code true} if the CharSequence ends with the suffix, case insensitive, or both {@code null}
    */
  def endsWithIgnoreCase[S: TypeOptions2[*, CharSequence, Option[CharSequence]]](suffix: S): Boolean = {
    val str2 = mapToCsOpt.input(suffix).orNull
    Strings.endsWithIgnoreCase(strOrNull, str2)
  }

  /** * <p>Compares two CharSequences, returning {@code true} if they represent equal sequences of characters.</p>
    *
    * NOTE: I package it just for symmetry. We have equals ignore case, so we should have queals. But the semantic of ops's equals method
    * has a little strange smell. em...
    *
    * Please use buildin equals in Object, or static equals method in commons string utils or Objects type as far as possible.
    *
    * <p>{@code null}s are handled without exceptions. Two {@code null} references are considered to be equal. The comparison is
    * <strong>case sensitive</strong>.</p>
    *
    * <pre> none.ops.equals(null) = true none.ops.equals("abc") = false "abc".ops.equals(null) = false "abc".ops.equals("abc") = true
    * "abc".ops.equals("ABC") = false </pre>
    *
    * @param other
    * @tparam S
    * @return
    */
  def equals[S: TypeOptions2[*, CharSequence, Option[CharSequence]]](other: S): Boolean = {
    val str2 = mapToCsOpt.input(other).orNull
    Strings.endsWithIgnoreCase(strOrNull, str2)
  }

  /** * <p>Compares given {@code string} to a CharSequences vararg of {@code searchStrings}, returning {@code true} if the {@code string} is
    * equal to any of the {@code searchStrings}.</p>
    *
    * {{{
    * StringUtils.equalsAny(null, (CharSequence[]) null) = false
    * StringUtils.equalsAny(null, null, null)    = true
    * StringUtils.equalsAny(null, "abc", "def")  = false
    * StringUtils.equalsAny("abc", null, "def")  = false
    * StringUtils.equalsAny("abc", "abc", "def") = true
    * StringUtils.equalsAny("abc", "ABC", "DEF") = false
    * }}}
    *
    * @param searchStrings
    *   a vararg of strings, may be {@code null}.
    * @tparam S
    *   CharSequence or Option[CharSequence]
    * @return
    *   {@code true} if the string is equal (case-sensitive) to any other element of {@code searchStrings}; {@code false} if {@code
    *   searchStrings} is null or contains no matches.
    */
  def equalsAnyIgnoreCase[S: TypeOptions2F[Seq, *, Seq[CharSequence], Seq[Option[CharSequence]]]](searchStrings: S*): Boolean = {
    def mapping                                    = TypeMapping.getMapping[TypeOptions2[*, Seq[CharSequence], Seq[Option[CharSequence]]]]
    def dealWithSeqString(strs: Seq[CharSequence]) = Strings.equalsAnyIgnoreCase(strOrNull, strs: _*)

    if (searchStrings == null)
      Strings.equalsAnyIgnoreCase(strOrNull, null)
    else
      mapping.input(searchStrings).fold(dealWithSeqString, s => dealWithSeqString(mapTo[Seq[CharSequence]].input(s)))
  }

  /** <p>Compares two CharSequences, returning {@code true} if they represent equal sequences of characters, ignoring case.</p>
    *
    * <p>{@code null}s are handled without exceptions. Two {@code null} references are considered equal. The comparison is <strong>case
    * insensitive</strong>.</p>
    *
    * {{{
    * none.ops.equalsIgnoreCase(null)   = true
    * null.ops.equalsIgnoreCase("abc")  = false
    * "abc".ops.equalsIgnoreCase(null)  = false
    * "abc".ops.equalsIgnoreCase("abc") = true
    * "abc".ops.equalsIgnoreCase("ABC") = true
    * }}}
    *
    * @param other
    *   the other CharSequence, may be {@code null}
    * @tparam S
    * @return
    */
  def equalsIgnoreCase[S: TypeOptions2[*, CharSequence, Option[CharSequence]]](other: S): Boolean = {
    val str1 = mapToCsOpt.input(other).orNull
    Strings.equalsIgnoreCase(strOrNull, str1)
  }

  /** Calls {@link String#getBytes(Charset)} in a null-safe manner.
    * @param charset
    *   The {@link Charset} to encode the {@code String}. If null, then use the default Charset.
    * @tparam C
    *   Charset or Option[CharSet]
    * @return
    *   The empty byte[] if {@code string} is null, the result of {@link String# getBytes ( Charset )} otherwise.
    */
  def getBytes[C: TypeOptions4[*, Charset, Option[Charset], String, Option[String]]](charset: C): Array[Byte] = {
    val mapping = TypeMapping.getMapping[TypeOptions4[*, Charset, Option[Charset], String, Option[String]]]

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

  /** <p>Checks if a String {@code str} contains Unicode digits, if yes then concatenate all the digits in {@code str} and return it as a
    * String.</p>
    *
    * <p>An empty ("") String will be returned if no digits found in {@code str}.</p>
    *
    * {{{
    * none.ops.getDigits  = null
    * "".ops.getDigits    = ""
    * "abc".ops.getDigits = ""
    * "1000$".ops.getDigits = "1000"
    * "1123~45".ops.getDigits = "112345"
    * "(541) 754-3010".ops.getDigits = "5417543010"
    * "\u0967\u0968\u0969".ops.getDigits = "\u0967\u0968\u0969"
    * }}}
    *
    * @return
    */
  def getDigits: String = Strings.getDigits(strOrNull)

  /** <p>Returns either the passed in CharSequence, or if the CharSequence is whitespace, empty ("") or {@code null}, the value supplied by
    * {@code defaultStrSupplier}.</p>
    *
    * <p>Whitespace is defined by {@link Character# isWhitespace ( char )}.</p>
    *
    * <p>Caller responsible for thread-safety and exception handling of default value supplier</p>
    *
    * {{{
    *
    * StringUtils.getIfBlank(null, () -> "NULL")   = "NULL"
    * StringUtils.getIfBlank("", () -> "NULL")     = "NULL"
    * StringUtils.getIfBlank(" ", () -> "NULL")    = "NULL"
    * StringUtils.getIfBlank("bat", () -> "NULL")  = "bat"
    * StringUtils.getIfBlank("", () -> null)       = null
    * StringUtils.getIfBlank("", null)             = null
    * }}}
    * @param defaultSupplier
    *   the supplier of default CharSequence to return
    * @tparam S
    *   CharSequence or Option[CharSequence]
    * @return
    *   the passed in CharSequence, or the default
    */
  def getIfBlank[S: TypeOptions2[*, CharSequence, Option[CharSequence]]](defaultSupplier: Supplier[S]): CharSequence =
    if (defaultSupplier == null) Strings.getIfBlank(strOrNull, null)
    else {
      val supplier: Supplier[CharSequence] = () => mapTo[Option[CharSequence]].input(defaultSupplier.get()).orNull
      Strings.getIfBlank(strOrNull, supplier)
    }

  /** <p>Returns either the passed in CharSequence, or if the CharSequence is empty or {@code null}, the value supplied by {@code
    * defaultStrSupplier}.
    *
    * <p>Caller responsible for thread-safety and exception handling of default value supplier</p>
    *
    * {{{
    *
    * null.ops.getIfEmpty(() =>"NULL")    = "NULL"
    * "".ops.getIfEmpty(() -> "NULL")      = "NULL"
    * " ".ops.getIfEmpty(() -> "NULL")     = " "
    * "bat".ops.getIfEmpty(() -> "NULL")   = "bat"
    * "".ops.getIfEmpty(() -> null)        = null
    * "".ops.getIfEmpty(null)              = null
    *
    * }}}
    *
    * @param defaultSupplier
    *   the supplier of default CharSequence to return
    * @tparam S
    *   CharSequence or Option[CharSequence]
    * @return
    *   the passed in CharSequence, or the default
    */
  def getIfEmpty[S: TypeOptions2[*, CharSequence, Option[CharSequence]]](defaultSupplier: Supplier[S]): CharSequence =
    if (defaultSupplier == null) Strings.getIfEmpty(strOrNull, null)
    else {
      val supplier: Supplier[CharSequence] = () => mapTo[Option[CharSequence]].input(defaultSupplier.get()).orNull
      Strings.getIfEmpty(strOrNull, supplier)
    }

  /** <p>Finds the first index within a CharSequence, handling {@code null}. This method uses {@link String# indexOf ( String, int)} if
    * possible.</p>
    *
    * <p>A {@code null} CharSequence will return {@code -1}.</p>
    *
    * {{{
    *
    * none.ops.indexOf(*) = -1
    * *.ops.indexOf(null) = -1
    * "".ops.indexOf("") = 0
    * "".ops.indexOf(*) = -1 (except when * = "")
    * "aabaabaa".ops.indexOf("a") = 0
    * "aabaabaa".ops.indexOf("b") = 2
    * "aabaabaa".ops.indexOf("ab") = 1
    * "aabaabaa".ops.indexOf("") = 0
    * }}}
    *
    * @param searchSeq
    *   the CharSequence to find, may be null
    * @tparam S
    *   CharSequence or Option[CharSequence]
    * @return
    *   the first index of the search CharSequence,
    * -1 if no match or {@code null} string input
    */
  def indexOf[S: TypeOptions2[*, CharSequence, Option[CharSequence]]](searchSeq: S): Int = {
    val str1 = mapToCsOpt.input(searchSeq).orNull
    Strings.indexOf(strOrNull, str1)
  }

  /** <p>Finds the first index within a CharSequence, handling {@code null}. This method uses {@link String# indexOf ( String, int)} if
    * possible.</p>
    *
    * <p>A {@code null} CharSequence will return {@code -1}. A negative start position is treated as zero. An empty ("") search CharSequence
    * always matches. A start position greater than the string length only matches an empty search CharSequence.</p>
    *
    * {{{
    * none.ops.indexOf(*, *)          = -1
    * *.ops.indexOf(null, *)          = -1
    * "".ops.indexOf("", 0)           = 0
    * "".ops.indexOf(*, 0)            = -1 (except when * = "")
    * "aabaabaa".ops.indexOf("a", 0)  = 0
    * "aabaabaa".ops.indexOf("b", 0)  = 2
    * "aabaabaa".ops.indexOf("ab", 0) = 1
    * "aabaabaa".ops.indexOf("b", 3)  = 5
    * "aabaabaa".ops.indexOf("b", 9)  = -1
    * "aabaabaa".ops.indexOf("b", -1) = 2
    * "aabaabaa".ops.indexOf("", 2)   = 2
    * "abc".ops.indexOf("", 9)        = 3
    * }}}
    *
    * @param searchSeq
    *   the CharSequence to find, may be null
    * @param startPos
    *   the start position, negative treated as zero
    * @tparam S
    *   CharSequence or Option[CharSequence]
    * @return
    *   the first index of the search CharSequence (always &ge; startPos),
    * -1 if no match or {@code null} string input
    */
  def indexOf[S: TypeOptions2[*, CharSequence, Option[CharSequence]]](searchSeq: S, startPos: Int): Int = {
    val str1 = mapToCsOpt.input(searchSeq).orNull
    Strings.indexOf(strOrNull, str1, startPos)
  }

  /** Returns the index within {@code seq} of the first occurrence of the specified character. If a character with value {@code searchChar}
    * occurs in the character sequence represented by {@code seq} {@code CharSequence} object, then the index (in Unicode code units) of the
    * first such occurrence is returned. For values of {@code searchChar} in the range from 0 to 0xFFFF (inclusive), this is the smallest
    * value <i>k</i> such that:
    * {{{
    * this.charAt(<i>k</i>) == searchChar
    * }}}
    * is true. For other values of {@code searchChar}, it is the smallest value <i>k</i> such that:
    * {{{
    * this.codePointAt(<i>k</i>) == searchChar
    * }}}
    * is true. In either case, if no such character occurs in {@code seq}, then {@code INDEX_NOT_FOUND (-1)} is returned.
    *
    * <p>Furthermore, a {@code null} or empty ("") CharSequence will return {@code INDEX_NOT_FOUND (-1)}.</p>
    *
    * {{{
    * StringUtils.indexOf(null, *)         = -1
    * StringUtils.indexOf("", *)           = -1
    * StringUtils.indexOf("aabaabaa", 'a') = 0
    * StringUtils.indexOf("aabaabaa", 'b') = 2
    * }}}
    *
    * @param searchChar
    *   the character to find
    * @return
    *   the first index of the search character,
    * -1 if no match or {@code null} string input
    */
  def indexOf(searchChar: Char): Int = Strings.indexOf(strOrNull, searchChar)

  /** Returns the index within {@code seq} of the first occurrence of the specified character. If a character with value {@code searchChar}
    * occurs in the character sequence represented by {@code seq} {@code CharSequence} object, then the index (in Unicode code units) of the
    * first such occurrence is returned. For values of {@code searchChar} in the range from 0 to 0xFFFF (inclusive), this is the smallest
    * value <i>k</i> such that:
    * {{{
    * this.charAt(<i>k</i>) == searchChar
    * }}}
    * is true. For other values of {@code searchChar}, it is the smallest value <i>k</i> such that:
    * {{{
    * this.codePointAt(<i>k</i>) == searchChar
    * }}}
    * is true. In either case, if no such character occurs in {@code seq}, then {@code INDEX_NOT_FOUND (-1)} is returned.
    *
    * <p>Furthermore, a {@code null} or empty ("") CharSequence will return {@code INDEX_NOT_FOUND (-1)}.</p>
    *
    * {{{
    * StringUtils.indexOf(null, *)         = -1
    * StringUtils.indexOf("", *)           = -1
    * StringUtils.indexOf("aabaabaa", 'a') = 0
    * StringUtils.indexOf("aabaabaa", 'b') = 2
    * }}}
    *
    * @param searchChar
    *   the character to find
    * @return
    *   the first index of the search character,
    * -1 if no match or {@code null} string input
    */
  def indexOf(searchChar: Int): Int = Strings.indexOf(strOrNull, searchChar)

  /** Returns the index within {@code seq} of the first occurrence of the specified character, starting the search at the specified index.
    * <p> If a character with value {@code searchChar} occurs in the character sequence represented by the {@code seq} {@code CharSequence}
    * object at an index no smaller than {@code startPos}, then the index of the first such occurrence is returned. For values of {@code
    * searchChar} in the range from 0 to 0xFFFF (inclusive), this is the smallest value <i>k</i> such that:
    * {{{
    * (this.charAt(<i>k</i>) == searchChar) &amp;&amp; (<i>k</i> >= startPos)
    * }}}
    * is true. For other values of {@code searchChar}, it is the smallest value <i>k</i> such that:
    * {{{
    * (this.codePointAt(<i>k</i>) == searchChar) && (<i>k</i> >= startPos)
    * }}}
    * is true. In either case, if no such character occurs in {@code seq} at or after position {@code startPos}, then {@code -1} is
    * returned.
    *
    * <p> There is no restriction on the value of {@code startPos}. If it is negative, it has the same effect as if it were zero: this
    * entire string may be searched. If it is greater than the length of this string, it has the same effect as if it were equal to the
    * length of this string: {@code (INDEX_NOT_FOUND) -1} is returned. Furthermore, a {@code null} or empty ("") CharSequence will return
    * {@code (INDEX_NOT_FOUND) -1}.
    *
    * <p>All indices are specified in {@code char} values (Unicode code units).
    *
    * {{{
    * none.ops.indexof(*, *)          = -1
    * "".ops.indexof(*, *)            = -1
    * "aabaabaa".ops.indexOf('b', 0)  = 2
    * "aabaabaa".ops.indexOf('b', 3)  = 5
    * "aabaabaa".ops.indexOf('b', 9)  = -1
    * "aabaabaa".ops.indexOf'b', -1)  = 2
    * }}}
    *
    * @param searchChar
    *   the character to find
    * @param startPos
    *   the start position, negative treated as zero
    * @return
    *   the first index of the search character (always &ge; startPos),
    * -1 if no match or {@code null} string input
    */
  def indexOf(searchChar: Char, startPos: Int): Int = Strings.indexOf(strOrNull, searchChar, startPos)

  /** Returns the index within {@code seq} of the first occurrence of the specified character, starting the search at the specified index.
    * <p> If a character with value {@code searchChar} occurs in the character sequence represented by the {@code seq} {@code CharSequence}
    * object at an index no smaller than {@code startPos}, then the index of the first such occurrence is returned. For values of {@code
    * searchChar} in the range from 0 to 0xFFFF (inclusive), this is the smallest value <i>k</i> such that:
    * {{{
    * (this.charAt(<i>k</i>) == searchChar) &amp;&amp; (<i>k</i> >= startPos)
    * }}}
    * is true. For other values of {@code searchChar}, it is the smallest value <i>k</i> such that:
    * {{{
    * (this.codePointAt(<i>k</i>) == searchChar) && (<i>k</i> >= startPos)
    * }}}
    * is true. In either case, if no such character occurs in {@code seq} at or after position {@code startPos}, then {@code -1} is
    * returned.
    *
    * <p> There is no restriction on the value of {@code startPos}. If it is negative, it has the same effect as if it were zero: this
    * entire string may be searched. If it is greater than the length of this string, it has the same effect as if it were equal to the
    * length of this string: {@code (INDEX_NOT_FOUND) -1} is returned. Furthermore, a {@code null} or empty ("") CharSequence will return
    * {@code (INDEX_NOT_FOUND) -1}.
    *
    * <p>All indices are specified in {@code char} values (Unicode code units).
    *
    * {{{
    * none.ops.indexof(*, *)          = -1
    * "".ops.indexof(*, *)            = -1
    * "aabaabaa".ops.indexOf('b', 0)  = 2
    * "aabaabaa".ops.indexOf('b', 3)  = 5
    * "aabaabaa".ops.indexOf('b', 9)  = -1
    * "aabaabaa".ops.indexOf'b', -1)  = 2
    * }}}
    *
    * @param searchChar
    *   the character to find
    * @param startPos
    *   the start position, negative treated as zero
    * @return
    *   the first index of the search character (always &ge; startPos),
    * -1 if no match or {@code null} string input
    */
  def indexOf(searchChar: Int, startPos: Int): Int = Strings.indexOf(strOrNull, searchChar, startPos)

  /** <p>Search a CharSequence to find the first index of any character in the given set of characters.</p>
    *
    * <p>A {@code null} String will return {@code -1}. A {@code null} or zero length search array will return {@code -1}.</p>
    *
    * {{{
    * null.ops.indexOfAny(*)                  = -1
    * "".ops.indexOfAny(*)                    = -1
    * *.ops.indexOfAny(null)                  = -1
    * *.ops.indexOfAny([])                    = -1
    * "zzabyycdxx".ops.indexOfAny('z', 'a') = 0
    * "zzabyycdxx".ops.indexOfAny('b', 'y') = 3
    * "aba", 'z')             = -1
    *
    * null.ops.indexOfAny(*)                      = -1
    * *.ops.indexOfAny(null)                      = -1
    * *.ops.indexOfAny([])                        = -1
    * "zzabyycdxx".ops.indexOfAny("ab", "cd")   = 2
    * "zzabyycdxx".ops.indexOfAny("cd", "ab")   = 2
    * "zzabyycdxx".ops.indexOfAny("mn", "op")   = -1
    * "zzabyycdxx".ops.indexOfAny("zab", "aby") = 1
    * "zzabyycdxx".ops.indexOfAny("")           = 0
    * "".ops.indexOfAny("")                     = 0
    * "".ops.indexOfAny("a")                    = -1
    * }}}
    *
    * @param searchArgs
    *   the chars to search for, may be null
    * @tparam S
    *   varargs of Char, CharSequence, Option[Char] or Option[CharSequence]
    * @return
    *   the index of any of the chars, -1 if no match or null input
    */
  def indexOfAny[
    S: TypeOptions3F[Seq, *, Seq[Char], Seq[CharSequence], Seq[Option[CharSequence]]]: TypeOptions3[*, Char, CharSequence, Option[
      SeqCharSequence
    ]]
  ](
    searchArgs: S*
  ): Int = {
    def seqMapping  = TypeMapping.getMapping[TypeOptions3[*, Seq[Char], Seq[CharSequence], Seq[Option[CharSequence]]]]
    def charMapping = TypeMapping.getMapping[TypeOptions3[*, Char, CharSequence, Option[SeqCharSequence]]]
    def indexOfNull = Strings.indexOfAny(strOrNull, null)

    def dealWithSeqChar(chars: Seq[Char])             = Strings.indexOfAny(strOrNull, chars: _*)
    def dealWithSeqString(strings: Seq[CharSequence]) = Strings.indexOfAny(strOrNull, strings: _*)
    def dealWithChar(char: Char)                      = Strings.indexOfAny(strOrNull, char)
    def dealWithString(string: CharSequence)          = Strings.indexOfAny(strOrNull, string)

    if (searchArgs == null)
      indexOfNull
    else if (searchArgs.length == 1)
      charMapping
        .input(searchArgs.head)
        .fold(
          dealWithChar,
          dealWithString,
          opt => opt.map(dealWithString).getOrElse(indexOfNull)
        )
    else
      seqMapping
        .input(searchArgs)
        .fold(
          dealWithSeqChar,
          dealWithSeqString,
          s => dealWithSeqString(mapTo[Seq[CharSequence]].input(s))
        )
  }

  /** <p>Searches a CharSequence to find the first index of any character not in the given set of characters.</p>
    *
    * <p>A {@code null} CharSequence will return {@code -1}. A {@code null} or zero length search array will return {@code -1}.</p>
    *
    * {{{
    * none.ops.indexOfAnyBut(*)                              = -1
    * "".ops.indexOfAnyBut(*)                                = -1
    * *.ops.indexOfAnyBut(null)                              = -1
    * *.ops.indexOfAnyBut([])                                = -1
    * "zzabyycdxx".ops.indexOfAnyBut('z', 'a') = 3
    * "aba".ops.indexOfAnyBut('z')             = 0
    * "aba".ops.indexOfAnyBut('a', 'b')        = -1
    *
    * }}}
    *
    * @param searchChars
    *   the chars to search for, may be null
    * @return
    *   the index of any of the chars, -1 if no match or null input
    */
  def indexOfAnyBut(searchChars: Char*): Int = Strings.indexOfAnyBut(strOrNull, searchChars: _*)

  /** <p>Search a CharSequence to find the first index of any character not in the given set of characters.</p>
    *
    * <p>A {@code null} CharSequence will return {@code -1}. A {@code null} or empty search string will return {@code -1}.</p>
    *
    * {{{
    * none.ops.indexOfAnyBut(*)            = -1
    * "".ops.indexOfAnyBut(*)              = -1
    * *.ops.indexOfAnyBut(null)            = -1
    * *.ops.indexOfAnyBut("")              = -1
    * "zzabyycdxx".ops.indexOfAnyBut("za") = 3
    * "zzabyycdxx".ops.indexOfAnyBut("")   = -1
    * "aba".ops.indexOfAnyBut("ab")        = -1
    * }}}
    *
    * @param searchChars
    *   the chars to search for, may be null
    * @return
    *   the index of any of the chars, -1 if no match or null input
    */
  def indexOfAnyBut(searchChars: String): Int = Strings.indexOfAnyBut(strOrNull, searchChars)

  /** <p>Search a CharSequence to find the first index of any character not in the given set of characters.</p>
    *
    * <p>A {@code null} CharSequence will return {@code -1}. A {@code null} or empty search string will return {@code -1}.</p>
    *
    * {{{
    * none.ops.indexOfAnyBut(*)            = -1
    * "".ops.indexOfAnyBut(*)              = -1
    * *.ops.indexOfAnyBut(null)            = -1
    * *.ops.indexOfAnyBut("")              = -1
    * "zzabyycdxx".ops.indexOfAnyBut("za") = 3
    * "zzabyycdxx".ops.indexOfAnyBut("")   = -1
    * "aba".ops.indexOfAnyBut("ab")        = -1
    * }}}
    *
    * @param searchChars
    *   the chars to search for, may be null
    * @return
    *   the index of any of the chars, -1 if no match or null input
    */
  def indexOfAnyBut(searchChars: Option[String]): Int = Strings.indexOfAnyBut(strOrNull, searchChars.orNull)

  /** <p>Compares all CharSequences in an array and returns the index at which the CharSequences begin to differ.</p>
    *
    * <p>For example, {@code indexOfDifference(new String[] {"i am a machine", "i am a robot"}) -> 7}</p>
    *
    * {{{
    *
    * none.ops.indexOfDifference(null) = -1
    * "".ops.indexOfDifference("") = -1
    * "".ops.indexOfDifference("abc") = 0
    * "abc".ops.indexOfDifference("") = 0
    * "abc".ops.indexOfDifference("abc") = -1
    * "ab".ops.indexOfDifference("abxyz") = 2
    * "abcde".ops.indexOfDifference("abxyz") = 2
    * "abcde".ops.indexOfDifference("xyz") = 0
    * }}}
    *
    * @param cs
    *   the second CharSequence, may be null
    * @tparam S
    *   CharSequence or Option[CharSequence]
    * @return
    *   the index where cs1 and cs2 begin to differ; -1 if they are equal
    */
  def indexOfDifference[S: TypeOptions2[*, CharSequence, Option[CharSequence]]](cs: S): Int = {
    val str1 = mapToCsOpt.input(cs).orNull
    Strings.indexOfDifference(strOrNull, str1)
  }

  /** <p>Case in-sensitive find of the first index within a CharSequence.</p>
    *
    * <p>A {@code null} CharSequence will return {@code -1}. A negative start position is treated as zero. An empty ("") search CharSequence
    * always matches. A start position greater than the string length only matches an empty search CharSequence.</p>
    *
    * {{{
    * null.ops.indexOfIgnoreCase(*)          = -1
    * *.ops.indexOfIgnoreCase(null)          = -1
    * "".ops.indexOfIgnoreCase("")           = 0
    * "aabaabaa".ops.indexOfIgnoreCase("a")  = 0
    * "aabaabaa".ops.indexOfIgnoreCase("b")  = 2
    * "aabaabaa".ops.indexOfIgnoreCase("ab") = 1
    * }}}
    *
    * @param searchStr
    *   the CharSequence to find, may be null
    * @tparam S
    *   CharSequence or Option[CharSequence]
    * @return
    *   the first index of the search CharSequence,
    * -1 if no match or {@code null} string input
    */
  def indexOfIgnoreCase[S: TypeOptions2[*, CharSequence, Option[CharSequence]]](searchStr: S): Int = {
    val str1 = mapToCsOpt.input(searchStr).orNull
    Strings.indexOfIgnoreCase(strOrNull, str1)
  }

  /** <p>Case in-sensitive find of the first index within a CharSequence from the specified position.</p>
    *
    * <p>A {@code null} CharSequence will return {@code -1}. A negative start position is treated as zero. An empty ("") search CharSequence
    * always matches. A start position greater than the string length only matches an empty search CharSequence.</p>
    *
    * {{{
    * none.ops.indexOfIgnoreCase(*, *)          = -1
    * *.ops.indexOfIgnoreCase(null, *)          = -1
    * "".ops.indexOfIgnoreCase("", 0)           = 0
    * "aabaabaa".ops.indexOfIgnoreCase("A", 0)  = 0
    * "aabaabaa".ops.indexOfIgnoreCase("B", 0)  = 2
    * "aabaabaa".ops.indexOfIgnoreCase("AB", 0) = 1
    * "aabaabaa".ops.indexOfIgnoreCase("B", 3)  = 5
    * "aabaabaa".ops.indexOfIgnoreCase("B", 9)  = -1
    * "aabaabaa".ops.indexOfIgnoreCase("B", -1) = 2
    * "aabaabaa".ops.indexOfIgnoreCase("", 2)   = 2
    * "abc".ops.indexOfIgnoreCase("", 9)        = -1
    * }}}
    *
    * @param searchStr
    *   the CharSequence to find, may be null
    * @param startPos
    *   the start position, negative treated as zero
    * @tparam S
    *   CharSequence or Option[CharSequence]
    * @return
    *   the first index of the search CharSequence (always &ge; startPos),
    * -1 if no match or {@code null} string input
    */
  def indexOfIgnoreCase[S: TypeOptions2[*, CharSequence, Option[CharSequence]]](searchStr: S, startPos: Int): Int = {
    val str1 = mapToCsOpt.input(searchStr).orNull
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

  def lastIndexOf[S: TypeOptions4[*, Char, Int, CharSequence, Option[CharSequence]]](searchArg: S): Int = {
    val mapping = TypeMapping.getMapping[TypeOptions4[*, Char, Int, CharSequence, Option[CharSequence]]]
    mapping
      .input(searchArg)
      .fold(
        ch => Strings.lastIndexOf(strOrNull, ch),
        i => Strings.lastIndexOf(strOrNull, i),
        str => Strings.lastIndexOf(strOrNull, str),
        ostr => Strings.lastIndexOf(strOrNull, ostr.orNull)
      )
  }

  def lastIndexOf[S: TypeOptions4[*, Char, Int, CharSequence, Option[CharSequence]]](searchArg: S, startPos: Int): Int = {
    val mapping = TypeMapping.getMapping[TypeOptions4[*, Char, Int, CharSequence, Option[CharSequence]]]
    mapping
      .input(searchArg)
      .fold(
        ch => Strings.lastIndexOf(strOrNull, ch, startPos),
        i => Strings.lastIndexOf(strOrNull, i, startPos),
        str => Strings.lastIndexOf(strOrNull, str, startPos),
        ostr => Strings.indexOf(strOrNull, ostr.orNull, startPos)
      )
  }

  def lastIndexOfAny[S: TypeOptions2F[Seq, *, Seq[CharSequence], Seq[Option[CharSequence]]]](searchArgs: S*): Int = {
    def mapping                                     = TypeMapping.getMapping[TypeOptions2[*, Seq[CharSequence], Seq[Option[CharSequence]]]]
    def dealWithCharSeqSeq(strs: Seq[CharSequence]) = Strings.lastIndexOfAny(strOrNull, strs: _*)

    if (searchArgs == null)
      Strings.lastIndexOfAny(strOrNull, null)
    else
      mapping.input(searchArgs).fold(dealWithCharSeqSeq, s => dealWithCharSeqSeq(mapTo[Seq[CharSequence]].input(s)))
  }

  def lastIndexOfIgnoreCase[S: TypeOptions2[*, String, Option[String]]](searchStr: S): Int = {
    val str1 = mapToStrOpt.input(searchStr).orNull
    Strings.lastIndexOfIgnoreCase(strOrNull, str1)
  }

  def lastIndexOfIgnoreCase[S: TypeOptions2[*, String, Option[String]]](searchStr: S, startPos: Int): Int = {
    val str1 = mapToStrOpt.input(searchStr).orNull
    Strings.lastIndexOfIgnoreCase(strOrNull, str1, startPos)
  }

  def lastOrdinalIndexOf[S: TypeOptions2[*, String, Option[String]]](searchStr: S, ordinal: Int): Int = {
    val str1 = mapToStrOpt.input(searchStr).orNull
    Strings.lastOrdinalIndexOf(strOrNull, str1, ordinal)
  }

  def left(len: Int): Option[String] = Option(Strings.left(strOrNull, len))

  def leftPad(size: Int): Option[String] = Option(Strings.leftPad(strOrNull, size))

  def leftPad(size: Int, padChar: Char): Option[String] = Option(Strings.leftPad(strOrNull, size, padChar))

  def leftPad[P: TypeOptions2[*, String, Option[String]]](size: Int, padStr: P): Option[String] = {
    val ps = mapToStrOpt.input(padStr).orNull
    Option(Strings.leftPad(strOrNull, size, ps))
  }

  def length: Int = Strings.length(strOrNull)

  def lowerCase: Option[String] = Option(Strings.lowerCase(strOrNull))

  def lowerCase(locale: Locale): Option[String] = Option(Strings.lowerCase(strOrNull, locale))

  def mid(pos: Int, len: Int): Option[String] = Option(Strings.mid(strOrNull, pos, len))

  def normalizeSpace: Option[String] = Option(Strings.normalizeSpace(strOrNull))

  def ordinalIndexOf[S: TypeOptions2[*, String, Option[String]]](searchStr: S, ordinal: Int): Int = {
    val str1 = mapToStrOpt.input(searchStr).orNull
    Strings.ordinalIndexOf(strOrNull, str1, ordinal)
  }

  def overlay[O: TypeOptions2[*, String, Option[String]]](overlay: O, start: Int, end: Int): Option[String] = {
    val str1    = mapToStrOpt.input(overlay).orNull
    val result2 = Strings.overlay(strOrNull, str1, start, end)
    Option(result2)
  }

  def prependIfMissing[P: TypeOptions2[*, CharSequence, Option[CharSequence]], Ps: TypeOptions2F[Seq, *, Seq[CharSequence], Seq[
    Option[CharSequence]
  ]]](
    prefix: P,
    prefixes: Ps*
  ): Option[String] = {

    def prefixStr = mapTo[Option[CharSequence]].input(prefix).orNull

    def prefixesMapping                             = TypeMapping.getMapping[TypeOptions2[*, Seq[CharSequence], Seq[Option[CharSequence]]]]
    def dealWithCharSeqSeq(strs: Seq[CharSequence]) = Strings.prependIfMissing(strOrNull, prefixStr, strs: _*)

    def result = prefixesMapping.input(prefixes).fold(dealWithCharSeqSeq, s => dealWithCharSeqSeq(mapTo[Seq[CharSequence]].input(s)))

    if (prefixes == null)
      Option(Strings.prependIfMissing(strOrNull, prefixStr, null))
    else
      Option(result)
  }

  def prependIfMissing(prefix: CharSequence): Option[String] =
    Option(Strings.prependIfMissing(strOrNull, prefix))
  def prependIfMissing(prefix: Option[CharSequence]): Option[String] =
    Option(Strings.prependIfMissing(strOrNull, prefix.orNull))

  def prependIfMissingIgnoreCase[P: TypeOptions2[*, String, Option[String]], Ps: TypeOptions2F[Seq, *, Seq[CharSequence], Seq[
    Option[CharSequence]
  ]]](
    prefix: P,
    prefixes: Ps*
  ): Option[String] = {
    def prefixStr       = mapToStrOpt.input(prefix).orNull
    def prefixesMapping = TypeMapping.getMapping[TypeOptions2[*, Seq[CharSequence], Seq[Option[CharSequence]]]]

    def dealWithCharSeqSeq(strs: Seq[CharSequence]) = Strings.prependIfMissingIgnoreCase(strOrNull, prefixStr, strs: _*)
    def result = prefixesMapping.input(prefixes).fold(dealWithCharSeqSeq, s => dealWithCharSeqSeq(mapTo[Seq[CharSequence]].input(s)))

    if (prefixes == null)
      Option(Strings.prependIfMissingIgnoreCase(strOrNull, prefixStr, null))
    else
      Option(result)
  }

  def prependIfMissingIgnoreCase(prefix: CharSequence): Option[String] =
    Option(Strings.prependIfMissingIgnoreCase(strOrNull, prefix))

  def prependIfMissingIgnoreCase(prefix: Option[CharSequence]): Option[String] =
    Option(Strings.prependIfMissingIgnoreCase(strOrNull, prefix.orNull))

  def remove(rmv: Char): Option[String] = Option(Strings.remove(strOrNull, rmv))

  def remove[R: TypeOptions2[*, String, Option[String]]](rmv: R): Option[String] = {
    val rmvStr = mapToStrOpt.input(rmv).orNull
    Option(Strings.remove(strOrNull, rmvStr))
  }

  def removeEnd[R: TypeOptions2[*, String, Option[String]]](rmv: R): Option[String] = {
    val rmvStr = mapToStrOpt.input(rmv).orNull
    Option(Strings.removeEnd(strOrNull, rmvStr))
  }

  def removeEndIgnoreCase[R: TypeOptions2[*, String, Option[String]]](rmv: R): Option[String] = {
    val rmvStr = mapToStrOpt.input(rmv).orNull
    Option(Strings.removeEndIgnoreCase(strOrNull, rmvStr))
  }

  def removeIgnoreCase[R: TypeOptions2[*, String, Option[String]]](rmv: R): Option[String] = {
    val rmvStr = mapToStrOpt.input(rmv).orNull
    Option(Strings.removeIgnoreCase(strOrNull, rmvStr))
  }

  def removeStart[R: TypeOptions2[*, String, Option[String]]](rmv: R): Option[String] = {
    val rmvStr = mapToStrOpt.input(rmv).orNull
    Option(Strings.removeStart(strOrNull, rmvStr))
  }

  def removeStartIgnoreCase[R: TypeOptions2[*, String, Option[String]]](rmv: R): Option[String] = {
    val rmvStr = mapToStrOpt.input(rmv).orNull
    Option(Strings.removeStartIgnoreCase(strOrNull, rmvStr))
  }

  def repeat(rep: Int): Option[String] = Option(Strings.repeat(strOrNull, rep))

  def repeat[S: TypeOptions2[*, String, Option[String]]](separator: S, repeat: Int): Option[String] = {
    val sep = mapToStrOpt.input(separator).orNull
    Option(Strings.repeat(strOrNull, sep, repeat))
  }

  def replace[S: TypeOptions2[*, String, Option[String]], R: TypeOptions2[*, String, Option[String]]](
    searchString: S,
    replacement: R
  ): Option[String] = {
    val sstr = mapToStrOpt.input(searchString).orNull
    val rstr = mapToStrOpt.input(replacement).orNull

    Option(Strings.replace(strOrNull, sstr, rstr))
  }

  def replace[S: TypeOptions2[*, String, Option[String]], R: TypeOptions2[*, String, Option[String]]](
    searchString: S,
    replacement: R,
    max: Int
  ): Option[String] = {
    val sstr = mapToStrOpt.input(searchString).orNull
    val rstr = mapToStrOpt.input(replacement).orNull

    Option(Strings.replace(strOrNull, sstr, rstr, max))
  }

  def replaceChars(searchChar: Char, replaceChar: Char): Option[String] =
    Option(Strings.replaceChars(strOrNull, searchChar, replaceChar))

  def replaceChars[S: TypeOptions2[*, String, Option[String]], R: TypeOptions2[*, String, Option[String]]](
    searchChars: S,
    replaceChars: R
  ): Option[String] = {
    val sstr = mapToStrOpt.input(searchChars).orNull
    val rstr = mapToStrOpt.input(replaceChars).orNull

    Option(Strings.replaceChars(strOrNull, sstr, rstr))
  }

  def replaceEach(searchList: Array[String], replacementList: Array[String]): Option[String] =
    Option(Strings.replaceEach(strOrNull, searchList, replacementList))

  def replaceEachRepeatedly(searchList: Array[String], replacementList: Array[String]): Option[String] =
    Option(Strings.replaceEachRepeatedly(strOrNull, searchList, replacementList))

  def replaceIgnoreCase[S: TypeOptions2[*, String, Option[String]], R: TypeOptions2[*, String, Option[String]]](
    searchString: S,
    replacement: R
  ): Option[String] = {
    val sstr = mapToStrOpt.input(searchString).orNull
    val rstr = mapToStrOpt.input(replacement).orNull

    Option(Strings.replaceIgnoreCase(strOrNull, sstr, rstr))
  }

  def replaceIgnoreCase[S: TypeOptions2[*, String, Option[String]], R: TypeOptions2[*, String, Option[String]]](
    searchString: S,
    replacement: R,
    max: Int
  ): Option[String] = {
    val sstr = mapToStrOpt.input(searchString).orNull
    val rstr = mapToStrOpt.input(replacement).orNull

    Option(Strings.replaceIgnoreCase(strOrNull, sstr, rstr, max))
  }

  def replaceOnce[S: TypeOptions2[*, String, Option[String]], R: TypeOptions2[*, String, Option[String]]](
    searchString: S,
    replacement: R
  ): Option[String] = {
    val sstr = mapToStrOpt.input(searchString).orNull
    val rstr = mapToStrOpt.input(replacement).orNull

    Option(Strings.replaceOnce(strOrNull, sstr, rstr))
  }

  def replaceOnceIgnoreCase[S: TypeOptions2[*, String, Option[String]], R: TypeOptions2[*, String, Option[String]]](
    searchString: S,
    replacement: R
  ): Option[String] = {
    val sstr = mapToStrOpt.input(searchString).orNull
    val rstr = mapToStrOpt.input(replacement).orNull

    Option(Strings.replaceOnceIgnoreCase(strOrNull, sstr, rstr))
  }

  def reverse: Option[String] = Option(Strings.reverse(strOrNull))

  def reverseDelimited(separatorChar: Char): Option[String] =
    Option(Strings.reverseDelimited(strOrNull, separatorChar))

  def right(len: Int): Option[String] = Option(Strings.right(strOrNull, len))

  def rightPad(size: Int): Option[String] = Option(Strings.rightPad(strOrNull, size))

  def rightPad(size: Int, padChar: Char): Option[String] = Option(Strings.rightPad(strOrNull, size, padChar))

  def rightPad[P: TypeOptions2[*, String, Option[String]]](size: Int, padStr: P): Option[String] = {
    val ps = mapToStrOpt.input(padStr).orNull
    Option(Strings.rightPad(strOrNull, size, ps))
  }

  def rotate(shift: Int): Option[String] = Option(Strings.rotate(strOrNull, shift))

  def split: Option[Array[String]] = Option(Strings.split(strOrNull))

  def split(separatorChar: Char): Option[Array[String]] = Option(Strings.split(strOrNull, separatorChar))

  def split[S: TypeOptions2[*, String, Option[String]]](separatorChars: S): Option[Array[String]] = {
    val sep = mapToStrOpt.input(separatorChars).orNull
    Option(Strings.split(strOrNull, sep))
  }

  def split[S: TypeOptions2[*, String, Option[String]]](separatorChars: S, max: Int): Option[Array[String]] = {
    val sep = mapToStrOpt.input(separatorChars).orNull
    Option(Strings.split(strOrNull, sep, max))
  }

  def splitByCharacterType: Option[Array[String]] = Option(Strings.splitByCharacterType(strOrNull))

  def splitByCharacterTypeCamelCase: Option[Array[String]] =
    Option(Strings.splitByCharacterTypeCamelCase(strOrNull))

  def splitByWholeSeparator[S: TypeOptions2[*, String, Option[String]]](separatorChars: S): Option[Array[String]] = {
    val sep = mapToStrOpt.input(separatorChars).orNull
    Option(Strings.splitByWholeSeparator(strOrNull, sep))
  }

  def splitByWholeSeparator[S: TypeOptions2[*, String, Option[String]]](separatorChars: S, max: Int): Option[Array[String]] = {
    val sep = mapToStrOpt.input(separatorChars).orNull
    Option(Strings.splitByWholeSeparator(strOrNull, sep, max))
  }

  def splitByWholeSeparatorPreserveAllTokens[S: TypeOptions2[*, String, Option[String]]](separatorChars: S): Option[Array[String]] = {
    val sep = mapToStrOpt.input(separatorChars).orNull
    Option(Strings.splitByWholeSeparatorPreserveAllTokens(strOrNull, sep))
  }

  def splitByWholeSeparatorPreserveAllTokens[S: TypeOptions2[*, String, Option[String]]](
    separatorChars: S,
    max: Int
  ): Option[Array[String]] = {
    val sep = mapToStrOpt.input(separatorChars).orNull
    Option(Strings.splitByWholeSeparatorPreserveAllTokens(strOrNull, sep, max))
  }

  def splitPreserveAllTokens: Option[Array[String]] = Option(Strings.splitPreserveAllTokens(strOrNull))

  def splitPreserveAllTokens(separatorChar: Char): Option[Array[String]] =
    Option(Strings.splitPreserveAllTokens(strOrNull, separatorChar))

  def splitPreserveAllTokens[S: TypeOptions2[*, String, Option[String]]](separatorChars: S): Option[Array[String]] = {
    val sep = mapToStrOpt.input(separatorChars).orNull
    Option(Strings.splitPreserveAllTokens(strOrNull, sep))
  }

  def splitPreserveAllTokens[S: TypeOptions2[*, String, Option[String]]](separatorChars: S, max: Int): Option[Array[String]] = {
    val sep = mapToStrOpt.input(separatorChars).orNull
    Option(Strings.splitPreserveAllTokens(strOrNull, sep, max))
  }

  def startsWith[S: TypeOptions2[*, String, Option[String]]](prefix: S): Boolean = {
    val pre = mapToStrOpt.input(prefix).orNull
    Strings.startsWith(strOrNull, pre)
  }

  def startsWithAny[CS: TypeOptions2F[Seq, *, Seq[CharSequence], Seq[Option[CharSequence]]]](searchStrings: CS*): Boolean = {
    def mapping = TypeMapping.getMapping[TypeOptions2[*, Seq[CharSequence], Seq[Option[CharSequence]]]]

    if (searchStrings == null) Strings.startsWithAny(strOrNull)
    else {
      val strs = mapping.input(searchStrings).fold(identity, { css => css.map(_.orNull) })
      Strings.startsWithAny(strOrNull, strs: _*)
    }
  }

  def startsWithIgnoreCase[P: TypeOptions2[*, String, Option[String]]](prefix: P): Boolean = {
    val str = mapToStrOpt.input(prefix).orNull
    Strings.startsWithIgnoreCase(strOrNull, str)
  }

  def strip: Option[String] = Option(Strings.strip(strOrNull))

  def strip[S: TypeOptions2[*, String, Option[String]]](stripChars: S): Option[String] = {
    val chars = mapToStrOpt.input(stripChars).orNull
    Option(Strings.strip(strOrNull, chars))
  }

  def stripAccents: Option[String] = Option(Strings.stripAccents(strOrNull))

  def stripEnd[S: TypeOptions2[*, String, Option[String]]](stripChars: S): Option[String] = {
    val chars = mapToStrOpt.input(stripChars).orNull
    Option(Strings.stripEnd(strOrNull, chars))
  }

  def stripStart[S: TypeOptions2[*, String, Option[String]]](stripChars: S): Option[String] = {
    val chars = mapToStrOpt.input(stripChars).orNull
    Option(Strings.stripStart(strOrNull, chars))
  }

  def stripToEmpty: Option[String] = Option(Strings.stripToEmpty(strOrNull))

  def stripToNone: Option[String] = Option(Strings.stripToNull(strOrNull))

  def substring(start: Int): Option[String] = Option(Strings.substring(strOrNull, start))

  def substring(start: Int, end: Int): Option[String] = Option(Strings.substring(strOrNull, start, end))

  def substringAfter(separator: Char): Option[String] = Option(Strings.substringAfter(strOrNull, separator))

  def substringAfter(separator: Int): Option[String] = Option(Strings.substringAfter(strOrNull, separator))

  def substringAfter[S: TypeOptions2[*, String, Option[String]]](separator: S): Option[String] = {
    val sep = mapToStrOpt.input(separator).orNull
    Option(Strings.substringAfter(strOrNull, sep))
  }

  def substringAfterLast(separator: Char): Option[String] = Option(Strings.substringAfterLast(strOrNull, separator))

  def substringAfterLast(separator: Int): Option[String] = Option(Strings.substringAfterLast(strOrNull, separator))

  def substringAfterLast[S: TypeOptions2[*, String, Option[String]]](separator: S): Option[String] = {
    val sep = mapToStrOpt.input(separator).orNull
    Option(Strings.substringAfterLast(strOrNull, sep))
  }

  def substringBefore(separator: Char): Option[String] = Option(Strings.substringBefore(strOrNull, separator))
  def substringBefore(separator: Int): Option[String]  = Option(Strings.substringBefore(strOrNull, separator))

  def substringBefore[S: TypeOptions2[*, String, Option[String]]](separator: S): Option[String] = {
    val sep = mapToStrOpt.input(separator).orNull
    Option(Strings.substringBefore(strOrNull, sep))
  }

  def substringBeforeLast[S: TypeOptions2[*, String, Option[String]]](separator: S): Option[String] = {
    val sep = mapToStrOpt.input(separator).orNull
    Option(Strings.substringBeforeLast(strOrNull, sep))
  }

  def substringBetween[S: TypeOptions2[*, String, Option[String]]](tag: S): Option[String] = {
    val t = mapToStrOpt.input(tag).orNull
    Option(Strings.substringBetween(strOrNull, t))
  }

  def substringBetween[S: TypeOptions2[*, String, Option[String]]](open: S, close: S): Option[String] = {
    val o = mapToStrOpt.input(open).orNull
    val c = mapToStrOpt.input(close).orNull
    Option(Strings.substringBetween(strOrNull, o, c))
  }

  def substringsBetween[S: TypeOptions2[*, String, Option[String]]](open: S, close: S): Option[Array[String]] = {
    val o = mapToStrOpt.input(open).orNull
    val c = mapToStrOpt.input(close).orNull
    Option(Strings.substringsBetween(strOrNull, o, c))
  }

  def swapCase: Option[String] = Option(Strings.swapCase(strOrNull))

  def toCodePoints: Option[Array[Int]] = Option(Strings.toCodePoints(strOrNull))

  def toRootLowerCase: Option[String] = Option(Strings.toRootLowerCase(strOrNull))

  def toRootUpperCase: Option[String] = Option(Strings.toRootUpperCase(strOrNull))

  def trim: Option[String] = Option(Strings.trim(strOrNull))

  def trimToEmpty: Option[String] = Option(Strings.trimToEmpty(strOrNull))

  def trimToNone: Option[String] = Option(Strings.trimToNull(strOrNull))

  def truncate(maxWidth: Int): Option[String] = Option(Strings.truncate(strOrNull, maxWidth))

  def truncate(offset: Int, maxWidth: Int): Option[String] = Option(Strings.truncate(strOrNull, offset, maxWidth))

  def uncapitalize: Option[String] = Option(Strings.uncapitalize(strOrNull))

  def unwrap(wrapChar: Char): Option[String] = Option(Strings.unwrap(strOrNull, wrapChar))

  def unwrap[S: TypeOptions2[*, String, Option[String]]](wrapToken: S): Option[String] = {
    val token = mapToStrOpt.input(wrapToken).orNull
    Option(Strings.unwrap(strOrNull, token))
  }

  def upperCase: Option[String] = Option(Strings.upperCase(strOrNull))

  def upperCase(locale: Locale): Option[String] = Option(Strings.upperCase(strOrNull, locale))

  def wrap(wrapChar: Char): Option[String] = Option(Strings.wrap(strOrNull, wrapChar))

  def wrap[S: TypeOptions2[*, String, Option[String]]](wrapToken: S): Option[String] = {
    val token = mapToStrOpt.input(wrapToken).orNull
    Option(Strings.wrap(strOrNull, token))
  }

  def wrapIfMissing(wrapChar: Char): Option[String] = Option(Strings.wrapIfMissing(strOrNull, wrapChar))

  def wrapIfMissing[S: TypeOptions2[*, String, Option[String]]](wrapToken: S): Option[String] = {
    val token = mapToStrOpt.input(wrapToken).orNull
    Option(Strings.wrapIfMissing(strOrNull, token))
  }
}
