package commons.lang3.bridge

import net.scalax.simple.adt.{TypeAdt => Adt}
import org.apache.commons.lang3.{StringUtils => Strings}

import java.nio.charset.Charset
import java.util.Locale
import java.util.function.Supplier
import java.util.Objects
import scala.collection.compat._

private object privateUtils {
  def mapToStrOpt[T: Adt.CoProducts2[*, String, Option[String]]](t: T): Option[String] = {
    val applyM = Adt.CoProduct2[String, Option[String]].instance(t)
    applyM.fold2(Option(_)).fold1(identity)
  }
  def mapToCsOpt[T: Adt.CoProducts2[*, CharSequence, Option[CharSequence]]](t: T): Option[CharSequence] = {
    val applyM = Adt.CoProduct2[CharSequence, Option[CharSequence]].instance(t)
    applyM.fold2(Option(_)).fold1(identity)
  }
  def mapToCharSeq(t: Seq[Option[Char]]): Seq[Char]                         = t.collect { case Some(s) => s }
  def mapToCharSequenceSeq(t: Seq[Option[CharSequence]]): Seq[CharSequence] = for (i <- t) yield i.orNull
}

/** TODO
  *
  * @author
  *   mars
  * @version 1.0.0
  * @since 2022/08/28
  *   21:04
  */
class StringCommons[T: Adt.CoProducts2[*, String, Option[String]]](value: T) {

  import privateUtils._

  @inline private def strProAdt: Adt.CoProduct2[String, Option[String]] = Adt.CoProduct2[String, Option[String]].instance(value)

  @inline private def strOpt: Option[String] = mapToStrOpt(value)
  @inline private def strOrNull: String      = strProAdt.fold2(identity).fold1(_.orNull)

  /** * <p>Abbreviates a String using ellipses. This will turn "Now is the time for all good men" into "Now is the time for..."</p>
    *
    *   - `str`is less than or equal to `maxWidth`, return `str`.
    *   - Else abbreviate it to `(substring(str, 0, max-3) + "...")`.
    *   - If `maxWidth`is less than `4`, throw an `IllegalArgumentException`.
    *   - In no case will it return a String of length greater than `maxWidth`.
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
    *   abbreviated String, `None` if none input
    * @throws IllegalArgumentException
    *   if the width is too small
    */
  @inline def abbreviate(maxWidth: Int): Option[String] = Option(Strings.abbreviate(strOrNull, maxWidth))

  /** <p>Abbreviates a String using ellipses. This will turn "Now is the time for all good men" into "...is the time for..."</p>
    *
    * <p>Works like `Option(String).abbreviate(int)`, but allows you to specify a "left edge" offset. Note that this left edge is not
    * necessarily going to be the leftmost character in the result, or the first character following the ellipses, but it will appear
    * somewhere in the result.
    *
    * <p>In no case will it return a String of length greater than `maxWidth`.</p>
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
    *   abbreviated String Option, `None` if none input
    * @throws IllegalArgumentException
    *   if the width is too small
    */
  @inline def abbreviate(offset: Int, maxWidth: Int): Option[String] = Option(Strings.abbreviate(strOrNull, offset, maxWidth))

  /** <p>Abbreviates a String using another given String as replacement marker. This will turn "Now is the time for all good men" into "Now
    * is the time for..." if "..." was defined as the replacement marker.</p>
    *
    * <p>Specifically:</p>
    *   - If the number of characters in `str`is less than or equal to `maxWidth`, return `str`.
    *   - Else abbreviate it to `(substring(str, 0, max-abbrevMarker.length) + abbrevMarker)`.
    *   - If `maxWidth` is less than `abbrevMarker.length + 1`, throw an `IllegalArgumentException`.
    *   - In no case will it return a String of length greater than `maxWidth`.
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
    *   maximum length of result String, must be at least `abbrevMarker.length + 1`
    * @tparam Abb
    *   String or Option[String]
    * @return
    *   abbreviated String Option, `None` if none input
    * @throws IllegalArgumentException
    *   if the width is too small
    */
  def abbreviate[Abb: Adt.CoProducts2[*, String, Option[String]]](abbrevMarker: Abb, maxWidth: Int): Option[String] = {
    val abbrevMarkerOrNull = mapToStrOpt(abbrevMarker).orNull
    Option(Strings.abbreviate(strOrNull, abbrevMarkerOrNull, maxWidth))
  }

  /** <p>Abbreviates a String using a given replacement marker. This will turn "Now is the time for all good men" into "...is the time
    * for..." if "..." was defined as the replacement marker.</p>
    *
    * <p>Works like `abbreviate(String, String, int)`, but allows you to specify a "left edge" offset. Note that this left edge is not
    * necessarily going to be the leftmost character in the result, or the first character following the replacement marker, but it will
    * appear somewhere in the result.
    *
    * <p>In no case will it return a String of length greater than `maxWidth`.</p>
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
    *   abbreviated String Option, `None` if none input
    * @throws IllegalArgumentException
    *   if the width is too small
    */
  def abbreviate[Abb: Adt.CoProducts2[*, String, Option[String]]](abbrevMarker: Abb, offset: Int, maxWidth: Int): Option[String] = {
    val abbrevMarkerOrNull = mapToStrOpt(abbrevMarker).orNull
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
    *   the length to abbreviate `str`to.
    * @tparam M
    *   String or Option[String]
    * @return
    *   the abbreviated String if the above criteria is met, or the original String supplied for abbreviation.
    */
  def abbreviateMiddle[M: Adt.CoProducts2[*, String, Option[String]]](middle: M, length: Int): Option[String] = {
    val middleOrNull = mapToStrOpt(middle).orNull
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
  def appendIfMissing[S: Adt.CoProducts2[*, String, Option[String]], SS: Adt.CoProducts2[*, CharSequence, Option[CharSequence]]](
    suffix: S,
    suffixes: SS*
  ): Option[String] = {
    def suffixOrNull           = mapToStrOpt(suffix).orNull
    def applyM                 = Adt.CoProduct2[CharSequence, Option[CharSequence]].typeOnly[SS]
    def sfs: Seq[CharSequence] = applyM.fold2(_.higherKindApply[Seq](suffixes)).fold1(_.higherKindApply[Seq](suffixes).map(_.orNull))

    if (suffixes == null) Option(Strings.appendIfMissing(strOrNull, suffixOrNull))
    else {
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
  def appendIfMissingIgnoreCase[S: Adt.CoProducts2[*, String, Option[String]], SS: Adt.CoProducts2[*, CharSequence, Option[CharSequence]]](
    suffix: S,
    suffixes: SS*
  ): Option[String] = {
    def suffixOrNull = mapToStrOpt(suffix).orNull
    def applyM       = Adt.CoProduct2[CharSequence, Option[CharSequence]].typeOnly[SS]
    def sfs: Seq[CharSequence] =
      applyM.fold2(_.higherKindApply[Seq](suffixes)).fold1(func => func.higherKindApply[Seq](suffixes).map(_.orNull))

    if (suffixes == null) Option(Strings.appendIfMissingIgnoreCase(strOrNull, suffixOrNull))
    else {
      Option(Strings.appendIfMissingIgnoreCase(strOrNull, suffixOrNull, sfs: _*))
    }
  }

  // helpers for method call without suffixes
  def appendIfMissingIgnoreCase(suffix: CharSequence): Option[String] =
    Option(Strings.appendIfMissingIgnoreCase(strOrNull, suffix))

  def appendIfMissingIgnoreCase(suffix: Option[CharSequence]): Option[String] =
    Option(Strings.appendIfMissingIgnoreCase(strOrNull, suffix.orNull))

  /** <p>Capitalizes a String changing the first character to title case as per [[Character# toTitleCase ( int )]]. No other characters are
    * changed.</p>
    *
    * {{{
    * none.capitalize = None
    * Some("").capitalize = Some("")
    * Some("cat").capitalize = "Cat"
    * Some("cAt") .capitalize = "CAt"
    * Some("'cat'").capitalize = "'cat'"
    * }}}
    *
    * @return
    *   the capitalized String, `none` if none String input
    * @see
    *   #uncapitalize(String)
    */
  @inline def capitalize: Option[String] = Option(Strings.capitalize(strOrNull))

  /** <p>Centers a String in a larger String of size `size` using the space character (' ').</p>
    *
    * <p>If the size is less than the String length, the original String is returned. A `null` String returns `null`. A negative size is
    * treated as zero.</p>
    *
    * <p>Equivalent to `center(str, size, " ")`.</p>
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
    *   centered Option[String], `None` if none input
    */
  @inline def center(size: Int): Option[String] = Option(Strings.center(strOrNull, size))

  /** <p>Centers a String in a larger String of size `size`. Uses a supplied character as the value to pad the String with.</p>
    *
    * <p>If the size is less than the String length, the String is returned. A `null` String returns `null`. A negative size is treated as
    * zero.</p>
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
    *   centered String, `none` if none String input
    */
  @inline def center(size: Int, padChar: Char): Option[String] = Option(Strings.center(strOrNull, size, padChar))

  /** <p>Centers a String in a larger String of size `size`. Uses a supplied character as the value to pad the String with.</p>
    *
    * <p>If the size is less than the String length, the String is returned. A `null` String returns `null`. A negative size is treated as
    * zero.</p>
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
    *   centered String, `None` if none input
    */
  def center[P: Adt.CoProducts2[*, String, Option[String]]](size: Int, padStr: P): Option[String] = {
    val padStrOrNull = mapToStrOpt(padStr).orNull
    Option(Strings.center(strOrNull, size, padStrOrNull))
  }

  /** <p>Removes one newline from end of a String if it's there, otherwise leave it alone. A newline is &quot;`\n`&quot;, &quot; `\r`&quot;,
    * or &quot;`\r\n`&quot;.</p>
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
    *   Option[String] without newline, `None` if null String input
    */
  def chomp: Option[String] = Option(Strings.chomp(strOrNull))

  /** <p>Remove the last character from a String.</p>
    *
    * <p>If the String ends in `\r\n`, then remove both of them.</p>
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
    *   String without last character, `None` if None String input
    */
  def chop: Option[String] = Option(Strings.chop(strOrNull))

  /** <p>Compare two Strings lexicographically, as per [[String# compareTo ( String )]], returning :</p>
    *
    *   - `int = 0`, if `str1` is equal to `str2` (or both `null`)
    *   - `int < 0`, if `str1` is less than `str2`
    *   - `int > 0`, if `str1` is greater than `str2`
    *
    * <p>This is a `null` safe version of :</p>
    * {{{
    * str1.compareTo(str2)
    * }}}
    *
    * <p>`none` value is considered less than non-`null` value. Two `null` references are considered equal.</p>
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
    *   &lt; 0, 0, &gt; 0, if `this` is respectively less, equal or greater than `other`
    */
  def compare[O: Adt.CoProducts2[*, String, Option[String]]](other: O): Int = {
    val otherOrNull = mapToStrOpt(other).orNull
    Strings.compare(strOrNull, otherOrNull)
  }

  /** <p>Compare two Strings lexicographically, as per [[String# compareTo ( String )]], returning :</p>
    *
    *   - `int = 0`, if `str1` is equal to `str2` (or both `null`)
    *   - `int < 0`, if `str1` is less than `str2`
    *   - `int > 0`, if `str1` is greater than `str2`
    *
    * <p>This is a `null` safe version of :</p>
    * {{{
    * str1.compareTo(str2)
    * }}}
    *
    * <p>`null` inputs are handled according to the `nullIsLess` parameter. Two `null` references are considered equal.</p>
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
    *   whether consider `None` value less than non-`None` value
    * @tparam O
    *   String Or Option
    * @return
    *   &lt; 0, 0, &gt; 0, if `this` is respectively less, equal ou greater than `other`
    */
  def compare[O: Adt.CoProducts2[*, String, Option[String]]](other: O, nullIsNull: Boolean): Int = {
    val otherOrNull = mapToStrOpt(other).orNull
    Strings.compare(strOrNull, otherOrNull, nullIsNull)
  }

  /** <p>Compare two Strings lexicographically, ignoring case differences, as per [[String# compareToIgnoreCase ( String )]], returning
    * :</p>
    *
    *   - `int = 0`, if `str1` is equal to `str2` (or both `null`)
    *   - `int < 0`, if `str1` is less than `str2`
    *   - `int > 0`, if `str1` is greater than `str2`
    *
    * <p>This is a `null` safe version of :</p>
    * {{{
    *  str1.compareToIgnoreCase(str2)
    * }}}
    * <p>`none` value is considered less than non-`null` value. Two `null` references are considered equal. Comparison is case
    * insensitive.</p>
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
    *   &lt; 0, 0, &gt; 0, if `this` is respectively less, equal ou greater than `other`, ignoring case differences.
    */
  def compareIgnoreCase[O: Adt.CoProducts2[*, String, Option[String]]](other: O): Int = {
    val otherOrNull = mapToStrOpt(other).orNull
    Strings.compareIgnoreCase(strOrNull, otherOrNull)
  }

  /** <p>Compare two Strings lexicographically, ignoring case differences, as per [[String# compareToIgnoreCase ( String )]], returning
    * :</p>
    *
    *   - `int = 0`, if `str1` is equal to `str2` (or both `null`)
    *   - `int < 0`, if `str1` is less than `str2`
    *   - `int > 0`, if `str1` is greater than `str`
    *
    * <p>This is a `null` safe version of :</p>
    *
    * {{{
    * str1.compareToIgnoreCase(str2)
    * }}}
    *
    * <p>`none` inputs are handled according to the `nullIsLess` parameter. Two `null` references are considered equal. Comparison is case
    * insensitive.</p>
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
    *   whether consider `None` value less than non-`None` value
    * @tparam O
    *   String or Option[String]
    * @return
    *   &lt; 0, 0, &gt; 0, if `this` is respectively less, equal ou greater than `other`, ignoring case differences.
    */
  def compareIgnoreCase[O: Adt.CoProducts2[*, String, Option[String]]](other: O, nullIsLess: Boolean): Int = {
    val otherOrNull = mapToStrOpt(other).orNull
    Strings.compareIgnoreCase(strOrNull, otherOrNull, nullIsLess)
  }

  /** * <p>Checks if CharSequence contains a search CharSequence, handling `null`. This method uses {@link String# indexOf ( String )} if
    * possible.</p>
    *
    * <p>A `none` CharSequence will return `false`.</p>
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
  def contains[To: Adt.CoProducts2[*, String, Option[String]]](searchSeq: To): Boolean = {
    val str1 = mapToStrOpt(searchSeq).orNull
    Strings.contains(strOrNull, str1)
  }

  /** <p>Checks if CharSequence contains a search character, handling `null`. This method uses {@link String# indexOf ( int )} if
    * possible.</p>
    *
    * <p>A `none` or empty ("") CharSequence will return `false`.</p>
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

  /** <p>Checks if CharSequence contains a search character, handling `null`. This method uses {@link String# indexOf ( int )} if
    * possible.</p>
    *
    * <p>A `none` or empty ("") CharSequence will return `false`.</p>
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
    * <p> A `none` CharSequence will return `false`. A `null` search CharSequence will return `false`. </p>
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
    *   the `true` if any of the chars are found, `false` if no match or null input
    */
  def containsAny[S: Adt.CoProducts4[*, Char, CharSequence, Option[Char], Option[CharSequence]]](
    searchArgs: S*
  ): Boolean = {
    val applyM = Adt.CoProduct4[Char, CharSequence, Option[Char], Option[CharSequence]].typeOnly[S]
    val adtSeq = Adt.CoProduct2[Seq[Char], Seq[CharSequence]]

    val adtParameter: Adt.CoProduct2[Seq[Char], Seq[CharSequence]] =
      applyM
        .fold4 { func1 =>
          adtSeq.instance(func1.higherKindApply[Seq](searchArgs))
        }
        .fold3 { func2 =>
          adtSeq.instance(func2.higherKindApply[Seq](searchArgs))
        }
        .fold2 { func3 =>
          val applySeq   = for (s <- searchArgs) yield func3.adtFunctionApply(s)
          val collectSeq = applySeq.collect { case Some(t) => t }
          adtSeq.instance(collectSeq)
        }
        .fold1 { func4 =>
          val applySeq   = for (s <- searchArgs) yield func4.adtFunctionApply(s)
          val collectSeq = applySeq.collect { case Some(t) => t }
          adtSeq.instance(collectSeq)
        }

    adtParameter
      .fold2 { chars =>
        Strings.containsAny(strOrNull, chars: _*)
      }
      .fold1 { css =>
        if (css.length == 1)
          Strings.containsAny(strOrNull, css.head)
        else
          Strings.containsAny(strOrNull, css: _*)
      }

  }

  /** <p> Checks if the CharSequence contains any of the CharSequences in the given array, ignoring case. </p>
    *
    * <p>
    *
    * A `none` `cs` CharSequence will return `false`.
    *
    * A `null` or zero length search array will return `false`.
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
    *   `true` if any of the search CharSequences are found, `false` otherwise
    */
  def containsAnyIgnoreCase[S: Adt.CoProducts2[*, CharSequence, Option[CharSequence]]](searchArgs: S*): Boolean = {
    val applyM = Adt.CoProduct2[CharSequence, Option[CharSequence]].typeOnly[S]

    val newSeq: Seq[CharSequence] =
      applyM.fold2(t1 => t1.higherKindApply[Seq](searchArgs)).fold1(t2 => for (s <- searchArgs) yield t2.adtFunctionApply(s).orNull)

    Strings.containsAnyIgnoreCase(strOrNull, newSeq: _*)
  }

  /** <p>Checks if CharSequence contains a search CharSequence irrespective of case, handling `null`. Case-insensitivity is defined as by
    * [[String# equalsIgnoreCase ( String )]].
    *
    * <p>A `none` CharSequence will return `false`.</p>
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
    *   true if the CharSequence contains the search CharSequence irrespective of case or false if not or `null` string input
    */
  def containsIgnoreCase[S: Adt.CoProducts2[*, String, Option[String]]](searchStr: S): Boolean = {
    val searchStrOrNull = mapToStrOpt(searchStr).orNull
    Strings.containsIgnoreCase(strOrNull, searchStrOrNull)
  }

  /** <p> Checks that the CharSequence does not contain certain characters. </p>
    *
    * <p>
    *
    * A `none` CharSequence will return `true`. A `null` invalid character array will return `true`. An empty String ("") always returns
    * true.</p>
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
    * A `none` CharSequence will return `true`. A `None` invalid character array will return `true`. An empty String (Some("")) always
    * returns true.</p>
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
    * <p>A `none` CharSequence will return `true`. A `null` invalid character array will return `true`. An empty CharSequence (length()=0)
    * always returns true.</p>
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
  def containsNone[I: Adt.CoProducts2[*, Char, Option[Char]]](invalidChars: I*): Boolean = {
    val applyM = Adt.CoProduct2[Char, Option[Char]].typeOnly[I]
    val charSeq: Seq[Char] =
      applyM.fold2(_.higherKindApply[Seq](invalidChars)).fold1(func => mapToCharSeq(func.higherKindApply[Seq](invalidChars)))

    Strings.containsNone(strOrNull, charSeq: _*)
  }

  /** <p>Checks if the CharSequence contains only certain characters.</p>
    *
    * <p>A `none` CharSequence will return `false`. A `null` valid character String will return `false`. An empty String (length()=0) always
    * returns `true`.</p>
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
    * <p>A `none` CharSequence will return `false`. A `None` valid character String will return `false`. An empty String (length()=0) always
    * returns `true`.</p>
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
    * <p>A `none` CharSequence will return `false`. A `null` valid character array will return `false`. An empty CharSequence (length()=0)
    * always returns `true`.</p>
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
  def containsOnly[V: Adt.CoProducts2[*, Char, Option[Char]]](valid: V*): Boolean = {
    val applyM             = Adt.CoProduct2[Char, Option[Char]].typeOnly[V]
    val charSeq: Seq[Char] = applyM.fold2(_.higherKindApply[Seq](valid)).fold1(func => mapToCharSeq(func.higherKindApply[Seq](valid)))
    Strings.containsOnly(strOrNull, charSeq: _*)
  }

  /** <p>Check whether the given CharSequence contains any whitespace characters.</p>
    *
    * <p>Whitespace is defined by [[Character# isWhitespace ( char )]].</p>
    *
    * @return
    *   if the CharSequence is not empty and contains at least 1 (breaking) whitespace character
    */
  def containsWhitespace: Boolean = Strings.containsWhitespace(strOrNull)

  /** <p>Counts how many times the char appears in the given string.</p>
    *
    * <p>A `none` or empty ("") String input returns `0`.</p>
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
    *   the number of occurrences, 0 if the CharSequence is `null`
    */
  def countMatches(ch: Char): Int = Strings.countMatches(strOrNull, ch)

  /** <p>Counts how many times the substring appears in the larger string. Note that the code only counts non-overlapping matches.</p>
    *
    * <p>A `none` or empty ("") String input returns `0`.</p>
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
    *   the number of occurrences, 0 if either CharSequence is `null`
    */
  def countMatches[S: Adt.CoProducts2[*, String, Option[String]]](sub: S): Int = {
    val str1 = mapToStrOpt(sub).orNull
    Strings.countMatches(strOrNull, str1)
  }

  /** <p>Counts how many times the substring appears in the larger string. Note that the code only counts non-overlapping matches.</p>
    *
    * <p>A `null` or empty ("") String input returns `0`.</p>
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
    *   the number of occurrences, 0 if either CharSequence is `null`
    */
  def defaultIfBlank[S: Adt.CoProducts2[*, CharSequence, Option[CharSequence]]](defaultStr: S): CharSequence = {
    val defStr = mapToCsOpt(defaultStr).orNull
    val result = Strings.defaultIfBlank(strOrNull, defStr)
    result
  }

  /** Returns either the passed in CharSequence, or if the CharSequence is empty or `null`, the value of `defaultStr`.
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
  def defaultIfEmpty[S: Adt.CoProducts2[*, CharSequence, Option[CharSequence]]](defaultStr: S): CharSequence = {
    val str1   = mapToCsOpt(defaultStr).orNull
    val result = Strings.defaultIfEmpty(strOrNull, str1)
    result
  }

  /** <p>Returns either the passed in String, or if the String is `null`, an empty String ("").</p>
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

  /** <p>Returns either the passed in String, or if the String is `null`, the value of `defaultStr`.</p>
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
    *   the passed in String, or the default if it was `null`
    */
  def defaultString[S: Adt.CoProducts2[*, String, Option[String]]](defaultStr: S): String = {
    val str1 = mapToStrOpt(defaultStr).orNull
    Objects.toString(strOrNull, str1)
  }

  /** <p>Deletes all whitespaces from a String as defined by {@link Character# isWhitespace ( char )} .</p>
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
    * <p>For example, `"i am a machine".ops.difference("i am a robot") -> "robot"`.</p>
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
  def difference[S: Adt.CoProducts2[*, String, Option[String]]](other: S): Option[String] = {
    val str1   = mapToStrOpt(other).orNull
    val result = Strings.difference(strOrNull, str1)
    Option(result)
  }

  /** <p>Check if a CharSequence ends with a specified suffix.</p>
    *
    * <p>`null`s are handled without exceptions. Two `null` references are considered to be equal. The comparison is case sensitive.</p>
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
    *   if the CharSequence ends with the suffix, case sensitive, or both `null`
    */
  def endsWith[S: Adt.CoProducts2[*, String, Option[String]]](suffix: S): Boolean = {
    val str1 = mapToStrOpt(suffix).orNull
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
    *   the case-sensitive CharSequences to find, may be empty or contain `null`
    * @tparam S
    *   CharSequence or Option[CharSequence]
    * @return
    *   `true` if the input `sequence` is `null` AND no `searchStrings` are provided, or the input `sequence` ends in any of the provided
    *   case-sensitive `searchStrings`.
    */
  def endsWithAny[S: Adt.CoProducts2[*, CharSequence, Option[CharSequence]]](searchStrings: S*): Boolean = {
    def applyM = Adt.CoProduct2[CharSequence, Option[CharSequence]].typeOnly[S]
    def charSeqSeq: Seq[CharSequence] =
      applyM.fold2(_.higherKindApply[Seq](searchStrings)).fold1(func => mapToCharSequenceSeq(func.higherKindApply[Seq](searchStrings)))

    if (searchStrings == null)
      Strings.endsWithAny(strOrNull, null)
    else
      Strings.endsWithAny(strOrNull, charSeqSeq: _*)
  }

  /** <p>Case insensitive check if a CharSequence ends with a specified suffix.</p>
    *
    * <p>`null`s are handled without exceptions. Two `null` references are considered to be equal. The comparison is case insensitive.</p>
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
    *   `true` if the CharSequence ends with the suffix, case insensitive, or both `null`
    */
  def endsWithIgnoreCase[S: Adt.CoProducts2[*, CharSequence, Option[CharSequence]]](suffix: S): Boolean = {
    val str2 = mapToCsOpt(suffix).orNull
    Strings.endsWithIgnoreCase(strOrNull, str2)
  }

  /** * <p>Compares two CharSequences, returning `true` if they represent equal sequences of characters.</p>
    *
    * NOTE: I package it just for symmetry. We have equals ignore case, so we should have queals. But the semantic of ops's equals method
    * has a little strange smell. em...
    *
    * Please use buildin equals in Object, or static equals method in commons string utils or Objects type as far as possible.
    *
    * <p>`null`s are handled without exceptions. Two `null` references are considered to be equal. The comparison is <strong>case
    * sensitive</strong>.</p>
    *
    * {{{
    * none.ops.equals(null) = true
    * none.ops.equals("abc") = false
    * "abc".ops.equals(null) = false
    * "abc".ops.equals("abc") = true
    * "abc".ops.equals("ABC") = false
    * }}}
    *
    * @param other
    * @tparam S
    * @return
    */
  def equals[S: Adt.CoProducts2[*, CharSequence, Option[CharSequence]]](other: S): Boolean = {
    val str2 = mapToCsOpt(other).orNull
    Strings.endsWithIgnoreCase(strOrNull, str2)
  }

  /** * <p>Compares given `string` to a CharSequences vararg of `searchStrings`, returning `true` if the `string` is equal to any of the
    * `searchStrings`.</p>
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
    *   a vararg of strings, may be `null`.
    * @tparam S
    *   CharSequence or Option[CharSequence]
    * @return
    *   `true` if the string is equal (case-sensitive) to any other element of `searchStrings`; `false` if {@code searchStrings} is null or
    *   contains no matches.
    */
  def equalsAnyIgnoreCase[S: Adt.CoProducts2[*, CharSequence, Option[CharSequence]]](searchStrings: S*): Boolean = {
    def applyM = Adt.CoProduct2[CharSequence, Option[CharSequence]].typeOnly[S]
    def charSeqSeq: Seq[CharSequence] =
      applyM.fold2(_.higherKindApply[Seq](searchStrings)).fold1(func => mapToCharSequenceSeq(func.higherKindApply[Seq](searchStrings)))

    if (searchStrings == null)
      Strings.equalsAnyIgnoreCase(strOrNull, null)
    else
      Strings.equalsAnyIgnoreCase(strOrNull, charSeqSeq: _*)
  }

  /** <p>Compares two CharSequences, returning `true` if they represent equal sequences of characters, ignoring case.</p>
    *
    * <p>`null`s are handled without exceptions. Two `null` references are considered equal. The comparison is <strong>case
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
    *   the other CharSequence, may be `null`
    * @tparam S
    * @return
    */
  def equalsIgnoreCase[S: Adt.CoProducts2[*, CharSequence, Option[CharSequence]]](other: S): Boolean = {
    val str1 = mapToCsOpt(other).orNull
    Strings.equalsIgnoreCase(strOrNull, str1)
  }

  /** Calls [[String#getBytes(Charset)]] in a null-safe manner.
    * @param charset
    *   The [[Charset]] to encode the `String`. If null, then use the default Charset.
    * @tparam C
    *   Charset or Option[CharSet]
    * @return
    *   The empty byte[] if `string` is null, the result of [[String# getBytes ( Charset )]] otherwise.
    */
  def getBytes[C: Adt.CoProducts4[*, Charset, Option[Charset], String, Option[String]]](charset: C): Array[Byte] = {
    val applyM = Adt.CoProduct4[Charset, Option[Charset], String, Option[String]].instance(charset)

    def dealWithCharsetOptFunc(c: Charset): Array[Byte] = Strings.getBytes(strOrNull, c)
    def dealWithStringOptFunc(c: String): Array[Byte]   = Strings.getBytes(strOrNull, c)
    val dealWithCharsetOpt                              = dealWithCharsetOptFunc _
    def dealWithStringOpt                               = dealWithStringOptFunc _

    applyM
      .fold4(dealWithCharsetOpt)
      .fold3(dealWithCharsetOpt.compose(_.orNull))
      .fold2(dealWithStringOpt)
      .fold1(dealWithStringOpt.compose(_.orNull))
  }

  /** <p>Checks if a String `str`contains Unicode digits, if yes then concatenate all the digits in `str`and return it as a String.</p>
    *
    * <p>An empty ("") String will be returned if no digits found in `str`.</p>
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

  /** <p>Returns either the passed in CharSequence, or if the CharSequence is whitespace, empty ("") or `null`, the value supplied by
    * `defaultStrSupplier`.</p>
    *
    * <p>Whitespace is defined by [[Character# isWhitespace ( char )]].</p>
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
  def getIfBlank[S: Adt.CoProducts2[*, CharSequence, Option[CharSequence]]](defaultSupplier: Supplier[S]): CharSequence =
    if (defaultSupplier == null) Strings.getIfBlank(strOrNull, null)
    else {
      val supplier: Supplier[CharSequence] = () => mapToCsOpt(defaultSupplier.get()).orNull
      Strings.getIfBlank(strOrNull, supplier)
    }

  /** Returns either the passed in CharSequence, or if the CharSequence is empty or `none`, the value supplied by `defaultStrSupplier`.
    *
    * Caller responsible for thread-safety and exception handling of default value supplier
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
  def getIfEmpty[S: Adt.CoProducts2[*, CharSequence, Option[CharSequence]]](defaultSupplier: Supplier[S]): CharSequence =
    if (defaultSupplier == null) Strings.getIfEmpty(strOrNull, null)
    else {
      val supplier: Supplier[CharSequence] = () => mapToCsOpt(defaultSupplier.get()).orNull
      Strings.getIfEmpty(strOrNull, supplier)
    }

  /** <p>Finds the first index within a CharSequence, handling `null`. This method uses [[String# indexOf ( String, int)]] if possible.</p>
    *
    * <p>A `null` CharSequence will return `-1`.</p>
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
    *   the first index of the search CharSequence, -1 if no match or `null` string input
    */
  def indexOf[S: Adt.CoProducts2[*, CharSequence, Option[CharSequence]]](searchSeq: S): Int = {
    val str1 = mapToCsOpt(searchSeq).orNull
    Strings.indexOf(strOrNull, str1)
  }

  /** <p>Finds the first index within a CharSequence, handling `null`. This method uses [[String# indexOf ( String, int)]] if possible.</p>
    *
    * <p>A `null` CharSequence will return `-1`. A negative start position is treated as zero. An empty ("") search CharSequence always
    * matches. A start position greater than the string length only matches an empty search CharSequence.</p>
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
    *   the first index of the search CharSequence (always &ge; startPos), -1 if no match or `null` string input
    */
  def indexOf[S: Adt.CoProducts2[*, CharSequence, Option[CharSequence]]](searchSeq: S, startPos: Int): Int = {
    val str1 = mapToCsOpt(searchSeq).orNull
    Strings.indexOf(strOrNull, str1, startPos)
  }

  /** Returns the index within `seq` of the first occurrence of the specified character. If a character with value `searchChar` occurs in
    * the character sequence represented by `seq` `CharSequence` object, then the index (in Unicode code units) of the first such occurrence
    * is returned. For values of `searchChar` in the range from 0 to 0xFFFF (inclusive), this is the smallest value <i>k</i> such that:
    * {{{
    * this.charAt(<i>k</i>) == searchChar
    * }}}
    * is true. For other values of `searchChar`, it is the smallest value <i>k</i> such that:
    * {{{
    * this.codePointAt(<i>k</i>) == searchChar
    * }}}
    * is true. In either case, if no such character occurs in `seq`, then `INDEX_NOT_FOUND (-1)` is returned.
    *
    * <p>Furthermore, a `null` or empty ("") CharSequence will return `INDEX_NOT_FOUND (-1)`.</p>
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
    *   the first index of the search character, -1 if no match or `null` string input
    */
  def indexOf(searchChar: Char): Int = Strings.indexOf(strOrNull, searchChar)

  /** Returns the index within `seq` of the first occurrence of the specified character. If a character with value `searchChar` occurs in
    * the character sequence represented by `seq` `CharSequence` object, then the index (in Unicode code units) of the first such occurrence
    * is returned. For values of `searchChar` in the range from 0 to 0xFFFF (inclusive), this is the smallest value <i>k</i> such that:
    * {{{
    * this.charAt(<i>k</i>) == searchChar
    * }}}
    * is true. For other values of `searchChar`, it is the smallest value <i>k</i> such that:
    * {{{
    * this.codePointAt(<i>k</i>) == searchChar
    * }}}
    * is true. In either case, if no such character occurs in `seq`, then `INDEX_NOT_FOUND (-1)` is returned.
    *
    * <p>Furthermore, a `null` or empty ("") CharSequence will return `INDEX_NOT_FOUND (-1)`.</p>
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
    *   the first index of the search character, -1 if no match or `null` string input
    */
  def indexOf(searchChar: Int): Int = Strings.indexOf(strOrNull, searchChar)

  /** Returns the index within `seq` of the first occurrence of the specified character, starting the search at the specified index. <p> If
    * a character with value `searchChar` occurs in the character sequence represented by the `seq` `CharSequence` object at an index no
    * smaller than `startPos`, then the index of the first such occurrence is returned. For values of `searchChar` in the range from 0 to
    * 0xFFFF (inclusive), this is the smallest value <i>k</i> such that:
    * {{{
    * (this.charAt(<i>k</i>) == searchChar) &amp;&amp; (<i>k</i> >= startPos)
    * }}}
    * is true. For other values of `searchChar`, it is the smallest value <i>k</i> such that:
    * {{{
    * (this.codePointAt(<i>k</i>) == searchChar) && (<i>k</i> >= startPos)
    * }}}
    * is true. In either case, if no such character occurs in `seq` at or after position `startPos`, then `-1` is returned.
    *
    * <p> There is no restriction on the value of `startPos`. If it is negative, it has the same effect as if it were zero: this entire
    * string may be searched. If it is greater than the length of this string, it has the same effect as if it were equal to the length of
    * this string: `(INDEX_NOT_FOUND) -1` is returned. Furthermore, a `null` or empty ("") CharSequence will return `(INDEX_NOT_FOUND) -1`.
    *
    * <p>All indices are specified in `char` values (Unicode code units).
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
    *   the first index of the search character (always &ge; startPos), -1 if no match or `null` string input
    */
  def indexOf(searchChar: Char, startPos: Int): Int = Strings.indexOf(strOrNull, searchChar, startPos)

  /** Returns the index within `seq` of the first occurrence of the specified character, starting the search at the specified index. <p> If
    * a character with value `searchChar` occurs in the character sequence represented by the `seq` `CharSequence` object at an index no
    * smaller than `startPos`, then the index of the first such occurrence is returned. For values of `searchChar` in the range from 0 to
    * 0xFFFF (inclusive), this is the smallest value <i>k</i> such that:
    * {{{
    * (this.charAt(<i>k</i>) == searchChar) &amp;&amp; (<i>k</i> >= startPos)
    * }}}
    * is true. For other values of `searchChar`, it is the smallest value <i>k</i> such that:
    * {{{
    * (this.codePointAt(<i>k</i>) == searchChar) && (<i>k</i> >= startPos)
    * }}}
    * is true. In either case, if no such character occurs in `seq` at or after position `startPos`, then `-1` is returned.
    *
    * <p> There is no restriction on the value of `startPos`. If it is negative, it has the same effect as if it were zero: this entire
    * string may be searched. If it is greater than the length of this string, it has the same effect as if it were equal to the length of
    * this string: `(INDEX_NOT_FOUND) -1` is returned. Furthermore, a `null` or empty ("") CharSequence will return `(INDEX_NOT_FOUND) -1`.
    *
    * <p>All indices are specified in `char` values (Unicode code units).
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
    *   the first index of the search character (always &ge; startPos), -1 if no match or `null` string input
    */
  def indexOf(searchChar: Int, startPos: Int): Int = Strings.indexOf(strOrNull, searchChar, startPos)

  /** <p>Search a CharSequence to find the first index of any character in the given set of characters.</p>
    *
    * <p>A `null` String will return `-1`. A `null` or zero length search array will return `-1`.</p>
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
    S: Adt.CoProducts6[*, Char, Option[Char], CharSequence, Option[CharSequence], SeqCharSequence, Option[SeqCharSequence]]
  ](
    searchArgs: S*
  ): Int = {

    def typeCharAdt =
      Adt.CoProduct6[Char, Option[Char], CharSequence, Option[CharSequence], SeqCharSequence, Option[SeqCharSequence]].typeOnly[S]
    def charAdt = Adt.CoProduct6[Char, Option[Char], CharSequence, Option[CharSequence], SeqCharSequence, Option[SeqCharSequence]]

    def indexOfNull = Strings.indexOfAny(strOrNull, null)

    def dealWithSeqChar(chars: Seq[Char])             = Strings.indexOfAny(strOrNull, chars: _*)
    def dealWithSeqString(strings: Seq[CharSequence]) = Strings.indexOfAny(strOrNull, strings: _*)
    def dealWithChar(char: Char)                      = Strings.indexOfAny(strOrNull, char)
    def dealWithString(string: CharSequence)          = Strings.indexOfAny(strOrNull, string)

    if (searchArgs == null) indexOfNull
    else if (searchArgs.length == 1)
      charAdt
        .instance(searchArgs.head)
        .fold6(arg0 => dealWithChar(arg0))
        .fold5(arg0 => arg0.map(dealWithChar).getOrElse(indexOfNull))
        .fold4(arg0 => dealWithString(arg0))
        .fold3(arg0 => arg0.map(dealWithString).getOrElse(indexOfNull))
        .fold2(arg0 => dealWithString(arg0))
        .fold1(arg0 => arg0.map(dealWithString).getOrElse(indexOfNull))
    else
      typeCharAdt
        .fold6(func => dealWithSeqChar(func.higherKindApply[Seq](searchArgs)))
        .fold5(func => dealWithSeqChar(mapToCharSeq(func.higherKindApply[Seq](searchArgs))))
        .fold4(func => dealWithSeqString(func.higherKindApply[Seq](searchArgs)))
        .fold3(func => dealWithSeqString(mapToCharSequenceSeq(func.higherKindApply[Seq](searchArgs))))
        .fold2(func => dealWithSeqString(func.higherKindApply[Seq](searchArgs)))
        .fold1(func => dealWithSeqString(mapToCharSequenceSeq(func.higherKindApply[Seq](searchArgs))))

  }

  /** <p>Searches a CharSequence to find the first index of any character not in the given set of characters.</p>
    *
    * <p>A `null` CharSequence will return `-1`. A `null` or zero length search array will return `-1`.</p>
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
    * <p>A `null` CharSequence will return `-1`. A `null` or empty search string will return `-1`.</p>
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
    * <p>A `null` CharSequence will return `-1`. A `null` or empty search string will return `-1`.</p>
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
    * <p>For example, `indexOfDifference(new String[] {"i am a machine", "i am a robot"`) -> 7}</p>
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
  def indexOfDifference[S: Adt.CoProducts2[*, CharSequence, Option[CharSequence]]](cs: S): Int = {
    val str1 = mapToCsOpt(cs).orNull
    Strings.indexOfDifference(strOrNull, str1)
  }

  /** <p>Case in-sensitive find of the first index within a CharSequence.</p>
    *
    * <p>A `null` CharSequence will return `-1`. A negative start position is treated as zero. An empty ("") search CharSequence always
    * matches. A start position greater than the string length only matches an empty search CharSequence.</p>
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
    *   the first index of the search CharSequence, -1 if no match or `null` string input
    */
  def indexOfIgnoreCase[S: Adt.CoProducts2[*, CharSequence, Option[CharSequence]]](searchStr: S): Int = {
    val str1 = mapToCsOpt(searchStr).orNull
    Strings.indexOfIgnoreCase(strOrNull, str1)
  }

  /** <p>Case in-sensitive find of the first index within a CharSequence from the specified position.</p>
    *
    * <p>A `null` CharSequence will return `-1`. A negative start position is treated as zero. An empty ("") search CharSequence always
    * matches. A start position greater than the string length only matches an empty search CharSequence.</p>
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
    *   the first index of the search CharSequence (always &ge; startPos), -1 if no match or `null` string input
    */
  def indexOfIgnoreCase[S: Adt.CoProducts2[*, CharSequence, Option[CharSequence]]](searchStr: S, startPos: Int): Int = {
    val str1 = mapToCsOpt(searchStr).orNull
    Strings.indexOfIgnoreCase(strOrNull, str1, startPos)
  }

  /** * <p>Checks if the CharSequence contains only lowercase characters.</p>
    *
    * <p>`null` will return `false`. An empty CharSequence (length()=0) will return `false`.</p>
    *
    * {{{
    * none.ops.isAllLowerCase   = false
    * "".ops.isAllLowerCase     = false
    * "  ".ops.isAllLowerCase   = false
    * "abc".ops.isAllLowerCase  = true
    * "abC".ops.isAllLowerCase  = false
    * "ab c".ops.isAllLowerCase = false
    * "ab1c".ops.isAllLowerCase = false
    * "ab/c".ops.isAllLowerCase = false
    * }}}
    *
    * @return
    *   `true` if only contains lowercase characters, and is non-null
    */
  def isAllLowerCase: Boolean = Strings.isAllLowerCase(strOrNull)

  /** <p>Checks if the CharSequence contains only uppercase characters.</p>
    *
    * <p>`null` will return `false`. An empty String (length()=0) will return `false`.</p>
    *
    * {{{
    * none.ops.isAllUpperCase   = false
    * "".ops.isAllUpperCase     = false
    * "  ".ops.isAllUpperCase   = false
    * "ABC".ops.isAllUpperCase  = true
    * "aBC".ops.isAllUpperCase  = false
    * "A C".ops.isAllUpperCase  = false
    * "A1C".ops.isAllUpperCase  = false
    * "A/C".ops.isAllUpperCase  = false
    * }}}
    *
    * @return
    */
  def isAllUpperCase: Boolean = Strings.isAllUpperCase(strOrNull)

  /** <p>Checks if the CharSequence contains only Unicode letters.</p>
    *
    * <p>`null` will return `false`. An empty CharSequence (length()=0) will return `false`.</p>
    *
    * {{{
    * none.ops.isAlpha   = false
    * "".ops.isAlpha     = false
    * "  ".ops.isAlpha   = false
    * "abc".ops.isAlpha  = true
    * "ab2c".ops.isAlpha = false
    * "ab-c".ops.isAlpha = false
    * }}}
    *
    * @return
    *   `true` if only contains letters, and is non-null
    */
  def isAlpha: Boolean = Strings.isAlpha(strOrNull)

  /** <p>Checks if the CharSequence contains only Unicode letters or digits.</p>
    *
    * <p>`null` will return `false`. An empty CharSequence (length()=0) will return `false`.</p>
    *
    * {{{
    * none.ops.isAlphanumeric = false
    * "".ops.isAlphanumeric = false "
    * ".ops.isAlphanumeric = false
    * "abc".ops.isAlphanumeric = true
    * "abc".ops.isAlphanumeric = false
    * "ab2c".ops.isAlphanumeric = true
    * "ab-c".ops.isAlphanumeric = false
    * }}}
    *
    * @return
    *   `true` if only contains letters or digits, and is non-null
    */
  def isAlphanumeric: Boolean = Strings.isAlphanumeric(strOrNull)

  /** <p>Checks if the CharSequence contains only Unicode letters, digits or space (`' '`).</p>
    *
    * <p>`null` will return `false`. An empty CharSequence (length()=0) will return `true`.</p>
    *
    * {{{
    * StringUtils.ops.isAlphanumericSpace(null)   = false
    * StringUtils.ops.isAlphanumericSpace("")     = true
    * StringUtils.ops.isAlphanumericSpace("  ")   = true
    * StringUtils.ops.isAlphanumericSpace("abc")  = true
    * StringUtils.ops.isAlphanumericSpace("ab c") = true
    * StringUtils.ops.isAlphanumericSpace("ab2c") = true
    * StringUtils.ops.isAlphanumericSpace("ab-c") = false
    * }}}
    *
    * @return
    *   `true` if only contains letters, digits or space, and is non-null
    */
  def isAlphanumericSpace: Boolean = Strings.isAlphanumericSpace(strOrNull)

  /** <p>Checks if the CharSequence contains only Unicode letters and space (' ').</p>
    *
    * <p>`null` will return `false` An empty CharSequence (length()=0) will return `true`.</p>
    *
    * {{{
    * none.ops.isAlphaSpace = false
    * "".ops.isAlphaSpace = true
    * " ".ops.isAlphaSpace = true
    * "abc".ops.isAlphaSpace = true
    * "abc".ops.isAlphaSpace = true
    * "ab2c".ops.isAlphaSpace = false
    * "ab-c".ops.isAlphaSpace = false
    * }}}
    *
    * @return
    */
  def isAlphaSpace: Boolean = Strings.isAlphaSpace(strOrNull)

  /** <p>Checks if the CharSequence contains only ASCII printable characters.</p>
    *
    * <p>`null` will return `false`. An empty CharSequence (length()=0) will return `true`.</p>
    *
    * {{{
    * none.ops.isAsciiPrintable     = false
    * "".ops.isAsciiPrintable       = true
    * " ".ops.isAsciiPrintable      = true
    * "Ceki".ops.isAsciiPrintable   = true
    * "ab2c".ops.isAsciiPrintable   = true
    * "!ab-c~".ops.isAsciiPrintable = true
    * "\u0020".ops.isAsciiPrintable = true
    * "\u0021".ops.isAsciiPrintable = true
    * "\u007e".ops.isAsciiPrintable = true
    * "\u007f".ops.isAsciiPrintable = false
    * "Ceki G\u00fclc\u00fc".ops.isAsciiPrintable = false
    * }}}
    *
    * @return
    */
  def isAsciiPrintable: Boolean = Strings.isAsciiPrintable(strOrNull)

  /** <p>Checks if a CharSequence is empty (""), null or whitespace only.</p>
    *
    * <p>Whitespace is defined by [[Character# isWhitespace ( char )]].</p>
    *
    * {{{
    * null.ops.isBlank      = true
    * "".ops.isBlank        = true
    * " ".ops.isBlank       = true
    * "bob".ops.isBlank     = false
    * "  bob  ".ops.isBlank = false
    * }}}
    *
    * @return
    *   `true` if the CharSequence is none, empty or whitespace only
    */
  def isBlank: Boolean = Strings.isBlank(strOrNull)

  /** <p>Checks if a CharSequence is empty ("") or null.</p>
    *
    * {{{
    * StringUtils.isEmpty(null)      = true
    * StringUtils.isEmpty("")        = true
    * StringUtils.isEmpty(" ")       = false
    * StringUtils.isEmpty("bob")     = false
    * StringUtils.isEmpty("  bob  ") = false
    * }}}
    *
    * <p>NOTE: This method changed in Lang version 2.0. It no longer trims the CharSequence. That functionality is available in
    * isBlank().</p>
    *
    * @return
    *   `true` if the CharSequence is empty or none
    */
  def isEmpty: Boolean = Strings.isEmpty(strOrNull)

  /** <p>Checks if the CharSequence contains mixed casing of both uppercase and lowercase characters.</p>
    *
    * <p>`null` will return `false`. An empty CharSequence (`length()=0`) will return `false`.</p>
    *
    * {{{
    * none.ops.isMixedCase    = false
    * "".ops.isMixedCase      = false
    * "ABC".ops.isMixedCase   = false
    * "abc".ops.isMixedCase   = false
    * "aBc".ops.isMixedCase   = true
    * "A c".ops.isMixedCase   = true
    * "A1c".ops.isMixedCase   = true
    * "a/C".ops.isMixedCase   = true
    * "aC\t".ops.isMixedCase  = true
    * }}}
    *
    * @return
    *   `true` if the CharSequence contains both uppercase and lowercase characters
    */
  def isMixedCase: Boolean = Strings.isMixedCase(strOrNull)

  /** <p>Checks if none of the CharSequences are empty (""), null or whitespace only.</p>
    *
    * <p>Whitespace is defined by [[Character# isWhitespace ( char )]].</p>
    *
    * {{{
    * none.ops.isNotBlank      = false
    * "".ops.isNotBlank        = false
    * " ".ops.isNotBlank       = false
    * "bob".ops.isNotBlank     = true
    * "  bob  ".ops.isNotBlank = true
    * }}}
    *
    * @return
    *   `true` if the CharSequence is not empty and not null and not whitespace only
    */
  def isNotBlank: Boolean = Strings.isNotBlank(strOrNull)

  /** <p>Checks if a CharSequence is not empty ("") and not null.</p>
    *
    * {{{
    * none.ops.isNotBlank      = false
    * "".ops.isNotBlank        = false
    * " ".ops.isNotBlank       = true
    * "bob".ops.isNotBlank     = true
    * "  bob  ".ops.isNotBlank = true
    * }}}
    *
    * @return
    *   `true` if the CharSequence is not empty and not null
    */
  def isNotEmpty: Boolean = Strings.isNotEmpty(strOrNull)

  /** <p>Checks if the CharSequence contains only Unicode digits. A decimal point is not a Unicode digit and returns false.</p>
    *
    * <p>`null` will return `false`. An empty CharSequence (length()=0) will return `false`.</p>
    *
    * <p>Note that the method does not allow for a leading sign, either positive or negative. Also, if a String passes the numeric test, it
    * may still generate a NumberFormatException when parsed by Integer.parseInt or Long.parseLong, e.g. if the value is outside the range
    * for int or long respectively.</p>
    *
    * {{{
    * none.ops.isNumeric   = false
    * "".ops.isNumeric     = false
    * "  ".ops.isNumeric   = false
    * "123".ops.isNumeric  = true
    * "\u0967\u0968\u0969".ops.isNumeric  = true
    * "12 3".ops.isNumeric = false
    * "ab2c".ops.isNumeric = false
    * "12-3".ops.isNumeric = false
    * "12.3".ops.isNumeric = false
    * "-123".ops.isNumeric = false
    * "+123".ops.isNumeric = false
    * }}}
    *
    * @return
    *   `true` if only contains digits, and is non-null
    */
  def isNumeric: Boolean = Strings.isNumeric(strOrNull)

  /** <p>Checks if the CharSequence contains only Unicode digits or space (`' '`). A decimal point is not a Unicode digit and returns
    * false.</p>
    *
    * <p>`null` will return `false`. An empty CharSequence (length()=0) will return `true`.</p>
    *
    * {{{
    * none.ops.isNumericSpace   = false
    * "".ops.isNumericSpace     = true
    * "  ".ops.isNumericSpace   = true
    * "123".ops.isNumericSpace  = true
    * "12 3".ops.isNumericSpace = true
    * "\u0967\u0968\u0969".ops.isNumericSpace  = true
    * "\u0967\u0968 \u0969".ops.isNumericSpace  = true
    * "ab2c".ops.isNumericSpace = false
    * "12-3".ops.isNumericSpace = false
    * "12.3".ops.isNumericSpace = false
    * }}}
    *
    * @return
    *   `true` if only contains digits or space, and is non-null
    */
  def isNumericSpace: Boolean = Strings.isNumericSpace(strOrNull)

  /** <p>Checks if the CharSequence contains only whitespace.</p>
    *
    * <p>Whitespace is defined by [[Character# isWhitespace ( char )]].</p>
    *
    * <p>`null` will return `false`. An empty CharSequence (length()=0) will return `true`.</p>
    *
    * {{{
    * none.ops.isWhitespace   = false
    * "".ops.isWhitespace     = true
    * "  ".ops.isWhitespace   = true
    * "abc".ops.isWhitespace  = false
    * "ab2c".ops.isWhitespace = false
    * "ab-c".ops.isWhitespace = false
    * }}}
    *
    * @return
    *   if only contains whitespace, and is non-null
    */
  def isWhitespace: Boolean = Strings.isWhitespace(strOrNull)

  /** <p>Finds the last index within a CharSequence, handling `null`. This method uses [[String# lastIndexOf ( String, int)]] if
    * possible.</p>
    *
    * <p>A `null` CharSequence will return `-1`. A negative start position returns `-1`. An empty ("") search CharSequence always matches
    * unless the start position is negative. A start position greater than the string length searches the whole string. The search starts at
    * the startPos and works backwards; matches starting after the start position are ignored. </p>
    *
    * {{{
    * none.ops.lastIndexOf(*)          = -1
    * *.ops.lastIndexOf(null)          = -1
    * "".ops.lastIndexOf("")           = 0
    * "aabaabaa".ops.lastIndexOf("a")  = 7
    * "aabaabaa".ops.lastIndexOf("b")  = 5
    * "aabaabaa".ops.lastIndexOf("ab") = 4
    * "aabaabaa".ops.lastIndexOf("")   = 8
    * }}}
    *
    * @param searchArg
    *   the CharSequence to find, may be null
    * @tparam S
    *   Char or CharSequence or Option[CharSequence]
    * @return
    *   the last index of the search String,
    */
  def lastIndexOf[S: Adt.CoProducts4[*, Char, Int, CharSequence, Option[CharSequence]]](searchArg: S): Int = {
    val applyM = Adt.CoProduct4[Char, Int, CharSequence, Option[CharSequence]].instance(searchArg)
    applyM
      .fold4(ch => Strings.lastIndexOf(strOrNull, ch))
      .fold3(i => Strings.lastIndexOf(strOrNull, i))
      .fold2(str => Strings.lastIndexOf(strOrNull, str))
      .fold1(ostr => Strings.lastIndexOf(strOrNull, ostr.orNull))
  }

  /** Returns the index within `seq` of the last occurrence of the specified character, searching backward starting at the specified index.
    * For values of `searchChar` in the range from 0 to 0xFFFF (inclusive), the index returned is the largest value <i>k</i> such that:
    * <blockquote><pre> (this.charAt(<i>k</i>) == searchChar) &amp;&amp; (<i>k</i> &lt;= startPos) </pre></blockquote> is true. For other
    * values of `searchChar`, it is the largest value <i>k</i> such that: <blockquote><pre> (this.codePointAt(<i>k</i>) == searchChar)
    * &amp;&amp; (<i>k</i> &lt;= startPos) </pre></blockquote> is true. In either case, if no such character occurs in `seq`` at or before
    * position `startPos`, then `-1` is returned. Furthermore, a `null` or empty ("") `CharSequence`` will return `-1`. A start position
    * greater than the string length searches the whole string. The search starts at the `startPos` and works backwards; matches starting
    * after the start position are ignored.
    *
    * <p>All indices are specified in `char` values (Unicode code units).
    *
    * {{{
    * StringUtils.lastIndexOf(null, *, *)          = -1
    * StringUtils.lastIndexOf("", *,  *)           = -1
    * StringUtils.lastIndexOf("aabaabaa", 'b', 8)  = 5
    * StringUtils.lastIndexOf("aabaabaa", 'b', 4)  = 2
    * StringUtils.lastIndexOf("aabaabaa", 'b', 0)  = -1
    * StringUtils.lastIndexOf("aabaabaa", 'b', 9)  = 5
    * StringUtils.lastIndexOf("aabaabaa", 'b', -1) = -1
    * StringUtils.lastIndexOf("aabaabaa", 'a', 0)  = 0
    * }}}
    *
    * @param searchArg
    *   the element to find
    * @param startPos
    *   the start position
    * @tparam S
    *   Char or CharSequence or Option[CharSequence]
    * @return
    *   the last index of the search character (always &le; startPos), -1 if no match or `null` string input
    */
  def lastIndexOf[S: Adt.CoProducts4[*, Char, Int, CharSequence, Option[CharSequence]]](searchArg: S, startPos: Int): Int = {
    val applyM = Adt.CoProduct4[Char, Int, CharSequence, Option[CharSequence]].instance(searchArg)
    applyM
      .fold4(ch => Strings.lastIndexOf(strOrNull, ch, startPos))
      .fold3(i => Strings.lastIndexOf(strOrNull, i, startPos))
      .fold2(str => Strings.lastIndexOf(strOrNull, str, startPos))
      .fold1(ostr => Strings.indexOf(strOrNull, ostr.orNull, startPos))
  }

  /** <p>Find the latest index of any substring in a set of potential substrings.</p>
    *
    * <p>A `null` CharSequence will return `-1`. A `null` search array will return `-1`. A `null` or zero length search array entry will be
    * ignored, but a search array containing "" will return the length of `str`if `str`is not null. This method uses
    * [[String# indexOf( String )]] if possible</p>
    *
    * {{{
    * none.ops.lastIndexOfAny(*)                    = -1
    * *.ops.lastIndexOfAny(null)                    = -1
    * *.ops.lastIndexOfAny([])                      = -1
    * *.ops.lastIndexOfAny([null])                  = -1
    * "zzabyycdxx".ops.lastIndexOfAny("ab", "cd") = 6
    * "zzabyycdxx".ops.lastIndexOfAny("cd", "ab") = 6
    * "zzabyycdxx".ops.lastIndexOfAny("mn", "op") = -1
    * "zzabyycdxx".ops.lastIndexOfAny("mn", "op") = -1
    * "zzabyycdxx".ops.lastIndexOfAny("mn", "")   = 10
    * }}}
    *
    * @param searchArgs
    *   the CharSequences to search for, may be null
    * @tparam S
    *   varargs for CharSequences or Option[CharSequences]
    * @return
    *   the last index of any of the CharSequences, -1 if no match
    */
  def lastIndexOfAny[S: Adt.CoProducts2[*, CharSequence, Option[CharSequence]]](searchArgs: S*): Int = {
    def applyM = Adt.CoProduct2[CharSequence, Option[CharSequence]].typeOnly[S]
    def seq: Seq[CharSequence] =
      applyM.fold2(_.higherKindApply[Seq](searchArgs)).fold1(func => mapToCharSequenceSeq(func.higherKindApply[Seq](searchArgs)))

    if (searchArgs == null)
      Strings.lastIndexOfAny(strOrNull, null)
    else
      Strings.lastIndexOfAny(strOrNull, seq: _*)
  }

  /** <p>Case in-sensitive find of the last index within a CharSequence.</p>
    *
    * <p>A `null` CharSequence will return `-1`. A negative start position returns `-1`. An empty ("") search CharSequence always matches
    * unless the start position is negative. A start position greater than the string length searches the whole string.</p>
    *
    * {{{
    * none.ops.lastIndexOfIgnoreCase(*)          = -1
    * *.ops.lastIndexOfIgnoreCase(null)          = -1
    * "aabaabaa".ops.lastIndexOfIgnoreCase("A")  = 7
    * "aabaabaa".ops.lastIndexOfIgnoreCase("B")  = 5
    * "aabaabaa".ops.lastIndexOfIgnoreCase("AB") = 4
    * }}}
    *
    * @param searchStr
    *   the CharSequence to find, may be null
    * @tparam S
    *   varargs of CharSequence or Option[CharSequence]
    * @return
    *   the first index of the search CharSequence,
    */
  def lastIndexOfIgnoreCase[S: Adt.CoProducts2[*, CharSequence, Option[CharSequence]]](searchStr: S): Int = {
    val str1 = mapToCsOpt(searchStr).orNull
    Strings.lastIndexOfIgnoreCase(strOrNull, str1)
  }

  /** <p>Case in-sensitive find of the last index within a CharSequence from the specified position.</p>
    *
    * <p>A `null` CharSequence will return `-1`. A negative start position returns `-1`. An empty ("") search CharSequence always matches
    * unless the start position is negative. A start position greater than the string length searches the whole string. The search starts at
    * the startPos and works backwards; matches starting after the start position are ignored. </p>
    *
    * {{{
    * none.ops.lastIndexOfIgnoreCase(*, *) = -1
    * *.ops.lastIndexOfIgnoreCase(null, *) = -1
    * "aabaabaa".ops.lastIndexOfIgnoreCase("A", 8) = 7
    * "aabaabaa".ops.lastIndexOfIgnoreCase("B", 8) = 5
    * "aabaabaa".ops.lastIndexOfIgnoreCase("AB", 8) = 4
    * "aabaabaa".ops.lastIndexOfIgnoreCase("B", 9) = 5
    * "aabaabaa".ops.lastIndexOfIgnoreCase("B", -1) = -1
    * "aabaabaa".ops.lastIndexOfIgnoreCase("A", 0) = 0
    * "aabaabaa".ops.lastIndexOfIgnoreCase("B", 0) = -1
    * }}}
    *
    * @param searchStr
    *   the CharSequence to find, may be null
    * @param startPos
    *   the start position
    * @tparam S
    *   CharSequence or Option[CharSequence]
    * @return
    *   the last index of the search CharSequence (always &le; startPos), -1 if no match or `null` input
    */
  def lastIndexOfIgnoreCase[S: Adt.CoProducts2[*, CharSequence, Option[CharSequence]]](searchStr: S, startPos: Int): Int = {
    val str1 = mapToCsOpt(searchStr).orNull
    Strings.lastIndexOfIgnoreCase(strOrNull, str1, startPos)
  }

  /** <p>Finds the n-th last index within a String, handling `null`. This method uses [[String# lastIndexOf ( String )]].</p>
    *
    * <p>A `null` String will return `-1`.</p>
    *
    * {{{
    * none.ops.lastIndexOf(*, *)          = -1
    * *.ops.lastIndexOf(null, *)          = -1
    * "".ops.lastIndexOf("", *)           = 0
    * "aabaabaa".ops.lastIndexOf("a", 1)  = 7
    * "aabaabaa".ops.lastIndexOf("a", 2)  = 6
    * "aabaabaa".ops.lastIndexOf("b", 1)  = 5
    * "aabaabaa".ops.lastIndexOf("b", 2)  = 2
    * "aabaabaa".ops.lastIndexOf("ab", 1) = 4
    * "aabaabaa".ops.lastIndexOf("ab", 2) = 1
    * "aabaabaa".ops.lastIndexOf("", 1)   = 8
    * "aabaabaa".ops.lastIndexOf("", 2)   = 8
    * }}}
    *
    * <p>Note that 'tail(CharSequence str, int n)' may be implemented as: </p>
    *
    * {{{
    * str.substring(lastOrdinalIndexOf(str, "\n", n) + 1)
    * }}}
    *
    * @param searchStr
    *   the CharSequence to find, may be null
    * @param ordinal
    *   the n-th last `searchStr` to find
    * @tparam S
    *   CharSequence or Option[CharSequence]
    * @return
    *   the n-th last index of the search CharSequence, `-1` (`INDEX_NOT_FOUND`) if no match or `null` string input
    */
  def lastOrdinalIndexOf[S: Adt.CoProducts2[*, CharSequence, Option[CharSequence]]](searchStr: S, ordinal: Int): Int = {
    val str1 = mapToCsOpt(searchStr).orNull
    Strings.lastOrdinalIndexOf(strOrNull, str1, ordinal)
  }

  /** <p>Gets the leftmost `len` characters of a String.</p>
    *
    * <p>If `len` characters are not available, or the String is `null`, the String will be returned without an exception. An empty String
    * is returned if len is negative.</p>
    *
    * {{{
    * none.ops.left(*)    = null
    * *.ops.left(-ve)     = ""
    * "".ops.left(*)      = ""
    * "abc".ops.left( 0)   = ""
    * "abc".ops.left(2)   = "ab"
    * "abc".ops.left(4)   = "abc"
    * }}}
    *
    * @param len
    *   the length of the required String
    * @return
    *   the leftmost characters, `none` if none String input
    */
  def left(len: Int): Option[String] = Option(Strings.left(strOrNull, len))

  /** <p>Left pad a String with spaces (' ').</p>
    *
    * <p>The String is padded to the size of `size`.</p>
    *
    * {{{
    * none.ops.leftPad(*)   = null
    * "".ops.leftPad(3)     = "   "
    * "bat".ops.leftPad(3)  = "bat"
    * "bat".ops.leftPad(5)  = "  bat"
    * "bat".ops.leftPad(1)  = "bat"
    * "bat".ops.leftPad(-1) = "bat"
    * }}}
    *
    * @param size
    *   the size to pad to
    * @return
    *   left padded String or original String if no padding is necessary, `none` if none String input
    */
  def leftPad(size: Int): Option[String] = Option(Strings.leftPad(strOrNull, size))

  /** <p>Left pad a String with a specified character.</p>
    *
    * <p>Pad to a size of `size`.</p>
    *
    * {{{
    * StringUtils.leftPad(null, *, *)     = null
    * StringUtils.leftPad("", 3, 'z')     = "zzz"
    * StringUtils.leftPad("bat", 3, 'z')  = "bat"
    * StringUtils.leftPad("bat", 5, 'z')  = "zzbat"
    * StringUtils.leftPad("bat", 1, 'z')  = "bat"
    * StringUtils.leftPad("bat", -1, 'z') = "bat"
    * }}}
    *
    * @param size
    *   the size to pad to
    * @param padChar
    *   the character to pad with
    * @return
    *   left padded String or original String if no padding is necessary, `none` if none String input
    */
  def leftPad(size: Int, padChar: Char): Option[String] = Option(Strings.leftPad(strOrNull, size, padChar))

  /** <p>Left pad a String with a specified String.</p>
    *
    * <p>Pad to a size of `size`.</p>
    *
    * {{{
    * none.ops.leftPad(*, *) = null
    * "".ops.leftPad(3, "z") = "zzz"
    * "bat".ops.leftPad(3, "yz") = "bat"
    * "bat".ops.leftPad(5, "yz") = "yzbat"
    * "bat".ops.leftPad(8, "yz") = "yzyzybat"
    * "bat".ops.leftPad(1, "yz") ="bat"
    * "bat".ops.leftPad(-1, "yz") = "bat"
    * "bat".ops.leftPad(5, null) = " bat"
    * "bat".ops.leftPad(5, "") = "bat"
    * }}}
    *
    * @param size
    *   the size to pad to
    * @param padStr
    *   the String to pad with, null or empty treated as single space
    * @tparam P
    *   String or Option[String]
    * @return
    *   left padded String or original String if no padding is necessary, `none` if none String input
    */
  def leftPad[P: Adt.CoProducts2[*, String, Option[String]]](size: Int, padStr: P): Option[String] = {
    val ps = mapToStrOpt(padStr).orNull
    Option(Strings.leftPad(strOrNull, size, ps))
  }

  /** * Gets a CharSequence length or `0` if the CharSequence is `null`.
    *
    * @return
    *   CharSequence length or `0` if the CharSequence is `null`.
    */
  def length: Int = Strings.length(strOrNull)

  /** <p>Converts a String to lower case as per [[String # toLowerCase ( )]].</p>
    *
    * <p>A `null` input String returns `null`.</p>
    *
    * {{{
    * null.ops.lowerCase  = null
    * "".ops.lowerCase    = ""
    * "aBc".ops.lowerCase = "abc"
    * }}}
    *
    * <p><strong>Note:</strong> As described in the documentation for @[[String String#toLowerCase ( ) ]], the result of this method is
    * affected by the current locale. For platform-independent case transformations, the method [[String # lowerCase ( String, Locale)]]
    * should be used with a specific locale (e.g. [[Locale.ENGLISH Locale.ENGLISH]]).</p>
    *
    * @return
    *   the lower cased String, `none` if none String input
    */
  def lowerCase: Option[String] = Option(Strings.lowerCase(strOrNull))

  /** <p>Converts a String to lower case as per [[String# toLowerCase ( Locale )]].</p>
    *
    * <p>A `null` input String returns `null`.</p>
    *
    * {{{
    * none.ops.lowerCase(Locale.ENGLISH)  = null
    * "".ops.lowerCase(Locale.ENGLISH)    = ""
    * "aBc".opw.lowerCase(Locale.ENGLISH) = "abc"
    * }}}
    *
    * @param locale
    *   the locale that defines the case transformation rules, must not be null
    * @return
    *   the lower cased String, `none` if none String input
    */
  def lowerCase(locale: Locale): Option[String] = Option(Strings.lowerCase(strOrNull, locale))

  /** <p>Gets `len` characters from the middle of a String.</p>
    *
    * <p>If `len` characters are not available, the remainder of the String will be returned without an exception. If the String is `null`,
    * `null` will be returned. An empty String is returned if len is negative or exceeds the length of `str`.</p>
    *
    * {{{}}} none.ops.mid(*, *) = null *.ops.mid(*, -ve) = "" "".ops.mid(0, *) = "" "abc".ops.mid(0, 2) = "ab" "abc".ops.mid(0, 4) = "abc"
    * "abc".ops.mid(2, 4) = "c" "abc".ops.mid(4, 2) = "" "abc".ops.mid(-2, 2) = "ab" }}}
    *
    * @param pos
    *   the position to start from, negative treated as zero
    * @param len
    *   the length of the required String
    * @return
    *   the middle characters, `none` if none String input
    */
  def mid(pos: Int, len: Int): Option[String] = Option(Strings.mid(strOrNull, pos, len))

  /** <p> Similar to <a href="http://www.w3.org/TR/xpath/#function-normalize-space">http://www.w3.org/TR/xpath/#function-normalize
    * -space</a> </p> <p> The function returns the argument string with whitespace normalized by using [[trim trim(String)]] to remove
    * leading and trailing whitespace and then replacing sequences of whitespace characters by a single space. </p> In XML Whitespace
    * characters are the same as those allowed by the <a href="http://www.w3.org/TR/REC-xml/#NT-S">S</a> production, which is S ::= (#x20 |
    * #x9 | #xD | #xA)+ <p> Java's regexp pattern \s defines whitespace as [ \t\n\x0B\f\r]
    *
    * <p>For reference:</p> <ul> <li>\x0B = vertical tab</li> <li>\f = #xC = form feed</li> <li>#x20 = space</li> <li>#x9 = \t</li> <li>#xA
    * \= \n</li> <li>#xD = \r</li> </ul>
    *
    * <p> The difference is that Java's whitespace includes vertical tab and form feed, which this functional will also normalize.
    * Additionally [[trim(String)]] removes control characters (char &lt;= 32) from both ends of this String. </p>
    *
    * @return
    *   the modified string with whitespace normalized, `none` if none String input
    */
  def normalizeSpace: Option[String] = Option(Strings.normalizeSpace(strOrNull))

  /** <p>Finds the n-th index within a CharSequence, handling `null`. This method uses [[String# indexOf ( String )]] if possible.</p>
    * <p><b>Note:</b> The code starts looking for a match at the start of the target, incrementing the starting index by one after each
    * successful match (unless `searchStr` is an empty string in which case the position is never incremented and `0` is returned
    * immediately). This means that matches may overlap.</p> <p>A `null` CharSequence will return `-1`.</p>
    *
    * {{{
    * none.ops.ordinalIndexOf(*, *)          = -1
    * *.ops.ordinalIndexOf(null, *)          = -1
    * "".ops.ordinalIndexOf("", *)           = 0
    * "aabaabaa".ops.ordinalIndexOf("a", 1)  = 0
    * "aabaabaa".ops.ordinalIndexOf("a", 2)  = 1
    * "aabaabaa".ops.ordinalIndexOf("b", 1)  = 2
    * "aabaabaa".ops.ordinalIndexOf("b", 2)  = 5
    * "aabaabaa".ops.ordinalIndexOf("ab", 1) = 1
    * "aabaabaa".ops.ordinalIndexOf("ab", 2) = 4
    * "aabaabaa".ops.ordinalIndexOf("", 1)   = 0
    * "aabaabaa".ops.ordinalIndexOf("", 2)   = 0
    * }}}
    *
    * <p>Matches may overlap:</p>
    * {{{
    * "ababab".ops.ordinalIndexOf("aba", 1)   = 0
    * "ababab".ops.ordinalIndexOf("aba", 2)   = 2
    * "ababab".ops.ordinalIndexOf("aba", 3)   = -1
    *
    * "abababab".ops.ordinalIndexOf("abab", 1) = 0
    * "abababab".ops.ordinalIndexOf("abab", 2) = 2
    * "abababab".ops.ordinalIndexOf("abab", 3) = 4
    * "abababab".ops.ordinalIndexOf("abab", 4) = -1
    * }}}
    *
    * <p>Note that 'head(CharSequence str, int n)' may be implemented as: </p>
    *
    * {{{
    * str.substring(0, lastOrdinalIndexOf(str, "\n", n))
    * }}}
    *
    * @param searchStr
    *   the CharSequence to find, may be null
    * @param ordinal
    *   the n-th `searchStr` to find
    * @tparam S
    *   CharSequence or Option[CharSequence]
    * @return
    *   the n-th index of the search CharSequence, `-1` (`INDEX_NOT_FOUND`) if no match or `null` string input
    */
  def ordinalIndexOf[S: Adt.CoProducts2[*, CharSequence, Option[CharSequence]]](searchStr: S, ordinal: Int): Int = {
    val str1 = mapToCsOpt(searchStr).orNull
    Strings.ordinalIndexOf(strOrNull, str1, ordinal)
  }

  /** <p>Overlays part of a String with another String.</p>
    *
    * <p>A `null` string input returns `None`. A negative index is treated as zero. An index greater than the string length is treated as
    * the string length. The start index is always the smaller of the two indices.</p>
    *
    * {{{
    * none.ops.overlay(*, *, *)            = null
    * "".ops.overlay("abc", 0, 0)          = "abc"
    * "abcdef".ops.overlay(none, 2, 4)     = "abef"
    * "abcdef".ops.overlay("", 2, 4)       = "abef"
    * "abcdef".ops.overlay("", 4, 2)       = "abef"
    * "abcdef".ops.overlay("zzzz", 2, 4)   = "abzzzzef"
    * "abcdef".ops.overlay("zzzz", 4, 2)   = "abzzzzef"
    * "abcdef".ops.overlay("zzzz", -1, 4)  = "zzzzef"
    * "abcdef".ops.overlay("zzzz", 2, 8)   = "abzzzz"
    * "abcdef".ops.overlay("zzzz", -2, -3) = "zzzzabcdef"
    * "abcdef".ops.overlay("zzzz", 8, 10)  = "abcdefzzzz"
    * }}}
    *
    * @param overlay
    *   the String to overlay, may be null
    * @param start
    *   the position to start overlaying at
    * @param end
    *   the position to stop overlaying before
    * @tparam O
    *   String or Option[String]
    * @return
    *   overlayed String, `none` if none String input
    */
  def overlay[O: Adt.CoProducts2[*, String, Option[String]]](overlay: O, start: Int, end: Int): Option[String] = {
    val str1    = mapToStrOpt(overlay).orNull
    val result2 = Strings.overlay(strOrNull, str1, start, end)
    Option(result2)
  }

  /** Prepends the prefix to the start of the string if the string does not already start with any of the prefixes.
    *
    * {{{
    * none.ops.prependIfMissing(none) = null
    * "abc".ops.prependIfMissing(null) = "abc"
    * "".ops.prependIfMissing("xyz") = "xyz"
    * "abc".ops.prependIfMissing("xyz") = "xyzabc"
    * "xyzabc".ops.prependIfMissing("xyz") = "xyzabc"
    * "XYZabc".ops.prependIfMissing("xyz") = "xyzXYZabc"
    * }}}
    * <p>With additional prefixes,</p>
    * {{{
    * none..ops.prependIfMissing(none, none) = None
    * "abc".ops.prependIfMissing(none, none) = "abc"
    * "".ops.prependIfMissing("xyz", none) = "xyz"
    * "abc".ops.prependIfMissing("xyz", none) = "xyzabc"
    * "abc".ops.prependIfMissing("xyz", "") = "abc"
    * "abc".ops.prependIfMissing("xyz", "mno") = "xyzabc"
    * "xyzabc".ops.prependIfMissing("xyz", "mno") = "xyzabc"
    * "mnoabc".ops.prependIfMissing("xyz", "mno") = "mnoabc"
    * "XYZabc".ops.prependIfMissing("xyz", "mno") = "xyzXYZabc"
    * "MNOabc".ops.prependIfMissing("xyz", "mno") = "xyzMNOabc"
    * }}}
    *
    * @param prefix
    *   The prefix to prepend to the start of the string.
    * @param prefixes
    *   Additional prefixes that are valid.
    * @tparam P
    *   CharSequence or Option[CharSequence]
    * @tparam Ps
    *   CharSequence or Option[CharSequence]
    * @return
    *   A new String if prefix was prepended, the same string otherwise.
    */
  def prependIfMissing[
    P: Adt.CoProducts2[*, CharSequence, Option[CharSequence]],
    Ps: Adt.CoProducts2[*, CharSequence, Option[CharSequence]]
  ](
    prefix: P,
    prefixes: Ps*
  ): Option[String] = {

    def prefixStr: CharSequence = mapToCsOpt(prefix).orNull

    def prefixesApplyM = Adt.CoProduct2[CharSequence, Option[CharSequence]].typeOnly[Ps]
    def result: Seq[CharSequence] =
      prefixesApplyM.fold2(_.higherKindApply[Seq](prefixes)).fold1(func => mapToCharSequenceSeq(func.higherKindApply[Seq](prefixes)))

    if (prefixes == null)
      Option(Strings.prependIfMissing(strOrNull, prefixStr, null))
    else
      Option(Strings.prependIfMissing(strOrNull, prefixStr, result: _*))

  }

  /** Prepends the prefix to the start of the string if the string does not already start with any of the prefixes.
    *
    * {{{
    * none.ops.prependIfMissing(none) = null
    * "abc".ops.prependIfMissing(null) = "abc"
    * "".ops.prependIfMissing("xyz") = "xyz"
    * "abc".ops.prependIfMissing("xyz") = "xyzabc"
    * "xyzabc".ops.prependIfMissing("xyz") = "xyzabc"
    * "XYZabc".ops.prependIfMissing("xyz") = "xyzXYZabc"
    * }}}
    * <p>With additional prefixes,</p>
    * {{{
    * none..ops.prependIfMissing(none, none) = None
    * "abc".ops.prependIfMissing(none, none) = "abc"
    * "".ops.prependIfMissing("xyz", none) = "xyz"
    * "abc".ops.prependIfMissing("xyz", none) = "xyzabc"
    * "abc".ops.prependIfMissing("xyz", "") = "abc"
    * "abc".ops.prependIfMissing("xyz", "mno") = "xyzabc"
    * "xyzabc".ops.prependIfMissing("xyz", "mno") = "xyzabc"
    * "mnoabc".ops.prependIfMissing("xyz", "mno") = "mnoabc"
    * "XYZabc".ops.prependIfMissing("xyz", "mno") = "xyzXYZabc"
    * "MNOabc".ops.prependIfMissing("xyz", "mno") = "xyzMNOabc"
    * }}}
    *
    * @param prefix
    *   The prefix to prepend to the start of the string.
    * @return
    *   A new String if prefix was prepended, the same string otherwise.
    */
  def prependIfMissing(prefix: CharSequence): Option[String] =
    Option(Strings.prependIfMissing(strOrNull, prefix))

  /** Prepends the prefix to the start of the string if the string does not already start with any of the prefixes.
    *
    * {{{
    * none.ops.prependIfMissing(none) = null
    * "abc".ops.prependIfMissing(null) = "abc"
    * "".ops.prependIfMissing("xyz") = "xyz"
    * "abc".ops.prependIfMissing("xyz") = "xyzabc"
    * "xyzabc".ops.prependIfMissing("xyz") = "xyzabc"
    * "XYZabc".ops.prependIfMissing("xyz") = "xyzXYZabc"
    * }}}
    * <p>With additional prefixes,</p>
    * {{{
    * none..ops.prependIfMissing(none, none) = None
    * "abc".ops.prependIfMissing(none, none) = "abc"
    * "".ops.prependIfMissing("xyz", none) = "xyz"
    * "abc".ops.prependIfMissing("xyz", none) = "xyzabc"
    * "abc".ops.prependIfMissing("xyz", "") = "abc"
    * "abc".ops.prependIfMissing("xyz", "mno") = "xyzabc"
    * "xyzabc".ops.prependIfMissing("xyz", "mno") = "xyzabc"
    * "mnoabc".ops.prependIfMissing("xyz", "mno") = "mnoabc"
    * "XYZabc".ops.prependIfMissing("xyz", "mno") = "xyzXYZabc"
    * "MNOabc".ops.prependIfMissing("xyz", "mno") = "xyzMNOabc"
    * }}}
    *
    * @param prefix
    *   The prefix to prepend to the start of the string.
    * @return
    *   A new String if prefix was prepended, the same string otherwise.
    */
  def prependIfMissing(prefix: Option[CharSequence]): Option[String] =
    Option(Strings.prependIfMissing(strOrNull, prefix.orNull))

  /** Prepends the prefix to the start of the string if the string does not already start, case insensitive, with any of the prefixes.
    *
    * {{{
    * StringUtils.prependIfMissingIgnoreCase(null, null) = null
    * StringUtils.prependIfMissingIgnoreCase("abc", null) = "abc"
    * StringUtils.prependIfMissingIgnoreCase("", "xyz") = "xyz"
    * StringUtils.prependIfMissingIgnoreCase("abc", "xyz") = "xyzabc"
    * StringUtils.prependIfMissingIgnoreCase("xyzabc", "xyz") = "xyzabc"
    * StringUtils.prependIfMissingIgnoreCase("XYZabc", "xyz") = "XYZabc"
    * }}}
    * <p>With additional prefixes,</p>
    * {{{
    * StringUtils.prependIfMissingIgnoreCase(null, null, null) = null
    * StringUtils.prependIfMissingIgnoreCase("abc", null, null) = "abc"
    * StringUtils.prependIfMissingIgnoreCase("", "xyz", null) = "xyz"
    * StringUtils.prependIfMissingIgnoreCase("abc", "xyz", new CharSequence[]{null}) = "xyzabc"
    * StringUtils.prependIfMissingIgnoreCase("abc", "xyz", "") = "abc"
    * StringUtils.prependIfMissingIgnoreCase("abc", "xyz", "mno") = "xyzabc"
    * StringUtils.prependIfMissingIgnoreCase("xyzabc", "xyz", "mno") = "xyzabc"
    * StringUtils.prependIfMissingIgnoreCase("mnoabc", "xyz", "mno") = "mnoabc"
    * StringUtils.prependIfMissingIgnoreCase("XYZabc", "xyz", "mno") = "XYZabc"
    * StringUtils.prependIfMissingIgnoreCase("MNOabc", "xyz", "mno") = "MNOabc"
    * }}}
    *
    * @param prefix
    *   The prefix to prepend to the start of the string.
    * @param prefixes
    *   Additional prefixes that are valid (optional).
    * @tparam P
    *   String or Option[String]
    * @tparam Ps
    *   CharSequence or Option[CharSequence]
    * @return
    *   A new String if prefix was prepended, the same string otherwise.
    */
  def prependIfMissingIgnoreCase[P: Adt.CoProducts2[*, String, Option[String]], Ps: Adt.CoProducts2[*, CharSequence, Option[CharSequence]]](
    prefix: P,
    prefixes: Ps*
  ): Option[String] = {
    def prefixStr = mapToStrOpt(prefix).orNull

    def prefixesApplyM = Adt.CoProduct2[CharSequence, Option[CharSequence]].typeOnly[Ps]
    def result: Seq[CharSequence] =
      prefixesApplyM.fold2(_.higherKindApply[Seq](prefixes)).fold1(func => mapToCharSequenceSeq(func.higherKindApply[Seq](prefixes)))

    if (prefixes == null)
      Option(Strings.prependIfMissingIgnoreCase(strOrNull, prefixStr, null))
    else
      Option(Strings.prependIfMissingIgnoreCase(strOrNull, prefixStr, result: _*))
  }

  /** Prepends the prefix to the start of the string if the string does not already start, case insensitive, with any of the prefixes.
    *
    * {{{
    * StringUtils.prependIfMissingIgnoreCase(null, null) = null
    * StringUtils.prependIfMissingIgnoreCase("abc", null) = "abc"
    * StringUtils.prependIfMissingIgnoreCase("", "xyz") = "xyz"
    * StringUtils.prependIfMissingIgnoreCase("abc", "xyz") = "xyzabc"
    * StringUtils.prependIfMissingIgnoreCase("xyzabc", "xyz") = "xyzabc"
    * StringUtils.prependIfMissingIgnoreCase("XYZabc", "xyz") = "XYZabc"
    * }}}
    * <p>With additional prefixes,</p>
    * {{{
    * StringUtils.prependIfMissingIgnoreCase(null, null, null) = null
    * StringUtils.prependIfMissingIgnoreCase("abc", null, null) = "abc"
    * StringUtils.prependIfMissingIgnoreCase("", "xyz", null) = "xyz"
    * StringUtils.prependIfMissingIgnoreCase("abc", "xyz", new CharSequence[]{null}) = "xyzabc"
    * StringUtils.prependIfMissingIgnoreCase("abc", "xyz", "") = "abc"
    * StringUtils.prependIfMissingIgnoreCase("abc", "xyz", "mno") = "xyzabc"
    * StringUtils.prependIfMissingIgnoreCase("xyzabc", "xyz", "mno") = "xyzabc"
    * StringUtils.prependIfMissingIgnoreCase("mnoabc", "xyz", "mno") = "mnoabc"
    * StringUtils.prependIfMissingIgnoreCase("XYZabc", "xyz", "mno") = "XYZabc"
    * StringUtils.prependIfMissingIgnoreCase("MNOabc", "xyz", "mno") = "MNOabc"
    * }}}
    *
    * @param prefix
    *   The prefix to prepend to the start of the string.
    * @return
    *   A new String if prefix was prepended, the same string otherwise.
    */
  def prependIfMissingIgnoreCase(prefix: CharSequence): Option[String] =
    Option(Strings.prependIfMissingIgnoreCase(strOrNull, prefix))

  /** Prepends the prefix to the start of the string if the string does not already start, case insensitive, with any of the prefixes.
    *
    * {{{
    * StringUtils.prependIfMissingIgnoreCase(null, null) = null
    * StringUtils.prependIfMissingIgnoreCase("abc", null) = "abc"
    * StringUtils.prependIfMissingIgnoreCase("", "xyz") = "xyz"
    * StringUtils.prependIfMissingIgnoreCase("abc", "xyz") = "xyzabc"
    * StringUtils.prependIfMissingIgnoreCase("xyzabc", "xyz") = "xyzabc"
    * StringUtils.prependIfMissingIgnoreCase("XYZabc", "xyz") = "XYZabc"
    * }}}
    * <p>With additional prefixes,</p>
    * {{{
    * StringUtils.prependIfMissingIgnoreCase(null, null, null) = null
    * StringUtils.prependIfMissingIgnoreCase("abc", null, null) = "abc"
    * StringUtils.prependIfMissingIgnoreCase("", "xyz", null) = "xyz"
    * StringUtils.prependIfMissingIgnoreCase("abc", "xyz", new CharSequence[]{null}) = "xyzabc"
    * StringUtils.prependIfMissingIgnoreCase("abc", "xyz", "") = "abc"
    * StringUtils.prependIfMissingIgnoreCase("abc", "xyz", "mno") = "xyzabc"
    * StringUtils.prependIfMissingIgnoreCase("xyzabc", "xyz", "mno") = "xyzabc"
    * StringUtils.prependIfMissingIgnoreCase("mnoabc", "xyz", "mno") = "mnoabc"
    * StringUtils.prependIfMissingIgnoreCase("XYZabc", "xyz", "mno") = "XYZabc"
    * StringUtils.prependIfMissingIgnoreCase("MNOabc", "xyz", "mno") = "MNOabc"
    * }}}
    *
    * @param prefix
    *   The prefix to prepend to the start of the string.
    * @return
    *   A new String if prefix was prepended, the same string otherwise.
    */
  def prependIfMissingIgnoreCase(prefix: Option[CharSequence]): Option[String] =
    Option(Strings.prependIfMissingIgnoreCase(strOrNull, prefix.orNull))

  /** <p>Removes all occurrences of a character from within the source string.</p>
    *
    * <p>A `null` source string will return `null`. An empty ("") source string will return the empty string.</p>
    *
    * {{{
    * none.ops.remove(*) = None
    * "".ops.remove(*) = ""
    * "queued".ops.remove('u') = "qeed"
    * "queued".ops.remove('z') = "queued"
    * }}}
    *
    * @param rmv
    *   the char to search for and remove, may be null
    * @return
    *   the substring with the char removed if found, `None` if null String input
    */
  def remove(rmv: Char): Option[String] = Option(Strings.remove(strOrNull, rmv))

  /** <p>Removes all occurrences of a substring from within the source string.</p>
    *
    * <p>A `null` source string will return `null`. An empty ("") source string will return the empty string. A `null` remove string will
    * return the source string. An empty ("") remove string will return the source string.</p>
    *
    * {{{
    * none.ops.remove(*)        = None
    * "".ops.remove(*)          = ""
    * *.ops.remove(null)        = *
    * *.ops.remove("")          = *
    * "queued".ops.remove("ue") = "qd"
    * "queued".ops.remove("zz") = "queued"
    * }}}
    *
    * @param rmv
    *   the String to search for and remove, may be null
    * @tparam R
    *   String or Option[String]
    * @return
    *   the substring with the string removed if found, `None` if null String input
    */
  def remove[R: Adt.CoProducts2[*, String, Option[String]]](rmv: R): Option[String] = {
    val rmvStr = mapToStrOpt(rmv).orNull
    Option(Strings.remove(strOrNull, rmvStr))
  }

  /** <p>Removes a substring only if it is at the end of a source string, otherwise returns the source string.</p>
    *
    * <p>A `null` source string will return `null`. An empty ("") source string will return the empty string. A `null` search string will
    * return the source string.</p>
    *
    * {{{
    * none.ops.removeEnd(*)                    = null
    * "".ops.removeEnd(*)                      = ""
    * *.ops.removeEnd(none)                    = *
    * "www.domain.com".ops.removeEnd(".com.")  = "www.domain.com"
    * "www.domain.com".ops.removeEnd(".com")   = "www.domain"
    * "www.domain.com".ops.removeEnd("domain") = "www.domain.com"
    * "abc".ops.removeEnd("")                  = "abc"
    * }}}
    *
    * @param rmv
    *   the String to search for and remove, may be null
    * @tparam R
    *   String or Option[String]
    * @return
    *   the substring with the string removed if found, `none` if none String input
    */
  def removeEnd[R: Adt.CoProducts2[*, String, Option[String]]](rmv: R): Option[String] = {
    val rmvStr = mapToStrOpt(rmv).orNull
    Option(Strings.removeEnd(strOrNull, rmvStr))
  }

  /** <p>Case insensitive removal of a substring if it is at the end of a source string, otherwise returns the source string.</p>
    *
    * <p>A `null` source string will return `null`. An empty ("") source string will return the empty string. A `null` search string will
    * return the source string.</p>
    *
    * {{{
    * StringUtils.removeEndIgnoreCase(null, *)      = null
    * StringUtils.removeEndIgnoreCase("", *)        = ""
    * StringUtils.removeEndIgnoreCase(*, null)      = *
    * StringUtils.removeEndIgnoreCase("www.domain.com", ".com.")  = "www.domain.com"
    * StringUtils.removeEndIgnoreCase("www.domain.com", ".com")   = "www.domain"
    * StringUtils.removeEndIgnoreCase("www.domain.com", "domain") = "www.domain.com"
    * StringUtils.removeEndIgnoreCase("abc", "")    = "abc"
    * StringUtils.removeEndIgnoreCase("www.domain.com", ".COM") = "www.domain")
    * StringUtils.removeEndIgnoreCase("www.domain.COM", ".com") = "www.domain")
    * }}}
    *
    * @param rmv
    *   the String to search for (case insensitive) and remove, may be null
    * @tparam R
    *   String or Option[String]
    * @return
    *   the substring with the string removed if found, `none` if none String input
    */
  def removeEndIgnoreCase[R: Adt.CoProducts2[*, String, Option[String]]](rmv: R): Option[String] = {
    val rmvStr = mapToStrOpt(rmv).orNull
    Option(Strings.removeEndIgnoreCase(strOrNull, rmvStr))
  }

  /** * <p> Case insensitive removal of all occurrences of a substring from within the source string. </p>
    *
    * <p> A `null` source string will return `null`. An empty ("") source string will return the empty string. A `null` remove string will
    * return the source string. An empty ("") remove string will return the source string. </p>
    *
    * {{{
    * none.ops.removeIgnoreCase(*)        = null
    * "".ops.removeIgnoreCase(*)          = ""
    * *.ops.removeIgnoreCase(null)        = *
    * *.ops.removeIgnoreCase("")          = *
    * "queued".ops.removeIgnoreCase("ue") = "qd"
    * "queued".ops.removeIgnoreCase("zz") = "queued"
    * "quEUed".ops.removeIgnoreCase("UE") = "qd"
    * "queued".ops.removeIgnoreCase("zZ") = "queued"
    * }}}
    *
    * @param rmv
    *   the String to search for (case insensitive) and remove, may be null
    * @tparam R
    *   String or Option[String]
    * @return
    *   the substring with the string removed if found, `none` if none String input
    */
  def removeIgnoreCase[R: Adt.CoProducts2[*, String, Option[String]]](rmv: R): Option[String] = {
    val rmvStr = mapToStrOpt(rmv).orNull
    Option(Strings.removeIgnoreCase(strOrNull, rmvStr))
  }

  /** * <p>Removes a substring only if it is at the beginning of a source string, otherwise returns the source string.</p>
    *
    * <p>A `null` source string will return `null`. An empty ("") source string will return the empty string. A `null` search string will
    * return the source string.</p>
    *
    * {{{
    * none.ops.removeStart(*)      = null
    * "".ops.removeStart(*)        = ""
    * *.ops.removeStart(null)      = *
    * "www.domain.com".ops.removeStart("www.")   = "domain.com"
    * "domain.com".ops.removeStart("www.")       = "domain.com"
    * "www.domain.com".ops.removeStart("domain") = "www.domain.com"
    * "abc".ops.removeStart("")    = "abc"
    * }}}
    *
    * @param rmv
    *   the String to search for and remove, may be null
    * @tparam R
    *   String or Option[String]
    * @return
    *   the substring with the string removed if found, `none` if none String input
    */
  def removeStart[R: Adt.CoProducts2[*, String, Option[String]]](rmv: R): Option[String] = {
    val rmvStr = mapToStrOpt(rmv).orNull
    Option(Strings.removeStart(strOrNull, rmvStr))
  }

  /** <p>Case insensitive removal of a substring if it is at the beginning of a source string, otherwise returns the source string.</p>
    *
    * <p>A `null` source string will return `null`. An empty ("") source string will return the empty string. A `null` search string will
    * return the source string.</p>
    *
    * {{{
    * none.ops.removeStartIgnoreCase(*)      = None
    * "".ops.removeStartIgnoreCase(*)        = ""
    * *.ops.removeStartIgnoreCase(null)      = *
    * "www.domain.com".ops.removeStartIgnoreCase("www.")   = "domain.com"
    * "www.domain.com".ops.removeStartIgnoreCase("WWW.")   = "domain.com"
    * "domain.com".ops.removeStartIgnoreCase("www.")       = "domain.com"
    * "www.domain.com".ops.removeStartIgnoreCase("domain") = "www.domain.com"
    * "abc".ops.removeStartIgnoreCase("")    = "abc"
    * }}}
    *
    * @param rmv
    *   the String to search for (case insensitive) and remove, may be null
    * @tparam R
    *   String or Option[String]
    * @return
    *   the substring with the string removed if found, `none` if none String input
    */
  def removeStartIgnoreCase[R: Adt.CoProducts2[*, String, Option[String]]](rmv: R): Option[String] = {
    val rmvStr = mapToStrOpt(rmv).orNull
    Option(Strings.removeStartIgnoreCase(strOrNull, rmvStr))
  }

  /** <p>Repeat a String `repeat` times to form a new String.</p>
    *
    * {{{
    * none.ops.repeat(2) = null
    * "".ops.repeat(0)   = ""
    * "".ops.repeat(2)   = ""
    * "a".ops.repeat(3)  = "aaa"
    * "ab".ops.repeat(2) = "abab"
    * "a".ops.repeat(-2) = ""
    * }}}
    *
    * @param rep
    *   number of times to repeat str, negative treated as zero
    * @return
    *   a new String consisting of the original String repeated, `none` if none String input
    */
  def repeat(rep: Int): Option[String] = Option(Strings.repeat(strOrNull, rep))

  /** <p>Repeat a String `repeat` times to form a new String, with a String separator injected each time. </p>
    *
    * {{{
    * none.ops.repeat(none, 2) = None
    * none.ops.repeat("x", 2) = None
    * "".ops.repeat(none, 0) = ""
    * "".ops.repeat("", 2) = ""
    * "".ops.repeat("x", 3) = "xxx"
    * "?".ops.repeat(", ", 3) = "?, ?, ?"
    * }}}
    *
    * @param separator
    *   the String to inject, may be null
    * @param repeat
    *   number of times to repeat str, negative treated as zero
    * @tparam S
    *   String or Option[String]
    * @return
    *   a new String consisting of the original String repeated, `none` if none String input
    */
  def repeat[S: Adt.CoProducts2[*, String, Option[String]]](separator: S, repeat: Int): Option[String] = {
    val sep = mapToStrOpt(separator).orNull
    Option(Strings.repeat(strOrNull, sep, repeat))
  }

  /** <p>Replaces all occurrences of a String within another String.</p>
    *
    * <p>A `null` reference passed to this method is a no-op.</p>
    *
    * {{{
    * none.ops.replace(*, *) = None
    * "".ops.replace*, *) = ""
    * "any".ops.replace, *) = "any"
    * "any".ops.replace(*, null) = "any"
    * "any".ops.replace("", *) = "any"
    * "aba".ops.replace("a", null) = "aba"
    * "aba".ops.replace("a", "") = "b"
    * "aba".ops.replace("a", "z") = "zbz"
    * }}}
    *
    * @param searchString
    *   the String to search for, may be null
    * @param replacement
    *   the String to replace it with, may be null
    * @tparam S
    *   String or Option[String]
    * @tparam R
    *   String or Option[String]
    * @return
    *   the text with any replacements processed,
    */
  def replace[S: Adt.CoProducts2[*, String, Option[String]], R: Adt.CoProducts2[*, String, Option[String]]](
    searchString: S,
    replacement: R
  ): Option[String] = {
    val sstr = mapToStrOpt(searchString).orNull
    val rstr = mapToStrOpt(replacement).orNull

    Option(Strings.replace(strOrNull, sstr, rstr))
  }

  /** <p>Replaces a String with another String inside a larger String, for the first `max` values of the search String.</p>
    *
    * <p>A `null` reference passed to this method is a no-op.</p>
    *
    * {{{
    * none.ops.replace(*, *, *) = None
    * "".ops.replace(*, *, *) = ""
    * "any".ops.replace(none, *, *) = "any"
    * "any".ops.replace(*, null, *) = "any"
    * "any".ops.replace("", *, *) = "any"
    * "any".ops.replace(*, *, 0) = "any"
    * "abaa".ops.replace("a", null, -1) = "abaa"
    * "abaa".ops.replace("a", "", -1) = "b"
    * "abaa".ops.replace("a", "z", 0) = "abaa"
    * "abaa".ops.replace("a", "z", 1) = "zbaa"
    * "abaa".ops.replace("a", "z", 2) = "zbza"
    * "abaa".ops.replace("a", "z", -1) = "zbzz"
    * }}}
    *
    * @param searchString
    *   the String to search for, may be null
    * @param replacement
    *   the String to replace it with, may be null
    * @param max
    *   maximum number of values to replace, or `-1` if no maximum
    * @tparam S
    *   String or Option[String]
    * @tparam R
    *   String or Option[String]
    * @return
    *   the text with any replacements processed, `none` if none String input
    */
  def replace[S: Adt.CoProducts2[*, String, Option[String]], R: Adt.CoProducts2[*, String, Option[String]]](
    searchString: S,
    replacement: R,
    max: Int
  ): Option[String] = {
    val sstr = mapToStrOpt(searchString).orNull
    val rstr = mapToStrOpt(replacement).orNull

    Option(Strings.replace(strOrNull, sstr, rstr, max))
  }

  /** <p>Replaces all occurrences of a character in a String with another. This is a null-safe version of
    * [[String# replace ( char,char)]].</p>
    *
    * <p>A `null` string input returns `null`. An empty ("") string input returns an empty string.</p>
    *
    * {{{
    * none.ops.replaceChar(*, *) = None
    * "".ops.replaceChar(*, *) = ""
    * "abcba".ops.replaceChar('b', 'y') = "aycya"
    * "abcba".ops.replaceChar('z', 'y') = "abcba"
    * }}}
    *
    * @param searchChar
    *   the character to search for, may be null
    * @param replaceChar
    *   the character to replace, may be null
    * @return
    *   modified String, `null` if null string input
    */
  def replaceChars(searchChar: Char, replaceChar: Char): Option[String] =
    Option(Strings.replaceChars(strOrNull, searchChar, replaceChar))

  /** <p>Replaces multiple characters in a String in one go. This method can also be used to delete characters.</p>
    *
    * <p>For example:<br> `replaceChars(&quot;hello&quot;, &quot;ho&quot;, &quot;jy&quot;) = jelly`.</p>
    *
    * <p>A `null` string input returns `null`. An empty ("") string input returns an empty string. A null or empty set of search characters
    * returns the input string.</p>
    *
    * <p>The length of the search characters should normally equal the length of the replace characters. If the search characters is longer,
    * then the extra search characters are deleted. If the search characters is shorter, then the extra replace characters are ignored.</p>
    *
    * {{{
    * none.ops.replaceChars(*, *)           = null
    * "".ops.replaceChars(*, *)             = ""
    * "abc".ops.replaceChars(null, *)       = "abc"
    * "abc".ops.replaceChars("", *)         = "abc"
    * "abc".ops.replaceChars("b", null)     = "ac"
    * "abc".ops.replaceChars("b", "")       = "ac"
    * "abcba".ops.replaceChars("bc", "yz")  = "ayzya"
    * "abcba".ops.replaceChars("bc", "y")   = "ayya"
    * "abcba".ops.replaceChars("bc", "yzx") = "ayzya"
    * }}}
    *
    * @param searchChars
    *   a set of characters to search for, may be null
    * @param replaceChars
    *   a set of characters to replace, may be null
    * @tparam S
    *   String or Option[String]
    * @tparam R
    *   String or Option[String]
    * @return
    *   modified String, `null` if null string input
    */
  def replaceChars[S: Adt.CoProducts2[*, String, Option[String]], R: Adt.CoProducts2[*, String, Option[String]]](
    searchChars: S,
    replaceChars: R
  ): Option[String] = {
    val sstr = mapToStrOpt(searchChars).orNull
    val rstr = mapToStrOpt(replaceChars).orNull

    Option(Strings.replaceChars(strOrNull, sstr, rstr))
  }

  /** <p> Replaces all occurrences of Strings within another String. </p>
    *
    * <p> A `null` reference passed to this method is a no-op, or if any "search string" or "string to replace" is null, that replace will
    * be ignored. This will not repeat. For repeating replaces, call the overloaded method. </p>
    *
    * {{{
    * none.ops.replaceEach(*, *)        = null
    * "".ops.replaceEach(*, *)          = ""
    * "aba".ops.replaceEach(null, null) = "aba"
    * "aba".ops.replaceEach(new String[0], null) = "aba"
    * "aba".ops.replaceEach(null, new String[0]) = "aba"
    * "aba".ops.replaceEach(new String[]{"a"}, null)  = "aba"
    * "aba".ops.replaceEach(new String[]{"a"}, new String[]{""})  = "b"
    * "aba".ops.replaceEach(new String[]{null}, new String[]{"a"})  = "aba"
    * "abcde".ops.replaceEach(new String[]{"ab", "d"}, new String[]{"w", "t"})  = "wcte"
    * (example of how it does not repeat)
    * StringUtils.replaceEach("abcde", new String[]{"ab", "d"}, new String[]{"d", "t"})  = "dcte"
    * }}}
    *
    * @param searchList
    *   the Strings to search for, no-op if null
    * @param replacementList
    *   the Strings to replace them with, no-op if null
    * @return
    *   the text with any replacements processed, `none` if none String input
    * @throws IllegalArgumentException
    *   if the lengths of the arrays are not the same (null is ok, and/or size 0)
    */
  def replaceEach(searchList: Array[String], replacementList: Array[String]): Option[String] =
    Option(Strings.replaceEach(strOrNull, searchList, replacementList))

  /** * <p> Replaces all occurrences of Strings within another String. </p>
    *
    * <p> A `null` reference passed to this method is a no-op, or if any "search string" or "string to replace" is null, that replace will
    * be ignored. </p>
    *
    * {{{
    * none.ops.replaceEachRepeatedly(*, *)                                                 = null
    * "".ops.replaceEachRepeatedly(*, *)                                                   = ""
    * "aba".ops.replaceEachRepeatedly(null, null)                                          = "aba"
    * "aba".ops.replaceEachRepeatedly(new String[0], null)                                 = "aba"
    * "aba".ops.replaceEachRepeatedly(null, new String[0])                                 = "aba"
    * "aba".ops.replaceEachRepeatedly(new String[]{"a"}, null)                             = "aba"
    * "aba".ops.replaceEachRepeatedly(new String[]{"a"}, new String[]{""})                 = "b"
    * "aba".ops.replaceEachRepeatedly(new String[]{null}, new String[]{"a"})               = "aba"
    * "abcde".ops.replaceEachRepeatedly(new String[]{"ab", "d"}, new String[]{"w", "t"})   = "wcte"
    *  // (example of how it repeats)
    *  "abcde".ops.replaceEachRepeatedly(new String[]{"ab", "d"}, new String[]{"d", "t"})  = "tcte"
    *  "abcde".ops.replaceEachRepeatedly(new String[]{"ab", "d"}, new String[]{"d", "ab"}) = IllegalStateException
    * }}}
    *
    * @param searchList
    *   the Strings to search for, no-op if null
    * @param replacementList
    *   the Strings to replace them with, no-op if null
    * @return
    *   the text with any replacements processed, `none` if none String input
    */
  def replaceEachRepeatedly(searchList: Array[String], replacementList: Array[String]): Option[String] =
    Option(Strings.replaceEachRepeatedly(strOrNull, searchList, replacementList))

  /** <p>Case insensitively replaces a String with another String inside a larger String, for the first `max` values of the search
    * String.</p>
    *
    * <p>A `null` reference passed to this method is a no-op.</p>
    *
    * {{{
    * none.ops.replaceIgnoreCase(*, *, *)         = null
    * "".ops.replaceIgnoreCase(*, *, *)           = ""
    * "any".ops.replaceIgnoreCase(null, *, *)     = "any"
    * "any".ops.replaceIgnoreCase(*, null, *)     = "any"
    * "any".ops.replaceIgnoreCase("", *, *)       = "any"
    * "any".ops.replaceIgnoreCase(*, *, 0)        = "any"
    * "abaa".ops.replaceIgnoreCase("a", null, -1) = "abaa"
    * "abaa".ops.replaceIgnoreCase("a", "", -1)   = "b"
    * "abaa".ops.replaceIgnoreCase("a", "z", 0)   = "abaa"
    * "abaa".ops.replaceIgnoreCase("A", "z", 1)   = "zbaa"
    * "abAa".ops.replaceIgnoreCase("a", "z", 2)   = "zbza"
    * "abAa".ops.replaceIgnoreCase("a", "z", -1)  = "zbzz"
    * }}}
    *
    * @param searchString
    *   the String to search for (case insensitive), may be null
    * @param replacement
    *   the String to replace it with, may be null
    * @tparam S
    *   String or Option[String]
    * @tparam R
    *   String or Option[String]
    * @return
    *   the text with any replacements processed, `none` if none String input
    */
  def replaceIgnoreCase[S: Adt.CoProducts2[*, String, Option[String]], R: Adt.CoProducts2[*, String, Option[String]]](
    searchString: S,
    replacement: R
  ): Option[String] = {
    val sstr = mapToStrOpt(searchString).orNull
    val rstr = mapToStrOpt(replacement).orNull

    Option(Strings.replaceIgnoreCase(strOrNull, sstr, rstr))
  }

  /** <p>Case insensitively replaces a String with another String inside a larger String, for the first `max` values of the search
    * String.</p>
    *
    * <p>A `null` reference passed to this method is a no-op.</p>
    *
    * {{{
    * none.ops.replaceIgnoreCase(*, *, *) = None
    * "".ops.replaceIgnoreCase(*, *, *) = ""
    * "any".ops.replaceIgnoreCase(null, *, *) ="any"
    * "any".ops.replaceIgnoreCase(*, null, *) = "any"
    * "any".ops.replaceIgnoreCase("", *, *) = "any"
    * "any".ops.replaceIgnoreCase(*, *, 0) = "any"
    * "abaa".ops.replaceIgnoreCase("a", null, -1) = "abaa"
    * "abaa".ops.replaceIgnoreCase("a", "", -1) = "b"
    * "abaa".ops.replaceIgnoreCase("a", "z", 0) = "abaa"
    * "abaa".ops.replaceIgnoreCase("A", "z", 1) = "zbaa"
    * "abAa".ops.replaceIgnoreCase("a", "z", 2) = "zbza"
    * "abAa".ops.replaceIgnoreCase("a", "z", -1) = "zbzz"
    * }}}
    * @param searchString
    *   the String to search for, may be null
    * @param replacement
    *   the String to replace with, may be null
    * @param max
    *   maximum number of values to replace, or `-1` if no maximum
    * @tparam S
    *   String or Option[String]
    * @tparam R
    *   String or Option[String]
    * @return
    *   the text with any replacements processed, `none` if none String input
    */
  def replaceIgnoreCase[S: Adt.CoProducts2[*, String, Option[String]], R: Adt.CoProducts2[*, String, Option[String]]](
    searchString: S,
    replacement: R,
    max: Int
  ): Option[String] = {
    val sstr = mapToStrOpt(searchString).orNull
    val rstr = mapToStrOpt(replacement).orNull

    Option(Strings.replaceIgnoreCase(strOrNull, sstr, rstr, max))
  }

  /** <p>Replaces a String with another String inside a larger String, once.</p>
    *
    * <p>A `null` reference passed to this method is a no-op.</p>
    *
    * {{{
    * none.ops.replaceOnce(*, *)        = null
    * "".ops.replaceOnce(*, *)          = ""
    * "any".ops.replaceOnce(null, *)    = "any"
    * "any".ops.replaceOnce(*, null)    = "any"
    * "any".ops.replaceOnce("", *)      = "any"
    * "aba".ops.replaceOnce("a", null)  = "aba"
    * "aba".ops.replaceOnce("a", "")    = "ba"
    * "aba".ops.replaceOnce("a", "z")   = "zba"
    * }}}
    *
    * @param searchString
    *   the String to search for, may be null
    * @param replacement
    *   the String to replace with, may be null
    * @tparam S
    *   String or Option[String]
    * @tparam R
    *   String or Option[String]
    * @return
    *   the text with any replacements processed, `none` if none String input
    */
  def replaceOnce[S: Adt.CoProducts2[*, String, Option[String]], R: Adt.CoProducts2[*, String, Option[String]]](
    searchString: S,
    replacement: R
  ): Option[String] = {
    val sstr = mapToStrOpt(searchString).orNull
    val rstr = mapToStrOpt(replacement).orNull

    Option(Strings.replaceOnce(strOrNull, sstr, rstr))
  }

  /** <p>Case insensitively replaces a String with another String inside a larger String, once.</p>
    *
    * <p>A `null` reference passed to this method is a no-op.</p>
    *
    * {{{
    * none.ops.replaceOnceIgnoreCase(*, *)        = null
    * "".ops.replaceOnceIgnoreCase(*, *)          = ""
    * "any".ops.replaceOnceIgnoreCase(null, *)    = "any"
    * "any".ops.replaceOnceIgnoreCase(*, null)    = "any"
    * "any".ops.replaceOnceIgnoreCase("", *)      = "any"
    * "aba".ops.replaceOnceIgnoreCase("a", null)  = "aba"
    * "aba".ops.replaceOnceIgnoreCase("a", "")    = "ba"
    * "aba".ops.replaceOnceIgnoreCase("a", "z")   = "zba"
    * "FoOFoofoo".ops.replaceOnceIgnoreCase("foo", "") = "Foofoo"
    * }}}
    *
    * @param searchString
    *   the String to search for (case insensitive), may be null
    * @param replacement
    *   the String to replace with, may be null
    * @tparam S
    *   String or Option[String]
    * @tparam R
    *   String or Option[String]
    * @return
    *   the text with any replacements processed, `none` if none String input
    */
  def replaceOnceIgnoreCase[S: Adt.CoProducts2[*, String, Option[String]], R: Adt.CoProducts2[*, String, Option[String]]](
    searchString: S,
    replacement: R
  ): Option[String] = {
    val sstr = mapToStrOpt(searchString).orNull
    val rstr = mapToStrOpt(replacement).orNull

    Option(Strings.replaceOnceIgnoreCase(strOrNull, sstr, rstr))
  }

  /** <p>Reverses a String as per [[StringBuilder# reverse ( )]].</p>
    *
    * <p>A `null` String returns `null`.</p>
    *
    * {{{
    * none.ops.reverse  = None
    * "".ops.reverse    = ""
    * "bat".ops.reverse = "tab"
    * }}}
    *
    * @return
    *   the reversed String, `none` if none String input
    */
  def reverse: Option[String] = Option(Strings.reverse(strOrNull))

  /** <p>Reverses a String that is delimited by a specific character.</p>
    *
    * <p>The Strings between the delimiters are not reversed. Thus java.lang.String becomes String.lang.java (if the delimiter is
    * `'.'`).</p>
    *
    * {{{
    * none.ops.reverseDelimited(*)      = null
    * "".ops.reverseDelimited(*)        = ""
    * "a.b.c", 'x') = "a.b.c"
    * "a.b.c", ".") = "c.b.a"
    * }}}
    *
    * @param separatorChar
    *   the separator character to use
    * @return
    *   the reversed String, `none` if none String input
    */
  def reverseDelimited(separatorChar: Char): Option[String] =
    Option(Strings.reverseDelimited(strOrNull, separatorChar))

  /** <p>Gets the rightmost `len` characters of a String.</p>
    *
    * <p>If `len` characters are not available, or the String is `null`, the String will be returned without an an exception. An empty
    * String is returned if len is negative.</p>
    *
    * {{{
    * none.ops.right(*)    = null
    * *.ops.right(-ve)     = ""
    * "".ops.right(*)      = ""
    * "abc".ops.right(0)   = ""
    * "abc".ops.right(2)   = "bc"
    * "abc".ops.right(4)   = "abc"
    * }}}
    *
    * @param len
    *   the length of the required String
    * @return
    *   the rightmost characters, `none` if none String input
    */
  def right(len: Int): Option[String] = Option(Strings.right(strOrNull, len))

  /** <p>Right pad a String with spaces (' ').</p>
    *
    * <p>The String is padded to the size of `size`.</p>
    *
    * {{{
    * none.ops.rightPad(*)   = null
    * "".ops.rightPad(3)     = "   "
    * "bat".ops.rightPad(3)  = "bat"
    * "bat".ops.rightPad(5)  = "bat  "
    * "bat".ops.rightPad(1)  = "bat"
    * "bat".ops.rightPad(-1) = "bat"
    * }}}
    *
    * @param size
    *   the size to pad to
    * @return
    *   right padded String or original String if no padding is necessary, `none` if none String input
    */
  def rightPad(size: Int): Option[String] = Option(Strings.rightPad(strOrNull, size))

  /** * <p>Right pad a String with a specified character.</p>
    *
    * <p>The String is padded to the size of `size`.</p>
    *
    * {{{
    * none.ops.rightPad(*, *)     = null
    * "".ops.rightPad(3, 'z')     = "zzz"
    * "bat".ops.rightPad(3, 'z')  = "bat"
    * "bat".ops.rightPad(5, 'z')  = "batzz"
    * "bat".ops.rightPad(1, 'z')  = "bat"
    * "bat".ops.rightPad(-1, 'z') = "bat"
    * }}}
    *
    * @param size
    *   the size to pad to
    * @param padChar
    *   the character to pad with
    * @return
    *   right padded String or original String if no padding is necessary, `none` if none String input
    */
  def rightPad(size: Int, padChar: Char): Option[String] = Option(Strings.rightPad(strOrNull, size, padChar))

  /** <p>Right pad a String with a specified String.</p>
    *
    * <p>The String is padded to the size of `size`.</p>
    *
    * {{{
    * none.ops.rightPad(*, *) = None
    * "".ops.rightPad(3, "z") = "zzz"
    * "bat".ops.rightPad(3, "yz") = "bat"
    * "bat".ops.rightPad(5, "yz") = "batyz"
    * "bat".ops.rightPad(8, "yz") = "batyzyzy"
    * "bat".ops.rightPad(1, "yz") = "bat"
    * "bat".ops.rightPad(-1, "yz") = "bat"
    * "bat".ops.rightPad(5, null) = "bat "
    * "bat".ops.rightPad(5, "") = "bat "
    * }}}
    *
    * @param size
    *   the size to pad to
    * @param padStr
    *   the String to pad with, null or empty treated as single spacethe String to pad with, null or empty treated as single space
    * @tparam P
    *   String or Option[String]
    * @return
    *   right padded String or original String if no padding is necessary, `none` if none String input
    */
  def rightPad[P: Adt.CoProducts2[*, String, Option[String]]](size: Int, padStr: P): Option[String] = {
    val ps = mapToStrOpt(padStr).orNull
    Option(Strings.rightPad(strOrNull, size, ps))
  }

  /** <p>Rotate (circular shift) a String of `shift` characters.</p>
    *
    *   - If `shift > 0`, right circular shift (ex : ABCDEF \=&gt; FABCDE)
    *   - If `shift < 0`, left circular shift (ex : ABCDEF =&gt; BCDEFA)
    *
    * {{{
    * none.ops.rotate(*)        = null
    * "".ops.rotate(*)          = ""
    * "abcdefg".ops.rotate(0)   = "abcdefg"
    * "abcdefg".ops.rotate(2)   = "fgabcde"
    * "abcdefg".ops.rotate(-2)  = "cdefgab"
    * "abcdefg".ops.rotate(7)   = "abcdefg"
    * "abcdefg".ops.rotate(-7)  = "abcdefg"
    * "abcdefg".ops.rotate(9)   = "fgabcde"
    * "abcdefg".ops.rotate(-9)  = "cdefgab"
    * }}}
    *
    * @param shift
    *   number of time to shift (positive : right shift, negative : left shift)
    * @return
    *   the rotated String, or the original String if `shift == 0`, or `none` if none String input
    */
  def rotate(shift: Int): Option[String] = Option(Strings.rotate(strOrNull, shift))

  /** Splits the provided text into an array, using whitespace as the separator. Whitespace is defined by `Character# isWhitespace(char)`.
    *
    * <p>The separator is not included in the returned String array. Adjacent separators are treated as one separator. For more control over
    * the split use the StrTokenizer class.</p>
    *
    * <p>A `none` input String returns `None`.</p>
    *
    * {{{
    * none.ops.split       = null
    * "".ops.split         = []
    * "abc def".ops.split  = ["abc", "def"]
    * "abc  def".ops.split = ["abc", "def"]
    * " abc ".ops.split    = ["abc"]
    * }}}
    *
    * @return
    *   an array of parsed Strings, `none` if none String input
    */
  def split: Option[Array[String]] = Option(Strings.split(strOrNull))

  /** <p>Splits the provided text into an array, separators specified. This is an alternative to using StringTokenizer.</p>
    *
    * <p>The separator is not included in the returned String array. Adjacent separators are treated as one separator. For more control over
    * the split use the StrTokenizer class.</p>
    *
    * <p>A `null` input String returns `null`. A `null` separatorChars splits on whitespace.</p>
    *
    * {{{
    * none.ops.split(*) = None
    * "".ops.split(*) = []
    * "abc def".ops.split(none) = ["abc", "def"]
    * "abc def".ops.split(" ") = ["abc", "def"]
    * "abc def".ops.split(" ") = ["abc", "def"]
    * "ab:cd:ef".ops.split(":") = ["ab", "cd", "ef"]
    * }}}
    *
    * @param separatorChar
    *   the characters used as the delimiters, `null` splits on whitespace
    * @return
    */
  def split(separatorChar: Char): Option[Array[String]] = Option(Strings.split(strOrNull, separatorChar))

  /** <p>Splits the provided text into an array, separators specified. This is an alternative to using StringTokenizer.</p>
    *
    * <p>The separator is not included in the returned String array. Adjacent separators are treated as one separator. For more control over
    * the split use the StrTokenizer class.</p>
    *
    * <p>A `null` input String returns `null`. A `null` separatorChars splits on whitespace.</p>
    *
    * {{{
    * none.ops.split(*)         = null
    * "".ops.split(*)           = []
    * "abc def".ops.split(null) = ["abc", "def"]
    * "abc def".ops.split(" ")  = ["abc", "def"]
    * "abc  def".ops.split(" ") = ["abc", "def"]
    * "ab:cd:ef".ops.split(":") = ["ab", "cd", "ef"]
    * }}}
    *
    * @param separatorChars
    *   the characters used as the delimiters, `null` splits on whitespace
    * @tparam S
    *   String or Option[String]
    * @return
    *   an array of parsed Strings, `none` if none String input
    */
  def split[S: Adt.CoProducts2[*, String, Option[String]]](separatorChars: S): Option[Array[String]] = {
    val sep = mapToStrOpt(separatorChars).orNull
    Option(Strings.split(strOrNull, sep))
  }

  /** <p>Splits the provided text into an array with a maximum length, separators specified.</p>
    *
    * <p>The separator is not included in the returned String array. Adjacent separators are treated as one separator.</p>
    *
    * <p>A `null` input String returns `null`. A `null` separatorChars splits on whitespace.</p>
    *
    * <p>If more than `max` delimited substrings are found, the last returned string includes all characters after the first {@code max - 1}
    * returned strings (including separator characters).</p>
    *
    * {{{
    * none.ops.split(*, *)            = null
    * "".ops.split(*, *)              = []
    * "ab cd ef".ops.split(null, 0)   = ["ab", "cd", "ef"]
    * "ab   cd ef".ops.split(null, 0) = ["ab", "cd", "ef"]
    * "ab:cd:ef".ops.split(":", 0)    = ["ab", "cd", "ef"]
    * "ab:cd:ef".ops.split(":", 2)    = ["ab", "cd:ef"]
    * }}}
    *
    * @param separatorChars
    *   characters used as the delimiters, `none` splits on whitespace
    * @param max
    *   the maximum number of elements to include in the array. A zero or negative value implies no limit
    * @tparam S
    *   String or Option[String]
    * @return
    *   an array of parsed Strings, `none` if none String input
    */
  def split[S: Adt.CoProducts2[*, String, Option[String]]](separatorChars: S, max: Int): Option[Array[String]] = {
    val sep = mapToStrOpt(separatorChars).orNull
    Option(Strings.split(strOrNull, sep, max))
  }

  /** <p>Splits a String by Character type as returned by `java.lang.Character.getType(char)`. Groups of contiguous characters of the same
    * type are returned as complete tokens.
    * {{{
    * none.ops.splitByCharacterType         = null
    * "".ops.splitByCharacterType           = []
    * "ab de fg".ops.splitByCharacterType   = ["ab", " ", "de", " ", "fg"]
    * "ab   de fg".ops.splitByCharacterType = ["ab", "   ", "de", " ", "fg"]
    * "ab:cd:ef".ops.splitByCharacterType   = ["ab", ":", "cd", ":", "ef"]
    * "number5".ops.splitByCharacterType    = ["number", "5"]
    * "fooBar".ops.splitByCharacterType     = ["foo", "B", "ar"]
    * "foo200Bar".ops.splitByCharacterType  = ["foo", "200", "B", "ar"]
    * "ASFRules".ops.splitByCharacterType   = ["ASFR", "ules"]
    * }}}
    *
    * @return
    *   an array of parsed Strings, `none` if none String input
    */
  def splitByCharacterType: Option[Array[String]] = Option(Strings.splitByCharacterType(strOrNull))

  /** * <p>Splits a String by Character type as returned by `java.lang.Character.getType(char)`. Groups of contiguous characters of the same
    * type are returned as complete tokens, with the following exception: the character of type `Character.UPPERCASE_LETTER`, if any,
    * immediately preceding a token of type `Character.LOWERCASE_LETTER` will belong to the following token rather than to the preceding, if
    * any, `Character.UPPERCASE_LETTER` token.
    * {{{
    * none.ops.splitByCharacterTypeCamelCase         = null
    * "".ops.splitByCharacterTypeCamelCase           = []
    * "ab de fg".ops.splitByCharacterTypeCamelCase   = ["ab", " ", "de", " ", "fg"]
    * "ab   de fg".ops.splitByCharacterTypeCamelCase = ["ab", "   ", "de", " ", "fg"]
    * "ab:cd:ef".ops.splitByCharacterTypeCamelCase   = ["ab", ":", "cd", ":", "ef"]
    * "number5".ops.splitByCharacterTypeCamelCase    = ["number", "5"]
    * "fooBar".ops.splitByCharacterTypeCamelCase     = ["foo", "Bar"]
    * "foo200Bar".ops.splitByCharacterTypeCamelCase  = ["foo", "200", "Bar"]
    * "ASFRules".ops.splitByCharacterTypeCamelCase   = ["ASF", "Rules"]
    * }}}
    *
    * @return
    *   an array of parsed Strings, `none` if none String input
    */
  def splitByCharacterTypeCamelCase: Option[Array[String]] =
    Option(Strings.splitByCharacterTypeCamelCase(strOrNull))

  /** <p>Splits the provided text into an array, separator string specified. Returns a maximum of `max` substrings.</p>
    *
    * <p>The separator(s) will not be included in the returned String array. Adjacent separators are treated as one separator.</p>
    *
    * <p>A `null` input String returns `null`. A `null` separator splits on whitespace.</p>
    *
    * {{{
    * StringUtils.splitByWholeSeparator(null, *, *)               = null
    * StringUtils.splitByWholeSeparator("", *, *)                 = []
    * StringUtils.splitByWholeSeparator("ab de fg", null, 0)      = ["ab", "de", "fg"]
    * StringUtils.splitByWholeSeparator("ab   de fg", null, 0)    = ["ab", "de", "fg"]
    * StringUtils.splitByWholeSeparator("ab:cd:ef", ":", 2)       = ["ab", "cd:ef"]
    * StringUtils.splitByWholeSeparator("ab-!-cd-!-ef", "-!-", 5) = ["ab", "cd", "ef"]
    * StringUtils.splitByWholeSeparator("ab-!-cd-!-ef", "-!-", 2) = ["ab", "cd-!-ef"]
    * }}}
    *
    * @param separatorChars
    *   String containing the String to be used as a delimiter, `null` splits on whitespace
    * @tparam S
    *   String or Option[String]
    * @return
    *   an array of parsed Strings, `null` if null String was input
    */
  def splitByWholeSeparator[S: Adt.CoProducts2[*, String, Option[String]]](separatorChars: S): Option[Array[String]] = {
    val sep = mapToStrOpt(separatorChars).orNull
    Option(Strings.splitByWholeSeparator(strOrNull, sep))
  }

  /** <p>Splits the provided text into an array, separator string specified. Returns a maximum of `max` substrings.</p>
    *
    * <p>The separator(s) will not be included in the returned String array. Adjacent separators are treated as one separator.</p>
    *
    * <p>A `null` input String returns `null`. A `null` separator splits on whitespace.</p>
    *
    * {{{
    * null.ops.splitByWholeSeparator(*, *)               = null
    * "".ops.splitByWholeSeparator(*, *)                 = []
    * "ab de fg".ops.splitByWholeSeparator(none, 0)      = ["ab", "de", "fg"]
    * "ab   de fg".ops.splitByWholeSeparator(null, 0)    = ["ab", "de", "fg"]
    * "ab:cd:ef".ops.splitByWholeSeparator(":", 2)       = ["ab", "cd:ef"]
    * "ab-!-cd-!-ef".ops.splitByWholeSeparator("-!-", 5) = ["ab", "cd", "ef"]
    * "ab-!-cd-!-ef".ops.splitByWholeSeparator("-!-", 2) = ["ab", "cd-!-ef"]
    * }}}
    *
    * @param separatorChars
    *   String containing the String to be used as a delimiter, `null` splits on whitespace
    * @param max
    *   the maximum number of elements to include in the returned array. A zero or negative value implies no limit.
    * @tparam S
    *   String or Option[String]
    * @return
    *   an array of parsed Strings, `null` if null String was input
    */
  def splitByWholeSeparator[S: Adt.CoProducts2[*, String, Option[String]]](separatorChars: S, max: Int): Option[Array[String]] = {
    val sep = mapToStrOpt(separatorChars).orNull
    Option(Strings.splitByWholeSeparator(strOrNull, sep, max))
  }

  /** <p>Splits the provided text into an array, separator string specified. </p>
    *
    * <p>The separator is not included in the returned String array. Adjacent separators are treated as separators for empty tokens. For
    * more control over the split use the StrTokenizer class.</p>
    *
    * <p>A `null` input String returns `null`. A `null` separator splits on whitespace.</p>
    *
    * {{{
    * none.ops.splitByWholeSeparatorPreserveAllTokens(*)               = null
    * "".ops.splitByWholeSeparatorPreserveAllTokens(*)                 = []
    * "ab de fg".ops.splitByWholeSeparatorPreserveAllTokens(null)      = ["ab", "de", "fg"]
    * "ab   de fg".ops.splitByWholeSeparatorPreserveAllTokens(null)    = ["ab", "", "", "de", "fg"]
    * "ab:cd:ef".ops.splitByWholeSeparatorPreserveAllTokens(":")       = ["ab", "cd", "ef"]
    * "ab-!-cd-!-ef".ops.splitByWholeSeparatorPreserveAllTokens("-!-") = ["ab", "cd", "ef"]
    * }}}
    *
    * @param separatorChars
    *   the characters used as the delimiters, `none` splits on whitespace
    * @tparam S
    *   String containing the String to be used as a delimiter, `null` splits on whitespace
    * @return
    *   an array of parsed Strings, `null` if null String was input
    */
  def splitByWholeSeparatorPreserveAllTokens[S: Adt.CoProducts2[*, String, Option[String]]](separatorChars: S): Option[Array[String]] = {
    val sep = mapToStrOpt(separatorChars).orNull
    Option(Strings.splitByWholeSeparatorPreserveAllTokens(strOrNull, sep))
  }

  /** <p>Splits the provided text into an array, separator string specified. Returns a maximum of `max` substrings.</p>
    *
    * <p>The separator is not included in the returned String array. Adjacent separators are treated as separators for empty tokens. For
    * more control over the split use the StrTokenizer class.</p>
    *
    * <p>A `null` input String returns `null`. A `null` separator splits on whitespace.</p>
    *
    * {{{
    * none.ops.splitByWholeSeparatorPreserveAllTokens(*, *)               = null
    * "".ops.splitByWholeSeparatorPreserveAllTokens(*, *)                 = []
    * "ab de fg".ops.splitByWholeSeparatorPreserveAllTokens(null, 0)      = ["ab", "de", "fg"]
    * "ab   de fg".ops.splitByWholeSeparatorPreserveAllTokens(null, 0)    = ["ab", "", "", "de", "fg"]
    * "ab:cd:ef".ops.splitByWholeSeparatorPreserveAllTokens(":", 2)       = ["ab", "cd:ef"]
    * "ab-!-cd-!-ef".ops.splitByWholeSeparatorPreserveAllTokens("-!-", 5) = ["ab", "cd", "ef"]
    * "ab-!-cd-!-ef".ops.splitByWholeSeparatorPreserveAllTokens("-!-", 2) = ["ab", "cd-!-ef"]
    * }}}
    *
    * @param separatorChars
    *   String containing the String to be used as a delimiter, `null` splits on whitespace
    * @param max
    *   the maximum number of elements to include in the returned array. A zero or negative value implies no limit.
    * @tparam S
    *   String or Option[String]
    * @return
    *   an array of parsed Strings, `null` if null String was input
    */
  def splitByWholeSeparatorPreserveAllTokens[S: Adt.CoProducts2[*, String, Option[String]]](
    separatorChars: S,
    max: Int
  ): Option[Array[String]] = {
    val sep = mapToStrOpt(separatorChars).orNull
    Option(Strings.splitByWholeSeparatorPreserveAllTokens(strOrNull, sep, max))
  }

  /** <p>Splits the provided text into an array, using whitespace as the separator, preserving all tokens, including empty tokens created by
    * adjacent separators. This is an alternative to using StringTokenizer. Whitespace is defined by
    * [[Character# isWhitespace ( char)]].</p>
    *
    * <p>The separator is not included in the returned String array. Adjacent separators are treated as separators for empty tokens. For
    * more control over the split use the StrTokenizer class.</p>
    *
    * <p>A `null` input String returns `null`.</p>
    *
    * {{{
    * none.ops.splitPreserveAllTokens       = null
    * "".ops.splitPreserveAllTokens         = []
    * "abc def".ops.splitPreserveAllTokens  = ["abc", "def"]
    * "abc  def".ops.splitPreserveAllTokens = ["abc", "", "def"]
    * " abc ".ops.splitPreserveAllTokens    = ["", "abc", ""]
    * }}}
    * @return
    *   an array of parsed Strings, `none` if none String input
    */
  def splitPreserveAllTokens: Option[Array[String]] = Option(Strings.splitPreserveAllTokens(strOrNull))

  /** <p>Splits the provided text into an array, separator specified, preserving all tokens, including empty tokens created by adjacent
    * separators. This is an alternative to using StringTokenizer.</p>
    *
    * <p>The separator is not included in the returned String array. Adjacent separators are treated as separators for empty tokens. For
    * more control over the split use the StrTokenizer class.</p>
    *
    * <p>A `null` input String returns `null`.</p>
    *
    * {{{
    * none.ops.splitPreserveAllTokens(*)         = null
    * "".ops.splitPreserveAllTokens(*)           = []
    * "a.b.c".ops.splitPreserveAllTokens('.')    = ["a", "b", "c"]
    * "a..b.c".ops.splitPreserveAllTokens('.')   = ["a", "", "b", "c"]
    * "a:b:c".ops.splitPreserveAllTokens('.')    = ["a:b:c"]
    * "a\tb\nc".ops.splitPreserveAllTokens(null) = ["a", "b", "c"]
    * "a b c".ops.splitPreserveAllTokens(' ')    = ["a", "b", "c"]
    * "a b c ".ops.splitPreserveAllTokens(' ')   = ["a", "b", "c", ""]
    * "a b c  ".ops.splitPreserveAllTokens(' ')   = ["a", "b", "c", "", ""]
    * " a b c".ops.splitPreserveAllTokens(' ')   = ["", a", "b", "c"]
    * "  a b c".ops.splitPreserveAllTokens(' ')  = ["", "", a", "b", "c"]
    * " a b c ".ops.splitPreserveAllTokens(' ')  = ["", a", "b", "c", ""]
    * }}}
    *
    * @param separatorChar
    *   the character used as the delimiter, `null` splits on whitespace
    * @return
    *   an array of parsed Strings, `none` if none String input
    */
  def splitPreserveAllTokens(separatorChar: Char): Option[Array[String]] =
    Option(Strings.splitPreserveAllTokens(strOrNull, separatorChar))

  /** <p>Splits the provided text into an array, separators specified, preserving all tokens, including empty tokens created by adjacent
    * separators. This is an alternative to using StringTokenizer.</p>
    *
    * <p>The separator is not included in the returned String array. Adjacent separators are treated as separators for empty tokens. For
    * more control over the split use the StrTokenizer class.</p>
    *
    * <p>A `null` input String returns `null`. A `null` separatorChars splits on whitespace.</p>
    *
    * {{{
    * StringUtils.splitPreserveAllTokens(null, *)           = null
    * StringUtils.splitPreserveAllTokens("", *)             = []
    * StringUtils.splitPreserveAllTokens("abc def", null)   = ["abc", "def"]
    * StringUtils.splitPreserveAllTokens("abc def", " ")    = ["abc", "def"]
    * StringUtils.splitPreserveAllTokens("abc  def", " ")   = ["abc", "", def"]
    * StringUtils.splitPreserveAllTokens("ab:cd:ef", ":")   = ["ab", "cd", "ef"]
    * StringUtils.splitPreserveAllTokens("ab:cd:ef:", ":")  = ["ab", "cd", "ef", ""]
    * StringUtils.splitPreserveAllTokens("ab:cd:ef::", ":") = ["ab", "cd", "ef", "", ""]
    * StringUtils.splitPreserveAllTokens("ab::cd:ef", ":")  = ["ab", "", cd", "ef"]
    * StringUtils.splitPreserveAllTokens(":cd:ef", ":")     = ["", cd", "ef"]
    * StringUtils.splitPreserveAllTokens("::cd:ef", ":")    = ["", "", cd", "ef"]
    * StringUtils.splitPreserveAllTokens(":cd:ef:", ":")    = ["", cd", "ef", ""]
    * }}}
    *
    * @param separatorChars
    *   the characters used as the delimiters, `null` splits on whitespace
    * @tparam S
    *   String or Option[String]
    * @return
    *   an array of parsed Strings, `none` if none String input
    */
  def splitPreserveAllTokens[S: Adt.CoProducts2[*, String, Option[String]]](separatorChars: S): Option[Array[String]] = {
    val sep = mapToStrOpt(separatorChars).orNull
    Option(Strings.splitPreserveAllTokens(strOrNull, sep))
  }

  /** <p>Splits the provided text into an array with a maximum length, separators specified, preserving all tokens, including empty tokens
    * created by adjacent separators.</p>
    *
    * <p>The separator is not included in the returned String array. Adjacent separators are treated as separators for empty tokens.
    * Adjacent separators are treated as one separator.</p>
    *
    * <p>A `null` input String returns `null`. A `null` separatorChars splits on whitespace.</p>
    *
    * <p>If more than `max` delimited substrings are found, the last returned string includes all characters after the first {@code max - 1}
    * returned strings (including separator characters).</p>
    *
    * {{{
    * none.ops.splitPreserveAllTokens(*, *)            = null
    * "".ops.splitPreserveAllTokens(*, *)              = []
    * "ab de fg".ops.splitPreserveAllTokens(null, 0)   = ["ab", "de", "fg"]
    * "ab   de fg".ops.splitPreserveAllTokens(null, 0) = ["ab", "", "", "de", "fg"]
    * "ab:cd:ef".ops.splitPreserveAllTokens(":", 0)    = ["ab", "cd", "ef"]
    * "ab:cd:ef".ops.splitPreserveAllTokens(":", 2)    = ["ab", "cd:ef"]
    * "ab   de fg".ops.splitPreserveAllTokens(null, 2) = ["ab", "  de fg"]
    * "ab   de fg".ops.splitPreserveAllTokens(null, 3) = ["ab", "", " de fg"]
    * "ab   de fg".ops.splitPreserveAllTokens(null, 4) = ["ab", "", "", "de fg"]
    * }}}
    *
    * @param separatorChars
    *   the characters used as the delimiters, `null` splits on whitespace
    * @param max
    *   the maximum number of elements to include in the array. A zero or negative value implies no limit
    * @tparam S
    *   String or Option[String]
    * @return
    *   an array of parsed Strings, `none` if none String input
    */
  def splitPreserveAllTokens[S: Adt.CoProducts2[*, String, Option[String]]](separatorChars: S, max: Int): Option[Array[String]] = {
    val sep = mapToStrOpt(separatorChars).orNull
    Option(Strings.splitPreserveAllTokens(strOrNull, sep, max))
  }

  /** <p>Check if a CharSequence starts with a specified prefix.</p>
    *
    * <p>`null`s are handled without exceptions. Two `null` references are considered to be equal. The comparison is case sensitive.</p>
    *
    * {{{
    * none.ops.startWith(null)      = true
    * none.ops.startWith("abc")     = false
    * "abcdef".ops.startWith(null)  = false
    * "abcdef".ops.startWith("abc") = true
    * "ABCDEF".ops.startWith("abc") = false
    * }}}
    *
    * @param prefix
    *   the prefix to find, may be null
    * @tparam S
    *   String or Option[String]
    * @return
    *   `true` if the CharSequence starts with the prefix, case sensitive, or both `null`
    */
  def startsWith[S: Adt.CoProducts2[*, String, Option[String]]](prefix: S): Boolean = {
    val pre = mapToStrOpt(prefix).orNull
    Strings.startsWith(strOrNull, pre)
  }

  /** <p>Case insensitive check if a CharSequence starts with a specified prefix.</p>
    *
    * <p>`null`s are handled without exceptions. Two `null` references are considered to be equal. The comparison is case insensitive.</p>
    *
    * {{{
    * none.ops.startsWithAny(none) = true
    * none.ops.startsWithAny("abc") = false
    * "abcdef".ops.startsWithAny(none) = false
    * "abcdef".ops.startsWithAny("abc") = true
    * "ABCDEF".ops.startsWithAny("abc") = true
    * }}}
    *
    * @param searchStrings
    *   the case-sensitive CharSequence prefixes, may be empty or contain `null`
    * @tparam CS
    *   CharSequence or Option[CharSequence]
    * @return
    *   `true` if the input `sequence` is `null` AND no `searchStrings` are provided, or the input `sequence` begins with any of the
    *   provided case-sensitive `searchStrings`.
    */
  def startsWithAny[CS: Adt.CoProducts2[*, CharSequence, Option[CharSequence]]](searchStrings: CS*): Boolean = {
    def applyM = Adt.CoProduct2[CharSequence, Option[CharSequence]].typeOnly[CS]
    def strs: Seq[CharSequence] =
      applyM.fold2(_.higherKindApply[Seq](searchStrings)).fold1(func => for (c <- func.higherKindApply[Seq](searchStrings)) yield c.orNull)

    if (searchStrings == null) Strings.startsWithAny(strOrNull)
    else
      Strings.startsWithAny(strOrNull, strs: _*)
  }

  /** <p>Case insensitive check if a CharSequence starts with a specified prefix.</p>
    *
    * <p>`null`s are handled without exceptions. Two `null` references are considered to be equal. The comparison is case insensitive.</p>
    *
    * {{{
    * none.ops.startsWithIgnoreCase(null)      = true
    * none.ops.startsWithIgnoreCase("abc")     = false
    * "abcdef".ops.startsWithIgnoreCase(none)  = false
    * "abcdef".ops.startsWithIgnoreCase("abc") = true
    * "ABCDEF".ops.startsWithIgnoreCase("abc") = true
    * }}}
    *
    * @param prefix
    *   the prefix to find, may be null
    * @tparam P
    *   String or Option[String]
    * @return
    *   `true` if the CharSequence starts with the prefix, case insensitive, or both `null`
    */
  def startsWithIgnoreCase[P: Adt.CoProducts2[*, String, Option[String]]](prefix: P): Boolean = {
    val str = mapToStrOpt(prefix).orNull
    Strings.startsWithIgnoreCase(strOrNull, str)
  }

  /** <p>Strips whitespace from the start and end of a String.</p>
    *
    * <p>This is similar to [[# trim ( String )]] but removes whitespace. Whitespace is defined by [[Character# isWhitespace ( char)]].</p>
    *
    * <p>A `null` input String returns `null`.</p>
    *
    * {{{
    * none.ops.strip     = null
    * "".ops.strip       = ""
    * "   ".ops.strip    = ""
    * "abc".ops.strip    = "abc"
    * "  abc".ops.strip  = "abc"
    * "abc  ".ops.strip  = "abc"
    * " abc ".ops.strip  = "abc"
    * " ab c ".ops.strip = "ab c"
    * }}}
    *
    * @return
    *   the stripped String, `none` if none String input
    */
  def strip: Option[String] = Option(Strings.strip(strOrNull))

  /** <p>Strips any of a set of characters from the start and end of a String. This is similar to [[String# trim ( )]] but allows the
    * characters to be stripped to be controlled.</p>
    *
    * <p>A `null` input String returns `null`. An empty string ("") input returns the empty string.</p>
    *
    * <p>If the stripChars String is `null`, whitespace is stripped as defined by [[Character# isWhitespace ( char )]]. Alternatively use
    * [[# strip ( String )]].</p>
    *
    * {{{
    * none.ops.strip(*)          = null
    * "".ops.strip(*)            = ""
    * "abc".ops.strip(null)      = "abc"
    * "  abc".ops.strip(null)    = "abc"
    * "abc  ".ops.strip(null)    = "abc"
    * " abc ".ops.strip(null)    = "abc"
    * "  abcyx".ops.strip("xyz") = "  abc"
    * }}}
    *
    * @param stripChars
    *   the characters to remove, null treated as whitespace
    *
    * @tparam S
    *   String or Option[String]
    * @return
    *   the stripped String, `none` if none String input
    */
  def strip[S: Adt.CoProducts2[*, String, Option[String]]](stripChars: S): Option[String] = {
    val chars = mapToStrOpt(stripChars).orNull
    Option(Strings.strip(strOrNull, chars))
  }

  /** <p>Removes diacritics (~= accents) from a string. The case will not be altered.</p> <p>For instance, '&agrave;' will be replaced by
    * 'a'.</p> <p>Note that ligatures will be left as is.</p>
    *
    * {{{
    * none.ops.stripAccents                = null
    * "".ops.stripAccents)                  = ""
    * "control".ops.stripAccents           = "control"
    * "&eacute;clair".ops.stripAccents     = "eclair"
    * }}}
    *
    * @return
    *   input text with diacritics removed
    */
  def stripAccents: Option[String] = Option(Strings.stripAccents(strOrNull))

  /** <p>Strips any of a set of characters from the end of a String.</p>
    *
    * <p>A `null` input String returns `null`. An empty string ("") input returns the empty string.</p>
    *
    * <p>If the stripChars String is `null`, whitespace is stripped as defined by [[Character# isWhitespace ( char )]].</p>
    *
    * {{{
    * none.ops.stripEnd(*)          = null
    * "".ops.stripEnd(*)            = ""
    * "abc".ops.stripEnd("")        = "abc"
    * "abc".ops.stripEnd(null)      = "abc"
    * "  abc".ops.stripEnd(null)    = "  abc"
    * "abc  ".ops.stripEnd(null)    = "abc"
    * " abc ".ops.stripEnd(null)    = " abc"
    * "  abcyx".ops.stripEnd("xyz") = "  abc"
    * "120.00".ops.stripEnd(".0")   = "12"
    * }}}
    *
    * @param stripChars
    *   the set of characters to remove, null treated as whitespace
    * @tparam S
    *   String or Option[String]
    * @return
    *   the stripped String, `none` if none String input
    */
  def stripEnd[S: Adt.CoProducts2[*, String, Option[String]]](stripChars: S): Option[String] = {
    val chars = mapToStrOpt(stripChars).orNull
    Option(Strings.stripEnd(strOrNull, chars))
  }

  /** <p>Strips any of a set of characters from the end of a String.</p>
    *
    * <p>A `null` input String returns `null`. An empty string ("") input returns the empty string.</p>
    *
    * <p>If the stripChars String is `null`, whitespace is stripped as defined by [[Character# isWhitespace ( char )]].</p>
    *
    * {{{
    * none.ops.stripStart(*)          = null
    * "".ops.stripStart( *)            = ""
    * "abc".ops.stripStart( "")        = "abc"
    * "abc".ops.stripStart( null)      = "abc"
    * "  abc".ops.stripStart( null)    = "  abc"
    * "abc  ".ops.stripStart( null)    = "abc"
    * " abc ".ops.stripStart( null)    = " abc"
    * "  abcyx".ops.stripStart( "xyz") = "  abc"
    * "120.00".ops.stripStart( ".0")   = "12"
    * }}}
    *
    * @param stripChars
    *   the set of characters to remove, null treated as whitespace
    * @tparam S
    *   String or Option string
    * @return
    *   the stripped String, `none` if none String input
    */
  def stripStart[S: Adt.CoProducts2[*, String, Option[String]]](stripChars: S): Option[String] = {
    val chars = mapToStrOpt(stripChars).orNull
    Option(Strings.stripStart(strOrNull, chars))
  }

  /** <p>Strips whitespace from the start and end of a String returning an empty String if `null` input.</p>
    *
    * <p>This is similar to [[# trimToEmpty ( String )]] but removes whitespace. Whitespace is defined by
    * [[Character# isWhitespace( char )]].</p>
    *
    * {{{
    * none.ops.stripToEmpty = ""
    * "".ops.stripToEmpty = ""
    * " ".ops.stripToEmpty = ""
    * "abc".ops.stripToEmpty = "abc" "
    * abc".ops.stripToEmpty = "abc"
    * "abc ".ops.stripToEmpty = "abc"
    * " abc ".ops.stripToEmpty = "abc"
    * " ab c ".ops.stripToEmpty = "ab c"
    * }}}
    *
    * @return
    *   the trimmed String, or an empty String if `null` input
    */
  def stripToEmpty: Option[String] = Option(Strings.stripToEmpty(strOrNull))

  /** <p>Strips whitespace from the start and end of a String returning `null` if the String is empty ("") after the strip.</p>
    *
    * <p>This is similar to [[# trimToNull ( String )]] but removes whitespace. Whitespace is defined by
    * [[Character# isWhitespace( char )]].</p>
    *
    * {{{
    * none.ops.stripToNone     = null
    * "".ops.stripToNone       = null
    * "   ".ops.stripToNone    = null
    * "abc".ops.stripToNone    = "abc"
    * "  abc".ops.stripToNone  = "abc"
    * "abc  ".ops.stripToNone  = "abc"
    * " abc ".ops.stripToNone  = "abc"
    * " ab c ".ops.stripToNone = "ab c"
    * }}}
    *
    * @return
    *   the stripped String, `null` if whitespace, empty or null String input
    */
  def stripToNone: Option[String] = Option(Strings.stripToNull(strOrNull))

  /** <p>Gets a substring from the specified String avoiding exceptions.</p>
    *
    * <p>A negative start position can be used to start `n` characters from the end of the String.</p>
    *
    * <p>A `null` String will return `null`. An empty ("") String will return "".</p>
    *
    * {{{
    * none.ops.substring(*) = null
    * "".ops.substring(*) = ""
    * "abc".ops.substring(0) = "abc"
    * "abc".ops.substring(2) = "c"
    * "abc".ops.substring(4) = ""
    * "abc".ops.substring(-2) = "bc"
    * "abc".ops.substring(-4) = "abc"
    * }}}
    *
    * @param start
    *   the position to start from, negative means count back from the end of the String by this many characters
    * @return
    *   substring from start position, `none` if none String input
    */
  def substring(start: Int): Option[String] = Option(Strings.substring(strOrNull, start))

  /** <p>Gets a substring from the specified String avoiding exceptions.</p>
    *
    * <p>A negative start position can be used to start/end `n` characters from the end of the String.</p>
    *
    * <p>The returned substring starts with the character in the `start` position and ends before the `end` position. All position counting
    * is zero-based -- i.e., to start at the beginning of the string use `start = 0`. Negative start and end positions can be used to
    * specify offsets relative to the end of the String.</p>
    *
    * <p>If `start` is not strictly to the left of `end`, "" is returned.</p>
    *
    * {{{
    * none.ops.substring(*, *) = null
    * "".ops.substring(* , *) = ""
    * "abc".ops.substring(0, 2) = "ab"
    * "abc".ops.substring(2, 0) = ""
    * "abc".ops.substring(2, 4) = "c"
    * "abc".ops.substring(4, 6) = ""
    * "abc".ops.substring(2, 2) = ""
    * "abc".ops.substring(-2, -1) = "b"
    * "abc".ops.substring(-4, 2) = "ab"
    * }}}
    *
    * @param start
    *   the position to start from, negative means count back from the end of the String by this many characters
    * @param end
    *   the position to end at (exclusive), negative means count back from the end of the String by this many characters
    * @return
    *   substring from start position to end position, `none` if none String input
    */
  def substring(start: Int, end: Int): Option[String] = Option(Strings.substring(strOrNull, start, end))

  /** <p>Gets the substring after the first occurrence of a separator. The separator is not returned.</p>
    *
    * <p>A `null` string input will return `null`. An empty ("") string input will return the empty string.
    *
    * <p>If nothing is found, the empty string is returned.</p>
    *
    * {{{
    * none.ops.substringAfter(*) = None
    * "".ops.substringAfter(*) = ""
    * "abc".ops.substringAfter('a') = "bc"
    * "abcba".ops.substringAfter('b') = "cba"
    * "abc".ops.substringAfter('c') = ""
    * "abc".ops.substringAfter('d') = "" "
    * abc".ops.substringAfter(32) = "abc"
    * }}}
    *
    * @param separator
    *   the character to search.
    * @return
    *   the substring after the first occurrence of the separator, `none` if none String input
    */
  def substringAfter(separator: Char): Option[String] = Option(Strings.substringAfter(strOrNull, separator))

  /** <p>Gets the substring after the first occurrence of a separator. The separator is not returned.</p>
    *
    * <p>A `null` string input will return `null`. An empty ("") string input will return the empty string.
    *
    * <p>If nothing is found, the empty string is returned.</p>
    *
    * {{{
    * none.ops.substringAfter(*) = None
    * "".ops.substringAfter(*) = ""
    * "abc".ops.substringAfter('a') = "bc"
    * "abcba".ops.substringAfter('b') = "cba"
    * "abc".ops.substringAfter('c') = ""
    * "abc".ops.substringAfter('d') = "" "
    * abc".ops.substringAfter(32) = "abc"
    * }}}
    *
    * @param separator
    *   the character to search.
    * @return
    *   the substring after the first occurrence of the separator, `none` if none String input
    */
  def substringAfter(separator: Int): Option[String] = Option(Strings.substringAfter(strOrNull, separator))

  /** <p>Gets the substring after the first occurrence of a separator. The separator is not returned.</p>
    *
    * <p>A `null` string input will return `null`. An empty ("") string input will return the empty string. A `null` separator will return
    * the empty string if the input string is not `null`.</p>
    *
    * <p>If nothing is found, the empty string is returned.</p>
    *
    * {{{
    * none.ops.substringAfter(*)      = null
    * "".ops.substringAfter(*)        = ""
    * *.ops.substringAfter(null)      = ""
    * "abc".ops.substringAfter("a")   = "bc"
    * "abcba".ops.substringAfter("b") = "cba"
    * "abc".ops.substringAfter("c")   = ""
    * "abc".ops.substringAfter("d")   = ""
    * "abc".ops.substringAfter("")    = "abc"
    * }}}
    *
    * @param separator
    *   the String to search for, may be null
    * @tparam S
    *   String or Option[String]
    * @return
    *   the substring after the first occurrence of the separator, `none` if none String input
    */
  def substringAfter[S: Adt.CoProducts2[*, String, Option[String]]](separator: S): Option[String] = {
    val sep = mapToStrOpt(separator).orNull
    Option(Strings.substringAfter(strOrNull, sep))
  }

  /** <p>Gets the substring after the last occurrence of a separator. The separator is not returned.</p>
    *
    * <p>A `null` string input will return `null`. An empty ("") string input will return the empty string.
    *
    * <p>If nothing is found, the empty string is returned.</p>
    *
    * {{{
    * none.ops.substringAfterLast(*)      = null
    * "".ops.substringAfterLast(*)        = ""
    * "abc".ops.substringAfterLast('a')   = "bc"
    * " bc".ops.substringAfterLast(32)    = "bc"
    * "abcba".ops.substringAfterLast('b') = "a"
    * "abc".ops.substringAfterLast('c')   = ""
    * "a".ops.substringAfterLast('a')     = ""
    * "a".ops.substringAfterLast('z')     = ""
    * }}}
    *
    * @param separator
    *   the String to search for, may be null
    * @return
    *   the substring after the last occurrence of the separator, `none` if none String input
    */
  def substringAfterLast(separator: Char): Option[String] = Option(Strings.substringAfterLast(strOrNull, separator))

  /** <p>Gets the substring after the last occurrence of a separator. The separator is not returned.</p>
    *
    * <p>A `null` string input will return `null`. An empty ("") string input will return the empty string.
    *
    * <p>If nothing is found, the empty string is returned.</p>
    *
    * {{{
    * none.ops.substringAfterLast(*)      = null
    * "".ops.substringAfterLast(*)        = ""
    * "abc".ops.substringAfterLast('a')   = "bc"
    * " bc".ops.substringAfterLast(32)    = "bc"
    * "abcba".ops.substringAfterLast('b') = "a"
    * "abc".ops.substringAfterLast('c')   = ""
    * "a".ops.substringAfterLast('a')     = ""
    * "a".ops.substringAfterLast('z')     = ""
    * }}}
    *
    * @param separator
    *   the String to search for, may be null
    * @return
    *   the substring after the last occurrence of the separator, `none` if none String input
    */
  def substringAfterLast(separator: Int): Option[String] = Option(Strings.substringAfterLast(strOrNull, separator))

  /** <p>Gets the substring after the last occurrence of a separator. The separator is not returned.</p>
    *
    * <p>A `null` string input will return `null`. An empty ("") string input will return the empty string. An empty or {@code null}
    * separator will return the empty string if the input string is not `null`.</p>
    *
    * <p>If nothing is found, the empty string is returned.</p>
    *
    * {{{
    * none.ops.substringAfterLast(*)      = null
    * "".ops.substringAfterLast(*)        = ""
    * *.ops.substringAfterLast("")        = ""
    * *.ops.substringAfterLast(null)      = ""
    * "abc".ops.substringAfterLast("a")   = "bc"
    * "abcba".ops.substringAfterLast("b") = "a"
    * "abc".ops.substringAfterLast("c")   = ""
    * "a".ops.substringAfterLast("a")     = ""
    * "a".ops.substringAfterLast("z")     = ""
    * }}}
    *
    * @param separator
    *   the String to search for, may be null
    * @tparam S
    *   String or Option[String]
    * @return
    *   the substring after the last occurrence of the separator, `none` if none String input
    */
  def substringAfterLast[S: Adt.CoProducts2[*, String, Option[String]]](separator: S): Option[String] = {
    val sep = mapToStrOpt(separator).orNull
    Option(Strings.substringAfterLast(strOrNull, sep))
  }

  /** <p> Gets the substring before the first occurrence of a separator. The separator is not returned. </p>
    *
    * <p> A `null` string input will return `null`. An empty ("") string input will return the empty string. </p>
    *
    * <p> If nothing is found, the string input is returned. </p>
    *
    * {{{
    * none.ops.substringBefore(*)      = null
    * "".ops.substringBefore(*)        = ""
    * "abc".ops.substringBefore('a')   = ""
    * "abcba".ops.substringBefore('b') = "a"
    * "abc".ops.substringBefore('c')   = "ab"
    * "abc".ops.substringBefore('d')   = "abc"
    * }}}
    *
    * @param separator
    *   the String to search for, may be null
    * @return
    *   the substring before the first occurrence of the separator, `none` if none String input
    */
  def substringBefore(separator: Char): Option[String] = Option(Strings.substringBefore(strOrNull, separator))

  /** <p> Gets the substring before the first occurrence of a separator. The separator is not returned. </p>
    *
    * <p> A `null` string input will return `null`. An empty ("") string input will return the empty string. </p>
    *
    * <p> If nothing is found, the string input is returned. </p>
    *
    * {{{
    * none.ops.substringBefore(*)      = null
    * "".ops.substringBefore(*)        = ""
    * "abc".ops.substringBefore('a')   = ""
    * "abcba".ops.substringBefore('b') = "a"
    * "abc".ops.substringBefore('c')   = "ab"
    * "abc".ops.substringBefore('d')   = "abc"
    * }}}
    *
    * @param separator
    *   the String to search for, may be null
    * @return
    *   the substring before the first occurrence of the separator, `none` if none String input
    */
  def substringBefore(separator: Int): Option[String] = Option(Strings.substringBefore(strOrNull, separator))

  /** <p>Gets the substring before the first occurrence of a separator. The separator is not returned.</p>
    *
    * <p>A `null` string input will return `null`. An empty ("") string input will return the empty string. A `null` separator will return
    * the input string.</p>
    *
    * <p>If nothing is found, the string input is returned.</p>
    *
    * {{{
    * none.ops.substringBefore(*)      = null
    * "".ops.substringBefore(*)        = ""
    * "abc".ops.substringBefore("a")   = ""
    * "abcba".ops.substringBefore("b") = "a"
    * "abc".ops.substringBefore("c")   = "ab"
    * "abc".ops.substringBefore("d")   = "abc"
    * "abc".ops.substringBefore("")    = ""
    * "abc".ops.substringBefore(null)  = "abc"
    * }}}
    *
    * @param separator
    *   the String to search for, may be null
    * @tparam S
    *   String or Option[String]
    * @return
    *   the substring before the first occurrence of the separator, `none` if none String input
    */
  def substringBefore[S: Adt.CoProducts2[*, String, Option[String]]](separator: S): Option[String] = {
    val sep = mapToStrOpt(separator).orNull
    Option(Strings.substringBefore(strOrNull, sep))
  }

  /** <p>Gets the substring before the last occurrence of a separator. The separator is not returned.</p>
    *
    * <p>A `null` string input will return `null`. An empty ("") string input will return the empty string. An empty or {@code null}
    * separator will return the input string.</p>
    *
    * <p>If nothing is found, the string input is returned.</p>
    *
    * {{{
    * none.ops.substringBeforeLast(*)      = null
    * "".ops.substringBeforeLast(*)        = ""
    * "abcba".ops.substringBeforeLast("b") = "abc"
    * "abc".ops.substringBeforeLast("c")   = "ab"
    * "a".ops.substringBeforeLast("a")     = ""
    * "a".ops.substringBeforeLast("z")     = "a"
    * "a".ops.substringBeforeLast(null)    = "a"
    * "a".ops.substringBeforeLast("")      = "a"
    * }}}
    *
    * @param separator
    *   the String to search for, may be null
    * @tparam S
    *   String or Option[String]
    * @return
    *   the substring before the last occurrence of the separator, `none` if none String input
    */
  def substringBeforeLast[S: Adt.CoProducts2[*, String, Option[String]]](separator: S): Option[String] = {
    val sep = mapToStrOpt(separator).orNull
    Option(Strings.substringBeforeLast(strOrNull, sep))
  }

  /** <p>Gets the String that is nested in between two instances of the same String.</p>
    *
    * <p>A `null` input String returns `null`. A `null` tag returns `null`.</p>
    *
    * {{{
    * none.ops.substringsBetween(*)            = null
    * "".ops.substringsBetween("")             = ""
    * "".ops.substringsBetween("tag")          = null
    * "tagabctag".ops.substringsBetween(null)  = null
    * "tagabctag".ops.substringsBetween("")    = ""
    * "tagabctag".ops.substringsBetween("tag") = "abc"
    * }}}
    *
    * @param tag
    *   the String before and after the substring, may be null
    * @tparam S
    *   String or Option[String]
    * @return
    *   the substring, `null` if no match
    */
  def substringBetween[S: Adt.CoProducts2[*, String, Option[String]]](tag: S): Option[String] = {
    val t = mapToStrOpt(tag).orNull
    Option(Strings.substringBetween(strOrNull, t))
  }

  /** <p>Gets the String that is nested in between two Strings. Only the first match is returned.</p>
    *
    * <p>A `null` input String returns `null`. A `null` open/close returns `null` (no match). An empty ("") open and close returns an empty
    * string.</p>
    *
    * {{{
    * "wx[b]yz".ops.substringBetween("[", "]") = "b"
    * none.ops.substringBetween(*, *)          = null
    * *.ops.substringBetween(null, *)          = null
    * *.ops.substringBetween(*, null)          = null
    * "".ops.substringBetween("", "")          = ""
    * "".ops.substringBetween("", "]")         = null
    * "".ops.substringBetween("[", "]")        = null
    * "yabcz".ops.substringBetween("", "")     = ""
    * "yabcz".ops.substringBetween("y", "z")   = "abc"
    * "yabczyabcz".ops.substringBetween("y", "z")   = "abc"
    * }}}
    *
    * @param open
    *   the String before the substring, may be null
    * @param close
    *   the String after the substring, may be null
    * @tparam S
    *   String or Option[String]
    * @return
    *   the substring, `null` if no match
    */
  def substringBetween[S: Adt.CoProducts2[*, String, Option[String]]](open: S, close: S): Option[String] = {
    val o = mapToStrOpt(open).orNull
    val c = mapToStrOpt(close).orNull
    Option(Strings.substringBetween(strOrNull, o, c))
  }

  /** <p>Searches a String for substrings delimited by a start and end tag, returning all matching substrings in an array.</p>
    *
    * <p>A `null` input String returns `null`. A `null` open/close returns `null` (no match). An empty ("") open/close returns `null` (no
    * match).</p>
    *
    * {{{
    * "[a][b][c]".ops.substringsBetween("[", "]") = ["a","b","c"]
    * none..ops.substringsBetween(*, *)            = null
    * *.ops.substringsBetween(null, *)            = null
    * *.ops.substringsBetween(*, null)            = null
    * "".ops.substringsBetween("[", "]")          = []
    * }}}
    *
    * @param open
    *   the String identifying the start of the substring, empty returns null
    * @param close
    *   the String identifying the end of the substring, empty returns null
    * @tparam S
    *   String or Option[String]
    * @return
    *   a String Array of substrings, or `null` if no match
    */
  def substringsBetween[S: Adt.CoProducts2[*, String, Option[String]]](open: S, close: S): Option[Array[String]] = {
    val o = mapToStrOpt(open).orNull
    val c = mapToStrOpt(close).orNull
    Option(Strings.substringsBetween(strOrNull, o, c))
  }

  /** * <p>Swaps the case of a String changing upper and title case to lower case, and lower case to upper case.</p>
    *
    * <ul> <li>Upper case character converts to Lower case</li> <li>Title case character converts to Lower case</li> <li>Lower case
    * character converts to Upper case</li> </ul>
    *
    * <p>For a word based algorithm, see [[org.apache.commons.lang3.text.WordUtils# swapCase ( String )]]. A `none` input String returns
    * `None`.</p>
    *
    * {{{
    * none.ops.swapCase                 = null
    * "".ops.swapCase                   = ""
    * "The dog has a BONE".ops.swapCase = "tHE DOG HAS A bone"
    * }}}
    *
    * <p>NOTE: This method changed in Lang version 2.0. It no longer performs a word based algorithm. If you only use ASCII, you will notice
    * no change. That functionality is available in org.apache.commons.lang3.text.WordUtils.</p>
    *
    * @return
    *   the changed String, `none` if none String input
    */
  def swapCase: Option[String] = Option(Strings.swapCase(strOrNull))

  /** <p>Converts a `CharSequence` into an array of code points.</p>
    *
    * <p>Valid pairs of surrogate code units will be converted into a single supplementary code point. Isolated surrogate code units (i.e. a
    * high surrogate not followed by a low surrogate or a low surrogate not preceded by a high surrogate) will be returned as-is.</p>
    *
    * {{{
    * none.ops.toCodePoints = None
    * "".ops.toCodePoints = Some([]) // empty array
    * }}}
    *
    * @return
    *   an array of code points
    */
  def toCodePoints: Option[Array[Int]] = Option(Strings.toCodePoints(strOrNull))

  /** Converts the given source String as a lower-case using the [[Locale#ROOT]] locale in a null-safe manner.
    *
    * @return
    *   the given source String as a lower-case using the [[Locale# ROOT]] locale or null.
    */
  def toRootLowerCase: Option[String] = Option(Strings.toRootLowerCase(strOrNull))

  /** Converts the given source String as a upper-case using the [[Locale#ROOT]] locale in a null-safe manner.
    *
    * @return
    *   the given source String as a upper-case using the [[Locale# ROOT]] locale or null.
    */
  def toRootUpperCase: Option[String] = Option(Strings.toRootUpperCase(strOrNull))

  /** <p>Removes control characters (char &lt;= 32) from both ends of this String, handling `null` by returning `null`.</p>
    *
    * <p>The String is trimmed using [[String# trim ( )]]. Trim removes start and end characters &lt;= 32. To strip whitespace use
    * [[# strip ( String )]].</p>
    *
    * <p>To trim your choice of characters, use the [[# strip ( String, String)]] methods.</p>
    *
    * {{{
    * none.ops.trim          = null
    * "".ops.trim            = ""
    * "     ".ops.trim       = ""
    * "abc".ops.trim         = "abc"
    * "    abc    ".ops.trim = "abc"
    * }}}
    *
    * @return
    *   the trimmed string, `none` if none String input
    */
  def trim: Option[String] = Option(Strings.trim(strOrNull))

  /** <p>Removes control characters (char &lt;= 32) from both ends of this String returning `null` if the String is empty ("") after the
    * trim or if it is `null`.
    *
    * <p>The String is trimmed using [[String# trim ( )]]. Trim removes start and end characters &lt;= 32. To strip whitespace use
    * [[# stripToNull ( String )]].</p>
    *
    * {{{
    * none.ops.trimToEmpty          = null
    * "".ops.trimToEmpty            = null
    * "     ".ops.trimToEmpty       = null
    * "abc".ops.trimToEmpty         = "abc"
    * "    abc    ".ops.trimToEmpty = "abc"
    * }}}
    *
    * @return
    *   the trimmed String, `null` if only chars &lt;= 32, empty or null String input
    */
  def trimToEmpty: Option[String] = Option(Strings.trimToEmpty(strOrNull))

  /** <p>Removes control characters (char &lt;= 32) from both ends of this String returning `null` if the String is empty ("") after the
    * trim or if it is `null`.
    *
    * <p>The String is trimmed using [[String# trim ( )]]. Trim removes start and end characters &lt;= 32. To strip whitespace use
    * [[# stripToNull ( String )]].</p>
    *
    * {{{
    * none.ops.trimToNone          = null
    * "".ops.trimToNone            = null
    * "     ".ops.trimToNone       = null
    * "abc".ops.trimToNone         = "abc"
    * "    abc    ".ops.trimToNone = "abc"
    * }}}
    *
    * @return
    *   the trimmed String, `null` if only chars &lt;= 32, empty or null String input
    */
  def trimToNone: Option[String] = Option(Strings.trimToNull(strOrNull))

  /** * <p>Truncates a String. This will turn "Now is the time for all good men" into "Now is the time for".</p>
    *
    * <p>Specifically:</p>
    *   - If `str`is less than `maxWidth`characters long, return it.
    *   - Else truncate it to `substring(str, 0, maxWidth)`.
    *   - If `maxWidth`is less than `0`, throw an `IllegalArgumentException`.
    *   - In no case will it return a String of length greater than `maxWidth`
    *
    * {{{
    * none.ops.truncate(0)       = null
    * none.ops.truncate( 2)       = null
    * "".ops.truncate(4)         = ""
    * "abcdefg".ops.truncate(4)  = "abcd"
    * "abcdefg".ops.truncate(6)  = "abcdef"
    * "abcdefg".ops.truncate(7)  = "abcdefg"
    * "abcdefg".ops.truncate(8)  = "abcdefg"
    * "abcdefg".ops.truncate(-1) = throws an IllegalArgumentException
    * }}}
    *
    * @param maxWidth
    *   maximum length of result String, must be positive
    * @return
    *   truncated String, `none` if none String input
    * @throws IllegalArgumentException
    *   If `maxWidth`is less than `0`
    */
  def truncate(maxWidth: Int): Option[String] = Option(Strings.truncate(strOrNull, maxWidth))

  /** <p>Truncates a String. This will turn "Now is the time for all good men" into "is the time for all".</p>
    *
    * <p>Works like `truncate(String, int)`, but allows you to specify a "left edge" offset.
    *
    * <p>Specifically:</p> <ul> <li>If `str`is less than `maxWidth`characters long, return it.</li> <li>Else truncate it to `substring(str,
    * offset, maxWidth)`.</li> <li>If `maxWidth`is less than `0`, throw an {@code IllegalArgumentException} .</li> <li>If `offset` is less
    * than `0`, throw an `IllegalArgumentException`.</li> <li>In no case will it return a String of length greater than `maxWidth`.</li>
    * </ul>
    *
    * {{{
    * none.ops.truncate(0, 0) = null
    * none.ops.truncate(2, 4) = null
    * "".ops.truncate(0, 10) = ""
    * "".ops.truncate(2, 10) = ""
    * "abcdefghij".ops.truncate(0, 3) = "abc"
    * "abcdefghij".ops.truncate(5, 6) = "fghij"
    * "raspberry peach".ops.truncate(10, 15) = "peach"
    * "abcdefghijklmno".ops.truncate(0, 10) = "abcdefghij"
    * "abcdefghijklmno".ops.truncate(-1, 10) = throws an IllegalArgumentException
    * "abcdefghijklmno".ops.truncate(Integer.MIN_VALUE, 10) = throws an IllegalArgumentException
    * "abcdefghijklmno".ops.truncate(Integer.MIN_VALUE, Integer.MAX_VALUE) = throws an IllegalArgumentException
    * "abcdefghijklmno".ops.truncate(0, Integer.MAX_VALUE) = "abcdefghijklmno"
    * "abcdefghijklmno".ops.truncate(1, 10) = "bcdefghijk"
    * "abcdefghijklmno".ops.truncate(2, 10) = "cdefghijkl"
    * "abcdefghijklmno".ops.truncate(3, 10) = "defghijklm"
    * "abcdefghijklmno".ops.truncate(4, 10) = "efghijklmn"
    * "abcdefghijklmno".ops.truncate(5, 10) = "fghijklmno"
    * "abcdefghijklmno".ops.truncate(5, 5) = "fghij"
    * "abcdefghijklmno".ops.truncate(5, 3) = "fgh"
    * "abcdefghijklmno".ops.truncate(10, 3) = "klm"
    * "abcdefghijklmno".ops.truncate(10, Integer.MAX_VALUE) = "klmno"
    * "abcdefghijklmno".ops.truncate(13, 1) = "n"
    * "abcdefghijklmno".ops.truncate(13, Integer.MAX_VALUE) = "no"
    * "abcdefghijklmno".ops.truncate(14, 1) = "o"
    * "abcdefghijklmno".ops.truncate(14, Integer.MAX_VALUE) = "o"
    * "abcdefghijklmno".ops.truncate(15, 1) = ""
    * "abcdefghijklmno".ops.truncate(15, Integer.MAX_VALUE) = ""
    * "abcdefghijklmno".ops.truncate(Integer.MAX_VALUE, Integer.MAX_VALUE) = ""
    * "abcdefghij".ops.truncate(3, -1) = throws an IllegalArgumentException
    * "abcdefghij".ops.truncate(-2, 4) = throws an IllegalArgumentException
    * }}}
    *
    * @param offset
    *   left edge of source String
    * @param maxWidth
    *   maximum length of result String, must be positive
    * @return
    *   truncated String, `none` if none String input
    * @throws IllegalArgumentException
    *   If `offset` or `maxWidth`is less than `0`
    */
  def truncate(offset: Int, maxWidth: Int): Option[String] = Option(Strings.truncate(strOrNull, offset, maxWidth))

  /** <p>Uncapitalizes a String, changing the first character to lower case as per [[Character# toLowerCase ( int )]]. No other characters
    * are changed.</p>
    *
    * <p>For a word based algorithm, see [[org.apache.commons.lang3.text.WordUtils# uncapitalize ( String )]]. A `none` input String returns
    * `None`.</p>
    *
    * {{{
    * none.ops.uncapitalize  = null
    * "".ops.uncapitalize    = ""
    * "cat".ops.uncapitalize = "cat"
    * "Cat".ops.uncapitalize = "cat"
    * "CAT".ops.uncapitalize = "cAT"
    * }}}
    *
    * @return
    *   the uncapitalized String, `none` if none String input
    */
  def uncapitalize: Option[String] = Option(Strings.uncapitalize(strOrNull))

  /** <p> Unwraps a given string from a character. </p>
    *
    * {{{
    * none.ops.unwrap(null)         = null
    * none.ops.unwrap('\0')         = null
    * none.ops.unwrap('1')          = null
    * "a".ops.unwrap('a')           = "a"
    * "aa".ops.unwrap('a')           = ""
    * "\'abc\'".ops.unwrap('\'')    = "abc"
    * "AABabcBAA".ops.unwrap('A')   = "ABabcBA"
    * "A".ops.unwrap('#')           = "A"
    * "#A".ops.unwrap('#')          = "#A"
    * "A#".ops.unwrap('#')          = "A#"
    * }}}
    *
    * @param wrapChar
    *   the character used to unwrap
    * @return
    *   unwrapped String or the original string if it is not quoted properly with the wrapChar
    */
  def unwrap(wrapChar: Char): Option[String] = Option(Strings.unwrap(strOrNull, wrapChar))

  /** <p> Unwraps a given string from anther string. </p>
    *
    * {{{
    * StringUtils.unwrap(null, null)         = null
    * StringUtils.unwrap(null, "")           = null
    * StringUtils.unwrap(null, "1")          = null
    * StringUtils.unwrap("a", "a")           = "a"
    * StringUtils.unwrap("aa", "a")          = ""
    * StringUtils.unwrap("\'abc\'", "\'")    = "abc"
    * StringUtils.unwrap("\"abc\"", "\"")    = "abc"
    * StringUtils.unwrap("AABabcBAA", "AA")  = "BabcB"
    * StringUtils.unwrap("A", "#")           = "A"
    * StringUtils.unwrap("#A", "#")          = "#A"
    * StringUtils.unwrap("A#", "#")          = "A#"
    * }}}
    *
    * @param wrapToken
    *   the String used to unwrap
    * @tparam S
    *   String or Option[String]
    * @return
    *   unwrapped String or the original string if it is not quoted properly with the wrapToken
    */
  def unwrap[S: Adt.CoProducts2[*, String, Option[String]]](wrapToken: S): Option[String] = {
    val token = mapToStrOpt(wrapToken).orNull
    Option(Strings.unwrap(strOrNull, token))
  }

  /** <p>Converts a String to upper case as per [[String# toUpperCase ( )]].</p>
    *
    * <p>A `null` input String returns `null`.</p>
    *
    * {{{
    * none.ops.upperCase  = null
    * "".ops.upperCase    = ""
    * "aBc".ops.upperCase = "ABC"
    * }}}
    *
    * <p><strong>Note:</strong> As described in the documentation for [[String# toUpperCase ( )]], the result of this method is affected by
    * the current locale. For platform-independent case transformations, the method [[# lowerCase ( String, Locale)]] should be used with a
    * specific locale (e.g. [[Locale# ENGLISH]]).</p>
    *
    * @return
    *   the upper cased String, `none` if none String input
    */
  def upperCase: Option[String] = Option(Strings.upperCase(strOrNull))

  /** <p>Converts a String to upper case as per {@link String# toUpperCase ( Locale )} .</p>
    *
    * <p>A `null` input String returns `null`.</p>
    *
    * {{{
    * none.ops.upperCase(Locale.ENGLISH)  = null
    * "".ops.upperCase(Locale.ENGLISH)    = ""
    * "aBc".ops.upperCase(Locale.ENGLISH) = "ABC"
    * }}}
    *
    * @param locale
    *   the locale that defines the case transformation rules, must not be null
    * @return
    *   the upper cased String, `none` if none String input
    */
  def upperCase(locale: Locale): Option[String] = Option(Strings.upperCase(strOrNull, locale))

  /** <p> Wraps a string with a char. </p>
    *
    * {{{
    * none.ops.wrap(*)        = null
    * "".ops.wrap(*)          = ""
    * "ab".ops.wrap('\0')     = "ab"
    * "ab".ops.wrap('x')      = "xabx"
    * "ab".ops.wrap('\'')     = "'ab'"
    * "\"ab\"".ops.wrap('\"') = "\"\"ab\"\""
    * }}}
    *
    * @param wrapChar
    *   the char that will wrap `str`
    * @return
    *   the wrapped string, or `null` if `str==null`
    */
  def wrap(wrapChar: Char): Option[String] = Option(Strings.wrap(strOrNull, wrapChar))

  /** <p> Wraps a String with another String. </p>
    *
    * <p> A `null` input String returns `null`. </p>
    *
    * {{{
    * none.ops.wrap(*)         = null
    * "".ops.wrap(*)           = ""
    * "ab".ops.wrap(null)      = "ab"
    * "ab".ops.wrap("x")       = "xabx"
    * "ab".ops.wrap("\"")      = "\"ab\""
    * "\"ab\"".ops.wrap("\"")  = "\"\"ab\"\""
    * "ab".ops.wrap("'")       = "'ab'"
    * "'abcd'".ops.wrap("'")   = "''abcd''"
    * "\"abcd\"".ops.wrap("'") = "'\"abcd\"'"
    * "'abcd'".ops.wrap("\"")  = "\"'abcd'\""
    * }}}
    *
    * @param wrapWith
    *   the String that will wrap str
    * @tparam S
    *   String or Option[String]
    * @return
    *   wrapped String, `None` if none String input
    */
  def wrap[S: Adt.CoProducts2[*, String, Option[String]]](wrapWith: S): Option[String] = {
    val token = mapToStrOpt(wrapWith).orNull
    Option(Strings.wrap(strOrNull, token))
  }

  /** <p> Wraps a string with a char if that char is missing from the start or end of the given string. </p>
    *
    * <p>A new `String` will not be created if `str`is already wrapped.</p>
    *
    * {{{
    * none.ops.wrapIfMissing(*)        = null
    * "".ops.wrapIfMissing(*)          = ""
    * "ab".ops.wrapIfMissing('\0')     = "ab"
    * "ab".ops.wrapIfMissing('x')      = "xabx"
    * "ab".ops.wrapIfMissing('\'')     = "'ab'"
    * "\"ab\"".ops.wrapIfMissing('\"') = "\"ab\""
    * "/".ops.wrapIfMissing('/')  = "/"
    * "a/b/c".ops.wrapIfMissing('/')  = "/a/b/c/"
    * "/a/b/c".ops.wrapIfMissing('/')  = "/a/b/c/"
    * "a/b/c/".ops.wrapIfMissing('/')  = "/a/b/c/"
    * }}}
    *
    * @param wrapChar
    *   the char that will wrap `str`
    * @return
    *   the wrapped string, or `null` if `str==null`
    */
  def wrapIfMissing(wrapChar: Char): Option[String] = Option(Strings.wrapIfMissing(strOrNull, wrapChar))

  /** <p> Wraps a string with a string if that string is missing from the start or end of the given string. </p>
    *
    * <p>A new `String` will not be created if `str`is already wrapped.</p>
    *
    * {{{
    * none.ops.wrapIfMissing(*)         = null
    * "".ops.wrapIfMissing(*)           = ""
    * "ab".ops.wrapIfMissing(none)      = "ab"
    * "ab".ops.wrapIfMissing("x")       = "xabx"
    * "ab".ops.wrapIfMissing("\"")      = "\"ab\""
    * "\"ab\"".ops.wrapIfMissing("\"")  = "\"ab\""
    * "ab".ops.wrapIfMissing("'")       = "'ab'"
    * "'abcd'".ops.wrapIfMissing("'")   = "'abcd'"
    * "\"abcd\"".ops.wrapIfMissing("'") = "'\"abcd\"'"
    * "'abcd'".ops.wrapIfMissing("\"")  = "\"'abcd'\""
    * "/".ops.wrapIfMissing("/")  = "/"
    * "a/b/c".ops.wrapIfMissing("/")  = "/a/b/c/"
    * "/a/b/c".ops.wrapIfMissing("/")  = "/a/b/c/"
    * "a/b/c/".ops.wrapIfMissing("/")  = "/a/b/c/"
    * }}}
    *
    * @param wrapWith
    *   the string that will wrap `str`
    * @tparam S
    *   String or Option[String]
    * @return
    *   the wrapped string, or `null` if `str==null`
    */
  def wrapIfMissing[S: Adt.CoProducts2[*, String, Option[String]]](wrapWith: S): Option[String] = {
    val token = mapToStrOpt(wrapWith).orNull
    Option(Strings.wrapIfMissing(strOrNull, token))
  }
}
