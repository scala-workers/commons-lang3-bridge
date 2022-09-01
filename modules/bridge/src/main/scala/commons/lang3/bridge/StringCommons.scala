package commons.lang3.bridge

import commons.lang3.bridge.{TypeMappingInnerHelper => helper}
import org.apache.commons.lang3.{CharSet, StringUtils => Strings}

import java.util.Locale
import java.util.function.Supplier

/** TODO
  *
  * @author
  *   mars
  * @version 1.0.0
  * @since 2022/08/28
  *   21:04
  */
class StringCommons[T: StrToOpt](value: T) {

  import helper._
  import privateUtils._

  private object privateUtils {

    @inline def getMapper[I, O](implicit map: SingleTypeMap[I, O]): SingleTypeMap[I, O] = map

    trait SingleTypeMap[I, O] {
      protected def input(i: I): O
      def func: I => O = i => this.input(i)
    }

    object SingleTypeMap {

      implicit def toStrOpt[U: StrToOpt]: SingleTypeMap[U, Option[String]] = {
        val mapping = TypeMapping.getMapping[StrToOpt, U]
        t => mapping.input(t).fold(Option(_), identity)
      }

      implicit def seqOptionCharToSeqChar: SingleTypeMap[Seq[Option[Char]], Seq[Char]] = tranCharSeqOptFunc
      implicit def seqOptionCharSequenceToSeqCharSequence: SingleTypeMap[Seq[Option[CharSequence]], Seq[CharSequence]] =
        tranCharSeqSeqOptFunc
    }

    private def strToOpt[U: StrToOpt](t: U): Option[String] = {
      val mapping = TypeMapping.getMapping[StrToOpt, U]
      mapping.input(t).fold(Option(_), identity)
    }

    private def tranCharSeqOptFunc(seq: Seq[Option[Char]]): Seq[Char] = seq.filter(_.isDefined).map(_.get)

    private def tranCharSeqSeqOptFunc(seq: Seq[Option[CharSequence]]): Seq[CharSequence] = seq.map(_.orNull)
  }

  private def strOpt: Option[String] = {
    val toStrOpt = getMapper[T, Option[String]].func
    toStrOpt(value)
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
  def abbreviate(maxWidth: Int): Option[String] = Option(Strings.abbreviate(strOpt.orNull, maxWidth))

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
  def abbreviate(offset: Int, maxWidth: Int): Option[String] =
    Option(Strings.abbreviate(strOpt.orNull, offset, maxWidth))

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
  def abbreviate[Abb: StrToOpt](abbrevMarker: Abb, maxWidth: Int): Option[String] = {
    val toStrOpt = getMapper[Abb, Option[String]].func
    Option(Strings.abbreviate(strOpt.orNull, toStrOpt(abbrevMarker).orNull, maxWidth))
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
  def abbreviate[Abb: StrToOpt](abbrevMarker: Abb, offset: Int, maxWidth: Int): Option[String] = {
    val toStrOpt = getMapper[Abb, Option[String]].func
    Option(Strings.abbreviate(strOpt.orNull, toStrOpt(abbrevMarker).orNull, offset, maxWidth))
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
  def abbreviateMiddle[M: StrToOpt](middle: M, length: Int): Option[String] = {
    val toStrOpt = getMapper[M, Option[String]].func
    Option(Strings.abbreviateMiddle(strOpt.orNull, toStrOpt(middle).orNull, length))
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
  def appendIfMissing[S: StrToOpt](suffix: S, suffixes: CharSequence*): Option[String] = {
    val toStrOpt = getMapper[S, Option[String]].func
    Option(Strings.appendIfMissing(strOpt.orNull, toStrOpt(suffix).orNull, suffixes: _*))
  }

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
  def appendIfMissingIgnoreCase[S: StrToOpt](suffix: S, suffixes: CharSequence*): Option[String] = {
    val toStrOpt = getMapper[S, Option[String]].func
    Option(Strings.appendIfMissingIgnoreCase(strOpt.orNull, toStrOpt(suffix).orNull, suffixes: _*))
  }

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
  def capitalize: Option[String] = Option(Strings.capitalize(strOpt.orNull))

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
  def center(size: Int): Option[String] = Option(Strings.center(strOpt.orNull, size))

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
  def center(size: Int, padChar: Char): Option[String] =
    Option(Strings.center(strOpt.orNull, size, padChar))

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
  def center[P: StrToOpt](size: Int, padStr: P): Option[String] = {
    val toStrOpt = getMapper[P, Option[String]].func
    Option(Strings.center(strOpt.orNull, size, toStrOpt(padStr).orNull))
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
  def chomp: Option[String] = Option(Strings.chomp(strOpt.orNull))

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
  def chop: Option[String] = Option(Strings.chop(strOpt.orNull))

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
  def compare[O: StrToOpt](other: O): Int = {
    val toStrOpt = getMapper[O, Option[String]].func
    Strings.compare(strOpt.orNull, toStrOpt(other).orNull)
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
  def compare[O: StrToOpt](other: O, nullIsNull: Boolean): Int = {
    val toStrOpt = getMapper[O, Option[String]].func
    Strings.compare(strOpt.orNull, toStrOpt(other).orNull, nullIsNull)
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
  def compareIgnoreCase[O: StrToOpt](other: O): Int = {
    val toStrOpt = getMapper[O, Option[String]].func
    Strings.compareIgnoreCase(strOpt.orNull, toStrOpt(other).orNull)
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
  def compareIgnoreCase[O: StrToOpt](other: O, nullIsLess: Boolean): Int = {
    val toStrOpt = getMapper[O, Option[String]].func
    Strings.compareIgnoreCase(strOpt.orNull, toStrOpt(other).orNull, nullIsLess)
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
  def contains[To: StrToOpt](searchSeq: To): Boolean = {
    val toStrOpt = getMapper[To, Option[String]].func
    Strings.contains(strOpt.orNull, toStrOpt(searchSeq).orNull)
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
  def contains(searchChar: Char): Boolean = Strings.contains(strOpt.orNull, searchChar)

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
    def dealWithSeqChar(chars: Seq[Char]): Boolean = Strings.containsAny(strOpt.orNull, chars.toArray[Char]: _*)

    def dealWithSeqCharSequence(css: Seq[CharSequence]): Boolean = if (css.length == 1)
      Strings.containsAny(strOpt.orNull, css.head)
    else
      Strings.containsAny(strOpt.orNull, css: _*)

    def mapping             = TypeMapping.getMapping[VarArgsOfCharOrString, S]
    def charOptSeqMapper    = getMapper[Seq[Option[Char]], Seq[Char]].func
    def charSeqOptSeqMapper = getMapper[Seq[Option[CharSequence]], Seq[CharSequence]].func

    if (searchArgs == null)
      Strings.containsAny(strOpt.orNull, null)
    else
      mapping
        .input(searchArgs)
        .fold(
          dealWithSeqChar,
          dealWithSeqCharSequence,
          charOptSeqMapper.andThen(dealWithSeqChar),
          charSeqOptSeqMapper.andThen(dealWithSeqCharSequence)
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
  def containsAnyIgnoreCase[S: VarArgsOfString](searchArgs: S*): Boolean = {
    def dealWithSeqCharSeq(strs: Seq[CharSequence]) = Strings.containsAnyIgnoreCase(strOpt.orNull, strs: _*)
    def mapping                                     = TypeMapping.getMapping[VarArgsOfString, S]
    def charSeqSeqOptMappper                        = getMapper[Seq[Option[CharSequence]], Seq[CharSequence]].func

    if (searchArgs == null)
      Strings.equalsAnyIgnoreCase(strOpt.orNull, null)
    else
      mapping
        .input(searchArgs)
        .fold(
          dealWithSeqCharSeq,
          charSeqSeqOptMappper.andThen(dealWithSeqCharSeq)
        )
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
  def containsIgnoreCase[S: StrToOpt](searchStr: S): Boolean = {
    val toStrOpt = getMapper[S, Option[String]].func
    Strings.containsIgnoreCase(strOpt.orNull, toStrOpt(searchStr).orNull)
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
  def containsNone(invalidChars: String): Boolean =
    Strings.containsNone(strOpt.orNull, invalidChars)

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
  def containsNone(invalidChars: Option[String]): Boolean =
    Strings.containsNone(strOpt.orNull, invalidChars.orNull)

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
    def dealWithSeqChar(chars: Seq[Char]): Boolean = Strings.containsNone(strOpt.orNull, chars: _*)
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
  def containsOnly(validChars: String): Boolean =
    Strings.containsOnly(strOpt.orNull, validChars)

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
  def containsOnly(validChars: Option[String]): Boolean =
    Strings.containsOnly(strOpt.orNull, validChars.orNull)

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
    val mapping = TypeMapping.getMapping[VarArgsOfChar, V]
    mapping
      .input(valid)
      .fold(
        chars => Strings.containsOnly(strOpt.orNull, chars: _*),
        ocs => Strings.containsOnly(strOpt.orNull, ocs.filter(_.isDefined).map(_.get): _*)
      )
  }

  /** <p>Check whether the given CharSequence contains any whitespace characters.</p>
    *
    * <p>Whitespace is defined by {@link Character# isWhitespace ( char )}.</p>
    *
    * @return
    *   if the CharSequence is not empty and contains at least 1 (breaking) whitespace character
    */
  def containsWhitespace: Boolean = Strings.containsWhitespace(strOpt.orNull)

  def countMatches(ch: Char): Int = Strings.countMatches(strOpt.orNull, ch)

  def countMatches[S: StrToOpt](sub: S): Int = {
    val mapping = TypeMapping.getMapping[StrToOpt, S]
    mapping
      .input(sub)
      .fold(
        sub => Strings.countMatches(strOpt.orNull, sub),
        os => Strings.countMatches(strOpt.orNull, os.orNull)
      )
  }

  def defaultIfBlank[S: StrToOpt](defaultStr: S): Option[String] = {
    val mapping = TypeMapping.getMapping[StrToOpt, S]
    val result = mapping
      .input(defaultStr)
      .fold(
        str => Strings.defaultIfBlank(strOpt.orNull, str),
        os => Strings.defaultIfBlank(strOpt.orNull, os.orNull)
      )
    Option(result)
  }

  def defaultIfEmpty[S: StrToOpt](defaultStr: S): Option[String] = {
    val mapping = TypeMapping.getMapping[StrToOpt, S]
    val result = mapping
      .input(defaultStr)
      .fold(
        str => Strings.defaultIfEmpty(strOpt.orNull, str),
        os => Strings.defaultIfEmpty(strOpt.orNull, os.orNull)
      )
    Option(result)
  }

  def defaultString: String = Strings.defaultString(strOpt.orNull)

  def defaultString[S: StrToOpt](defaultStr: S): String = {
    val mapping = TypeMapping.getMapping[StrToOpt, S]
    val result = mapping
      .input(defaultStr)
      .fold(
        str => Strings.defaultString(strOpt.orNull, str),
        os => Strings.defaultString(strOpt.orNull, os.orNull)
      )
    result
  }

  def orDefault: Option[String] = Some(Strings.defaultString(strOpt.orNull))

  def orDefault[S: StrToOpt](defaultStr: S): Option[String] = {
    val mapping = TypeMapping.getMapping[StrToOpt, S]
    val result = mapping
      .input(defaultStr)
      .fold(
        str => Strings.defaultString(strOpt.orNull, str),
        os => Strings.defaultString(strOpt.orNull, os.orNull)
      )
    Option(result)
  }

  def deleteWhitespace(): Option[String] = strOpt.map(Strings.deleteWhitespace)

  def difference[S: StrToOpt](other: S): Option[String] = {
    val mapping = TypeMapping.getMapping[StrToOpt, S]
    val result = mapping
      .input(other)
      .fold(
        str => Strings.difference(strOpt.orNull, str),
        os => Strings.difference(strOpt.orNull, os.orNull)
      )
    Option(result)
  }

  def endsWith[S: StrToOpt](suffix: S): Boolean = {
    val mapping = TypeMapping.getMapping[StrToOpt, S]
    mapping
      .input(suffix)
      .fold(
        str => Strings.endsWith(strOpt.orNull, str),
        os => Strings.endsWith(strOpt.orNull, os.orNull)
      )
  }

  def endWithAny[S: VarArgsOfString](searchStrings: S*): Boolean = {
    if (searchStrings == null) {
      return Strings.endsWithAny(strOpt.orNull, null)
    }

    val mapping = TypeMapping.getMapping[VarArgsOfString, S]
    mapping
      .input(searchStrings)
      .fold(
        strs => Strings.endsWithAny(strOpt.orNull, strs: _*),
        os => Strings.endsWithAny(strOpt.orNull, os.map(_.orNull): _*)
      )
  }

  def equalsAnyIgnoreCase[S: VarArgsOfString](searchStrings: S*): Boolean = {
    if (searchStrings == null) {
      return Strings.equalsAnyIgnoreCase(strOpt.orNull, null)
    }

    val mapping = TypeMapping.getMapping[VarArgsOfString, S]
    mapping
      .input(searchStrings)
      .fold(
        strs => Strings.equalsAnyIgnoreCase(strOpt.orNull, strs: _*),
        os => Strings.equalsAnyIgnoreCase(strOpt.orNull, os.map(_.orNull): _*)
      )
  }

  def equalsIgnoreCase[S: StrToOpt](other: S): Boolean = {
    val mapping = TypeMapping.getMapping[StrToOpt, S]
    mapping
      .input(other)
      .fold(
        str => Strings.equalsIgnoreCase(strOpt.orNull, str),
        os => Strings.equalsIgnoreCase(strOpt.orNull, os.orNull)
      )
  }

  def getBytes[C: CharsetOrName](charset: C): Array[Byte] = {
    val mapping = TypeMapping.getMapping[CharsetOrName, C]
    mapping
      .input(charset)
      .fold(
        chs => Strings.getBytes(strOpt.orNull, chs),
        ochs => Strings.getBytes(strOpt.orNull, ochs.orNull),
        name => Strings.getBytes(strOpt.orNull, name),
        optn => Strings.getBytes(strOpt.orNull, optn.orNull)
      )
  }

  def getDigits: String =
    Strings.getDigits(strOpt.orNull)

  def getIfBlank(defaultSupplier: Supplier[String]): String =
    Strings.getIfBlank(strOpt.orNull, defaultSupplier)

  def getIfEmpty(defaultSupplier: Supplier[String]): String =
    Strings.getIfEmpty(strOpt.orNull, defaultSupplier)

  def indexOf[S: StrToOpt](searchSeq: S): Int = {
    val mapping = TypeMapping.getMapping[StrToOpt, S]
    mapping
      .input(searchSeq)
      .fold(
        strs => Strings.indexOf(strOpt.orNull, strs),
        os => Strings.indexOf(strOpt.orNull, os.orNull)
      )
  }

  def indexOf(searchChar: Char): Int = Strings.indexOf(strOpt.orNull, searchChar)

  def indexOfAny[S: VarArgsOfCharOrString](searchArgs: S*): Int = {
    if (searchArgs == null) {
      return Strings.indexOfAny(strOpt.orNull, null)
    }
    if (searchArgs.length == 1) {
      searchArgs.head match {
        case c: Char   => Strings.indexOfAny(strOpt.orNull, c)
        case s: String => Strings.indexOfAny(strOpt.orNull, s)
        case opt: Option[_] =>
          opt match {
            case Some(c: Char)     => Strings.indexOfAny(strOpt.orNull, c)
            case Some(str: String) => Strings.indexOfAny(strOpt.orNull, str)
            case _                 => Strings.indexOfAny(strOpt.orNull, null)
          }
      }
    }

    val mapping = TypeMapping.getMapping[VarArgsOfCharOrString, S]
    mapping
      .input(searchArgs)
      .fold(
        chars => Strings.indexOfAny(strOpt.orNull, chars: _*),
        strings => Strings.indexOfAny(strOpt.orNull, strings: _*),
        ocs => Strings.indexOfAny(strOpt.orNull, ocs.filter(_.isDefined).map(_.get): _*),
        oss => Strings.indexOfAny(strOpt.orNull, oss.map(_.orNull): _*)
      )
  }

  def indexOfAnyBut[S: VarArgsOfChar](searchChars: S*): Int = {
    if (searchChars == null) {
      return Strings.indexOfAnyBut(strOpt.orNull, null)
    }

    val mapping = TypeMapping.getMapping[VarArgsOfChar, S]
    mapping
      .input(searchChars)
      .fold(
        chars =>
          if (chars.length == 1) { Strings.indexOfAnyBut(strOpt.orNull, chars.head) }
          else { Strings.indexOfAnyBut(strOpt.orNull, chars: _*) },
        ocs =>
          if (ocs.length == 1) { Strings.indexOfAnyBut(strOpt.orNull, ocs.head.get) }
          else { Strings.indexOfAnyBut(strOpt.orNull, ocs.filter(_.isDefined).map(_.get): _*) }
      )
  }

  def indexOfDifference[S: StrToOpt](cs: S): Int = {
    val mapping = TypeMapping.getMapping[StrToOpt, S]
    mapping
      .input(cs)
      .fold(
        str => Strings.indexOfDifference(strOpt.orNull, str),
        os => Strings.indexOfDifference(strOpt.orNull, os.orNull)
      )
  }

  def indexOfIgnoreCase[S: StrToOpt](searchStr: S): Int = {
    val mapping = TypeMapping.getMapping[StrToOpt, S]
    mapping
      .input(searchStr)
      .fold(
        str => Strings.indexOfIgnoreCase(strOpt.orNull, str),
        os => Strings.indexOfIgnoreCase(strOpt.orNull, os.orNull)
      )
  }

  def indexOfIgnoreCase[S: StrToOpt](searchStr: S, startPos: Int): Int = {
    val mapping = TypeMapping.getMapping[StrToOpt, S]
    mapping
      .input(searchStr)
      .fold(
        str => Strings.indexOfIgnoreCase(strOpt.orNull, str, startPos),
        os => Strings.indexOfIgnoreCase(strOpt.orNull, os.orNull, startPos)
      )
  }

  def isAlpha: Boolean = Strings.isAlpha(strOpt.orNull)

  def isAlphanumeric: Boolean = Strings.isAlphanumeric(strOpt.orNull)

  def isAlphanumericSpace: Boolean = Strings.isAlphanumericSpace(strOpt.orNull)

  def isAlphaSpace: Boolean = Strings.isAlphaSpace(strOpt.orNull)

  def isAsciiPrintable: Boolean = Strings.isAsciiPrintable(strOpt.orNull)

  def isBlank: Boolean = Strings.isBlank(strOpt.orNull)

  def isEmpty: Boolean = Strings.isEmpty(strOpt.orNull)

  def isMixedCase: Boolean = Strings.isMixedCase(strOpt.orNull)

  def isNotBlank: Boolean = Strings.isNotBlank(strOpt.orNull)

  def isNotEmpty: Boolean = Strings.isNotEmpty(strOpt.orNull)

  def isNumeric: Boolean = Strings.isNumeric(strOpt.orNull)

  def isNumericSpace: Boolean = Strings.isNumericSpace(strOpt.orNull)

  def isWhitespace: Boolean = Strings.isWhitespace(strOpt.orNull)

  def join(arr: Array[Object]): String = Strings.join(arr, strOpt.orNull)

  def joinWith(objs: Object*): String = {
    if (objs == null) {
      return Strings.joinWith(strOpt.orNull, null)
    }
    Strings.joinWith(strOpt.orNull, objs: _*)
  }

  def lastIndexOf[S: CharOrString](searchArg: S): Int = {
    val mapping = TypeMapping.getMapping[CharOrString, S]
    mapping
      .input(searchArg)
      .fold(
        ch => Strings.lastIndexOf(strOpt.orNull, ch),
        str => Strings.lastIndexOf(strOpt.orNull, str),
        ostr => Strings.indexOf(strOpt.orNull, ostr.orNull)
      )
  }

  def lastIndexOf[S: CharOrString](searchArg: S, startPos: Int): Int = {
    val mapping = TypeMapping.getMapping[CharOrString, S]
    mapping
      .input(searchArg)
      .fold(
        ch => Strings.lastIndexOf(strOpt.orNull, ch, startPos),
        str => Strings.lastIndexOf(strOpt.orNull, str, startPos),
        ostr => Strings.indexOf(strOpt.orNull, ostr.orNull, startPos)
      )
  }

  def lastIndexOfAny[S: VarArgsOfString](searchArgs: S*): Int = {
    if (searchArgs == null) {
      return Strings.lastIndexOfAny(strOpt.orNull, null)
    }
    val mapping = TypeMapping.getMapping[VarArgsOfString, S]
    mapping
      .input(searchArgs)
      .fold(
        strs => Strings.lastIndexOfAny(strOpt.orNull, strs: _*),
        ostrs => Strings.lastIndexOfAny(strOpt.orNull, ostrs.map(_.orNull): _*)
      )
  }

  def lastIndexOfIgnoreCase[S: StrToOpt](searchStr: S): Int = {
    val mapping = TypeMapping.getMapping[StrToOpt, S]
    mapping
      .input(searchStr)
      .fold(
        str => Strings.lastIndexOfIgnoreCase(strOpt.orNull, str),
        ostr => Strings.lastIndexOfIgnoreCase(strOpt.orNull, ostr.orNull)
      )
  }

  def lastIndexOfIgnoreCase[S: StrToOpt](searchStr: S, startPos: Int): Int = {
    val mapping = TypeMapping.getMapping[StrToOpt, S]
    mapping
      .input(searchStr)
      .fold(
        str => Strings.lastIndexOfIgnoreCase(strOpt.orNull, str, startPos),
        ostr => Strings.lastIndexOfIgnoreCase(strOpt.orNull, ostr.orNull, startPos)
      )
  }

  def lastOrdinalIndexOf[S: StrToOpt](searchStr: S, ordinal: Int): Int = {
    val mapping = TypeMapping.getMapping[StrToOpt, S]
    mapping
      .input(searchStr)
      .fold(
        str => Strings.lastOrdinalIndexOf(strOpt.orNull, str, ordinal),
        ostr => Strings.lastOrdinalIndexOf(strOpt.orNull, ostr.orNull, ordinal)
      )
  }

  def left(len: Int): Option[String] = Option(Strings.left(strOpt.orNull, len))

  def leftPad(size: Int): Option[String] = Option(Strings.leftPad(strOpt.orNull, size))

  def leftPad(size: Int, padChar: Char): Option[String] = Option(Strings.leftPad(strOpt.orNull, size, padChar))

  def leftPad[P: StrToOpt](size: Int, padStr: P): Option[String] = {
    val mapping = TypeMapping.getMapping[StrToOpt, P]
    val ps      = mapping.input(padStr).fold(identity, _.orNull)
    Option(Strings.leftPad(strOpt.orNull, size, ps))
  }

  def length: Int = Strings.length(strOpt.orNull)

  def lowerCase: Option[String] = Option(Strings.lowerCase(strOpt.orNull))

  def lowerCase(locale: Locale): Option[String] = Option(Strings.lowerCase(strOpt.orNull, locale))

  def mid(pos: Int, len: Int): String = Strings.mid(strOpt.orNull, pos, len)

  def normalizeSpace: String = Strings.normalizeSpace(strOpt.orNull)

  def ordinalIndexOf[S: StrToOpt](searchStr: S, ordinal: Int): Int = {
    val mapping = TypeMapping.getMapping[StrToOpt, S]
    mapping
      .input(searchStr)
      .fold(
        str => Strings.ordinalIndexOf(strOpt.orNull, str, ordinal),
        ostr => Strings.ordinalIndexOf(strOpt.orNull, ostr.orNull, ordinal)
      )
  }

  def overlay[O: StrToOpt](overlay: O, start: Int, end: Int): Option[String] = {
    val mapping = TypeMapping.getMapping[StrToOpt, O]
    val result = mapping
      .input(overlay)
      .fold(
        str => Strings.overlay(strOpt.orNull, str, start, end),
        ostr => Strings.overlay(strOpt.orNull, ostr.orNull, start, end)
      )
    Option(result)
  }

  def prependIfMissing[P: StrToOpt, Ps: VarArgsOfString](prefix: P, prefixes: Ps*): Option[String] = {
    val prefixMapping = TypeMapping.getMapping[StrToOpt, P]
    val prefixStr     = prefixMapping.input(prefix).fold(identity, _.orNull)
    if (prefixes == null) {
      return Option(Strings.prependIfMissing(strOpt.orNull, prefixStr, null))
    }
    val prefixesMapping = TypeMapping.getMapping[VarArgsOfString, Ps]
    val result = prefixesMapping
      .input(prefixes)
      .fold(
        strs => Strings.prependIfMissing(strOpt.orNull, prefixStr, strs: _*),
        ostrs => Strings.prependIfMissing(strOpt.orNull, prefixStr, ostrs.map(_.orNull): _*)
      )
    Option(result)
  }

  def prependIfMissingIgnoreCase[P: StrToOpt, Ps: VarArgsOfString](prefix: P, prefixes: Ps*): Option[String] = {
    val prefixMapping = TypeMapping.getMapping[StrToOpt, P]
    val prefixStr     = prefixMapping.input(prefix).fold(identity, _.orNull)
    if (prefixes == null) {
      return Option(Strings.prependIfMissingIgnoreCase(strOpt.orNull, prefixStr, null))
    }
    val prefixesMapping = TypeMapping.getMapping[VarArgsOfString, Ps]
    val result = prefixesMapping
      .input(prefixes)
      .fold(
        strs => Strings.prependIfMissingIgnoreCase(strOpt.orNull, prefixStr, strs: _*),
        ostrs => Strings.prependIfMissingIgnoreCase(strOpt.orNull, prefixStr, ostrs.map(_.orNull): _*)
      )
    Option(result)
  }

  def remove(rmv: Char): Option[String] = Option(Strings.remove(strOpt.orNull, rmv))

  def remove[R: StrToOpt](rmv: R): Option[String] = {
    val mapping = TypeMapping.getMapping[StrToOpt, R]
    val rmvStr  = mapping.input(rmv).fold(identity, _.orNull)
    Option(Strings.remove(strOpt.orNull, rmvStr))
  }

  def removeEnd[R: StrToOpt](rmv: R): Option[String] = {
    val mapping = TypeMapping.getMapping[StrToOpt, R]
    val rmvStr  = mapping.input(rmv).fold(identity, _.orNull)
    Option(Strings.removeEnd(strOpt.orNull, rmvStr))
  }

  def removeEndIgnoreCase[R: StrToOpt](rmv: R): Option[String] = {
    val mapping = TypeMapping.getMapping[StrToOpt, R]
    val rmvStr  = mapping.input(rmv).fold(identity, _.orNull)
    Option(Strings.removeEndIgnoreCase(strOpt.orNull, rmvStr))
  }

  def removeIgnoreCase[R: StrToOpt](rmv: R): Option[String] = {
    val mapping = TypeMapping.getMapping[StrToOpt, R]
    val rmvStr  = mapping.input(rmv).fold(identity, _.orNull)
    Option(Strings.removeIgnoreCase(strOpt.orNull, rmvStr))
  }

  def removeStart[R: StrToOpt](rmv: R): Option[String] = {
    val mapping = TypeMapping.getMapping[StrToOpt, R]
    val rmvStr  = mapping.input(rmv).fold(identity, _.orNull)
    Option(Strings.removeStart(strOpt.orNull, rmvStr))
  }

  def removeStartIgnoreCase[R: StrToOpt](rmv: R): Option[String] = {
    val mapping = TypeMapping.getMapping[StrToOpt, R]
    val rmvStr  = mapping.input(rmv).fold(identity, _.orNull)
    Option(Strings.removeStartIgnoreCase(strOpt.orNull, rmvStr))
  }

  def repeat(rep: Int): Option[String] = Option(Strings.repeat(strOpt.orNull, rep))

  def repeat[S: StrToOpt](separator: S, repeat: Int): Option[String] = {
    val mapping = TypeMapping.getMapping[StrToOpt, S]
    val sep     = mapping.input(separator).fold(identity, _.orNull)
    Option(Strings.repeat(strOpt.orNull, sep, repeat))
  }

  def replace[S: StrToOpt, R: StrToOpt](searchString: S, replacement: R): Option[String] = {
    val searchMapping      = TypeMapping.getMapping[StrToOpt, S]
    val replacementMapping = TypeMapping.getMapping[StrToOpt, R]

    val sstr = searchMapping.input(searchString).fold(identity, _.orNull)
    val rstr = replacementMapping.input(replacement).fold(identity, _.orNull)

    Option(Strings.replace(strOpt.orNull, sstr, rstr))
  }

  def replace[S: StrToOpt, R: StrToOpt](searchString: S, replacement: R, max: Int): Option[String] = {
    val searchMapping      = TypeMapping.getMapping[StrToOpt, S]
    val replacementMapping = TypeMapping.getMapping[StrToOpt, R]

    val sstr = searchMapping.input(searchString).fold(identity, _.orNull)
    val rstr = replacementMapping.input(replacement).fold(identity, _.orNull)

    Option(Strings.replace(strOpt.orNull, sstr, rstr, max))
  }

  def replaceChars(searchChar: Char, replaceChar: Char): Option[String] =
    Option(Strings.replaceChars(strOpt.orNull, searchChar, replaceChar))

  def replaceChars[S: StrToOpt, R: StrToOpt](searchChars: S, replaceChars: R): Option[String] = {
    val searchMapping      = TypeMapping.getMapping[StrToOpt, S]
    val replacementMapping = TypeMapping.getMapping[StrToOpt, R]

    val sstr = searchMapping.input(searchChars).fold(identity, _.orNull)
    val rstr = replacementMapping.input(replaceChars).fold(identity, _.orNull)

    Option(Strings.replaceChars(strOpt.orNull, sstr, rstr))
  }

  def replaceEach(searchList: Array[String], replacementList: Array[String]): Option[String] =
    Option(Strings.replaceEach(strOpt.orNull, searchList, replacementList))

  def replaceEachRepeatedly(searchList: Array[String], replacementList: Array[String]): Option[String] =
    Option(Strings.replaceEachRepeatedly(strOpt.orNull, searchList, replacementList))

  def replaceIgnoreCase[S: StrToOpt, R: StrToOpt](searchString: S, replacement: R, max: Int): Option[String] = {
    val searchMapping      = TypeMapping.getMapping[StrToOpt, S]
    val replacementMapping = TypeMapping.getMapping[StrToOpt, R]

    val sstr = searchMapping.input(searchString).fold(identity, _.orNull)
    val rstr = replacementMapping.input(replacement).fold(identity, _.orNull)

    Option(Strings.replaceIgnoreCase(strOpt.orNull, sstr, rstr, max))
  }

  def replaceOnce[S: StrToOpt, R: StrToOpt](searchString: S, replacement: R): Option[String] = {
    val searchMapping      = TypeMapping.getMapping[StrToOpt, S]
    val replacementMapping = TypeMapping.getMapping[StrToOpt, R]

    val sstr = searchMapping.input(searchString).fold(identity, _.orNull)
    val rstr = replacementMapping.input(replacement).fold(identity, _.orNull)

    Option(Strings.replaceOnce(strOpt.orNull, sstr, rstr))
  }

  def reverse: Option[String] = Option(Strings.reverse(strOpt.orNull))

  def reverseDelimited(separatorChar: Char): Option[String] =
    Option(Strings.reverseDelimited(strOpt.orNull, separatorChar))

  def right(len: Int): Option[String] = Option(Strings.right(strOpt.orNull, len))

  def rightPad(size: Int): Option[String] = Option(Strings.rightPad(strOpt.orNull, size))

  def rightPad(size: Int, padChar: Char): Option[String] = Option(Strings.rightPad(strOpt.orNull, size, padChar))

  def rightPad[P: StrToOpt](size: Int, padStr: P): Option[String] = {
    val mapping = TypeMapping.getMapping[StrToOpt, P]
    val ps      = mapping.input(padStr).fold(identity, _.orNull)
    Option(Strings.rightPad(strOpt.orNull, size, ps))
  }

  def rotate(shift: Int): Option[String] = Option(Strings.rotate(strOpt.orNull, shift))

  def split: Option[Array[String]] = Option(Strings.split(strOpt.orNull))

  def split(separatorChar: Char): Option[Array[String]] = Option(Strings.split(strOpt.orNull, separatorChar))

  def split[S: StrToOpt](separatorChars: S): Option[Array[String]] = {
    val mapping = TypeMapping.getMapping[StrToOpt, S]
    val sep     = mapping.input(separatorChars).fold(identity, _.orNull)
    Option(Strings.split(strOpt.orNull, sep))
  }

  def split[S: StrToOpt](separatorChars: S, max: Int): Option[Array[String]] = {
    val mapping = TypeMapping.getMapping[StrToOpt, S]
    val sep     = mapping.input(separatorChars).fold(identity, _.orNull)
    Option(Strings.split(strOpt.orNull, sep, max))
  }

  def splitByCharacterType: Option[Array[String]] = Option(Strings.splitByCharacterType(strOpt.orNull))

  def splitByCharacterTypeCamelCase: Option[Array[String]] =
    Option(Strings.splitByCharacterTypeCamelCase(strOpt.orNull))

  def splitByWholeSeparator[S: StrToOpt](separatorChars: S): Option[Array[String]] = {
    val mapping = TypeMapping.getMapping[StrToOpt, S]
    val sep     = mapping.input(separatorChars).fold(identity, _.orNull)
    Option(Strings.splitByWholeSeparator(strOpt.orNull, sep))
  }

  def splitByWholeSeparator[S: StrToOpt](separatorChars: S, max: Int): Option[Array[String]] = {
    val mapping = TypeMapping.getMapping[StrToOpt, S]
    val sep     = mapping.input(separatorChars).fold(identity, _.orNull)
    Option(Strings.splitByWholeSeparator(strOpt.orNull, sep, max))
  }

  def splitByWholeSeparatorPreserveAllTokens[S: StrToOpt](separatorChars: S): Option[Array[String]] = {
    val mapping = TypeMapping.getMapping[StrToOpt, S]
    val sep     = mapping.input(separatorChars).fold(identity, _.orNull)
    Option(Strings.splitByWholeSeparatorPreserveAllTokens(strOpt.orNull, sep))
  }

  def splitByWholeSeparatorPreserveAllTokens[S: StrToOpt](separatorChars: S, max: Int): Option[Array[String]] = {
    val mapping = TypeMapping.getMapping[StrToOpt, S]
    val sep     = mapping.input(separatorChars).fold(identity, _.orNull)
    Option(Strings.splitByWholeSeparatorPreserveAllTokens(strOpt.orNull, sep, max))
  }

  def splitPreserveAllTokens: Option[Array[String]] = Option(Strings.splitPreserveAllTokens(strOpt.orNull))

  def splitPreserveAllTokens(separatorChar: Char): Option[Array[String]] =
    Option(Strings.splitPreserveAllTokens(strOpt.orNull, separatorChar))

  def splitPreserveAllTokens[S: StrToOpt](separatorChars: S): Option[Array[String]] = {
    val mapping = TypeMapping.getMapping[StrToOpt, S]
    val sep     = mapping.input(separatorChars).fold(identity, _.orNull)
    Option(Strings.splitPreserveAllTokens(strOpt.orNull, sep))
  }

  def splitPreserveAllTokens[S: StrToOpt](separatorChars: S, max: Int): Option[Array[String]] = {
    val mapping = TypeMapping.getMapping[StrToOpt, S]
    val sep     = mapping.input(separatorChars).fold(identity, _.orNull)
    Option(Strings.splitPreserveAllTokens(strOpt.orNull, sep, max))
  }

  def startsWith[S: StrToOpt](prefix: S): Boolean = {
    val mapping = TypeMapping.getMapping[StrToOpt, S]
    val pre     = mapping.input(prefix).fold(identity, _.orNull)
    Strings.startsWith(strOpt.orNull, pre)
  }

  def startsWithAny[CS: VarArgsOfString](searchStrings: CS*): Boolean = {
    val mapping = TypeMapping.getMapping[VarArgsOfString, CS]
    val strs    = mapping.input(searchStrings).fold(identity, { css => css.map(_.orNull) })
    Strings.startsWithAny(strOpt.orNull, strs: _*)
  }

  def startsWithIgnoreCase[P: StrToOpt](prefix: P): Boolean = {
    val mapping = TypeMapping.getMapping[StrToOpt, P]
    val str     = mapping.input(prefix).fold(identity, _.orNull)
    Strings.startsWithIgnoreCase(strOpt.orNull, str)
  }

  def strip: Option[String] = Option(Strings.strip(strOpt.orNull))

  def strip[S: StrToOpt](stripChars: S): Option[String] = {
    val mapping = TypeMapping.getMapping[StrToOpt, S]
    val chars   = mapping.input(stripChars).fold(identity, _.orNull)
    Option(Strings.strip(strOpt.orNull, chars))
  }

  def stripAccents: Option[String] = Option(Strings.stripAccents(strOpt.orNull))

  def stripEnd[S: StrToOpt](stripChars: S): Option[String] = {
    val mapping = TypeMapping.getMapping[StrToOpt, S]
    val chars   = mapping.input(stripChars).fold(identity, _.orNull)
    Option(Strings.stripEnd(strOpt.orNull, chars))
  }

  def stripStart[S: StrToOpt](stripChars: S): Option[String] = {
    val mapping = TypeMapping.getMapping[StrToOpt, S]
    val chars   = mapping.input(stripChars).fold(identity, _.orNull)
    Option(Strings.stripStart(strOpt.orNull, chars))
  }

  def stripToEmpty: String = Strings.stripToEmpty(strOpt.orNull)

  def stripToNone: Option[String] = Option(Strings.stripToNull(strOpt.orNull))

  def substring(start: Int): Option[String] = Option(Strings.substring(strOpt.orNull, start))

  def substring(start: Int, end: Int): Option[String] = Option(Strings.substring(strOpt.orNull, start, end))

  def substringAfter(separator: Char): Option[String] = Option(Strings.substringAfter(strOpt.orNull, separator))

  def substringAfter[S: StrToOpt](separator: S): Option[String] = {
    val mapping = TypeMapping.getMapping[StrToOpt, S]
    val sep     = mapping.input(separator).fold(identity, _.orNull)
    Option(Strings.substringAfter(strOpt.orNull, sep))
  }

  def substringAfterLast(separator: Char): Option[String] =
    Option(Strings.substringAfterLast(strOpt.orNull, separator))

  def substringAfterLast[S: StrToOpt](separator: S): Option[String] = {
    val mapping = TypeMapping.getMapping[StrToOpt, S]
    val sep     = mapping.input(separator).fold(identity, _.orNull)
    Option(Strings.substringAfterLast(strOpt.orNull, sep))
  }

  def substringBefore(separator: Char): Option[String] = Option(Strings.substringBefore(strOpt.orNull, separator))

  def substringBefore[S: StrToOpt](separator: S): Option[String] = {
    val mapping = TypeMapping.getMapping[StrToOpt, S]
    val sep     = mapping.input(separator).fold(identity, _.orNull)
    Option(Strings.substringBefore(strOpt.orNull, sep))
  }

  def substringBeforeLast[S: StrToOpt](separator: S): Option[String] = {
    val mapping = TypeMapping.getMapping[StrToOpt, S]
    val sep     = mapping.input(separator).fold(identity, _.orNull)
    Option(Strings.substringBeforeLast(strOpt.orNull, sep))
  }

  def substringBetween[S: StrToOpt](tag: S): Option[String] = {
    val mapping = TypeMapping.getMapping[StrToOpt, S]
    val t       = mapping.input(tag).fold(identity, _.orNull)
    Option(Strings.substringBetween(strOpt.orNull, t))
  }

  def substringBetween[S: StrToOpt](open: S, close: S): Option[String] = {
    val mapping = TypeMapping.getMapping[StrToOpt, S]
    val o       = mapping.input(open).fold(identity, _.orNull)
    val c       = mapping.input(close).fold(identity, _.orNull)
    Option(Strings.substringBetween(strOpt.orNull, o, c))
  }

  def substringsBetween[S: StrToOpt](open: S, close: S): Option[Array[String]] = {
    val mapping = TypeMapping.getMapping[StrToOpt, S]
    val o       = mapping.input(open).fold(identity, _.orNull)
    val c       = mapping.input(close).fold(identity, _.orNull)
    Option(Strings.substringsBetween(strOpt.orNull, o, c))
  }

  def swapCase: Option[String] = Option(Strings.swapCase(strOpt.orNull))

  def toCodePoints: Option[Array[Int]] = Option(Strings.toCodePoints(strOpt.orNull))

  def toRootLowerCase: Option[String] = Option(Strings.toRootLowerCase(strOpt.orNull))

  def toRootUpperCase: Option[String] = Option(Strings.toRootUpperCase(strOpt.orNull))

  def trim: Option[String] = Option(Strings.trim(strOpt.orNull))

  def trimToEmpty: Option[String] = Option(Strings.trimToEmpty(strOpt.orNull))

  def trimToNull: Option[String] = Option(Strings.trimToNull(strOpt.orNull))

  def truncate(maxWidth: Int): Option[String] = Option(Strings.truncate(strOpt.orNull, maxWidth))

  def truncate(offset: Int, maxWidth: Int): Option[String] = Option(Strings.truncate(strOpt.orNull, offset, maxWidth))

  def uncapitalize: Option[String] = Option(Strings.uncapitalize(strOpt.orNull))

  def unwrap(wrapChar: Char): Option[String] = Option(Strings.unwrap(strOpt.orNull, wrapChar))

  def unwrap[S: StrToOpt](wrapToken: S): Option[String] = {
    val mapping = TypeMapping.getMapping[StrToOpt, S]
    val token   = mapping.input(wrapToken).fold(identity, _.orNull)
    Option(Strings.unwrap(strOpt.orNull, token))
  }

  def upperCase: Option[String] = Option(Strings.upperCase(strOpt.orNull))

  def upperCase(locale: Locale): Option[String] = Option(Strings.upperCase(strOpt.orNull, locale))

  def wrap(wrapChar: Char): Option[String] = Option(Strings.wrap(strOpt.orNull, wrapChar))

  def wrap[S: StrToOpt](wrapToken: S): Option[String] = {
    val mapping = TypeMapping.getMapping[StrToOpt, S]
    val token   = mapping.input(wrapToken).fold(identity, _.orNull)
    Option(Strings.wrap(strOpt.orNull, token))
  }

  def wrapIfMissing(wrapChar: Char): Option[String] = Option(Strings.wrapIfMissing(strOpt.orNull, wrapChar))

  def wrapIfMissing[S: StrToOpt](wrapToken: S): Option[String] = {
    val mapping = TypeMapping.getMapping[StrToOpt, S]
    val token   = mapping.input(wrapToken).fold(identity, _.orNull)
    Option(Strings.wrapIfMissing(strOpt.orNull, token))
  }
}
