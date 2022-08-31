package commons.lang3.bridge

import org.apache.commons.lang3.{StringUtils => Strings}
import commons.lang3.bridge.{TypeMappingInnerHelper => helper}
import scala.reflect.ClassTag

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

  private def strToOpt[U: StrToOpt](t: U): Option[String] = {
    val mapping = TypeMapping.getMapping[StrToOpt, U]
    mapping.input(t).fold(Option(_), identity)
  }
  private def strOpt: Option[String] = strToOpt(value)

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
  def abbreviate[Abb: StrToOpt](abbrevMarker: Abb, maxWidth: Int): Option[String] =
    Option(Strings.abbreviate(strOpt.orNull, strToOpt(abbrevMarker).orNull, maxWidth))

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
  def abbreviate[Abb: StrToOpt](abbrevMarker: Abb, offset: Int, maxWidth: Int): Option[String] =
    Option(Strings.abbreviate(strOpt.orNull, strToOpt(abbrevMarker).orNull, offset, maxWidth))

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
  def abbreviateMiddle[M: StrToOpt](middle: M, length: Int): Option[String] =
    Option(Strings.abbreviateMiddle(strOpt.orNull, strToOpt(middle).orNull, length))

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
  def appendIfMissing[S: StrToOpt](suffix: S, suffixes: CharSequence*): Option[String] =
    Option(Strings.appendIfMissing(strOpt.orNull, strToOpt(suffix).orNull, suffixes: _*))

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
  def appendIfMissingIgnoreCase[S: StrToOpt](suffix: S, suffixes: CharSequence*): Option[String] =
    Option(Strings.appendIfMissingIgnoreCase(strOpt.orNull, strToOpt(suffix).orNull, suffixes: _*))

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
  def center[P: StrToOpt](size: Int, padStr: P): Option[String] =
    Option(Strings.center(strOpt.orNull, size, strToOpt(padStr).orNull))

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
  def compare[O: StrToOpt](other: O): Int = Strings.compare(strOpt.orNull, strToOpt(other).orNull)

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
  def compare[O: StrToOpt](other: O, nullIsNull: Boolean): Int =
    Strings.compare(strOpt.orNull, strToOpt(other).orNull, nullIsNull)

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
  def compareIgnoreCase[O: StrToOpt](other: O): Int =
    Strings.compareIgnoreCase(strOpt.orNull, strToOpt(other).orNull)

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
  def compareIgnoreCase[O: StrToOpt](other: O, nullIsLess: Boolean): Int =
    Strings.compareIgnoreCase(strOpt.orNull, strToOpt(other).orNull, nullIsLess)

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
  def contains[To: StrToOpt](searchSeq: To): Boolean =
    Strings.contains(strOpt.orNull, strToOpt(searchSeq).orNull)

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
  def containsAny[S: VarArgsOfCharOrString](searchArgs: S*)(implicit tt: ClassTag[S]): Boolean = {
    val mapping = TypeMapping.getMapping[VarArgsOfCharOrString, S]
    mapping
      .input(searchArgs)
      .fold(
        chars => Strings.containsAny(strOpt.orNull, chars: _*),
        css =>
          if (css.length == 1) { Strings.containsAny(strOpt.orNull, css.head) }
          else { Strings.containsAny(strOpt.orNull, css: _*) },
        chars => Strings.containsAny(strOpt.orNull, chars.filter(_.isDefined).map(_.get): _*),
        oss =>
          if (oss.length == 1) { Strings.containsAny(strOpt.orNull, oss.head.orNull) }
          else { Strings.containsAny(strOpt.orNull, oss.map(_.orNull): _*) }
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
<<<<<<< HEAD
    * Some("abcd").ops.containsAnyIgnoreCase(Some("ab"), Some("cd")) = true
    *
    * "abc".ops.containsAnyIgnoreCase("d", "abc") = true
    *
    * "abc".ops.containsAnyIgnoreCase("D", "ABC") = true
    *
    * "ABC".ops.containsAnyIgnoreCase( "d", "abc") = true
    *
    * </pre>
=======
    * None.containsIgnoreCase(*) = false
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
>>>>>>> 344c8d7 (string utils contains ignore case)
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
  def containsAnyIgnoreCase[S: VarArgsOfString](searchArgs: S*)(implicit tt: ClassTag[S]): Boolean = {
    val mapping = TypeMapping.getMapping[VarArgsOfString, S]
    mapping
      .input(searchArgs)
      .fold(
        strs => Strings.containsAnyIgnoreCase(strOpt.orNull, strs: _*),
        oss => Strings.containsAnyIgnoreCase(strOpt.orNull, oss.map(_.orNull): _*)
      )
  }

  /** <p>Checks if CharSequence contains a search CharSequence irrespective of case, handling {@code null}. Case-insensitivity is defined as
    * by {@link String# equalsIgnoreCase ( String )}.
    *
    * <p>A {@code null} CharSequence will return {@code false}.</p>
    *
    * <pre> None.ops.containsIgnoreCase(*) = false
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
<<<<<<< HEAD
    *   the CharSequence to find, may be null
    * @tparam S
    *   String Or Option[String]
    * @return
    *   true if the CharSequence contains the search CharSequence irrespective of case or false if not or {@code null} string input
    */
  def containsIgnoreCase[S: StrToOpt](searchStr: S): Boolean =
    Strings.containsIgnoreCase(strOpt.orNull, strToOpt(searchStr).orNull)

=======
    * @tparam S
    * @return
    */
  def containsIgnoreCase[S: StrToOpt](searchStr: S): Boolean =
    Strings.containsIgnoreCase(strOpt.orNull, strToOpt(searchStr).orNull)
>>>>>>> 344c8d7 (string utils contains ignore case)
}
