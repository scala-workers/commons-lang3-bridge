package commons.lang3.scala

import org.apache.commons.lang3.{StringUtils => Strings}

/**
 * TODO
 *
 * @author mars
 * @version 1.0.0
 * @since 2022/08/28 21:04
 */
class StringCommons[T: ToStringOpt](value: T) {

  import commons.lang3.scala.StringUtils.bridge._

  /**
   * * <p>Abbreviates a String using ellipses. This will turn
   * "Now is the time for all good men" into "Now is the time for..."</p>
   *
   * {@code str} is less than or equal to
   * {@code maxWidth}, return {@code str}.</li>
   * <li>Else abbreviate it to {@code (substring(str, 0, max-3) + "...")}.</li>
   * <li>If {@code maxWidth} is less than {@code 4}, throw an
   * {@code IllegalArgumentException}.</li>
   * <li>In no case will it return a String of length greater than
   * {@code maxWidth}.</li>
   * </ul>
   *
   * <pre>
   * None.abbreviate(*)      = None
   * None.abbreviate(4)        = Some("")
   * Some("abcdefg").abbreviate(6) = Some("abc...")
   * Some("abcdefg").abbreviate(7) = Some("abcdefg")
   * Some("abcdefg").abbreviate(8) = Some("abcdefg")
   * Some("abcdefg").abbreviate(4) = Some("a...")
   * Some("abcdefg").abbreviate(3) = IllegalArgumentException
   * </pre>
   *
   * @param maxWidth maximum length of result String, must be at least 4
   * @return abbreviated String, {@code None} if None input
   * @throws IllegalArgumentException if the width is too small
   */
  def abbreviate(maxWidth: Int): Option[String] = Option(Strings.abbreviate(value.strOpt.orNull, maxWidth))

  /**
   * <p>Abbreviates a String using ellipses. This will turn
   * "Now is the time for all good men" into "...is the time for..."</p>
   *
   * <p>Works like {@code Option(String).abbreviate(int)}, but allows you to specify
   * a "left edge" offset.  Note that this left edge is not necessarily going to
   * be the leftmost character in the result, or the first character following the
   * ellipses, but it will appear somewhere in the result.
   *
   * <p>In no case will it return a String of length greater than
   * {@code maxWidth}.</p>
   *
   * <pre>
   * None.abbreviate(*, *)                = None
   * Some("").abbreviate(0, 4)                  = Some("")
   * Some("abcdefghijklmno").abbreviate(-1, 10) = Some("abcdefg...")
   * Some("abcdefghijklmno").abbreviate(0, 10)  = Some("abcdefg...")
   * Some("abcdefghijklmno").abbreviate(1, 10)  = Some("abcdefg...")
   * Some("abcdefghijklmno").abbreviate(4, 10)  = Some("abcdefg...")
   * Some("abcdefghijklmno").abbreviate(5, 10)  = Some("...fghi...")
   * Some("abcdefghijklmno").abbreviate(6, 10)  = Some("...ghij...")
   * Some("abcdefghijklmno").abbreviate(8, 10)  = Some("...ijklmno")
   * Some("abcdefghijklmno").abbreviate(10, 10) = Some("...ijklmno")
   * Some("abcdefghijklmno").abbreviate(12, 10) = Some("...ijklmno")
   * Some("abcdefghij").abbreviate(0, 3)        = IllegalArgumentException
   * Some("abcdefghij").abbreviate(5, 6)        = IllegalArgumentException
   * </pre>
   *
   * @param offset   left edge of source String
   * @param maxWidth maximum length of result String, must be at least 4
   * @return abbreviated String Option, {@code None} if None input
   * @throws IllegalArgumentException if the width is too small
   */
  def abbreviate(offset: Int, maxWidth: Int): Option[String] =
    Option(Strings.abbreviate(value.strOpt.orNull, offset, maxWidth))

  /**
   * <p>Abbreviates a String using another given String as replacement marker. This will turn
   * "Now is the time for all good men" into "Now is the time for..." if "..." was defined
   * as the replacement marker.</p>
   *
   * <p>Specifically:</p>
   * <ul>
   * <li>If the number of characters in {@code str} is less than or equal to
   * {@code maxWidth}, return {@code str}.</li>
   * <li>Else abbreviate it to {@code (substring(str, 0, max-abbrevMarker.length) + abbrevMarker)}.</li>
   * <li>If {@code maxWidth} is less than {@code abbrevMarker.length + 1}, throw an
   * {@code IllegalArgumentException}.</li>
   * <li>In no case will it return a String of length greater than
   * {@code maxWidth}.</li>
   * </ul>
   *
   * <pre>
   * None.abbreviate(Some("..."), *)      = None
   * Some("abcdefg").abbreviate(None, *)  = Some("abcdefg")
   * Some("").abbreviate(Some("..."), 4)        = Some("")
   * Some("abcdefg").abbreviate(Some("."), 5)   = Some("abcd.")
   * Some("abcdefg").abbreviate(Some("."), 7)   = Some("abcdefg")
   * Some("abcdefg").abbreviate(Some("."), 8)   = Some("abcdefg")
   * Some("abcdefg").abbreviate(Some(".."), 4)  = Some("ab..")
   * Some("abcdefg").abbreviate(Some(".."), 3)  = Some("a..")
   * Some("abcdefg").abbreviate(Some(".."), 2)  = IllegalArgumentException
   * Some("abcdefg").abbreviate(Some("..."), 3) = IllegalArgumentException
   * </pre>
   *
   * @param abbrevMarker the String used as replacement marker
   * @param maxWidth     maximum length of result String, must be at least {@code abbrevMarker.length + 1}
   * @tparam Abb String or Option[String]
   * @return abbreviated String Option, {@code None} if None input
   * @throws IllegalArgumentException if the width is too small
   */
  def abbreviate[Abb: ToStringOpt](abbrevMarker: Abb, maxWidth: Int): Option[String] =
    Option(Strings.abbreviate(value.strOpt.orNull, abbrevMarker.strOpt.orNull, maxWidth))

  /**
   * <p>Abbreviates a String using a given replacement marker. This will turn
   * "Now is the time for all good men" into "...is the time for..." if "..." was defined
   * as the replacement marker.</p>
   *
   * <p>Works like {@code abbreviate(String, String, int)}, but allows you to specify
   * a "left edge" offset.  Note that this left edge is not necessarily going to
   * be the leftmost character in the result, or the first character following the
   * replacement marker, but it will appear somewhere in the result.
   *
   * <p>In no case will it return a String of length greater than {@code maxWidth}.</p>
   *
   * <pre>
   * None.abbreviate(None, *, *)                 = None
   * Some("abcdefghijklmno").abbreviate(None, *, *)    = Some("abcdefghijklmno")
   * Some("").abbreviate("...", 0, 4)                  = Some("")
   * Some("abcdefghijklmno").abbreviate("---", -1, 10) = Some("abcdefg---")
   * Some("abcdefghijklmno").abbreviate(",", 0, 10)    = Some("abcdefghi,")
   * Some("abcdefghijklmno").abbreviate(",", 1, 10)    = Some("abcdefghi,")
   * Some("abcdefghijklmno").abbreviate(",", 2, 10)    = Some("abcdefghi,")
   * Some("abcdefghijklmno").abbreviate("::", 4, 10)   = Some("::efghij::")
   * Some("abcdefghijklmno").abbreviate("...", 6, 10)  = Some("...ghij...")
   * Some("abcdefghijklmno").abbreviate("*", 9, 10)    = Some("*ghijklmno")
   * Some("abcdefghijklmno").abbreviate("'", 10, 10)   = Some("'ghijklmno")
   * Some("abcdefghijklmno").abbreviate("!", 12, 10)   = Some("!ghijklmno")
   * Some("abcdefghij").abbreviate("abra", 0, 4)       = IllegalArgumentException
   * Some("abcdefghij").abbreviate("...", 5, 6)        = IllegalArgumentException
   * </pre>
   *
   * @param abbrevMarker the String used as replacement marker
   * @param offset       left edge of source String
   * @param maxWidth     maximum length of result String, must be at least 4
   * @tparam Abb String or Option[String]
   * @return abbreviated String Option, {@code None} if None input
   * @throws IllegalArgumentException if the width is too small
   */
  def abbreviate[Abb: ToStringOpt](abbrevMarker: Abb, offset: Int, maxWidth: Int): Option[String] =
    Option(Strings.abbreviate(value.strOpt.orNull, abbrevMarker.strOpt.orNull, offset, maxWidth))

  /**
   * <p>Abbreviates a String to the length passed, replacing the middle characters with the supplied
   * replacement String.</p>
   *
   * <p>This abbreviation only occurs if the following criteria is met:</p>
   * <ul>
   * <li>Neither the String for abbreviation nor the replacement String are null or empty </li>
   * <li>The length to truncate to is less than the length of the supplied String</li>
   * <li>The length to truncate to is greater than 0</li>
   * <li>The abbreviated String will have enough room for the length supplied replacement String
   * and the first and last characters of the supplied String for abbreviation</li>
   * </ul>
   * <p>Otherwise, the returned String will be the same as the supplied String for abbreviation.
   * </p>
   *
   * <pre>
   * None.abbreviateMiddle(None, 0)             = None
   * Some("abc").abbreviateMiddle(None, 0)      = Some("abc")
   * Some("abc").abbreviateMiddle(".", 0)       = Some("abc")
   * Some("abc").abbreviateMiddle(".", 3)       = Some("abc")
   * Some("abcdef").abbreviateMiddle(".", 4)    = Some("ab.f")
   * </pre>
   *
   * @param middle the String to replace the middle characters with, may be null
   * @param length the length to abbreviate {@code str} to.
   * @tparam M String or Option[String]
   * @return the abbreviated String if the above criteria is met, or the original String supplied for abbreviation.
   */
  def abbreviateMiddle[M: ToStringOpt](middle: M, length: Int): Option[String] =
    Option(Strings.abbreviateMiddle(value.strOpt.orNull, middle.strOpt.orNull, length))

  /**
   * Appends the suffix to the end of the string if the string does not
   * already end with the suffix.
   *
   * @param suffix   The suffix to append to the end of the string.
   * @param suffixes Indicates whether the compare should ignore case.
   * @tparam S String Or Option
   * @return A new Option[String] if suffix was appended, the same string otherwise.
   */
  def appendIfMissing[S: ToStringOpt](suffix: S, suffixes: CharSequence*): Option[String] =
    Option(Strings.appendIfMissing(value.strOpt.orNull, suffix.strOpt.orNull, suffixes: _*))

  /**
   * Appends the suffix to the end of the string if the string does not
   * already end with any of the suffixes.
   *
   * <pre>
   * None.appendIfMissing(None) = None
   * Some("abc").appendIfMissing(None) = Some("abc")
   * Some("").appendIfMissing("xyz") = Some("xyz")
   * Some("abc").appendIfMissing("xyz") = Some("abcxyz")
   * Some("abcxyz").appendIfMissing("xyz") = Some("abcxyz")
   * Some("abcXYZ").appendIfMissing("xyz") = Some("abcXYZxyz")
   * </pre>
   * <p>With additional suffixes,</p>
   * <pre>
   * None.appendIfMissing(None, None) = None
   * Some("abc").appendIfMissing(null, null) = Some("abc")
   * Some("").appendIfMissing("xyz", null) = Some("xyz")
   * Some("abc").appendIfMissing(Some("xyz"), new CharSequence[]{null}) = Some("abcxyz")
   * Some("abc").appendIfMissing("xyz", "") = Some("abc")
   * Some("abc").appendIfMissing("xyz", "mno") = Some("abcxyz")
   * Some("abcxyz").appendIfMissing("xyz", "mno") = Some("abcxyz")
   * Some("abcmno").appendIfMissing("xyz", "mno") = Some("abcmno")
   * Some("abcXYZ").appendIfMissing("xyz", "mno") = Some("abcXYZxyz")
   * Some("abcMNO").appendIfMissing("xyz", "mno") = Some("abcMNOxyz")
   * </pre>
   *
   * @param suffix   The suffix to append to the end of the string.
   * @param suffixes Additional suffixes that are valid terminators.
   * @tparam S String Or Option[String]
   * @return A new String if suffix was appended, the same string otherwise.
   */
  def appendIfMissingIgnoreCase[S: ToStringOpt](suffix: S, suffixes: CharSequence*): Option[String] =
    Option(Strings.appendIfMissingIgnoreCase(value.strOpt.orNull, suffix.strOpt.orNull, suffixes: _*))

  /**
   * <p>Capitalizes a String changing the first character to title case as
   * per {@link Character# toTitleCase ( int )}. No other characters are changed.</p>
   *
   * <p>For a word based algorithm, see {@link org.apache.commons.lang3.text.WordUtils# capitalize ( String )}.
   * A {@code null} input String returns {@code null}.</p>
   *
   * <pre>
   * None.capitalize          = None
   * Some("").capitalize      = Some("")
   * Some("cat").capitalize   = "Cat"
   * Some("cAt") .capitalize  = "CAt"
   * Some("'cat'").capitalize = "'cat'"
   * </pre>
   *
   * @return the capitalized String, {@code null} if null String input
   * @see #uncapitalize(String)
   */
  def capitalize: Option[String] = Option(Strings.capitalize(value.strOpt.orNull))

  /**
   * <p>Centers a String in a larger String of size {@code size}
   * using the space character (' ').</p>
   *
   * <p>If the size is less than the String length, the original String is returned.
   * A {@code null} String returns {@code null}.
   * A negative size is treated as zero.</p>
   *
   * <p>Equivalent to {@code center(str, size, " ")}.</p>
   *
   * <pre>
   * None.center(*)   = None
   * Some("").center(4)     = Some("    ")
   * Some("ab").center(-1)  = Some("ab")
   * Some("ab").center(4)   = Some(" ab ")
   * Some("abcd").center(2) = Some("abcd")
   * Some("a").center(4)    = Some(" a  ")
   * </pre>
   *
   * @param size the int size of new String, negative treated as zero
   * @return centered Option[String], {@code None} if None input
   */
  def center(size: Int): Option[String] = Option(Strings.center(value.strOpt.orNull, size))

  /**
   * <p>Centers a String in a larger String of size {@code size}.
   * Uses a supplied character as the value to pad the String with.</p>
   *
   * <p>If the size is less than the String length, the String is returned.
   * A {@code null} String returns {@code null}.
   * A negative size is treated as zero.</p>
   *
   * <pre>
   * None.center(*, *)           = None
   * Some("").center(4, ' ')     = Some("    ")
   * Some("ab").center(-1, ' ')  = Some("ab")
   * Some("ab").center(4, ' ')   = Some(" ab ")
   * Some("abcd").center(2, ' ') = Some("abcd")
   * Some("a").center(4, ' ')    = Some(" a  ")
   * Some("a").center(4, 'y')    = Some("yayy")
   * </pre>
   *
   * @param size the int size of new String, negative treated as zero
   * @param padChar the character to pad the new String with
   * @return centered String, {@code null} if null String input
   */
  def center(size: Int, padChar: Char): Option[String] =
    Option(Strings.center(value.strOpt.orNull, size, padChar))

  /**
   * <p>Centers a String in a larger String of size {@code size}.
   * Uses a supplied character as the value to pad the String with.</p>
   *
   * <p>If the size is less than the String length, the String is returned.
   * A {@code null} String returns {@code null}.
   * A negative size is treated as zero.</p>
   *
   * <pre>
   * None.center(*, *)           = None
   * Some("").center(4, " ")     = Some("    ")
   * Some("ab").center(-1, " ")  = Some("ab")
   * Some("ab").center(4, " ")   = Some(" ab ")
   * Some("abcd").center(2, " ") = Some("abcd")
   * Some("a").center(4, " ")    = Some(" a  ")
   * Some("a").center(4, "yz")   = Some("yayz")
   * Some("abc").center(7, None) = Some("  abc  ")
   * Some("abc").center(7, "")   = Some("  abc  ")
   * </pre>
   *
   * @param size the int size of new String, negative treated as zero
   * @param padStr the String to pad the new String with, must not be null or empty
   * @tparam P String or Option[String]
   * @return centered String, {@code None} if None input
   */
  def center[P: ToStringOpt](size: Int, padStr: P): Option[String] =
    Option(Strings.center(value.strOpt.orNull, size, padStr.strOpt.orNull))

  /**
   * <p>Removes one newline from end of a String if it's there,
   * otherwise leave it alone.  A newline is &quot;{@code \n}&quot;,
   * &quot;{@code \r}&quot;, or &quot;{@code \r\n}&quot;.</p>
   *
   * <p>NOTE: This method changed in 2.0.
   * It now more closely matches Perl chomp.</p>
   *
   * <pre>
   * None.chomp          = None
   * Some("").chomp            = Some("")
   * Some("abc \r").chomp      = Some("abc ")
   * Some("abc\n").chomp       = Some("abc")
   * Some("abc\r\n").chomp     = Some("abc")
   * Some("abc\r\n\r\n").chomp = Some("abc\r\n")
   * Some("abc\n\r").chomp     = Some("abc\n")
   * Some("abc\n\rabc").chomp  = Some("abc\n\rabc")
   * Some("\r").chomp          = Some("")
   * Some("\n").chomp          = Some("")
   * Some("\r\n").chomp        = Some("")
   * </pre>
   *
   * @return Option[String] without newline, {@code None} if null String input
   */
  def chomp: Option[String] = Option(Strings.chomp(value.strOpt.orNull))

  /**
   * <p>Remove the last character from a String.</p>
   *
   * <p>If the String ends in {@code \r\n}, then remove both
   * of them.</p>
   *
   * <pre>
   * None.chop                = None
   * Some("").chop            = Some("")
   * Some("abc \r").chop      = Some("abc ")
   * Some("abc\n").chop       = Some("abc")
   * Some("abc\r\n").chop     = Some("abc")
   * Some("abc").chop         = Some("ab")
   * Some("abc\nabc").chop    = Some("abc\nab")
   * Some("a").chop           = Some("")
   * Some("\r").chop          = Some("")
   * Some("\n").chop          = Some("")
   * Some("\r\n").chop        = Some("")
   * </pre>
   *
   * @return String without last character, {@code None} if None String input
   */
  def chop: Option[String] = Option(Strings.chop(value.strOpt.orNull))

  /**
   * <p>Compare two Strings lexicographically, as per {@link String# compareTo ( String )}, returning :</p>
   * <ul>
   * <li>{@code int = 0}, if {@code str1} is equal to {@code str2} (or both {@code null})</li>
   * <li>{@code int < 0}, if {@code str1} is less than {@code str2}</li>
   * <li>{@code int > 0}, if {@code str1} is greater than {@code str2}</li>
   * </ul>
   *
   * <p>This is a {@code null} safe version of :</p>
   * <blockquote><pre>str1.compareTo(str2)</pre></blockquote>
   *
   * <p>{@code null} value is considered less than non-{@code null} value.
   * Two {@code null} references are considered equal.</p>
   *
   * <pre>
   * StringUtils.compare(null, null)   = 0
   * StringUtils.compare(null , "a")   &lt; 0
   * StringUtils.compare("a", null)    &gt; 0
   * StringUtils.compare("abc", "abc") = 0
   * StringUtils.compare("a", "b")     &lt; 0
   * StringUtils.compare("b", "a")     &gt; 0
   * StringUtils.compare("a", "B")     &gt; 0
   * StringUtils.compare("ab", "abc")  &lt; 0
   * </pre>
   *
   * @param other the String to compare to
   * @tparam O String Or Option[String]
   * @return &lt; 0, 0, &gt; 0, if {@code this} is respectively less, equal or greater than {@code other}
   */
  def compare[O: ToStringOpt](other: O): Int = Strings.compare(value.strOpt.orNull, other.strOpt.orNull)

  /**
   * <p>Compare two Strings lexicographically, as per {@link String# compareTo ( String )}, returning :</p>
   * <ul>
   * <li>{@code int = 0}, if {@code str1} is equal to {@code str2} (or both {@code null})</li>
   * <li>{@code int < 0}, if {@code str1} is less than {@code str2}</li>
   * <li>{@code int > 0}, if {@code str1} is greater than {@code str2}</li>
   * </ul>
   *
   * <p>This is a {@code null} safe version of :</p>
   * <blockquote><pre>str1.compareTo(str2)</pre></blockquote>
   *
   * <p>{@code null} inputs are handled according to the {@code nullIsLess} parameter.
   * Two {@code null} references are considered equal.</p>
   *
   * <pre>
   * None.compare(None, *)                  = 0
   * None.compare(Some("a"), true)          &lt; 0
   * None.compare(Some("a"), false)         &gt; 0
   * Some("a").compare(None, true)          &gt; 0
   * Some("a").compare(None, false)         &lt; 0
   * Some("abc").compare(Some("abc"), *)    = 0
   * Some("a").compare(Some("b"), *)        &lt; 0
   * Some("b").compare("a", *)              &gt; 0
   * Some("a").compare("B", *)              &gt; 0
   * Some("ab").compare("abc", *)           &lt; 0
   * </pre>
   *
   * @param other the String to compare to
   * @param nullIsNull whether consider {@code None} value less than non-{@code None} value
   * @tparam O String Or Option
   * @return &lt; 0, 0, &gt; 0, if {@code this} is respectively less, equal ou greater than {@code other}
   */
  def compare[O: ToStringOpt](other: O, nullIsNull: Boolean): Int =
    Strings.compare(value.strOpt.orNull, other.strOpt.orNull, nullIsNull)

  /**
   * <p>Compare two Strings lexicographically, ignoring case differences,
   * as per {@link String# compareToIgnoreCase ( String )}, returning :</p>
   * <ul>
   * <li>{@code int = 0}, if {@code str1} is equal to {@code str2} (or both {@code null})</li>
   * <li>{@code int < 0}, if {@code str1} is less than {@code str2}</li>
   * <li>{@code int > 0}, if {@code str1} is greater than {@code str2}</li>
   * </ul>
   *
   * <p>This is a {@code null} safe version of :</p>
   * <blockquote><pre>str1.compareToIgnoreCase(str2)</pre></blockquote>
   *
   * <p>{@code null} value is considered less than non-{@code null} value.
   * Two {@code null} references are considered equal.
   * Comparison is case insensitive.</p>
   *
   * <pre>
   * None.compareToIgnoreCase(None)         = 0
   * None.compareToIgnoreCase(None , "a")   &lt; 0
   * Some("a").compareToIgnoreCase(None)    &gt; 0
   * Some("abc").compareToIgnoreCase("abc") = 0
   * Some("abc").compareToIgnoreCase("ABC") = 0
   * Some("a").compareToIgnoreCase("b")     &lt; 0
   * Some("b").compareToIgnoreCase("a")     &gt; 0
   * Some("a").compareToIgnoreCase("B")     &lt; 0
   * Some("A").compareToIgnoreCase("b")     &lt; 0
   * Some("ab").compareToIgnoreCase("ABC")  &lt; 0
   * </pre>
   *
   * @param other the String to compare to
   * @tparam O String or Option[String]
   * @return &lt; 0, 0, &gt; 0, if {@code this} is respectively less, equal ou greater than {@code other},
   *         ignoring case differences.
   */
  def compareIgnoreCase[O: ToStringOpt](other: O): Int =
    Strings.compareIgnoreCase(value.strOpt.orNull, other.strOpt.orNull)

  /**
   * <p>Compare two Strings lexicographically, ignoring case differences,
   * as per {@link String# compareToIgnoreCase ( String )}, returning :</p>
   * <ul>
   * <li>{@code int = 0}, if {@code str1} is equal to {@code str2} (or both {@code null})</li>
   * <li>{@code int < 0}, if {@code str1} is less than {@code str2}</li>
   * <li>{@code int > 0}, if {@code str1} is greater than {@code str2}</li>
   * </ul>
   *
   * <p>This is a {@code null} safe version of :</p>
   * <blockquote><pre>str1.compareToIgnoreCase(str2)</pre></blockquote>
   *
   * <p>{@code null} inputs are handled according to the {@code nullIsLess} parameter.
   * Two {@code null} references are considered equal.
   * Comparison is case insensitive.</p>
   *
   * <pre>
   * StringUtils.compareIgnoreCase(null, null, *)     = 0
   * StringUtils.compareIgnoreCase(null , "a", true)  &lt; 0
   * StringUtils.compareIgnoreCase(null , "a", false) &gt; 0
   * StringUtils.compareIgnoreCase("a", null, true)   &gt; 0
   * StringUtils.compareIgnoreCase("a", null, false)  &lt; 0
   * StringUtils.compareIgnoreCase("abc", "abc", *)   = 0
   * StringUtils.compareIgnoreCase("abc", "ABC", *)   = 0
   * StringUtils.compareIgnoreCase("a", "b", *)       &lt; 0
   * StringUtils.compareIgnoreCase("b", "a", *)       &gt; 0
   * StringUtils.compareIgnoreCase("a", "B", *)       &lt; 0
   * StringUtils.compareIgnoreCase("A", "b", *)       &lt; 0
   * StringUtils.compareIgnoreCase("ab", "abc", *)    &lt; 0
   * </pre>
   *
   * @param other the String to compare to
   * @param nullIsLess whether consider {@code None} value less than non-{@code None} value
   * @tparam O String or Option[String]
   * @return &lt; 0, 0, &gt; 0, if {@code this} is respectively less, equal ou greater than {@code other},
   *             ignoring case differences.
   */
  def compareIgnoreCase[O: ToStringOpt](other: O, nullIsLess: Boolean): Int =
    Strings.compareIgnoreCase(value.strOpt.orNull, other.strOpt.orNull, nullIsLess)

  /**
   * * <p>Checks if CharSequence contains a search CharSequence, handling {@code null}.
   * This method uses {@link String# indexOf ( String )} if possible.</p>
   *
   * <p>A {@code null} CharSequence will return {@code false}.</p>
   *
   * <pre>
   * None.contains(*)           = false
   * *.contains(None)           = false
   * Some("").contains("")      = true
   * Some("abc").contains("")   = true
   * Some("abc").contains("a")  = true
   * Some("abc").contains("z")  = false
   * </pre>
   *
   * @param searchSeq the CharSequence to find, may be null
   * @tparam To String Or Option[String]
   * @return true if the CharSequence contains the search CharSequence,
   */
  def contains[To: ToStringOpt](searchSeq: To): Boolean =
    Strings.contains(value.strOpt.orNull, searchSeq.strOpt.orNull)

  /**
   * <p>Checks if CharSequence contains a search character, handling {@code null}.
   * This method uses {@link String# indexOf ( int )} if possible.</p>
   *
   * <p>A {@code null} or empty ("") CharSequence will return {@code false}.</p>
   *
   * <pre>
   * None.contains(*)          = false
   * Some("").contains(*)      = false
   * Some("abc").contains('a') = true
   * Some("abc").contains('z') = false
   * </pre>
   *
   * @param searchChar the character to find
   * @return true if the CharSequence contains the search character,
   */
  def contains(searchChar: Char): Boolean = Strings.contains(value.strOpt.orNull, searchChar)

  /**
   * <p>Checks if the CharSequence contains any character in the given
   * set of characters.</p>
   *
   * <p>A {@code None} CharSequence will return {@code false}.
   * A {@code None} or zero length search array will return {@code false}.</p>
   *
   * <pre>
   * None.containsAny(*)                          = false
   * Some("").containsAny(*)                      = false
   * Option(*).containsAny(None)                  = false
   * Option(*).containsAny([])                    = false
   * Some("zzabyycdxx").containsAny(['z', 'a'])   = true
   * Some("zzabyycdxx").containsAny(['b', 'y'])   = true
   * Some("zzabyycdxx").containsAny(['z', 'y'])   = true
   * Some("aba").containsAny(['z'])               = false
   * </pre>
   *
   * @param searchChars the chars to search for, may be null
   * @return if any of the chars are found,
   */
  def containsAnyChar(searchChars: Char*): Boolean =
    Strings.containsAny(value.strOpt.orNull, searchChars: _*)
  def containsAnyChar(searchChars: Array[Char]): Boolean =
    Strings.containsAny(value.strOpt.orNull, searchChars: _*)

  /**
   * <p>
   * Checks if the CharSequence contains any character in the given set of characters.
   * </p>
   *
   * <p>
   * A {@code None} CharSequence will return {@code false}. A {@code null} search CharSequence will return
   * {@code false}.
   * </p>
   *
   * <pre>
   * None.containsAny(*)                            = false
   * Some("").containsAny(*)                        = false
   * Option(*).containsAny(None)                    = false
   * Option(*).containsAny("")                      = false
   * Some("zzabyycdxx").containAny("za")            = true
   * Some("zzabyycdxx").containAny("by")            = true
   * Some("zzabyycdxx").containAny("zy")            = true
   * Some("zzabyycdxx").containAny("\tx")           = true
   * Some("zzabyycdxx").containAny("$.#yF")         = true
   * Some("aba").containAny("z")                    = false
   * </pre>
   *
   * @param searchChars the chars to search for, may be null
   * @tparam S String or Option[String]
   * @return the {@code true} if any of the chars are found, {@code false} if no match or null input
   */
  def containsAny[S: ToStringOpt](searchChars: S): Boolean =
    Strings.containsAny(value.strOpt.orNull, searchChars.strOpt.orNull)

  /**
   * <p>
   * Checks if the CharSequence contains any of the CharSequences in the given array.
   * </p>
   *
   * <p>
   * A {@code None} {@code cs} CharSequence will return {@code false}. A {@code null} or zero length search array will
   * return {@code false}.
   * </p>
   *
   * <pre>
   * StringUtils.containsAny(null, *)            = false
   * StringUtils.containsAny("", *)              = false
   * StringUtils.containsAny(*, null)            = false
   * StringUtils.containsAny(*, [])              = false
   * StringUtils.containsAny("abcd", "ab", null) = true
   * StringUtils.containsAny("abcd", "ab", "cd") = true
   * StringUtils.containsAny("abc", "d", "abc")  = true
   * </pre>
   *
   * * @param The array of CharSequences to search for, may be null. Individual CharSequences may be
   * null as well.
   *
   * @return {@code true} if any of the search CharSequences are found, {@code false} otherwise
   */
  def containsAnyString(searchCharSequences: CharSequence*): Boolean =
    Strings.containsAny(value.strOpt.orNull, searchCharSequences: _*)

  def containsAnyString(searchCharSequences: Array[String]): Boolean =
    Strings.containsAny(value.strOpt.orNull, searchCharSequences: _*)
}
