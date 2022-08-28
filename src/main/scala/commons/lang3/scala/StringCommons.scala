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
  import commons.lang3.scala.StringUtils.bridge.StringOptExt
  def contains[To: ToStringOpt](seq: To): Boolean = Strings.contains(value.strOpt.orNull, seq.strOpt.orNull)

  def contains(searchChar: Char): Boolean = Strings.contains(value.strOpt.orNull, searchChar)

  def abbreviate(maxWidth: Int): String = Strings.abbreviate(value.strOpt.orNull, maxWidth)

  def abbreviate(offset: Int, maxWidth: Int): String = Strings.abbreviate(value.strOpt.orNull, offset, maxWidth)

  def abbreviate[Abb: ToStringOpt](abbrevMarker: Abb, maxWidth: Int): String =
    Strings.abbreviate(value.strOpt.orNull, abbrevMarker.strOpt.orNull, maxWidth)

  def abbreviate[Abb: ToStringOpt](abbrevMarker: Abb, offset: Int, maxWidth: Int): String =
    Strings.abbreviate(value.strOpt.orNull, abbrevMarker.strOpt.orNull, offset, maxWidth)

  def abbreviateMiddle(middle: String, length: Int): String =
    Strings.abbreviateMiddle(value.strOpt.orNull, middle, length)

  def appendIfMissing[S: ToStringOpt](suffix: S, suffixes: CharSequence*): String =
    Strings.appendIfMissing(value.strOpt.orNull, suffix.strOpt.orNull, suffixes: _*)

  def appendIfMissingIgnoreCase[S: ToStringOpt](suffix: S, suffixes: CharSequence*): String =
    Strings.appendIfMissingIgnoreCase(value.strOpt.orNull, suffix.strOpt.orNull, suffixes: _*)

  def capitalize: String = Strings.capitalize(value.strOpt.orNull)

  def center(size: Int): String = Strings.center(value.strOpt.orNull, size)

  def center(size: Int, padChar: Char): String = Strings.center(value.strOpt.orNull, size, padChar)

  def center[P: ToStringOpt](size: Int, padStr: P): String =
    Strings.center(value.strOpt.orNull, size, padStr.strOpt.orNull)

  def chomp: String = Strings.chomp(value.strOpt.orNull)

  def chop: String = Strings.chop(value.strOpt.orNull)

  def compare[O: ToStringOpt](other: O): Int = Strings.compare(value.strOpt.orNull, other.strOpt.orNull)

  def compare[O: ToStringOpt](other: O, nullIsNull: Boolean): Int =
    Strings.compare(value.strOpt.orNull, other.strOpt.orNull, nullIsNull)

  def compareIgnoreCase[O: ToStringOpt](other: O): Int =
    Strings.compareIgnoreCase(value.strOpt.orNull, other.strOpt.orNull)

  def compareIgnoreCase[O: ToStringOpt](other: O, nullIsLess: Boolean): Int =
    Strings.compareIgnoreCase(value.strOpt.orNull, other.strOpt.orNull, nullIsLess)

}
