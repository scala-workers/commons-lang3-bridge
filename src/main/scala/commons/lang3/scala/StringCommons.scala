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
  import commons.lang3.scala.StringUtils.bridge.StringExt
  def contains[To: ToStringOpt](seq: To): Boolean = Strings.contains(value.strOpt.orNull, seq.strOpt.orNull)

  def contains(searchChar: Char): Boolean = Strings.contains(value.strOpt.orNull, searchChar)
}
