package commons.lang3.scala

import org.scalatest.funsuite.AnyFunSuite

/**
 * TODO
 *
 * @author mars
 * @version 1.0.0
 * @since 2022/08/28 01:18
 */
class StringUtilsSpec extends AnyFunSuite {

  /**
   * Supplementary character U+20000
   * See http://www.oracle.com/technetwork/articles/javase/supplementary-142654.html
   */
  private val CharU20000 = "\uD840\uDC00"
  /**
   * Supplementary character U+20001
   * See http://www.oracle.com/technetwork/articles/javase/supplementary-142654.html
   */
  private val CharU20001 = "\uD840\uDC01"
  /**
   * Incomplete supplementary character U+20000, high surrogate only.
   * See http://www.oracle.com/technetwork/articles/javase/supplementary-142654.html
   */
  private val CharUSuppCharHigh = "\uDC00"

  /**
   * Incomplete supplementary character U+20000, low surrogate only.
   * See http://www.oracle.com/technetwork/articles/javase/supplementary-142654.html
   */
  private val CharUSuppCharLow = "\uD840"
  import commons.lang3.scala.StringUtils.bridge._

  test("test contains operators"){

    val nullStr:String = null
    assert(!nullStr.ops.contains(' '))
    assert(!"".ops.contains(' '))

    assert(!"".ops.contains(nullStr))
    assert(!nullStr.ops.contains(nullStr))

    assert("abc".ops.contains('a'))
    assert("abc".ops.contains('b'))
    assert("abc".ops.contains('c'))
    assert(!"abc".ops.contains('z'))
  }

  test("test contains operators for option string") {
    val noneStr: Option[String] = None
    assert(!noneStr.ops.contains(' '))
    assert(!Some("").ops.contains(' '))

    assert(!Some("").ops.contains(None))

    assert(!None.ops.contains(None))

    assert(Some("abc").ops.contains('a'))
    assert(Some("abc").ops.contains('b'))
    assert(Some("abc").ops.contains('c'))
    assert(!Some("abc").ops.contains('z'))

  }
}
