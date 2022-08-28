package commons.scala.bridge.lang3

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * TODO
 *
 * @author mars
 * @version 1.0.0
 * @since 2022/08/28 01:18
 */
class StringUtilsSpec extends AnyFlatSpec with Matchers{

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
  import commons.scala.bridge.lang3.StringUtils.bridge._

  "Strings" should "test contains operators" in  {

    val nullStr:String = null
    nullStr.ops.contains(' ') should be (false)
    "".ops.contains(' ') should be (false)
    "".ops.contains(nullStr) should be (false)
    nullStr.ops.contains(nullStr) should be (false)

    "abc".ops.contains('a') should be (true)
    "abc".ops.contains('b') should be (true)
    "abc".ops.contains('c') should be (true)
    "abc".ops.contains('z') should be (false)
  }

  "Options" should "test contains operators for option string" in {
    val noneStr: Option[String] = None
    noneStr.contains(' ') should be(false)
    Some("").ops.contains(' ') should be(false)

    Some("").ops.contains(None) should be(false)

    None.ops.contains(None) should be(false)

    Some("abc").ops.contains('a') should be(true)
    Some("abc").ops.contains('b') should be(true)
    Some("abc").ops.contains('c') should be(true)
    Some("abc").ops.contains('z') should be(false)

  }
}
