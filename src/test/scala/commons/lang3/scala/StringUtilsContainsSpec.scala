package commons.lang3.scala

import org.scalatest.funsuite.AnyFunSuite

/**
 * TODO
 *
 * @author mars
 * @version 1.0.0
 * @since 2022/08/28 01:18
 */
class StringUtilsContainsSpec extends AnyFunSuite {

  import commons.lang3.scala.StringUtils.bridge._

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

  test("test string contains char") {

    val nullStr: String = null
    assert(!nullStr.ops.contains(' '))
    assert(!"".ops.contains(' '))

    assert(!"".ops.contains(nullStr))
    assert(!nullStr.ops.contains(nullStr))

    assert("abc".ops.contains('a'))
    assert("abc".ops.contains('b'))
    assert("abc".ops.contains('c'))
    assert(!"abc".ops.contains('z'))
  }

  test("test option string contains char ") {
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

  test("test string contains string") {
    val nullStr: String = null
    assert(!nullStr.ops.contains(nullStr))
    assert(!nullStr.ops.contains(""))
    assert(!nullStr.ops.contains("a"))
    assert(!"".ops.contains(nullStr))
    assert("".ops.contains(""))
    assert(!"".ops.contains("a"))
    assert("abc".ops.contains("a"))
    assert("abc".ops.contains("b"))
    assert("abc".ops.contains("c"))
    assert("abc".ops.contains(Some("abc")))
    assert(!"abc".ops.contains("z"))
  }

  test("test Option[string] contains Option[string]") {
    val none: Option[String] = None
    assert(!none.ops.contains(None))
    assert(!none.ops.contains(""))
    assert(!none.ops.contains(Some("a")))
    assert(!Some("").ops.contains(None))
    assert(Some("").ops.contains(Some("")))
    assert(!Some("").ops.contains(Some("a")))
    assert(Some("abc").ops.contains("a"))
    assert(Some("abc").ops.contains(Some("b")))
    assert(Some("abc").ops.contains("c"))
    assert(Some("abc").ops.contains(Some("abc")))
    assert(!Some("abc").ops.contains(Some("z")))
  }

  test("test contains String with Bad Supplementary chars") {
    assert(!CharUSuppCharHigh.ops.contains(CharU20001))
    assert(!CharUSuppCharLow.ops.contains(CharU20001))
    assert(!CharU20001.ops.contains(CharUSuppCharHigh))
    assert(CharU20001.indexOf(CharUSuppCharLow) == 0)
    assert(CharU20001.ops.contains(CharUSuppCharLow))
    assert((CharU20001 + CharUSuppCharLow + "a").ops.contains("a"))
    assert((CharU20001 + CharUSuppCharHigh + "a").ops.contains("a"))
  }

  test("test contains Option[String] with Bad Supplementary chars") {
    assert(!Some(CharUSuppCharHigh).ops.contains(Some(CharU20001)))
    assert(!Some(CharUSuppCharLow).ops.contains(CharU20001))
    assert(!Some(CharU20001).ops.contains(Some(CharUSuppCharHigh)))
    assert(CharU20001.indexOf(CharUSuppCharLow) == 0)
    assert(Some(CharU20001).ops.contains(CharUSuppCharLow))
    assert(Some(CharU20001 + CharUSuppCharLow + "a").ops.contains(Some("a")))
    assert(Some(CharU20001 + CharUSuppCharHigh + "a").ops.contains(Some("a")))
  }

  test("test contains any string char array") {
    val nullString: String = null
    val nullChars: Array[Char] = null

    assert(!nullString.ops.containsAnyChar(null))
    assert(!nullString.ops.containsAnyChar(new Array[Char](0): _*))
    assert(!nullString.ops.containsAnyChar('a', 'b'))

    assert(!"".ops.containsAnyChar(null))
    assert(!"".ops.containsAnyChar(new Array[Char](0): _*))
    assert(!"".ops.containsAnyChar('a', 'b'))

    assert(!"zzabyycdxx".ops.containsAnyChar(null))
    assert(!"zzabyycdxx".ops.containsAnyChar(new Array[Char](0): _*))
    assert("zzabyycdxx".ops.containsAnyChar('z', 'a'))
    assert("zzabyycdxx".ops.containsAnyChar('b', 'y'))
    assert("zzabyycdxx".ops.containsAnyChar('z', 'y'))
    assert(!"ab".ops.containsAnyChar(new Array[Char]('z'): _*))
  }

  test("test option string contains any string char array") {
    val noneString: String = null
    val nullChars: Array[Char] = null

    assert(!noneString.ops.containsAnyChar(null))
    assert(!noneString.ops.containsAnyChar(new Array[Char](0): _*))
    assert(!noneString.ops.containsAnyChar('a', 'b'))

    assert(!Some("").ops.containsAnyChar(null))
    assert(!Some("").ops.containsAnyChar(new Array[Char](0): _*))
    assert(!Some("").ops.containsAnyChar('a', 'b'))

    assert(!Some("zzabyycdxx").ops.containsAnyChar(null))
    assert(!Some("zzabyycdxx").ops.containsAnyChar(new Array[Char](0): _*))
    assert(Some("zzabyycdxx").ops.containsAnyChar('z', 'a'))
    assert(Some("zzabyycdxx").ops.containsAnyChar('b', 'y'))
    assert(Some("zzabyycdxx").ops.containsAnyChar('z', 'y'))
    assert(!Some("ab").ops.containsAnyChar(new Array[Char]('z'): _*))
  }
}
