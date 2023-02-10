package commons.lang3.bridge

import commons.lang3.bridge.StringUtils.ops._
import org.scalatest.funsuite.AnyFunSuite

/** TODO
  *
  * @author
  *   mars
  * @version 1.0.0
  * @since 2022/09/10
  *   16:26
  */
class StringUtilsIEmptyBlankSpec extends AnyFunSuite {
  val noneString: Option[String]         = None
  val nullString: String                 = null
  val noneStrings: Option[Array[String]] = None

  test("test string is empty") {
    assert(nullString.ops.isEmpty)
    assert("".ops.isEmpty)
    assert(!Some(" ").ops.isEmpty)
    assert(!"foo".ops.isEmpty)
    assert(!"  foo  ".ops.isEmpty)
  }

  test("test string is not empty") {
    assert(!nullString.ops.isNotEmpty)
    assert(!"".ops.isNotEmpty)
    assert(" ".ops.isNotEmpty)
    assert("foo".ops.isNotEmpty)
    assert("  foo  ".ops.isNotEmpty)
  }

  test("test string is blank") {
    assert(noneString.ops.isBlank)
    assert("".ops.isBlank)
    assert(StringUtilsSpec.WHITESPACE.ops.isBlank)
    assert(!"foo".ops.isBlank)
    assert(!Some("  foo  ").ops.isBlank)
  }

  test("test string is not blank") {
    assert(!noneString.ops.isNotBlank)
    assert(!"".ops.isNotBlank)
    assert(!Some(StringUtilsSpec.WHITESPACE).ops.isNotBlank)
    assert("foo".ops.isNotBlank)
    assert("  foo  ".ops.isNotBlank)
  }

}
