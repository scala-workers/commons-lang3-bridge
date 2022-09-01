package commons.lang3.bridge

import commons.lang3.bridge.StringUtils.ops._
import org.scalatest.funsuite.AnyFunSuite

import java.util
import java.util.Collections
import scala.collection.mutable

/** TODO
  *
  * @author
  *   mars
  * @version 1.0.0
  * @since 2022/09/01
  *   00:53
  */
class StringUtilsSpec extends AnyFunSuite {
  val (whitespace: String, non_whitespace: String, hard_space: String, trimmable: String, non_trimmable: String) = {
    val ws: mutable.StringBuilder  = new mutable.StringBuilder
    val nws: mutable.StringBuilder = new mutable.StringBuilder
    val hs: String                 = String.valueOf(160.toChar)
    val tr: mutable.StringBuilder  = new mutable.StringBuilder
    val ntr: mutable.StringBuilder = new mutable.StringBuilder

    for (i <- 0 until Character.MAX_VALUE) {
      if (Character.isWhitespace(i.toChar)) {
        ws.append(String.valueOf(i.toChar))
        if (i > 32) ntr.append(String.valueOf(i.toChar))
      } else if (i < 40) nws.append(String.valueOf(i.toChar))
    }
    for (i <- 0 to 32) {
      tr.append(String.valueOf(i.toChar))
    }
    (ws.toString, nws.toString, hs, tr.toString, ntr.toString)
  }
  val WHITESPACE     = whitespace
  val NON_WHITESPACE = non_whitespace
  val TRIMMABLE      = trimmable
  val NON_TRIMMABLE  = non_trimmable

  val ARRAY_LIST: Array[String]       = Array("foo", "bar", "baz")
  val EMPTY_ARRAY_LIST: Array[String] = Array()
  val NULL_ARRAY_LIST: Array[String]  = Array(null)
  val NULL_TO_STRING_LIST: Array[Object] = Array(new Object() {
    override def toString: String = null
  })
  val MIXED_ARRAY_LIST: Array[String]        = Array(null, "", "foo")
  val MIXED_TYPE_LIST: Array[Any]            = Array("foo", 2L)
  val LONG_PRIM_LIST: Array[Long]            = Array(1, 2)
  val INT_PRIM_LIST: Array[Int]              = Array(1, 2)
  val BYTE_PRIM_LIST: Array[Byte]            = Array(1, 2)
  val SHORT_PRIM_LIST: Array[Short]          = Array(1, 2)
  val CHAR_PRIM_LIST: Array[Char]            = Array('1', '2')
  val FLOAT_PRIM_LIST: Array[Float]          = Array(1, 2)
  val DOUBLE_PRIM_LIST: Array[Double]        = Array(1, 2)
  val MIXED_STRING_LIST: util.List[String]   = util.Arrays.asList(null, "", "foo")
  val MIXED_TYPE_OBJECT_LIST: util.List[Any] = util.Arrays.asList[Any]("foo", 2L)
  val STRING_LIST: util.List[String]         = util.Arrays.asList("foo", "bar", "baz")
  val EMPTY_STRING_LIST: List[String]        = List.empty
  val NULL_STRING_LIST: util.List[String]    = Collections.singletonList(null)

  val SEPARATOR: String          = ","
  val SEPARATOR_CHAR: Char       = ';'
  val COMMA_SEPARATOR_CHAR: Char = ','

  val TEXT_LIST: String       = "foo,bar,baz"
  val TEXT_LIST_CHAR: String  = "foo;bar;baz"
  val TEXT_LIST_NOSEP: String = "foobarbaz"

  val FOO_UNCAP: String = "foo"
  val FOO_CAP: String   = "Foo"

  val SENTENCE_UNCAP: String = "foo bar baz"
  val SENTENCE_CAP: String   = "Foo Bar Baz"

  val EMPTY: Array[Boolean]                  = Array()
  val ARRAY_FALSE_FALSE: Array[Boolean]      = Array(false, false)
  val ARRAY_FALSE_TRUE: Array[Boolean]       = Array(false, true)
  val ARRAY_FALSE_TRUE_FALSE: Array[Boolean] = Array(false, true, false)

  private def assertAbbreviateWithAbbrevMarkerAndOffset(expected: String, abbrevMarker: String, offset: Int, maxWidth: Int): Unit = {
    val abcdefghijklmno = "abcdefghijklmno"
    val message         = "abbreviate(String,String,int,int) failed"
    val result          = abcdefghijklmno.ops.abbreviate(abbrevMarker, offset, maxWidth)
    assert(result.isDefined)
    result foreach { actual =>
      if (offset >= 0 && offset < abcdefghijklmno.length) {
        assert(actual.indexOf(('a' + offset).toChar) != -1, message + " -- should contain offset character")
      }
      assert(actual.length <= maxWidth, message + " -- should not be greater than maxWidth")
      assert(expected == actual, message)
    }
  }

  private def assertAbbreviateWithOffset(expected: String, offset: Int, maxWidth: Int): Unit = {
    val abcdefghijklmno = "abcdefghijklmno"
    val message         = "abbreviate(String,int,int) failed"
    val result          = abcdefghijklmno.ops.abbreviate(offset, maxWidth)
    assert(result.isDefined)
    result foreach { actual =>
      if (offset >= 0 && offset < abcdefghijklmno.length) {
        assert(actual.indexOf(('a' + offset).toChar) != -1, message + " -- should contain offset character")
      }
      assert(actual.length <= maxWidth, message + " -- should not be greater than maxWidth")
      assert(expected == actual, message)
    }
  }

  private def innerTestSplitPreserveAllTokens(separator: Char, sepStr: String, noMatch: Char): Unit = {
    val msg = "Failed on separator hex(" + Integer.toHexString(separator) + "), noMatch hex(" + Integer.toHexString(
      noMatch
    ) + "), sepStr(" + sepStr + ")"
    val str = "a" + separator + "b" + separator + separator + noMatch + "c"
    val res = str.ops.splitPreserveAllTokens(sepStr)
    assert(res.isDefined)
    val resStr = res.get
    assert(4 == resStr.length, msg)
    assert("a" == resStr(0), msg)
    assert("b" == resStr(1), msg)
    assert("" == resStr(2), msg)
    assert(noMatch + "c" == resStr(3), msg)

    val str2 = separator + "a" + separator
    val res2 = str2.ops.splitPreserveAllTokens(sepStr)
    assert(res2.isDefined)
    val resStr2 = res2.get

    assert(3 == resStr2.length, msg)
    assert("" == resStr2(0), msg)
    assert("a" == resStr2(1), msg)
    assert("" == resStr2(2), msg)

    val res3 = str.ops.splitPreserveAllTokens(sepStr, -1)
    assert(res3.isDefined)
    val resStr3 = res3.get

    assert(4 == resStr3.length, msg)
    assert("a" == resStr3(0), msg)
    assert("b" == resStr3(1), msg)
    assert("" == resStr3(2), msg)
    assert(noMatch + "c" == resStr3(3), msg)

    val res4 = str.ops.splitPreserveAllTokens(sepStr, 0)
    assert(res4.isDefined)
    val resStr4 = res4.get

    assert(4 == resStr4.length, msg)
    assert("a" == resStr4(0), msg)
    assert("b" == resStr4(1), msg)
    assert("" == resStr4(2), msg)
    assert(noMatch + "c" == resStr4(3), msg)

    val res5 = str.ops.splitPreserveAllTokens(sepStr, 1)
    assert(res5.isDefined)
    val resStr5 = res5.get

    assert(1 == resStr5.length, msg)
    assert(str == resStr5(0), msg)

    val res6 = str.ops.splitPreserveAllTokens(sepStr, 2)
    assert(res6.isDefined)
    val resStr6 = res6.get

    assert(2 == resStr6.length, msg)
    assert("a" == resStr6(0), msg)
    assert(str.substring(2) == resStr6(1), msg)
  }
}
