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

  val noneString: Option[String] = None
  val nullString: String         = null

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

  test("test abbreviate marker with empty string") {
    val greaterThanMaxTest = "much too long text"
    assert(greaterThanMaxTest.ops.abbreviate("", 13).contains("much too long"))
  }

  test("test abbreviate string int") {
    assert(noneString.ops.abbreviate(10).isEmpty)
    assert("".ops.abbreviate(10).contains(""))
    assert(Some("short").ops.abbreviate(10).contains("short"))
    assert("Now is the time for all good men to come to the aid of their party.".ops.abbreviate(10).contains("Now is ..."))

    val raspberry = "raspberry peach"
    assert(raspberry.ops.abbreviate(14).contains("raspberry p..."))
    assert(Some("raspberry peach").ops.abbreviate(15).contains("raspberry peach"))
    assert("raspberry peach".ops.abbreviate(16).contains("raspberry peach"))
    assert("abcdefg".ops.abbreviate(6).contains("abc..."))
    assert("abcdefg".ops.abbreviate(7).contains("abcdefg"))
    assert("abcdefg".ops.abbreviate(8).contains("abcdefg"))
    assert(Some("abcdefg").ops.abbreviate(4).contains("a..."))
    assert("".ops.abbreviate(4).contains(""))

    assertThrows[IllegalArgumentException] {
      "abc".ops.abbreviate(3)
    }
  }

  test("test string abbreviate with offset and max width") {
    assert(nullString.ops.abbreviate(10, 12).isEmpty)
    assert("".ops.abbreviate(0, 10).contains(""))
    assert(Some("").ops.abbreviate(2, 10).contains(""))

    assertThrows[IllegalArgumentException] {
      "abcdefghij".ops.abbreviate(0, 3)
    }
    assertThrows[IllegalArgumentException] {
      "abcdefghij".ops.abbreviate(5, 6)
    }

    val raspberry = "raspberry peach"
    assert(raspberry.ops.abbreviate(11, 15).contains("raspberry peach"))

    assert(nullString.ops.abbreviate(7, 14).isEmpty)
    assertAbbreviateWithOffset("abcdefg...", -1, 10)
    assertAbbreviateWithOffset("abcdefg...", 0, 10)
    assertAbbreviateWithOffset("abcdefg...", 1, 10)
    assertAbbreviateWithOffset("abcdefg...", 2, 10)
    assertAbbreviateWithOffset("abcdefg...", 3, 10)
    assertAbbreviateWithOffset("abcdefg...", 4, 10)
    assertAbbreviateWithOffset("...fghi...", 5, 10)
    assertAbbreviateWithOffset("...ghij...", 6, 10)
    assertAbbreviateWithOffset("...hijk...", 7, 10)
    assertAbbreviateWithOffset("...ijklmno", 8, 10)
    assertAbbreviateWithOffset("...ijklmno", 9, 10)
    assertAbbreviateWithOffset("...ijklmno", 10, 10)
    assertAbbreviateWithOffset("...ijklmno", 10, 10)
    assertAbbreviateWithOffset("...ijklmno", 11, 10)
    assertAbbreviateWithOffset("...ijklmno", 12, 10)
    assertAbbreviateWithOffset("...ijklmno", 13, 10)
    assertAbbreviateWithOffset("...ijklmno", 14, 10)
    assertAbbreviateWithOffset("...ijklmno", 15, 10)
    assertAbbreviateWithOffset("...ijklmno", 16, 10)
    assertAbbreviateWithOffset("...ijklmno", Integer.MAX_VALUE, 10)
  }

  test("test string abbreviate with abbrev maker and max width") {
    assert(nullString.ops.abbreviate(null, 10).isEmpty)
    assert(None.ops.abbreviate("...", 10).isEmpty)
    assert(Some("paranaguacu").ops.abbreviate(None, 10).contains("paranaguacu"))
    assert("".ops.abbreviate("...", 2).contains(""))
    assert("waiheke".ops.abbreviate("**", 5).contains("wai**"))
    assert("And after a long time, he finally met his son.".ops.abbreviate(",,,,", 10).contains("And af,,,,"))

    val raspberry = "raspberry peach"
    assert(raspberry.ops.abbreviate("..", 14).contains("raspberry pe.."))
    assert(Some("raspberry peach").ops.abbreviate("---*---", 15).contains("raspberry peach"))
    assert("raspberry peach".ops.abbreviate(".", 16).contains("raspberry peach"))
    assert("abcdefg".ops.abbreviate(Some("()("), 6).contains("abc()("))
    assert("abcdefg".ops.abbreviate(";", 7).contains("abcdefg"))
    assert(Some("abcdefg").ops.abbreviate("_-", 8).contains("abcdefg"))
    assert("abcdefg".ops.abbreviate(".", 4).contains("abc."))
    assert("".ops.abbreviate("", 4).contains(""))

    assertThrows[IllegalArgumentException] {
      Some("abcdefghij").ops.abbreviate(Some("..."), 3)
    }

  }

  test("test strings abbreviate with abbrev maker, offset and max width") {
    assert(nullString.ops.abbreviate(None, 10, 12).isEmpty)
    assert(noneString.ops.abbreviate("...", 10, 12).isEmpty)
    assert("".ops.abbreviate(null, 0, 10).contains(""))
    assert(Some("").ops.abbreviate("...", 2, 10).contains(""))

    assertThrows[IllegalArgumentException] {
      Some("abcdefghij").ops.abbreviate(Some("::"), 0, 2)
    }
    assertThrows[IllegalArgumentException] {
      Some("abcdefghij").ops.abbreviate("!!!", 5, 6)
    }

    val raspberry = "raspberry peach"
    assert(raspberry.ops.abbreviate("--", 12, 15).contains("raspberry peach"))

    assert(noneString.ops.abbreviate(";", 7, 14).isEmpty)
    assertAbbreviateWithAbbrevMarkerAndOffset("abcdefgh;;", ";;", -1, 10)
    assertAbbreviateWithAbbrevMarkerAndOffset("abcdefghi.", ".", 0, 10)
    assertAbbreviateWithAbbrevMarkerAndOffset("abcdefgh++", "++", 1, 10)
    assertAbbreviateWithAbbrevMarkerAndOffset("abcdefghi*", "*", 2, 10)
    assertAbbreviateWithAbbrevMarkerAndOffset("abcdef{{{{", "{{{{", 4, 10)
    assertAbbreviateWithAbbrevMarkerAndOffset("abcdef____", "____", 5, 10)
    assertAbbreviateWithAbbrevMarkerAndOffset("==fghijk==", "==", 5, 10)
    assertAbbreviateWithAbbrevMarkerAndOffset("___ghij___", "___", 6, 10)
    assertAbbreviateWithAbbrevMarkerAndOffset("/ghijklmno", "/", 7, 10)
    assertAbbreviateWithAbbrevMarkerAndOffset("/ghijklmno", "/", 8, 10)
    assertAbbreviateWithAbbrevMarkerAndOffset("/ghijklmno", "/", 9, 10)
    assertAbbreviateWithAbbrevMarkerAndOffset("///ijklmno", "///", 10, 10)
    assertAbbreviateWithAbbrevMarkerAndOffset("//hijklmno", "//", 10, 10)
    assertAbbreviateWithAbbrevMarkerAndOffset("//hijklmno", "//", 11, 10)
    assertAbbreviateWithAbbrevMarkerAndOffset("...ijklmno", "...", 12, 10)
    assertAbbreviateWithAbbrevMarkerAndOffset("/ghijklmno", "/", 13, 10)
    assertAbbreviateWithAbbrevMarkerAndOffset("/ghijklmno", "/", 14, 10)
    assertAbbreviateWithAbbrevMarkerAndOffset("999ijklmno", "999", 15, 10)
    assertAbbreviateWithAbbrevMarkerAndOffset("_ghijklmno", "_", 16, 10)
    assertAbbreviateWithAbbrevMarkerAndOffset("+ghijklmno", "+", Integer.MAX_VALUE, 10)
  }
}
