package commons.lang3.bridge

import commons.lang3.bridge.StringUtils.ops._
import org.apache.commons.lang3.mutable.MutableInt
import org.scalatest.funsuite.AnyFunSuite

import java.nio.CharBuffer
import java.util
import java.util.Collections
import java.util.function.Supplier
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

  val noneString: Option[String]         = None
  val nullString: String                 = null
  val noneStrings: Option[Array[String]] = None

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

  test("test abbreviate middle") {
    // javadoc examples
    assert(noneString.ops.abbreviateMiddle(null, 0).isEmpty)
    assert("abc".ops.abbreviateMiddle(null, 0).contains("abc"))
    assert("abc".ops.abbreviateMiddle(Some("."), 0).contains("abc"))
    assert("abc".ops.abbreviateMiddle(Some("."), 3).contains("abc"))
    assert(Some("abcdef").ops.abbreviateMiddle(".", 4).contains("ab.f"))

    // JIRA issue (LANG-405) example (slightly different than actual expected result)
    assert(
      Some("A very long text with unimportant stuff in the middle but interesting start and " + "end to see if the text is complete.").ops
        .abbreviateMiddle("...", 50)
        .contains("A very long text with un...f the text is complete.")
    )

    // Test a much longer text :)
    val longText = "Start text" + "x".ops.repeat(10000) + "Close text"
    assert(longText.ops.abbreviateMiddle("->", 22).contains("Start text->Close text"))

    // Test negative length
    assert("abc".ops.abbreviateMiddle(".", -1).contains("abc"))

    // Test boundaries
    // Fails to change anything as method ensures first and last char are kept
    assert(Some("abc").ops.abbreviateMiddle(".", 1).contains("abc"))
    assert("abc".ops.abbreviateMiddle(Some("."), 2).contains("abc"))

    // Test length of n=1
    assert(Some("a").ops.abbreviateMiddle(".", 1).contains("a"))

    // Test smallest length that can lead to success
    assert("abcd".ops.abbreviateMiddle(".", 3).contains("a.d"))

    // More from LANG-405
    assert("abcdef".ops.abbreviateMiddle("..", 4).contains("a..f"))
    assert("abcdef".ops.abbreviateMiddle(".", 5).contains("ab.ef"))
  }

  test("test string append if missing") {
    assert(nullString.ops.appendIfMissing(noneString).isEmpty, "appendIfMissing(null,null)")
    assert(Some("abc").ops.appendIfMissing(nullString).contains("abc"), "appendIfMissing(abc,null)")
    assert("".ops.appendIfMissing(Some("xyz")).contains("xyz"), "appendIfMissing(\"\",xyz)")
    assert("abc".ops.appendIfMissing("xyz").contains("abcxyz"), "appendIfMissing(abc,xyz)")
    assert(Some("abcxyz").ops.appendIfMissing(Some("xyz")).contains("abcxyz"), "appendIfMissing(abcxyz,xyz)")
    assert("aXYZ".ops.appendIfMissing("xyz").contains("aXYZxyz"), "appendIfMissing(aXYZ,xyz)")

    assert(
      nullString.ops.appendIfMissing(nullString, null.asInstanceOf[Array[CharSequence]]: _*).isEmpty,
      "appendIfMissing(null,null,null)"
    )
    assert(
      "abc".ops.appendIfMissing(noneString, null.asInstanceOf[Array[CharSequence]]: _*).contains("abc"),
      "appendIfMissing(abc,null,null)"
    )
    assert("".ops.appendIfMissing("xyz", null.asInstanceOf[Array[CharSequence]]: _*).contains("xyz"), "appendIfMissing(\"\",xyz,null))")
    assert("abc".ops.appendIfMissing("xyz", nullString).contains("abcxyz"), "appendIfMissing(abc,xyz,{null})")
    assert("abc".ops.appendIfMissing("xyz", "").contains("abc"), "appendIfMissing(abc,xyz,\"\")")
    assert(Some("abc").ops.appendIfMissing("xyz", "mno").contains("abcxyz"), "appendIfMissing(abc,xyz,mno)")
    assert("abcxyz".ops.appendIfMissing("xyz", Some("mno")).contains("abcxyz"), "appendIfMissing(abcxyz,xyz,mno)")
    assert("abcmno".ops.appendIfMissing("xyz", "mno").contains("abcmno"), "appendIfMissing(abcmno,xyz,mno)")
    assert("abcXYZ".ops.appendIfMissing(Some("xyz"), Some("mno")).contains("abcXYZxyz"), "appendIfMissing(abcXYZ,xyz,mno)")
    assert("abcMNO".ops.appendIfMissing("xyz", "mno").contains("abcMNOxyz"), "appendIfMissing(abcMNO,xyz,mno)")

  }

  test("test append if missing ignore case") {
    assert(noneString.ops.appendIfMissingIgnoreCase(nullString).isEmpty, "appendIfMissingIgnoreCase(null,null)")
    assert("abc".ops.appendIfMissingIgnoreCase(noneString).contains("abc"), "appendIfMissingIgnoreCase(abc,null)")
    assert("".ops.appendIfMissingIgnoreCase("xyz").contains("xyz"), "appendIfMissingIgnoreCase(\"\",xyz)")
    assert(Some("abc").ops.appendIfMissingIgnoreCase("xyz").contains("abcxyz"), "appendIfMissingIgnoreCase(abc,xyz)")
    assert("abcxyz".ops.appendIfMissing("xyz").contains("abcxyz"), "appendIfMissingIgnoreCase(abcxyz,xyz)")
    assert("abcXYZ".ops.appendIfMissingIgnoreCase("xyz").contains("abcXYZ"), "appendIfMissingIgnoreCase(abcXYZ,xyz)")

    assert(nullString.ops.appendIfMissingIgnoreCase(noneString, null).isEmpty, "appendIfMissingIgnoreCase(null,null,null)")
    assert("abc".ops.appendIfMissingIgnoreCase(null, null).contains("abc"), "appendIfMissingIgnoreCase(abc,null,null)")
    assert("".ops.appendIfMissingIgnoreCase("xyz", null).contains("xyz"), "appendIfMissingIgnoreCase(\"\",xyz,null)")
    assert("abc".ops.appendIfMissingIgnoreCase("xyz", null).contains("abcxyz"), "appendIfMissingIgnoreCase(abc,xyz,{null})")
    assert(Some("abc").ops.appendIfMissingIgnoreCase(Some("xyz"), "").contains("abc"), "appendIfMissingIgnoreCase(abc,xyz,\"\")")
    assert("abc".ops.appendIfMissingIgnoreCase("xyz", "mno").contains("abcxyz"), "appendIfMissingIgnoreCase(abc,xyz,mno)")
    assert(Some("abcxyz").ops.appendIfMissingIgnoreCase("xyz", Some("mno")).contains("abcxyz"), "appendIfMissingIgnoreCase(abcxyz,xyz,mno)")
    assert("abcmno".ops.appendIfMissingIgnoreCase("xyz", "mno").contains("abcmno"), "appendIfMissingIgnoreCase(abcmno,xyz,mno)")
    assert(
      Some("abcXYZ").ops.appendIfMissingIgnoreCase(Some("xyz"), Some("mno")).contains("abcXYZ"),
      "appendIfMissingIgnoreCase(abcXYZ,xyz,mno)"
    )
    assert("abcMNO".ops.appendIfMissingIgnoreCase("abcMNO", "xyz", "mno").contains("abcMNO"), "appendIfMissingIgnoreCase(abcMNO,xyz,mno)")
  }

  test("test string capitalize") {
    assert(nullString.ops.capitalize.isEmpty)

    assert(Some("").ops.capitalize.contains(""), "capitalize(empty-string) failed")
    assert("x".ops.capitalize.contains("X"), "capitalize(single-char-string) failed")
    assert(FOO_CAP.ops.capitalize.contains(FOO_CAP), "capitalize(String) failed")
    assert(FOO_UNCAP.ops.capitalize.contains(FOO_CAP), "capitalize(string) failed")

    assert(Some("\u01C9").ops.capitalize.contains("\u01C8"), "capitalize(String) is not using TitleCase")

    // Javadoc examples
    assert(noneString.ops.capitalize.isEmpty)
    assert("".ops.capitalize.contains(""))
    assert("cat".ops.capitalize.contains("Cat"))
    assert("cAt".ops.capitalize.contains("CAt"))
    assert(Some("'cat'").ops.capitalize.contains("'cat'"))
  }

  test("test string utils center with size") {
    assert(nullString.ops.center(-1).isEmpty)
    assert(noneString.ops.center(4).isEmpty)
    assert(Some("").ops.center(4).contains("    "))
    assert("ab".ops.center(0).contains("ab"))
    assert("ab".ops.center(-1).contains("ab"))
    assert("ab".ops.center(1).contains("ab"))
    assert("".ops.center(4).contains("    "))
    assert("ab".ops.center(4).contains(" ab "))
    assert("abcd".ops.center(2).contains("abcd"))
    assert("a".ops.center(4).contains(" a  "))
    assert(Some("a").ops.center(5).contains("  a  "))
  }

  test("test string utils center with size and pad char") {
    assert(nullString.ops.center(-1, ' ').isEmpty)
    assert(noneString.ops.center(4, ' ').isEmpty)
    assert(Some("").ops.center(4, ' ').contains("    "))
    assert("ab".ops.center(0, ' ').contains("ab"))
    assert(Some("ab").ops.center(-1, ' ').contains("ab"))
    assert("ab".ops.center(1, ' ').contains("ab"))
    assert("".ops.center(4, ' ').contains("    "))
    assert("ab".ops.center(4, ' ').contains(" ab "))
    assert("abcd".ops.center(2, ' ').contains("abcd"))
    assert("a".ops.center(4, ' ').contains(" a  "))
    assert("a".ops.center(5, ' ').contains("  a  "))
    assert("a".ops.center(5, 'x').contains("xxaxx"))
  }

  test("test string utils chomp") {
    val chompCases = Array(
      Array(FOO_UNCAP + "\r\n", FOO_UNCAP),
      Array(FOO_UNCAP + "\n", FOO_UNCAP),
      Array(FOO_UNCAP + "\r", FOO_UNCAP),
      Array(FOO_UNCAP + " \r", FOO_UNCAP + " "),
      Array(FOO_UNCAP, FOO_UNCAP),
      Array(FOO_UNCAP + "\n\n", FOO_UNCAP + "\n"),
      Array(FOO_UNCAP + "\r\n\r\n", FOO_UNCAP + "\r\n"),
      Array("foo\nfoo", "foo\nfoo"),
      Array("foo\n\rfoo", "foo\n\rfoo"),
      Array("\n", ""),
      Array("\r", ""),
      Array("a", "a"),
      Array("\r\n", ""),
      Array("", ""),
      Array(nullString, nullString),
      Array(FOO_UNCAP + "\n\r", FOO_UNCAP + "\n")
    )
    for (chompCase <- chompCases) {
      val original       = chompCase(0)
      val expectedResult = chompCase(1)
      original.ops.chomp match {
        case None        => assert(expectedResult == null)
        case Some(value) => assert(value == expectedResult)
      }
    }
    // StringUtils.chomp(String, String) is deprecated
  }

  test("test default string") {
    assert(noneString.ops.defaultString == "")
    assert(Some("").ops.defaultString == "")
    assert("abc".ops.defaultString == "abc")
  }

  test("test string utils default with default value") {
    assert(nullString.ops.defaultString("NULL").contains("NULL"))
    assert("".ops.defaultString("NULL").contains(""))
    assert("abc".ops.defaultString("NULL").contains("abc"))
  }

  test("test string utils default if blank with char buffers") {
    assert(CharBuffer.wrap("").toString.ops.defaultIfBlank(CharBuffer.wrap("NULL")).get.toString == "NULL")
    assert(CharBuffer.wrap(" ").toString.ops.defaultIfBlank(CharBuffer.wrap("NULL")).get.toString == "NULL")
    assert(CharBuffer.wrap("abc").toString.ops.defaultIfBlank(CharBuffer.wrap("NULL")).get.toString == "abc")

    assert(CharBuffer.wrap("").toString.ops.defaultIfBlank(null.asInstanceOf[CharBuffer]).isEmpty)
    // Tests compatibility for the API return type
    val s = CharBuffer.wrap("abc").toString.ops.defaultIfBlank(CharBuffer.wrap("NULL"))
    assert(s.contains("abc"))
  }

  test("test string utils default string if blank string builder") {
    assert((new mutable.StringBuilder("")).toString().ops.defaultIfBlank(new mutable.StringBuilder("NULL")).get.toString == "NULL")
    assert(Some(new mutable.StringBuilder(" ").toString()).ops.defaultIfBlank(new mutable.StringBuilder("NULL")).get.toString == "NULL")
    assert((new mutable.StringBuilder("abc")).toString().ops.defaultIfBlank(new mutable.StringBuilder("NULL")).get.toString == "abc")

    assert((new mutable.StringBuilder("")).toString().ops.defaultIfBlank(null.asInstanceOf[mutable.StringBuilder]).isEmpty)
    // Tests compatibility for the API return type
    val s = Some(new mutable.StringBuilder("abc").toString()).ops.defaultIfBlank(new mutable.StringBuilder("NULL"))
    assert("abc" == s.get.toString)
  }

  test("test string utils default string if blank with string") {
    assert(nullString.ops.defaultIfBlank("NULL").contains("NULL"))
    assert("".ops.defaultIfBlank("NULL").contains("NULL"))
    assert(Some(" ").ops.defaultIfBlank("NULL").contains("NULL"))
    assert("abc".ops.defaultIfBlank("NULL").contains("abc"))
    assert("".ops.defaultIfBlank(null.asInstanceOf[String]).isEmpty)
    // Tests compatibility for the API return type
    val s = "abc".ops.defaultIfBlank("NULL")
    assert(s.contains("abc"))
  }

  test("test string get if blank with string supplier") {
    assert(nullString.ops.getIfBlank(() => "NULL") == "NULL")
    assert(Some("").ops.getIfBlank(() => "NULL") == "NULL")
    assert(" ".ops.getIfBlank(() => "NULL") == "NULL")
    assert("abc".ops.getIfBlank(() => "NULL") == "abc")
    assert("".ops.getIfBlank(() => None) == null)
    assert("".ops.getIfBlank(() => null.asInstanceOf[String]) == null)
    // Tests compatibility for the API return type
    val s = "abc".ops.getIfBlank(() => "NULL")
    assert(s.contains("abc"))
    // Checking that default value supplied only on demand
    val numberOfCalls = new MutableInt(0)
    val countingDefaultSupplier: Supplier[String] = () => {
      def foo(): String = {
        numberOfCalls.increment()
        "NULL"
      }

      foo()
    }
    "abc".ops.getIfBlank(countingDefaultSupplier)
    assert(0 == numberOfCalls.getValue)
    "".ops.getIfBlank(countingDefaultSupplier)
    assert(1 == numberOfCalls.getValue)
    " ".ops.getIfBlank(countingDefaultSupplier)
    assert(2 == numberOfCalls.getValue)
    noneString.ops.getIfBlank(countingDefaultSupplier)
    assert(3 == numberOfCalls.getValue)
  }

  test("test string default if empty with char buffers") {
    assert(Some(CharBuffer.wrap("").toString).ops.defaultIfEmpty(CharBuffer.wrap("NULL")).forall(_.toString == "NULL"))
    assert(nullString.ops.defaultIfEmpty("NULL").contains("NULL"))
    assert(CharBuffer.wrap("abc").toString.ops.defaultIfEmpty(CharBuffer.wrap("NULL")).contains("abc"))
    assert(CharBuffer.wrap("").toString.ops.defaultIfEmpty(null.asInstanceOf[CharBuffer]).isEmpty)
    // Tests compatibility for the API return type
    val s = CharBuffer.wrap("abc").toString.ops.defaultIfEmpty(CharBuffer.wrap("NULL"))
    assert(s.contains("abc"))
  }

  test("test string default if empty with string builder") {
    assert(
      Some(new mutable.StringBuilder("").toString()).ops.defaultIfEmpty(new mutable.StringBuilder("NULL")).forall(_.toString == "NULL")
    )
    assert((new mutable.StringBuilder("abc").toString()).ops.defaultIfEmpty(new mutable.StringBuilder("NULL")).get.toString == "abc")
    assert((new mutable.StringBuilder("")).toString().ops.defaultIfEmpty(noneString).isEmpty)
    // Tests compatibility for the API return type
    val s = (new mutable.StringBuilder("abc")).toString().ops.defaultIfEmpty(new mutable.StringBuilder("NULL"))
    assert(s.get.toString == "abc")
  }

  test("test string default if empty with string") {
    assert(nullString.ops.defaultIfEmpty("NULL").contains("NULL"))
    assert(Some("").ops.defaultIfEmpty("NULL").contains("NULL"))
    assert("abc".ops.defaultIfEmpty("NULL").contains("abc"))
    assert("".ops.defaultIfEmpty(null).isEmpty)
    // Tests compatibility for the API return type
    val s = "abc".ops.defaultIfEmpty("NULL")
    assert(s.contains("abc"))
  }

  test("get string get if emtpy with supplier") {
    assert(nullString.ops.getIfEmpty(() => "NULL") == "NULL")
    assert(Some("").ops.getIfEmpty(() => "NULL") == "NULL")
    assert("abc".ops.getIfEmpty(() => "NULL") == "abc")
    assert(Some("").ops.getIfEmpty(() => null) == null)
    assert("".ops.getIfEmpty(null.asInstanceOf[Supplier[String]]) == null)
    // Tests compatibility for the API return type
    val s = "abc".ops.getIfEmpty(() => "NULL")
    assert(s.contains("abc"))
    // Checking that default value supplied only on demand
    val numberOfCalls = new MutableInt(0)
    val countingDefaultSupplier: Supplier[String] = () => {
      def foo(): String = {
        numberOfCalls.increment()
        "NULL"
      }

      foo()
    }
    Some("abc").ops.getIfEmpty(countingDefaultSupplier)
    assert(0 == numberOfCalls.getValue)
    "".ops.getIfEmpty(countingDefaultSupplier)
    assert(1 == numberOfCalls.getValue)
    noneString.ops.getIfEmpty(countingDefaultSupplier)
    assert(2 == numberOfCalls.getValue)
  }

}
