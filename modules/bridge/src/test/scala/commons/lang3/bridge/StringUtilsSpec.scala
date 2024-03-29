package commons.lang3.bridge

import commons.lang3.bridge.StringUtils.ops._
import org.apache.commons.lang3.mutable.MutableInt
import org.apache.commons.lang3.{ArrayUtils, CharUtils, StringUtils => Strings}
import org.scalatest.funsuite.AnyFunSuite

import java.nio.CharBuffer
import java.nio.charset.StandardCharsets
import java.util
import java.util.function.Supplier
import java.util.{Collections, Locale, Objects}
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
  import StringUtilsSpec._

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

  private def innerTestSplit(separator: Char, sepStr: String, noMatch: Char): Unit = {
    val msg = "Failed on separator hex(" + Integer.toHexString(separator) + "), noMatch hex(" + Integer.toHexString(
      noMatch
    ) + "), sepStr(" + sepStr + ")"
    val str = "a" + separator + "b" + separator + separator + noMatch + "c"

    // (str, sepStr)
    val res1 = str.ops.split(sepStr).get
    assert(3 == res1.length, msg)
    assert("a" == res1(0))
    assert("b" == res1(1))
    assert((noMatch + "c") == res1(2))
    val str2 = separator + "a" + separator
    val res2 = str2.ops.split(sepStr).get
    assert(1 == res2.length, msg)
    assert("a" == res2(0), msg)
    val res3 = str.ops.split(sepStr, -1).get
    assert(3 == res3.length, msg)
    assert("a" == res3(0), msg)
    assert("b" == res3(1), msg)
    assert((noMatch + "c") == res3(2), msg)
    val res4 = str.ops.split(sepStr, 0).get
    assert(3 == res4.length, msg)
    assert("a" == res4(0), msg)
    assert("b" == res4(1), msg)
    assert((noMatch + "c") == res4(2), msg)
    val res5 = str.ops.split(sepStr, 1).get
    assert(1 == res5.length, msg)
    assert(str == res5(0), msg)
    val res6 = str.ops.split(sepStr, 2).get
    assert(2 == res6.length, msg)
    assert("a" == res6(0), msg)
    assert(str.substring(2) == res6(1), msg)
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
    assert(CharBuffer.wrap("").toString.ops.defaultIfBlank(CharBuffer.wrap("NULL")).toString == "NULL")
    assert(CharBuffer.wrap(" ").toString.ops.defaultIfBlank(CharBuffer.wrap("NULL")).toString == "NULL")
    assert(CharBuffer.wrap("abc").toString.ops.defaultIfBlank(CharBuffer.wrap("NULL")).toString == "abc")

    assert(CharBuffer.wrap("").toString.ops.defaultIfBlank(null.asInstanceOf[CharBuffer]) == null)
    // Tests compatibility for the API return type
    val s = CharBuffer.wrap("abc").toString.ops.defaultIfBlank(CharBuffer.wrap("NULL")).toString
    assert(s == "abc")
  }

  test("test string utils default string if blank string builder") {
    assert((new mutable.StringBuilder("")).toString().ops.defaultIfBlank(new mutable.StringBuilder("NULL")).toString == "NULL")
    assert(Some(new mutable.StringBuilder(" ").toString()).ops.defaultIfBlank(new mutable.StringBuilder("NULL")).toString == "NULL")
    assert((new mutable.StringBuilder("abc")).toString().ops.defaultIfBlank(new mutable.StringBuilder("NULL")).toString == "abc")

    assert((new mutable.StringBuilder("")).toString().ops.defaultIfBlank(null.asInstanceOf[mutable.StringBuilder]) == null)
    // Tests compatibility for the API return type
    val s = Some(new mutable.StringBuilder("abc").toString()).ops.defaultIfBlank(new mutable.StringBuilder("NULL")).toString
    assert("abc" == s)
  }

  test("test string utils default string if blank with string") {
    assert(nullString.ops.defaultIfBlank("NULL") == "NULL")
    assert("".ops.defaultIfBlank("NULL") == "NULL")
    assert(Some(" ").ops.defaultIfBlank("NULL") == "NULL")
    assert("abc".ops.defaultIfBlank("NULL") == "abc")
    assert("".ops.defaultIfBlank(null.asInstanceOf[String]) == null)
    // Tests compatibility for the API return type
    val s = "abc".ops.defaultIfBlank("NULL")
    assert(s == "abc")
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
    assert(s.toString == "abc")
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
    assert(Some(CharBuffer.wrap("").toString).ops.defaultIfEmpty(CharBuffer.wrap("NULL")).toString == "NULL")
    assert(nullString.ops.defaultIfEmpty("NULL").toString == "NULL")
    assert(CharBuffer.wrap("abc").toString.ops.defaultIfEmpty(CharBuffer.wrap("NULL")).toString == "abc")
    assert(CharBuffer.wrap("").toString.ops.defaultIfEmpty(null.asInstanceOf[CharBuffer]) == null)
    // Tests compatibility for the API return type
    val s = CharBuffer.wrap("abc").toString.ops.defaultIfEmpty(CharBuffer.wrap("NULL"))
    assert(s == "abc")
  }

  test("test string default if empty with string builder") {
    assert(
      Some(new mutable.StringBuilder("").toString()).ops.defaultIfEmpty(new mutable.StringBuilder("NULL")).toString == "NULL"
    )
    assert((new mutable.StringBuilder("abc").toString()).ops.defaultIfEmpty(new mutable.StringBuilder("NULL")).toString == "abc")
    assert((new mutable.StringBuilder("")).toString().ops.defaultIfEmpty(noneString) == null)
    // Tests compatibility for the API return type
    val s = (new mutable.StringBuilder("abc")).toString().ops.defaultIfEmpty(new mutable.StringBuilder("NULL"))
    assert(s.toString == "abc")
  }

  test("test string default if empty with string") {
    assert(nullString.ops.defaultIfEmpty("NULL") == "NULL")
    assert(Some("").ops.defaultIfEmpty("NULL") == "NULL")
    assert("abc".ops.defaultIfEmpty("NULL") == "abc")
    assert("".ops.defaultIfEmpty(null) == null)
    // Tests compatibility for the API return type
    val s = "abc".ops.defaultIfEmpty("NULL")
    assert(s == "abc")
  }

  test("get string get if emtpy with supplier") {
    assert(nullString.ops.getIfEmpty(() => "NULL") == "NULL")
    assert(Some("").ops.getIfEmpty(() => "NULL") == "NULL")
    assert("abc".ops.getIfEmpty(() => "NULL") == "abc")
    assert(Some("").ops.getIfEmpty(() => null) == null)
    assert("".ops.getIfEmpty(null.asInstanceOf[Supplier[String]]) == null)
    // Tests compatibility for the API return type
    val s = "abc".ops.getIfEmpty(() => "NULL")
    assert(s.toString == "abc")
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

  test("test string delete white space") {
    assert(nullString.ops.deleteWhitespace().isEmpty)
    assert("".ops.deleteWhitespace().contains(""))
    assert(Some("  \u000C  \t\t\u001F\n\n \u000B  ").ops.deleteWhitespace().contains(""))
    assert(WHITESPACE.ops.deleteWhitespace().contains(""))
    assert(NON_WHITESPACE.ops.deleteWhitespace().contains(NON_WHITESPACE))
    // Note: u-2007 and u-000A both cause problems in the source code
    // it should ignore 2007 but delete 000A
    assert(Some("  \u00A0  \t\t\n\n \u202F  ").ops.deleteWhitespace().contains("\u00A0\u202F"))
    assert("\u00A0\u202F".ops.deleteWhitespace().contains("\u00A0\u202F"))
    assert("\u000Bt  \t\n\u0009e\rs\n\n   \tt".ops.deleteWhitespace().contains("test"))
  }

  test("test string difference to string") {
    assert(noneString.ops.difference(nullString).ops.isEmpty)
    assert(Some("").ops.difference(Some("")).contains(""))
    assert("".ops.difference("abc").contains("abc"))
    assert("abc".ops.difference("").contains(""))
    assert(nullString.ops.difference("i am a robot").contains("i am a robot"))
    assert("i am a machine".ops.difference(noneString).contains("i am a machine"))
    assert("i am a machine".ops.difference("i am a robot").contains("robot"))
    assert("abc".ops.difference("abc").contains(""))
    assert("i am a robot".ops.difference("you are a robot").contains("you are a robot"))
  }

  test("test string index of difference between string") {
    assert(-1 == nullString.ops.indexOfDifference(noneString))
    assert(0 == nullString.ops.indexOfDifference("i am a robot"))
    assert(-1 == Some("").ops.indexOfDifference(""))
    assert(0 == Some("").ops.indexOfDifference("abc"))
    assert(0 == "abc".ops.indexOfDifference(""))
    assert(0 == "i am a machine".ops.indexOfDifference(nullString))
    assert(7 == "i am a machine".ops.indexOfDifference("i am a robot"))
    assert(-1 == "foo".ops.indexOfDifference("foo"))
    assert(0 == "i am a robot".ops.indexOfDifference("you are a robot"))
  }

  test("test string get bytes") {
    assert(ArrayUtils.EMPTY_BYTE_ARRAY sameElements nullString.ops.getBytes(nullString))
    assert(Strings.EMPTY.getBytes sameElements Strings.EMPTY.ops.getBytes(nullString))
    assert(
      Strings.EMPTY.getBytes(StandardCharsets.US_ASCII.name) sameElements
        Strings.EMPTY.ops.getBytes(StandardCharsets.US_ASCII.name)
    )
  }

  test("test string get digits") {
    assert(nullString.ops.getDigits == null)
    assert(Some("").ops.getDigits == "")
    assert("abc".ops.getDigits == "")
    assert("1000$".ops.getDigits == "1000")
    assert("123password45".ops.getDigits == "12345")
    assert(Some("(541) 754-3010").ops.getDigits == "5417543010")
    assert("\u0967\u0968\u0969".ops.getDigits == "\u0967\u0968\u0969")
  }

  test("test string is all lower case") {

    assert(!nullString.ops.isAllLowerCase)
    assert(!Strings.EMPTY.ops.isAllLowerCase)
    assert(!"  ".ops.isAllLowerCase)
    assert(Some("abc").ops.isAllLowerCase)
    assert(!"abc ".ops.isAllLowerCase)
    assert(!"abc\n".ops.isAllLowerCase)
    assert(!"abC".ops.isAllLowerCase)
    assert(!"ab c".ops.isAllLowerCase)
    assert(!"ab1c".ops.isAllLowerCase)
    assert(!"ab/c".ops.isAllLowerCase)
  }

  test("test string is all upper case") {
    assert(!noneString.ops.isAllUpperCase)
    assert(!Strings.EMPTY.ops.isAllUpperCase)
    assert(!Some("  ").ops.isAllUpperCase)
    assert(Some("ABC").ops.isAllUpperCase)
    assert(!"ABC ".ops.isAllUpperCase)
    assert(!"ABC\n".ops.isAllUpperCase)
    assert(!"aBC".ops.isAllUpperCase)
    assert(!"A C".ops.isAllUpperCase)
    assert(!"A1C".ops.isAllUpperCase)
    assert(!"A/C".ops.isAllUpperCase)
  }

  test("test string is mixed case") {
    assert(!nullString.ops.isMixedCase)
    assert(!Strings.EMPTY.ops.isMixedCase)
    assert(!" ".ops.isMixedCase)
    assert(!Some("A").ops.isMixedCase)
    assert(!"a".ops.isMixedCase)
    assert(!"/".ops.isMixedCase)
    assert(!"A/".ops.isMixedCase)
    assert(!"/b".ops.isMixedCase)
    assert(!"abc".ops.isMixedCase)
    assert(!"ABC".ops.isMixedCase)
    assert("aBc".ops.isMixedCase)
    assert("aBc ".ops.isMixedCase)
    assert("A c".ops.isMixedCase)
    assert("aBc\n".ops.isMixedCase)
    assert("A1c".ops.isMixedCase)
    assert("a/C".ops.isMixedCase)
  }

  test("test lang 623") {
    assert("\u00DE".ops.replaceChars('\u00DE', 't').contains("t"))
    assert(Some("\u00FE").ops.replaceChars('\u00FE', 't').contains("t"))
  }

  test("test lang 666") {
    assert("120.00".ops.stripEnd(".0").contains("12"))
    assert("121.00".ops.stripEnd(Some(".0")).contains("121"))
  }

  test("test string left pad with size and pad char") {
    assert(noneString.ops.leftPad(5, ' ').isEmpty)
    assert("".ops.leftPad(5, ' ').contains("     "))
    assert("abc".ops.leftPad(5, ' ').contains("  abc"))
    assert("abc".ops.leftPad(5, 'x').contains("xxabc"))
    assert("abc".ops.leftPad(5, '\uffff').contains("\uffff\uffffabc"))
    assert("abc".ops.leftPad(2, ' ').contains("abc"))
    val str = "aaa".ops.leftPad(10000, 'a').get // bigger than pad length
    assert(10000 == str.length)
    assert(str.ops.containsOnly('a'))
  }

  test("test string left pad with size and pad string") {
    assert(nullString.ops.leftPad(5, "-+").isEmpty)
    assert(noneString.ops.leftPad(5, nullString).isEmpty)
    assert("".ops.leftPad(5, " ").contains("     "))
    assert(Some("abc").ops.leftPad(7, "-+").contains("-+-+abc"))
    assert("abc".ops.leftPad(6, "-+~").contains("-+~abc"))
    assert("abc".ops.leftPad(5, "-+~").contains("-+abc"))
    assert("abc".ops.leftPad(2, " ").contains("abc"))
    assert("abc".ops.leftPad(-1, Some(" ")).contains("abc"))
    assert(Some("abc").ops.leftPad(5, noneString).contains("  abc"))
    assert("abc".ops.leftPad(5, Some("")).contains("  abc"))
  }

  test("test string lower case") {
    assert(nullString.ops.lowerCase.isEmpty)
    assert(noneString.ops.lowerCase(Locale.ENGLISH).isEmpty)
    assert("fOo test THING".ops.lowerCase.contains("foo test thing"), "lowerCase(String) failed")
    assert("".ops.lowerCase.contains(""), "lowerCase(empty-string) failed")
    assert("fOo test THING".ops.lowerCase(Locale.ENGLISH).contains("foo test thing"), "lowerCase(String, Locale) failed")
    assert("".ops.lowerCase(Locale.ENGLISH).contains(""), "lowerCase(empty-string, Locale) failed")
  }

  test("test string normalize space") {
    // Java says a non-breaking whitespace is not a whitespace.
    assert(!Character.isWhitespace('\u00A0'))
    //
    assert(nullString.ops.normalizeSpace.isEmpty)
    assert(Some("").ops.normalizeSpace.contains(""))
    assert(" ".ops.normalizeSpace.contains(""))
    assert("\t".ops.normalizeSpace.contains(""))
    assert("\n".ops.normalizeSpace.contains(""))
    assert("\u0009".ops.normalizeSpace.contains(""))
    assert("\u000B".ops.normalizeSpace.contains(""))
    assert("\u000C".ops.normalizeSpace.contains(""))
    assert("\u001C".ops.normalizeSpace.contains(""))
    assert("\u001D".ops.normalizeSpace.contains(""))
    assert("\u001E".ops.normalizeSpace.contains(""))
    assert("\u001F".ops.normalizeSpace.contains(""))
    assert("\f".ops.normalizeSpace.contains(""))
    assert("\r".ops.normalizeSpace.contains(""))
    assert("  a  ".ops.normalizeSpace.contains("a"))
    assert("  a  b   c  ".ops.normalizeSpace.contains("a b c"))
    assert("a\t\f\r  b\u000B   c\n".ops.normalizeSpace.contains("a b c"))
    assert(("a\t\f\r  " + HARD_SPACE + HARD_SPACE + "b\u000B   c\n").ops.normalizeSpace.contains("a   b c"))
    assert("\u0000b".ops.normalizeSpace.contains("b"))
    assert("b\u0000".ops.normalizeSpace.contains("b"))
  }

  test("test string overlay with start end") {
    assert(nullString.ops.overlay(noneString, 2, 4).isEmpty)
    assert(noneString.ops.overlay(nullString, -2, -4).isEmpty)

    assert("".ops.overlay(noneString, 0, 0).contains(""))
    assert("".ops.overlay("", 0, 0).contains(""))
    assert("".ops.overlay("zzzz", 0, 0).contains("zzzz"))
    assert("".ops.overlay("zzzz", 2, 4).contains("zzzz"))
    assert("".ops.overlay("zzzz", -2, -4).contains("zzzz"))

    assert("abcdef".ops.overlay(nullString, 2, 4).contains("abef"))
    assert("abcdef".ops.overlay(noneString, 4, 2).contains("abef"))
    assert(Some("abcdef").ops.overlay("", 2, 4).contains("abef"))
    assert("abcdef".ops.overlay(Some(""), 4, 2).contains("abef"))
    assert("abcdef".ops.overlay("zzzz", 2, 4).contains("abzzzzef"))
    assert("abcdef".ops.overlay("zzzz", 4, 2).contains("abzzzzef"))

    assert("abcdef".ops.overlay("zzzz", -1, 4).contains("zzzzef"))
    assert("abcdef".ops.overlay("zzzz", 4, -1).contains("zzzzef"))
    assert("abcdef".ops.overlay("zzzz", -2, -1).contains("zzzzabcdef"))
    assert("abcdef".ops.overlay("zzzz", -1, -2).contains("zzzzabcdef"))
    assert("abcdef".ops.overlay("zzzz", 4, 10).contains("abcdzzzz"))
    assert("abcdef".ops.overlay("zzzz", 10, 4).contains("abcdzzzz"))
    assert("abcdef".ops.overlay("zzzz", 8, 10).contains("abcdefzzzz"))
    assert("abcdef".ops.overlay("zzzz", 10, 8).contains("abcdefzzzz"))
  }

  test("test string prepend if missing") {
    assert(nullString.ops.prependIfMissing(noneString).isEmpty, "prependIfMissing(null,null)")
    assert("abc".ops.prependIfMissing(nullString).contains("abc"), "prependIfMissing(abc,null)")
    assert(Some("").ops.prependIfMissing("xyz").contains("xyz"), "prependIfMissing(\"\",xyz)")
    assert("abc".ops.prependIfMissing(Some("xyz")).contains("xyzabc"), "prependIfMissing(abc,xyz)")
    assert("xyzabc".ops.prependIfMissing("xyz").contains("xyzabc"), "prependIfMissing(xyzabc,xyz)")
    assert("XYZabc".ops.prependIfMissing("xyz").contains("xyzXYZabc"), "prependIfMissing(XYZabc,xyz)")

    assert(
      nullString.ops.prependIfMissing(noneString, null.asInstanceOf[Array[CharSequence]]: _*).isEmpty,
      "prependIfMissing(null,null null)"
    )
    assert(
      "abc".ops.prependIfMissing(nullString, null.asInstanceOf[Array[CharSequence]]: _*).contains("abc"),
      "prependIfMissing(abc,null,null)"
    )
    assert("".ops.prependIfMissing("xyz", null.asInstanceOf[Array[CharSequence]]: _*).contains("xyz"), "prependIfMissing(\"\",xyz,null)")
    assert("abc".ops.prependIfMissing("xyz", nullString).contains("xyzabc"), "prependIfMissing(abc,xyz,{null})")
    assert("abc".ops.prependIfMissing("xyz", "").contains("abc"), "prependIfMissing(abc,xyz,\"\")")
    assert("abc".ops.prependIfMissing("xyz", "mno").contains("xyzabc"), "prependIfMissing(abc,xyz,mno)")
    assert("xyzabc".ops.prependIfMissing("xyz", "mno").contains("xyzabc"), "prependIfMissing(xyzabc,xyz,mno)")
    assert("mnoabc".ops.prependIfMissing("xyz", "mno").contains("mnoabc"), "prependIfMissing(mnoabc,xyz,mno)")
    assert("XYZabc".ops.prependIfMissing("xyz", "mno").contains("xyzXYZabc"), "prependIfMissing(XYZabc,xyz,mno)")
    assert("MNOabc".ops.prependIfMissing("xyz", "mno").contains("xyzMNOabc"), "prependIfMissing(MNOabc,xyz,mno)")
  }

  test("test string prepend if missing ignore case") {
    assert(nullString.ops.prependIfMissingIgnoreCase(noneString).isEmpty, "prependIfMissingIgnoreCase(null,null)")
    assert("abc".ops.prependIfMissingIgnoreCase(nullString).contains("abc"), "prependIfMissingIgnoreCase(abc,null)")
    assert("".ops.prependIfMissingIgnoreCase("xyz").contains("xyz"), "prependIfMissingIgnoreCase(\"\",xyz)")
    assert("abc".ops.prependIfMissingIgnoreCase("xyz").contains("xyzabc"), "prependIfMissingIgnoreCase(abc,xyz)")
    assert("xyzabc".ops.prependIfMissingIgnoreCase("xyz").contains("xyzabc"), "prependIfMissingIgnoreCase(xyzabc,xyz)")
    assert("XYZabc".ops.prependIfMissingIgnoreCase("xyz").contains("XYZabc"), "prependIfMissingIgnoreCase(XYZabc,xyz)")

    assert(
      noneString.ops.prependIfMissingIgnoreCase(nullString, null.asInstanceOf[Array[CharSequence]]: _*).isEmpty,
      "prependIfMissingIgnoreCase(null,null null)"
    )
    assert(
      "abc".ops.prependIfMissingIgnoreCase(noneString, null.asInstanceOf[Array[CharSequence]]: _*).contains("abc"),
      "prependIfMissingIgnoreCase(abc,null,null)"
    )
    assert(
      "".ops.prependIfMissingIgnoreCase("xyz", null.asInstanceOf[Array[CharSequence]]: _*).contains("xyz"),
      "prependIfMissingIgnoreCase(\"\",xyz,null)"
    )
    assert("abc".ops.prependIfMissingIgnoreCase("xyz", noneString).contains("xyzabc"), "prependIfMissingIgnoreCase(abc,xyz,{null})")
    assert("abc".ops.prependIfMissingIgnoreCase("xyz", "").contains("abc"), "prependIfMissingIgnoreCase(abc,xyz,\"\")")
    assert("abc".ops.prependIfMissingIgnoreCase("xyz", "mno").contains("xyzabc"), "prependIfMissingIgnoreCase(abc,xyz,mno)")
    assert(Some("xyzabc").ops.prependIfMissingIgnoreCase("xyz", "mno").contains("xyzabc"), "prependIfMissingIgnoreCase(xyzabc,xyz,mno)")
    assert("mnoabc".ops.prependIfMissingIgnoreCase("xyz", "mno").contains("mnoabc"), "prependIfMissingIgnoreCase(mnoabc,xyz,mno)")
    assert("XYZabc".ops.prependIfMissingIgnoreCase("xyz", "mno").contains("XYZabc"), "prependIfMissingIgnoreCase(XYZabc,xyz,mno)")
    assert("MNOabc".ops.prependIfMissingIgnoreCase("xyz", "mno").contains("MNOabc"), "prependIfMissingIgnoreCase(MNOabc,xyz,mno)")
  }

  test("test string recapitalize") {
    // reflection type of tests: Sentences.
    assert(SENTENCE_UNCAP.ops.capitalize.ops.uncapitalize.contains(SENTENCE_UNCAP), "uncapitalize(capitalize(String)) failed")
    assert(SENTENCE_CAP.ops.uncapitalize.ops.capitalize.contains(SENTENCE_CAP), "capitalize(uncapitalize(String)) failed")

    // reflection type of tests: One word.
    assert(FOO_UNCAP.ops.capitalize.ops.uncapitalize.contains(FOO_UNCAP), "uncapitalize(capitalize(String)) failed")
    assert(FOO_CAP.ops.uncapitalize.ops.capitalize.contains(FOO_CAP), "capitalize(uncapitalize(String)) failed")
  }

  test("test string remove string") {
    // StringUtils.remove(null, *)        = null
    assert(nullString.ops.remove(nullString).isEmpty)
    assert(noneString.ops.remove("").isEmpty)
    assert(noneString.ops.remove("a").isEmpty)

    // StringUtils.remove("", *)          = ""
    assert("".ops.remove(nullString).contains(""))
    assert("".ops.remove("").contains(""))
    assert(Some("").ops.remove("a").contains(""))

    // StringUtils.remove(*, null)        = *
    assert(nullString.ops.remove(noneString).isEmpty)
    assert("".ops.remove(nullString).contains(""))
    assert("a".ops.remove(nullString).contains("a"))

    // StringUtils.remove(*, "")          = *
    assert(noneString.ops.remove("").isEmpty)
    assert("".ops.remove("").contains(""))
    assert("a".ops.remove("").contains("a"))

    // StringUtils.remove("queued", "ue") = "qd"
    assert("queued".ops.remove("ue").contains("qd"))

    // StringUtils.remove("queued", "zz") = "queued"
    assert("queued".ops.remove("zz").contains("queued"))
  }

  test("test string remove end") {
    assert(noneString.ops.removeEnd(nullString).isEmpty)
    assert(nullString.ops.removeEnd("").isEmpty)
    assert(nullString.ops.removeEnd(Some("a")).isEmpty)

    // StringUtils.removeEnd(*, null)      = *
    assert("".ops.removeEnd(nullString).contains(""))
    assert(Some("").ops.removeEnd("").contains(""))
    assert("".ops.removeEnd("a").contains(""))

    // All others:
    assert("www.domain.com.".ops.removeEnd(Some(".com")).contains("www.domain.com."))
    assert("www.domain.com".ops.removeEnd(".com").contains("www.domain"))
    assert("www.domain".ops.removeEnd(".com").contains("www.domain"))
    assert("domain.com".ops.removeEnd("").contains("domain.com"))
    assert("domain.com".ops.removeEnd(noneString).contains("domain.com"))
  }

  test("test string remove end ignore case") {
    assert(noneString.ops.removeEndIgnoreCase(nullString).isEmpty, "removeEndIgnoreCase(null, null)")
    assert(nullString.ops.removeEndIgnoreCase("").isEmpty, "removeEndIgnoreCase(null, \"\")")
    assert(nullString.ops.removeEndIgnoreCase("a").isEmpty, "removeEndIgnoreCase(null, \"a\")")

    // StringUtils.removeEnd(*, null)      = *
    assert("".ops.removeEndIgnoreCase(nullString).contains(""), "removeEndIgnoreCase(\"\", null)")
    assert("".ops.removeEndIgnoreCase("").contains(""), "removeEndIgnoreCase(\"\", \"\")")
    assert("".ops.removeEndIgnoreCase("a").contains(""), "removeEndIgnoreCase(\"\", \"a\")")

    // All others:
    assert(
      "www.domain.com.".ops.removeEndIgnoreCase(".com") contains "www.domain.com.",
      "removeEndIgnoreCase(\"www.domain.com.\", \".com\")"
    )
    assert("www.domain.com".ops.removeEndIgnoreCase(".com").contains("www.domain"), "removeEndIgnoreCase(\"www.domain.com\", \".com\")")
    assert("www.domain".ops.removeEndIgnoreCase(".com") contains "www.domain", "removeEndIgnoreCase(\"www.domain\", \".com\")")
    assert("domain.com".ops.removeEndIgnoreCase("") contains "domain.com", "removeEndIgnoreCase(\"domain.com\", \"\")")
    assert("domain.com".ops.removeEndIgnoreCase(nullString) contains "domain.com", "removeEndIgnoreCase(\"domain.com\", null)")

    // Case insensitive:
    assert("www.domain.com".ops.removeEndIgnoreCase(".COM") contains "www.domain", "removeEndIgnoreCase(\"www.domain.com\", \".COM\")")
    assert("www.domain.COM".ops.removeEndIgnoreCase(".com") contains "www.domain", "removeEndIgnoreCase(\"www.domain.COM\", \".com\")")
  }

  test("string test remove ignore case") {
    // StringUtils.removeIgnoreCase(null, *) = null
    assert(noneString.ops.removeIgnoreCase(nullString).isEmpty)
    assert(nullString.ops.removeIgnoreCase("").isEmpty)
    assert(noneString.ops.removeIgnoreCase("a").isEmpty)

    // StringUtils.removeIgnoreCase("", *) = ""
    assert("".ops.removeIgnoreCase(nullString).contains(""))
    assert("".ops.removeIgnoreCase(noneString).contains(""))
    assert("".ops.removeIgnoreCase("a").contains(""))

    // StringUtils.removeIgnoreCase(*, null) = *
    assert(nullString.ops.removeIgnoreCase(nullString).isEmpty)
    assert("".ops.removeIgnoreCase(noneString).contains(""))
    assert("a".ops.removeIgnoreCase(nullString).contains("a"))

    // StringUtils.removeIgnoreCase(*, "") = *
    assert(nullString.ops.removeIgnoreCase("").isEmpty)
    assert("".ops.removeIgnoreCase(Some("")).contains(""))
    assert("a".ops.removeIgnoreCase(Some("")).contains("a"))

    // StringUtils.removeIgnoreCase("queued", "ue") = "qd"
    assert("queued".ops.removeIgnoreCase("ue").contains("qd"))

    // StringUtils.removeIgnoreCase("queued", "zz") = "queued"
    assert("queued".ops.removeIgnoreCase("zz").contains("queued"))

    // IgnoreCase
    // StringUtils.removeIgnoreCase("quEUed", "UE") = "qd"
    assert("quEUed".ops.removeIgnoreCase("UE").contains("qd"))

    // StringUtils.removeIgnoreCase("queued", "zZ") = "queued"
    assert(Some("queued").ops.removeIgnoreCase("zZ").contains("queued"))

    // StringUtils.removeIgnoreCase("\u0130x", "x") = "\u0130"
    assert("\u0130x".ops.removeIgnoreCase("x").contains("\u0130"))

    // LANG-1453
    "İa".ops.removeIgnoreCase("a")
  }

  test("test string remove start") {
    // StringUtils.removeStart("", *)        = ""
    assert(nullString.ops.removeStart(noneString).isEmpty)
    assert(nullString.ops.removeStart("").isEmpty)
    assert(noneString.ops.removeStart("a").isEmpty)

    // StringUtils.removeStart(*, null)      = *
    assert("".ops.removeStart(nullString) contains "")
    assert("".ops.removeStart("") contains "")
    assert("".ops.removeStart("a") contains "")

    // All others:
    assert(Some("www.domain.com").ops.removeStart(Some("www.")) contains "domain.com")
    assert("domain.com".ops.removeStart("www.") contains "domain.com")
    assert("domain.com".ops.removeStart("").contains("domain.com"))
    assert("domain.com".ops.removeStart(noneString).contains("domain.com"))
  }

  test("test string remove start ignore case") {
    // StringUtils.removeStart("", *)        = ""
    assert(nullString.ops.removeStartIgnoreCase(nullString).isEmpty, "removeStartIgnoreCase(null, null)")
    assert(noneString.ops.removeStartIgnoreCase(Some("")).isEmpty, "removeStartIgnoreCase(null, \"\")")
    assert(nullString.ops.removeStartIgnoreCase("a").isEmpty, "removeStartIgnoreCase(null, \"a\")")

    // StringUtils.removeStart(*, null)      = *
    assert("".ops.removeStartIgnoreCase(nullString).contains(""), "removeStartIgnoreCase(\"\", null)")
    assert("".ops.removeStartIgnoreCase("").contains(""), "removeStartIgnoreCase(\"\", \"\")")
    assert("".ops.removeStartIgnoreCase("a").contains(""), "removeStartIgnoreCase(\"\", \"a\")")

    // All others:
    assert("www.domain.com".ops.removeStartIgnoreCase("www.") contains "domain.com", "removeStartIgnoreCase(\"www.domain.com\", \"www.\")")
    assert("domain.com".ops.removeStartIgnoreCase("www.") contains "domain.com", "removeStartIgnoreCase(\"domain.com\", \"www.\")")
    assert(Some("domain.com").ops.removeStartIgnoreCase("") contains "domain.com", "removeStartIgnoreCase(\"domain.com\", \"\")")
    assert("domain.com".ops.removeStartIgnoreCase(nullString) contains "domain.com", "removeStartIgnoreCase(\"domain.com\", null)")

    // Case insensitive:
    assert("www.domain.com".ops.removeStartIgnoreCase("WWW.").contains("domain.com"), "removeStartIgnoreCase(\"www.domain.com\", \"WWW.\")")

  }

  test("test string repeat with repeat times") {
    assert(noneString.ops.repeat(2).isEmpty)
    assert(Some("ab").ops.repeat(0).contains(""))
    assert("".ops.repeat(3).contains(""))
    assert("a".ops.repeat(3).contains("aaa"))
    assert("a".ops.repeat(-2).contains(""))
    assert("ab".ops.repeat(3).contains("ababab"))
    assert("abc".ops.repeat(3).contains("abcabcabc"))
    val str = "a".ops.repeat(10000) // bigger than pad limit
    assert(10000 == str.get.length)
    assert(str.ops.containsOnly('a'))
  }

  test("test string repeat separator and times") {
    assert(noneString.ops.repeat(noneString, 2).isEmpty)
    assert(nullString.ops.repeat("x", 2).isEmpty)
    assert("".ops.repeat(nullString, 2).contains(""))

    assert("ab".ops.repeat("", 0).contains(""))
    assert("".ops.repeat("", 2).contains(""))

    assert("".ops.repeat("x", 3).contains("xx"))

    assert("?".ops.repeat(", ", 3).contains("?, ?, ?"))
  }

  test("test string replace string array to string array") {
    // JAVADOC TESTS START
    assert(nullString.ops.replaceEach(Array[String]("a"), Array[String]("b")).isEmpty)
    assert("".ops.replaceEach(Array[String]("a"), Array[String]("b")) contains "")
    assert("aba".ops.replaceEach(null, null) contains "aba")
    assert("aba".ops.replaceEach(new Array[String](0), null) contains "aba")
    assert(Some("aba").ops.replaceEach(null, new Array[String](0)) contains "aba")
    assert("aba".ops.replaceEach(Array[String]("a"), null) contains "aba")

    assert("aba".ops.replaceEach(Array[String]("a"), Array[String]("")) contains "b")
    assert("aba".ops.replaceEach(Array[String](null), Array[String]("a")) contains "aba")
    assert("abcde".ops.replaceEach(Array[String]("ab", "d"), Array[String]("w", "t")) contains "wcte")
    assert("abcde".ops.replaceEach(Array[String]("ab", "d"), Array[String]("d", "t")) contains "dcte")
    // JAVADOC TESTS END

    assert("abc".ops.replaceEach(Array[String]("a", "b"), Array[String]("b", "c")) contains "bcc")
    assert(
      "d216.102oren".ops
        .replaceEach(
          Array[String](
            "a",
            "b",
            "c",
            "d",
            "e",
            "f",
            "g",
            "h",
            "i",
            "j",
            "k",
            "l",
            "m",
            "n",
            "o",
            "p",
            "q",
            "r",
            "s",
            "t",
            "u",
            "v",
            "w",
            "x",
            "y",
            "z",
            "A",
            "B",
            "C",
            "D",
            "E",
            "F",
            "G",
            "H",
            "I",
            "J",
            "K",
            "L",
            "M",
            "N",
            "O",
            "P",
            "Q",
            "R",
            "S",
            "T",
            "U",
            "V",
            "W",
            "X",
            "Y",
            "Z",
            "1",
            "2",
            "3",
            "4",
            "5",
            "6",
            "7",
            "8",
            "9"
          ),
          Array[String](
            "n",
            "o",
            "p",
            "q",
            "r",
            "s",
            "t",
            "u",
            "v",
            "w",
            "x",
            "y",
            "z",
            "a",
            "b",
            "c",
            "d",
            "e",
            "f",
            "g",
            "h",
            "i",
            "j",
            "k",
            "l",
            "m",
            "N",
            "O",
            "P",
            "Q",
            "R",
            "S",
            "T",
            "U",
            "V",
            "W",
            "X",
            "Y",
            "Z",
            "A",
            "B",
            "C",
            "D",
            "E",
            "F",
            "G",
            "H",
            "I",
            "J",
            "K",
            "L",
            "M",
            "5",
            "6",
            "7",
            "8",
            "9",
            "1",
            "2",
            "3",
            "4"
          )
        )
        .contains("q651.506bera")
    )

    // Test null safety inside arrays - LANG-552
    assert("aba".ops.replaceEach(Array[String]("a"), Array[String](null)) contains "aba")
    assert("aba".ops.replaceEach(Array[String]("a", "b"), Array[String]("c", null)) contains "cbc")

    assertThrows[IllegalArgumentException] {
      "abba".ops.replaceEach(Array[String]("a"), Array[String]("b", "a"))
    }
  }

  test("test string replace string to string") {
    assert(nullString.ops.replace(nullString, noneString).isEmpty)
    assert(noneString.ops.replace(nullString, "any").isEmpty)
    assert(nullString.ops.replace("any", null).isEmpty)
    assert(nullString.ops.replace("any", "any").isEmpty)

    assert(Some("").ops.replace(nullString, nullString).contains(""))
    assert("".ops.replace(noneString, "any").contains(""))
    assert("".ops.replace("any", nullString).contains(""))
    assert("".ops.replace(Some("any"), Some("any")).contains(""))

    assert("FOO".ops.replace("", "any").contains("FOO"))
    assert("FOO".ops.replace(nullString, "any").contains("FOO"))
    assert("FOO".ops.replace("F", nullString).contains("FOO"))
    assert("FOO".ops.replace(noneString, null).contains("FOO"))

    assert("foofoofoo".ops.replace("foo", "").contains(""))
    assert(Some("foofoofoo").ops.replace("foo", "bar").contains("barbarbar"))
    assert("foofoofoo".ops.replace("oo", "ar").contains("farfarfar"))
  }

  test("test string replace string to string with max times") {
    assert(nullString.ops.replace(noneString, noneString, 2).isEmpty)
    assert(noneString.ops.replace(nullString, Some("any"), 2).isEmpty)
    assert(nullString.ops.replace(Some("any"), noneString, 2).isEmpty)
    assert(nullString.ops.replace("any", "any", 2).isEmpty)

    assert("".ops.replace(noneString, noneString, 2).contains(""))
    assert("".ops.replace(noneString, "any", 2).contains(""))
    assert(Some("").ops.replace("any", noneString, 2).contains(""))
    assert("".ops.replace("any", "any", 2).contains(""))

    val str = new String(Array[Char]('o', 'o', 'f', 'o', 'o'))
    assert(str.ops.replace("x", "", -1).contains(str))

    assert("oofoo".ops.replace("o", "", -1).contains("f"))
    assert("oofoo".ops.replace("o", "", 0).contains("oofoo"))
    assert(Some("oofoo").ops.replace("o", "", 1).contains("ofoo"))
    assert("oofoo".ops.replace("o", "", 2).contains("foo"))
    assert("oofoo".ops.replace("o", "", 3).contains("fo"))
    assert("oofoo".ops.replace("o", "", 4).contains("f"))

    assert("oofoo".ops.replace("o", "", -5).contains("f"))
    assert("oofoo".ops.replace("o", "", 1000).contains("f"))
  }

  test("test string replace string char to char") {
    assert(nullString.ops.replaceChars('b', 'z').isEmpty)
    assert("".ops.replaceChars('b', 'z').contains(""))
    assert("abcba".ops.replaceChars('b', 'z').contains("azcza"))
    assert("abcba".ops.replaceChars('x', 'z').contains("abcba"))
  }

  test("test string replace chars string to chars string") {
    assert(nullString.ops.replaceChars(nullString, nullString).isEmpty)
    assert(noneString.ops.replaceChars("", null).isEmpty)
    assert(nullString.ops.replaceChars("a", noneString).isEmpty)
    assert(nullString.ops.replaceChars(nullString, "").isEmpty)
    assert(nullString.ops.replaceChars(noneString, Some("x")).isEmpty)

    assert("".ops.replaceChars(noneString, noneString).contains(""))
    assert("".ops.replaceChars("", noneString).contains(""))
    assert("".ops.replaceChars("a", nullString).contains(""))
    assert("".ops.replaceChars(nullString, "").contains(""))
    assert("".ops.replaceChars(nullString, "x").contains(""))

    assert(Some("abc").ops.replaceChars(noneString, nullString).contains("abc"))
    assert("abc".ops.replaceChars(nullString, "").contains("abc"))
    assert("abc".ops.replaceChars(nullString, "x").contains("abc"))

    assert("abc".ops.replaceChars("", nullString).contains("abc"))
    assert("abc".ops.replaceChars("", "").contains("abc"))
    assert("abc".ops.replaceChars(Some(""), Some("x")).contains("abc"))

    assert("abc".ops.replaceChars("b", null).contains("ac"))
    assert("abc".ops.replaceChars("b", "").contains("ac"))
    assert("abc".ops.replaceChars("b", "x").contains("axc"))

    assert("abcba".ops.replaceChars("bc", "yz").contains("ayzya"))
    assert("abcba".ops.replaceChars("bc", "y").contains("ayya"))
    assert("abcba".ops.replaceChars("bc", "yzx").contains("ayzya"))

    assert("abcba".ops.replaceChars("z", "w").contains("abcba"))
    assert("abcba".ops.replaceChars("z", "w").contains("abcba"))

    // Javadoc examples:
    assert(Some("hello").ops.replaceChars("ho", "jy").contains("jelly"))
    assert("abcba".ops.replaceChars("bc", "yz").contains("ayzya"))
    assert("abcba".ops.replaceChars("bc", "y").contains("ayya"))
    assert("abcba".ops.replaceChars("bc", "yzx").contains("ayzya"))

    // From https://issues.apache.org/bugzilla/show_bug.cgi?id=25454
    assert("abc".ops.replaceChars("ab", "bc").contains("bcc"))
    assert(
      "d216.102oren".ops
        .replaceChars(
          "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ123456789",
          "nopqrstuvwxyzabcdefghijklmNOPQRSTUVWXYZABCDEFGHIJKLM567891234"
        )
        .contains("q651.506bera")
    )
  }

  test("test string replace ignore case from string to string") {
    assert(nullString.ops.replaceIgnoreCase(noneString, noneString).isEmpty)
    assert(noneString.ops.replaceIgnoreCase(nullString, Some("any")).isEmpty)
    assert(nullString.ops.replaceIgnoreCase("any", nullString).isEmpty)
    assert(nullString.ops.replaceIgnoreCase("any", "any").isEmpty)

    assert("".ops.replaceIgnoreCase(nullString, nullString).contains(""))
    assert(Some("").ops.replaceIgnoreCase(noneString, "any").contains(""))
    assert("".ops.replaceIgnoreCase("any", nullString).contains(""))
    assert("".ops.replaceIgnoreCase("any", "any").contains(""))

    assert("FOO".ops.replaceIgnoreCase("", "any").contains("FOO"))
    assert("FOO".ops.replaceIgnoreCase(noneString, "any").contains("FOO"))
    assert("FOO".ops.replaceIgnoreCase("F", nullString).contains("FOO"))
    assert("FOO".ops.replaceIgnoreCase(nullString, nullString).contains("FOO"))

    assert("foofoofoo".ops.replaceIgnoreCase("foo", "").contains(""))
    assert("foofoofoo".ops.replaceIgnoreCase("foo", "bar").contains("barbarbar"))
    assert("foofoofoo".ops.replaceIgnoreCase("oo", "ar").contains("farfarfar"))

    // IgnoreCase
    assert(Some("foofoofoo").ops.replaceIgnoreCase("FOO", "").contains(""))
    assert("fooFOOfoo".ops.replaceIgnoreCase("foo", "bar").contains("barbarbar"))
    assert("foofOOfoo".ops.replaceIgnoreCase("OO", Some("ar")).contains("farfarfar"))
  }

  test("test string replace ignore case from string to string max times") {
    assert(noneString.ops.replaceIgnoreCase(nullString, nullString, 2).isEmpty)
    assert(nullString.ops.replaceIgnoreCase(nullString, Some("any"), 2).isEmpty)
    assert(nullString.ops.replaceIgnoreCase("any", noneString, 2).isEmpty)
    assert(noneString.ops.replaceIgnoreCase(Some("any"), "any", 2).isEmpty)

    assert(Some("").ops.replaceIgnoreCase(nullString, nullString, 2).contains(""))
    assert("".ops.replaceIgnoreCase(nullString, "any", 2).contains(""))
    assert("".ops.replaceIgnoreCase("any", nullString, 2).contains(""))
    assert("".ops.replaceIgnoreCase("any", "any", 2).contains(""))

    val str = new String(Array[Char]('o', 'o', 'f', 'o', 'o'))
    assert(Some(str).ops.replaceIgnoreCase("x", "", -1).contains(str))

    assert("oofoo".ops.replaceIgnoreCase("o", "", -1).contains("f"))
    assert(Some("oofoo").ops.replaceIgnoreCase("o", "", 0).contains("oofoo"))
    assert("oofoo".ops.replaceIgnoreCase("o", "", 1).contains("ofoo"))
    assert("oofoo".ops.replaceIgnoreCase("o", "", 2).contains("foo"))
    assert("oofoo".ops.replaceIgnoreCase("o", "", 3).contains("fo"))
    assert("oofoo".ops.replaceIgnoreCase("o", "", 4).contains("f"))

    assert("oofoo".ops.replaceIgnoreCase("o", "", -5).contains("f"))
    assert("oofoo".ops.replaceIgnoreCase("o", "", 1000).contains("f"))

    // IgnoreCase
    assert("oofoo".ops.replaceIgnoreCase("O", "", -1).contains("f"))
    assert("oofoo".ops.replaceIgnoreCase("O", "", 0).contains("oofoo"))
    assert("oofoo".ops.replaceIgnoreCase("O", "", 1).contains("ofoo"))
    assert("oofoo".ops.replaceIgnoreCase("O", "", 2).contains("foo"))
    assert("oofoo".ops.replaceIgnoreCase("O", "", 3).contains("fo"))
    assert("oofoo".ops.replaceIgnoreCase("O", "", 4).contains("f"))

    assert("oofoo".ops.replaceIgnoreCase("O", "", -5).contains("f"))
    assert("oofoo".ops.replaceIgnoreCase("O", "", 1000).contains("f"))

  }

  test("test string replace once from string to string") {
    assert(noneString.ops.replaceOnce(nullString, nullString).isEmpty)
    assert(nullString.ops.replaceOnce(nullString, "any").isEmpty)
    assert(nullString.ops.replaceOnce("any", null).isEmpty)
    assert(nullString.ops.replaceOnce("any", "any").isEmpty)

    assert("".ops.replaceOnce(nullString, nullString).contains(""))
    assert(Some("").ops.replaceOnce(noneString, "any").contains(""))
    assert("".ops.replaceOnce(Some("any"), noneString).contains(""))
    assert("".ops.replaceOnce("any", "any").contains(""))

    assert("FOO".ops.replaceOnce("", "any").contains("FOO"))
    assert("FOO".ops.replaceOnce(nullString, "any").contains("FOO"))
    assert("FOO".ops.replaceOnce("F", nullString).contains("FOO"))
    assert("FOO".ops.replaceOnce(nullString, nullString).contains("FOO"))

    assert("foofoofoo".ops.replaceOnce("foo", "").contains("foofoo"))
  }

  test("test string replace once ignore case from string to string") {
    assert(nullString.ops.replaceOnceIgnoreCase(nullString, nullString).isEmpty)
    assert(noneString.ops.replaceOnceIgnoreCase(nullString, "any").isEmpty)
    assert(nullString.ops.replaceOnceIgnoreCase(noneString, "any").isEmpty)
    assert(noneString.ops.replaceOnceIgnoreCase("any", "any").isEmpty)

    assert("".ops.replaceOnceIgnoreCase(noneString, noneString).contains(""))
    assert("".ops.replaceOnceIgnoreCase(noneString, "any").contains(""))
    assert("".ops.replaceOnceIgnoreCase("any", nullString).contains(""))
    assert(Some("").ops.replaceOnceIgnoreCase(Some("any"), Some("any")).contains(""))

    assert("FOO".ops.replaceOnceIgnoreCase("", "any").contains("FOO"))
    assert("FOO".ops.replaceOnceIgnoreCase(nullString, "any").contains("FOO"))
    assert("FOO".ops.replaceOnceIgnoreCase("F", null).contains("FOO"))
    assert("FOO".ops.replaceOnceIgnoreCase(nullString, nullString).contains("FOO"))

    assert("foofoofoo".ops.replaceOnceIgnoreCase("foo", "").contains("foofoo"))

    // Ignore Case
    assert("FoOFoofoo".ops.replaceOnceIgnoreCase("foo", "").contains("Foofoo"))
  }

  test("test string reverse") {
    assert(noneString.ops.reverse.isEmpty)
    assert("".ops.reverse.contains(""))
    assert(Some("backwards").ops.reverse.contains("sdrawkcab"))
  }

  test("test string reverse delimited") {
    assert(nullString.ops.reverseDelimited('.').isEmpty)
    assert("".ops.reverseDelimited('.').contains(""))
    assert("a.b.c".ops.reverseDelimited('.').contains("c.b.a"))
    assert("a b c".ops.reverseDelimited('.').contains("a b c"))
    assert("".ops.reverseDelimited('.').contains(""))
  }

  test("test string right pad with size") {
    assert(nullString.ops.rightPad(5).isEmpty)
    assert(Some("").ops.rightPad(5).contains("     "))
    assert("abc".ops.rightPad(5).contains("abc  "))
    assert("abc".ops.rightPad(2).contains("abc"))
    assert(Some("abc").ops.rightPad(-1).contains("abc"))
  }

  test("test string right pad with size and pad char") {
    assert(nullString.ops.rightPad(5, ' ').isEmpty)
    assert("".ops.rightPad(5, ' ').contains("     "))
    assert(Some("abc").ops.rightPad(5, ' ').contains("abc  "))
    assert("abc".ops.rightPad(2, ' ').contains("abc"))
    assert("abc".ops.rightPad(-1, ' ').contains("abc"))
    assert("abc".ops.rightPad(5, 'x').contains("abcxx"))
    val str = "aaa".ops.rightPad(10000, 'a') // bigger than pad length
    assert(str.get.length == 10000)
    assert(str.ops.containsOnly('a'))
  }

  test("test string right pad with size and pad string") {
    assert(nullString.ops.rightPad(5, "-+").isEmpty)
    assert("".ops.rightPad(5, " ").contains("     "))
    assert(noneString.ops.rightPad(8, null).isEmpty)
    assert("abc".ops.rightPad(7, Some("-+")).contains("abc-+-+"))
    assert("abc".ops.rightPad(6, "-+~").contains("abc-+~"))
    assert("abc".ops.rightPad(5, "-+~").contains("abc-+"))
    assert("abc".ops.rightPad(2, " ").contains("abc"))
    assert(Some("abc").ops.rightPad(-1, " ").contains("abc"))
    assert("abc".ops.rightPad(5, null).contains("abc  "))
    assert("abc".ops.rightPad(5, "").contains("abc  "))
  }

  test("test string rotate with shift") {
    assert(nullString.ops.rotate(1).isEmpty)
    assert("".ops.rotate(1).contains(""))
    assert(Some("abcdefg").ops.rotate(0).contains("abcdefg"))
    assert("abcdefg".ops.rotate(2).contains("fgabcde"))
    assert("abcdefg".ops.rotate(-2).contains("cdefgab"))
    assert("abcdefg".ops.rotate(7).contains("abcdefg"))
    assert("abcdefg".ops.rotate(-7).contains("abcdefg"))
    assert("abcdefg".ops.rotate(9).contains("fgabcde"))
    assert("abcdefg".ops.rotate(-9).contains("cdefgab"))
    assert("abcdefg".ops.rotate(17).contains("efgabcd"))
    assert("abcdefg".ops.rotate(-17).contains("defgabc"))
  }

  test("test string split") {
    assert(nullString.ops.split.isEmpty)
    assert("".ops.split.exists(_.length == 0))

    val str1 = "a b  .c"
    val res1 = str1.ops.split
    assert(res1.exists(_.length == 3))
    assert(res1.exists(elements => elements.head == "a"))
    assert(res1.exists(elements => elements(1) == "b"))
    assert(res1.exists(elements => elements(2) == ".c"))

    val str2 = " a "
    val res2 = str2.ops.split.get
    assert(1 == res2.length)
    assert("a" == res2.head)

    val str3 = "a" + WHITESPACE + "b" + NON_WHITESPACE + "c"
    val res3 = str3.ops.split.get
    assert(2 == res3.length)
    assert("a" == res3.head)
    assert("b" + NON_WHITESPACE + "c" == res3(1))
  }

  test("test string split with separator char") {
    assert(noneString.ops.split('.').isEmpty)
    assert(Some("").ops.split('.').exists(_.length == 0))

    val str1 = "a.b.. c"
    val res1 = str1.ops.split('.').get
    assert(3 == res1.length)
    assert("a" == res1.head)
    assert("b" == res1(1))
    assert(" c" == res1.last)

    val str2 = ".a."
    val res2 = str2.ops.split('.').get
    assert(res2.length == 1)
    assert("a" == res2.head)

    val str3 = "a b c"
    val res3 = str3.ops.split(' ').get
    assert(3 == res3.length)
    assert("a" == res3.head)
    assert("b" == res3(1))
    assert("c" == res3.last)
  }

  test("test string split with separator chars string and max times") {
    assert(nullString.ops.split(".").isEmpty)
    assert(noneString.ops.split(".", 3).isEmpty)

    assert("".ops.split(".").exists(_.length == 0))
    assert("".ops.split(".", 3).exists(_.length == 0))

    innerTestSplit('.', ".", ' ')
    innerTestSplit('.', ".", ',')
    innerTestSplit('.', ".,", 'x')
    for (i <- 0 until WHITESPACE.length) {
      for (j <- 0 until NON_WHITESPACE.length) {
        innerTestSplit(WHITESPACE.charAt(i), null, NON_WHITESPACE.charAt(j))
        innerTestSplit(WHITESPACE.charAt(i), String.valueOf(WHITESPACE.charAt(i)), NON_WHITESPACE.charAt(j))
      }
    }

    val expectedResults = Array("ab", "de fg")
    val results1        = "ab   de fg".ops.split(null, 2).get
    assert(expectedResults.length == results1.length)
    for (i <- expectedResults.indices) {
      assert(expectedResults(i) == results1(i))
    }

    val expectedResults2 = Array("ab", "cd:ef")
    val results2         = "ab:cd:ef".ops.split(":", 2).get
    assert(expectedResults2.length == results2.length)
    for (i <- expectedResults2.indices) {
      assert(expectedResults2(i) == results2(i))
    }
  }

  test("test string split by character type") {
    assert(nullString.ops.splitByCharacterType.isEmpty)
    assert("".ops.splitByCharacterType.exists(_.length == 0))

    assert("ab de fg".ops.splitByCharacterType.exists(Objects.deepEquals(Array[String]("ab", " ", "de", " ", "fg"), _)))

    assert(Some("ab   de fg").ops.splitByCharacterType.exists(Objects.deepEquals(Array[String]("ab", "   ", "de", " ", "fg"), _)))

    assert("ab:cd:ef".ops.splitByCharacterType.exists(Objects.deepEquals(Array[String]("ab", ":", "cd", ":", "ef"), _)))

    assert("number5".ops.splitByCharacterType.exists(Objects.deepEquals(Array[String]("number", "5"), _)))

    assert("fooBar".ops.splitByCharacterType.exists(Objects.deepEquals(Array[String]("foo", "B", "ar"), _)))

    assert("foo200Bar".ops.splitByCharacterType.exists(Objects.deepEquals(Array[String]("foo", "200", "B", "ar"), _)))

    assert("ASFRules".ops.splitByCharacterType.exists(Objects.deepEquals(Array[String]("ASFR", "ules"), _)))
  }

  test("test string split by character type camel case") {
    assert(noneString.ops.splitByCharacterTypeCamelCase.isEmpty)
    assert("".ops.splitByCharacterTypeCamelCase.exists(_.length == 0))

    assert("ab de fg".ops.splitByCharacterTypeCamelCase.exists(Objects.deepEquals(Array[String]("ab", " ", "de", " ", "fg"), _)))

    assert("ab   de fg".ops.splitByCharacterTypeCamelCase.exists(Objects.deepEquals(Array[String]("ab", "   ", "de", " ", "fg"), _)))

    assert(Some("ab:cd:ef").ops.splitByCharacterTypeCamelCase.exists(Objects.deepEquals(Array[String]("ab", ":", "cd", ":", "ef"), _)))

    assert("number5".ops.splitByCharacterTypeCamelCase.exists(Objects.deepEquals(Array[String]("number", "5"), _)))

    assert("fooBar".ops.splitByCharacterTypeCamelCase.exists(Objects.deepEquals(Array[String]("foo", "Bar"), _)))

    assert("foo200Bar".ops.splitByCharacterTypeCamelCase.exists(Objects.deepEquals(Array[String]("foo", "200", "Bar"), _)))

    assert("ASFRules".ops.splitByCharacterTypeCamelCase.exists(Objects.deepEquals(Array[String]("ASF", "Rules"), _)))
  }

  test("test string split by whole separator preserve all tokens string") {
    assert(nullString.ops.splitByWholeSeparatorPreserveAllTokens(".").isEmpty)

    assert("".ops.splitByWholeSeparatorPreserveAllTokens(".").exists(_.length == 0))

    // test whitespace
    var input    = "ab   de fg"
    var expected = Array[String]("ab", "", "", "de", "fg")

    var actual = input.ops.splitByWholeSeparatorPreserveAllTokens(null).get
    assert(expected.length == actual.length)
    for (i <- actual.indices) {
      assert(expected(i) == actual(i))
    }

    // test delimiter singlechar
    input = "1::2:::3::::4"
    expected = Array[String]("1", "", "2", "", "", "3", "", "", "", "4")

    actual = input.ops.splitByWholeSeparatorPreserveAllTokens(":").get
    assert(expected.length == actual.length)
    for (i <- actual.indices) {
      assert(expected(i) == actual(i))
    }

    // test delimiter multichar
    input = "1::2:::3::::4"
    expected = Array[String]("1", "2", ":3", "", "4")

    actual = input.ops.splitByWholeSeparatorPreserveAllTokens("::").get
    assert(expected.length == actual.length)
    for (i <- actual.indices) {
      assert(expected(i) == actual(i))
    }
  }

  test("test string split by whole separator preserve all tokens with separator and max times") {
    assert(nullString.ops.splitByWholeSeparatorPreserveAllTokens(".", -1).isEmpty)

    assert(Some("").ops.splitByWholeSeparatorPreserveAllTokens(".", -1).exists(_.length == 0))

    // test whitespace
    var input    = "ab   de fg"
    var expected = Array[String]("ab", "", "", "de", "fg")

    var actual = input.ops.splitByWholeSeparatorPreserveAllTokens(noneString, -1).get
    assert(expected.length == actual.length)
    for (i <- actual.indices) {
      assert(expected(i) == actual(i))
    }

    // test delimiter singlechar
    input = "1::2:::3::::4"
    expected = Array[String]("1", "", "2", "", "", "3", "", "", "", "4")

    actual = input.ops.splitByWholeSeparatorPreserveAllTokens(Some(":"), -1).get
    assert(expected.length == actual.length)
    for (i <- actual.indices) {
      assert(expected(i) == actual(i))
    }

    // test delimiter multichar
    input = "1::2:::3::::4"
    expected = Array[String]("1", "2", ":3", "", "4")

    actual = input.ops.splitByWholeSeparatorPreserveAllTokens("::", -1).get
    assert(expected.length == actual.length)
    for (i <- actual.indices) {
      assert(expected(i) == actual(i))
    }

    // test delimiter char with max
    input = "1::2::3:4"
    expected = Array[String]("1", "", "2", ":3:4")

    actual = input.ops.splitByWholeSeparatorPreserveAllTokens(":", 4).get
    assert(expected.length == actual.length)
    for (i <- actual.indices) {
      assert(expected(i) == actual(i))
    }
  }

  test("test string split by whole string separator") {
    assert(nullString.ops.splitByWholeSeparator(".").isEmpty)

    assert("".ops.splitByWholeSeparator(".").exists(_.length == 0))

    val stringToSplitOnNulls       = "ab   de fg"
    val splitOnNullExpectedResults = Array("ab", "de", "fg")

    val splitOnNullResults = stringToSplitOnNulls.ops.splitByWholeSeparator(null).get
    assert(splitOnNullExpectedResults.length == splitOnNullResults.length)
    for (i <- splitOnNullExpectedResults.indices) {
      assert(splitOnNullExpectedResults(i) == splitOnNullResults(i))
    }

    val stringToSplitOnCharactersAndString = "abstemiouslyaeiouyabstemiously"

    val splitOnStringExpectedResults = Array("abstemiously", "abstemiously")
    val splitOnStringResults         = stringToSplitOnCharactersAndString.ops.splitByWholeSeparator("aeiouy").get
    assert(splitOnStringExpectedResults.length == splitOnStringResults.length)
    for (i <- splitOnStringExpectedResults.indices) {
      assert(splitOnStringExpectedResults(i) == splitOnStringResults(i))
    }

    val splitWithMultipleSeparatorExpectedResults = Array("ab", "cd", "ef")
    val splitWithMultipleSeparator                = "ab:cd::ef".ops.splitByWholeSeparator(":").get
    assert(splitWithMultipleSeparatorExpectedResults.length == splitWithMultipleSeparator.length)
    for (i <- splitWithMultipleSeparatorExpectedResults.indices) {
      assert(splitWithMultipleSeparatorExpectedResults(i) == splitWithMultipleSeparator(i))
    }
  }

  test("test split by whole string separator and max times") {
    assert(noneString.ops.splitByWholeSeparator(".", 3).isEmpty)

    assert("".ops.splitByWholeSeparator(".", 3).exists(_.length == 0))

    val stringToSplitOnNulls       = "ab   de fg"
    val splitOnNullExpectedResults = Array("ab", "de fg")
    // String[] splitOnNullExpectedResults = { "ab", "de" } ;

    val splitOnNullResults = Some(stringToSplitOnNulls).ops.splitByWholeSeparator(null, 2).get
    assert(splitOnNullExpectedResults.length == splitOnNullResults.length)
    for (i <- splitOnNullExpectedResults.indices) {
      assert(splitOnNullExpectedResults(i) == splitOnNullResults(i))
    }

    val stringToSplitOnCharactersAndString = "abstemiouslyaeiouyabstemiouslyaeiouyabstemiously"

    val splitOnStringExpectedResults = Array("abstemiously", "abstemiouslyaeiouyabstemiously")
    // String[] splitOnStringExpectedResults = { "abstemiously", "abstemiously" } ;
    val splitOnStringResults = stringToSplitOnCharactersAndString.ops.splitByWholeSeparator("aeiouy", 2).get
    assert(splitOnStringExpectedResults.length == splitOnStringResults.length)
    for (i <- splitOnStringExpectedResults.indices) {
      assert(splitOnStringExpectedResults(i) == splitOnStringResults(i))
    }
  }

  test("test string split preserve all tokens") {
    assert(noneString.ops.splitPreserveAllTokens.isEmpty)
    assert("".ops.splitPreserveAllTokens.exists(_.length == 0))

    var str = "abc def"
    var res = Some(str).ops.splitPreserveAllTokens.get
    assert(2 == res.length)
    assert("abc" == res(0))
    assert("def" == res(1))

    str = "abc  def"
    res = str.ops.splitPreserveAllTokens.get
    assert(3 == res.length)
    assert("abc" == res(0))
    assert("" == res(1))
    assert("def" == res(2))

    str = " abc "
    res = str.ops.splitPreserveAllTokens.get
    assert(3 == res.length)
    assert("" == res(0))
    assert("abc" == res(1))
    assert("" == res(2))

    str = "a b .c"
    res = str.ops.splitPreserveAllTokens.get
    assert(3 == res.length)
    assert("a" == res(0))
    assert("b" == res(1))
    assert(".c" == res(2))

    str = " a b .c"
    res = str.ops.splitPreserveAllTokens.get
    assert(4 == res.length)
    assert("" == res(0))
    assert("a" == res(1))
    assert("b" == res(2))
    assert(".c" == res(3))

    str = "a  b  .c"
    res = str.ops.splitPreserveAllTokens.get
    assert(5 == res.length)
    assert("a" == res(0))
    assert("" == res(1))
    assert("b" == res(2))
    assert("" == res(3))
    assert(".c" == res(4))

    str = " a  "
    res = str.ops.splitPreserveAllTokens.get
    assert(4 == res.length)
    assert("" == res(0))
    assert("a" == res(1))
    assert("" == res(2))
    assert("" == res(3))

    str = " a  b"
    res = str.ops.splitPreserveAllTokens.get
    assert(4 == res.length)
    assert("" == res(0))
    assert("a" == res(1))
    assert("" == res(2))
    assert("b" == res(3))

    str = "a" + WHITESPACE + "b" + NON_WHITESPACE + "c"
    res = str.ops.splitPreserveAllTokens.get
    assert(WHITESPACE.length + 1 == res.length)
    assert("a" == res(0))
    for (i <- 1 until WHITESPACE.length - 1) {
      assert("" == res(i))
    }
    assert("b" + NON_WHITESPACE + "c" == res(WHITESPACE.length))
  }

  test("test string split preserve all tokens by separator char") {
    assert(noneString.ops.splitPreserveAllTokens('.').isEmpty)
    assert("".ops.splitPreserveAllTokens('.').exists(_.length == 0))

    var str = "a.b. c"
    var res = Some(str).ops.splitPreserveAllTokens('.').get
    assert(3 == res.length)
    assert("a" == res(0))
    assert("b" == res(1))
    assert(" c" == res(2))

    str = "a.b.. c"
    res = str.ops.splitPreserveAllTokens('.').get
    assert(4 == res.length)
    assert("a" == res(0))
    assert("b" == res(1))
    assert("" == res(2))
    assert(" c" == res(3))

    str = ".a."
    res = str.ops.splitPreserveAllTokens('.').get
    assert(3 == res.length)
    assert("" == res(0))
    assert("a" == res(1))
    assert("" == res(2))

    str = ".a.."
    res = str.ops.splitPreserveAllTokens('.').get
    assert(4 == res.length)
    assert("" == res(0))
    assert("a" == res(1))
    assert("" == res(2))
    assert("" == res(3))

    str = "..a."
    res = str.ops.splitPreserveAllTokens('.').get
    assert(4 == res.length)
    assert("" == res(0))
    assert("" == res(1))
    assert("a" == res(2))
    assert("" == res(3))

    str = "..a"
    res = str.ops.splitPreserveAllTokens('.').get
    assert(3 == res.length)
    assert("" == res(0))
    assert("" == res(1))
    assert("a" == res(2))

    str = "a b c"
    res = Some(str).ops.splitPreserveAllTokens(' ').get
    assert(3 == res.length)
    assert("a" == res(0))
    assert("b" == res(1))
    assert("c" == res(2))

    str = "a  b  c"
    res = str.ops.splitPreserveAllTokens(' ').get
    assert(5 == res.length)
    assert("a" == res(0))
    assert("" == res(1))
    assert("b" == res(2))
    assert("" == res(3))
    assert("c" == res(4))

    str = " a b c"
    res = str.ops.splitPreserveAllTokens(' ').get
    assert(4 == res.length)
    assert("" == res(0))
    assert("a" == res(1))
    assert("b" == res(2))
    assert("c" == res(3))

    str = "  a b c"
    res = str.ops.splitPreserveAllTokens(' ').get
    assert(5 == res.length)
    assert("" == res(0))
    assert("" == res(1))
    assert("a" == res(2))
    assert("b" == res(3))
    assert("c" == res(4))

    str = "a b c "
    res = Some(str).ops.splitPreserveAllTokens(' ').get
    assert(4 == res.length)
    assert("a" == res(0))
    assert("b" == res(1))
    assert("c" == res(2))
    assert("" == res(3))

    str = "a b c  "
    res = str.ops.splitPreserveAllTokens(' ').get
    assert(5 == res.length)
    assert("a" == res(0))
    assert("b" == res(1))
    assert("c" == res(2))
    assert("" == res(3))
    assert("" == res(3))

    // Match example in javadoc
    val expectedResults = Array("a", "", "b", "c")
    val results         = "a..b.c".ops.splitPreserveAllTokens('.').get
    assert(expectedResults.length == results.length)
    for (i <- expectedResults.indices) {
      assert(expectedResults(i) == results(i))
    }

  }

  test("test string split preserve all tokens with separator char and max times") {
    assert(noneString.ops.splitPreserveAllTokens(".").isEmpty)
    assert(nullString.ops.splitPreserveAllTokens(".", 3).isEmpty)

    assert("".ops.splitPreserveAllTokens(Some(".")).exists(_.length == 0))
    assert(Some("").ops.splitPreserveAllTokens(".", 3).exists(_.length == 0))

    innerTestSplitPreserveAllTokens('.', ".", ' ')
    innerTestSplitPreserveAllTokens('.', ".", ',')
    innerTestSplitPreserveAllTokens('.', ".,", 'x')
    for (i <- 0 until WHITESPACE.length) {
      for (j <- 0 until NON_WHITESPACE.length) {
        innerTestSplitPreserveAllTokens(WHITESPACE.charAt(i), null, NON_WHITESPACE.charAt(j))
        innerTestSplitPreserveAllTokens(WHITESPACE.charAt(i), String.valueOf(WHITESPACE.charAt(i)), NON_WHITESPACE.charAt(j))
      }
    }

    var expectedResults: Array[String] = Array("ab", "de fg")
    var results: Array[String]         = "ab de fg".ops.splitPreserveAllTokens(nullString, 2).get
    assert(expectedResults.length == results.length)
    for (i <- expectedResults.indices) {
      assert(expectedResults(i) == results(i))
    }

    expectedResults = Array("ab", "  de fg")
    results = "ab   de fg".ops.splitPreserveAllTokens(noneString, 2).get
    assert(expectedResults.length == results.length)
    for (i <- expectedResults.indices) {
      assert(expectedResults(i) == results(i))
    }

    expectedResults = Array("ab", "::de:fg")
    results = "ab:::de:fg".ops.splitPreserveAllTokens(":", 2).get
    assert(expectedResults.length == results.length)
    for (i <- expectedResults.indices) {
      assert(expectedResults(i) == results(i))
    }

    expectedResults = Array("ab", "", " de fg")
    results = Some("ab   de fg").ops.splitPreserveAllTokens(nullString, 3).get
    assert(expectedResults.length == results.length)
    for (i <- expectedResults.indices) {
      assert(expectedResults(i) == results(i))
    }

    expectedResults = Array("ab", "", "", "de fg")
    results = "ab   de fg".ops.splitPreserveAllTokens(noneString, 4).get
    assert(expectedResults.length == results.length)
    for (i <- expectedResults.indices) {
      assert(expectedResults(i) == results(i))
    }

    expectedResults = Array("ab", "cd:ef")
    results = "ab:cd:ef".ops.splitPreserveAllTokens(Some(":"), 2).get
    assert(expectedResults.length == results.length)
    for (i <- expectedResults.indices) {
      assert(expectedResults(i) == results(i))
    }

    expectedResults = Array("ab", ":cd:ef")
    results = "ab::cd:ef".ops.splitPreserveAllTokens(":", 2).get
    assert(expectedResults.length == results.length)
    for (i <- expectedResults.indices) {
      assert(expectedResults(i) == results(i))
    }

    expectedResults = Array("ab", "", ":cd:ef")
    results = "ab:::cd:ef".ops.splitPreserveAllTokens(":", 3).get
    assert(expectedResults.length == results.length)
    for (i <- expectedResults.indices) {
      assert(expectedResults(i) == results(i))
    }

    expectedResults = Array("ab", "", "", "cd:ef")
    results = Some("ab:::cd:ef").ops.splitPreserveAllTokens(Some(":"), 4).get
    assert(expectedResults.length == results.length)
    for (i <- expectedResults.indices) {
      assert(expectedResults(i) == results(i))
    }

    expectedResults = Array("", "ab", "", "", "cd:ef")
    results = ":ab:::cd:ef".ops.splitPreserveAllTokens(":", 5).get
    assert(expectedResults.length == results.length)
    for (i <- expectedResults.indices) {
      assert(expectedResults(i) == results(i))
    }

    expectedResults = Array("", "", "ab", "", "", "cd:ef")
    results = "::ab:::cd:ef".ops.splitPreserveAllTokens(":", 6).get
    assert(expectedResults.length == results.length)
    for (i <- expectedResults.indices) {
      assert(expectedResults(i) == results(i))
    }

  }

  test("test string swap case") {
    assert(nullString.ops.swapCase.isEmpty)
    assert(Some("").ops.swapCase.contains(""))
    assert("  ".ops.swapCase.contains("  "))

    assert(Some("I").ops.swapCase.contains("i"))
    assert("i".ops.swapCase.contains("I"))
    assert("i am here 123".ops.swapCase.contains("I AM HERE 123"))
    assert("I Am Here 123".ops.swapCase.contains("i aM hERE 123"))
    assert("i am HERE 123".ops.swapCase.contains("I AM here 123"))
    assert(Some("I AM HERE 123").ops.swapCase.contains("i am here 123"))

    val test   = "This String contains a TitleCase character: \u01C8"
    val expect = "tHIS sTRING CONTAINS A tITLEcASE CHARACTER: \u01C9"
    assert(test.ops.swapCase.contains(expect))
  }

  test("test string to code points") {
    val orphanedHighSurrogate = 0xd801
    val orphanedLowSurrogate  = 0xdc00
    val supplementary         = 0x2070e

    val codePoints: Array[Int] = Array('a', orphanedHighSurrogate, 'b', 'c', supplementary, 'd', orphanedLowSurrogate, 'e')
    val s                      = new String(codePoints, 0, codePoints.length)
    assert(Some(s).ops.toCodePoints.exists(e => Objects.deepEquals(e, codePoints)))

    assert(noneString.ops.toCodePoints.isEmpty)
    assert("".ops.toCodePoints.exists(e => Objects.deepEquals(e, ArrayUtils.EMPTY_INT_ARRAY)))
  }

  test("test string truncate with max width") {
    assert(noneString.ops.truncate(12).isEmpty)
    assertThrows[IllegalArgumentException] {
      nullString.ops.truncate(-1)
    }
    assertThrows[IllegalArgumentException] {
      noneString.ops.truncate(-10)
    }
    assertThrows[IllegalArgumentException] {
      noneString.ops.truncate(Integer.MIN_VALUE)
    }

    assert("".ops.truncate(10).contains(""))
    assert("".ops.truncate(10).contains(""))
    assert(Some("abcdefghij").ops.truncate(3).contains("abc"))
    assert("abcdefghij".ops.truncate(6).contains("abcdef"))
    assert("abcdefghij".ops.truncate(0).contains(""))
    assertThrows[IllegalArgumentException] {
      "abcdefghij".ops.truncate(-1)
    }
    assertThrows[IllegalArgumentException] {
      "abcdefghij".ops.truncate(-100)
    }
    assertThrows[IllegalArgumentException] {
      Some("abcdefghij").ops.truncate(Integer.MIN_VALUE)
    }
    assert("abcdefghijklmno".ops.truncate(10).contains("abcdefghij"))
    assert("abcdefghijklmno".ops.truncate(Integer.MAX_VALUE).contains("abcdefghijklmno"))
    assert("abcdefghijklmno".ops.truncate(5).contains("abcde"))
    assert("abcdefghijklmno".ops.truncate(3).contains("abc"))
  }

  test("test string truncate with offset and max width") {
    assert(nullString.ops.truncate(0, 12).isEmpty)
    assertThrows[IllegalArgumentException] {
      nullString.ops.truncate(-1, 0)
    }
    assertThrows[IllegalArgumentException] {
      noneString.ops.truncate(-10, -4)
    }
    assertThrows[IllegalArgumentException] {
      noneString.ops.truncate(Integer.MIN_VALUE, Integer.MIN_VALUE)
    }
    assert(nullString.ops.truncate(10, 12).isEmpty)
    assert("".ops.truncate(0, 10).contains(""))
    assert("".ops.truncate(2, 10).contains(""))
    assert(Some("abcdefghij").ops.truncate(0, 3).contains("abc"))
    assert("abcdefghij".ops.truncate(5, 6).contains("fghij"))
    assert("abcdefghij".ops.truncate(0, 0).contains(""))
    assertThrows[IllegalArgumentException] {
      "abcdefghij".ops.truncate(0, -1)
    }
    assertThrows[IllegalArgumentException] {
      "abcdefghij".ops.truncate(0, -10)
    }
    assertThrows[IllegalArgumentException] {
      "abcdefghij".ops.truncate(0, -100)
    }
    assertThrows[IllegalArgumentException] {
      Some("abcdefghij").ops.truncate(1, -100)
    }
    assertThrows[IllegalArgumentException] {
      "abcdefghij".ops.truncate(0, Integer.MIN_VALUE)
    }
    assertThrows[IllegalArgumentException] {
      "abcdefghij".ops.truncate(-1, 0)
    }
    assertThrows[IllegalArgumentException] {
      "abcdefghij".ops.truncate(-10, 0)
    }
    assertThrows[IllegalArgumentException] {
      "abcdefghij".ops.truncate(-100, 1)
    }
    assertThrows[IllegalArgumentException] {
      "abcdefghij".ops.truncate(Integer.MIN_VALUE, 0)
    }
    assertThrows[IllegalArgumentException] {
      "abcdefghij".ops.truncate(-1, -1)
    }
    assertThrows[IllegalArgumentException] {
      "abcdefghij".ops.truncate(-10, -10)
    }
    assertThrows[IllegalArgumentException] {
      "abcdefghij".ops.truncate(-100, -100)
    }
    assertThrows[IllegalArgumentException] {
      "abcdefghij".ops.truncate(Integer.MIN_VALUE, Integer.MIN_VALUE)
    }
    val raspberry = "raspberry peach"
    assert(raspberry.ops.truncate(10, 15).contains("peach"))
    assert(Some("abcdefghijklmno").ops.truncate(0, 10).contains("abcdefghij"))
    assert("abcdefghijklmno".ops.truncate(0, Integer.MAX_VALUE).contains("abcdefghijklmno"))
    assert("abcdefghijklmno".ops.truncate(1, 10).contains("bcdefghijk"))
    assert(Some("abcdefghijklmno").ops.truncate(2, 10).contains("cdefghijkl"))
    assert("abcdefghijklmno".ops.truncate(3, 10).contains("defghijklm"))
    assert("abcdefghijklmno".ops.truncate(4, 10).contains("efghijklmn"))
    assert("abcdefghijklmno".ops.truncate(5, 10).contains("fghijklmno"))
    assert("abcdefghijklmno".ops.truncate(5, 5).contains("fghij"))
    assert("abcdefghijklmno".ops.truncate(5, 3).contains("fgh"))
    assert("abcdefghijklmno".ops.truncate(10, 3).contains("klm"))
    assert("abcdefghijklmno".ops.truncate(10, Integer.MAX_VALUE).contains("klmno"))
    assert("abcdefghijklmno".ops.truncate(13, 1).contains("n"))
    assert("abcdefghijklmno".ops.truncate(13, Integer.MAX_VALUE).contains("no"))
    assert("abcdefghijklmno".ops.truncate(14, 1).contains("o"))
    assert("abcdefghijklmno".ops.truncate(14, Integer.MAX_VALUE).contains("o"))
    assert("abcdefghijklmno".ops.truncate(15, 1).contains(""))
    assert("abcdefghijklmno".ops.truncate(15, Integer.MAX_VALUE).contains(""))
    assert("abcdefghijklmno".ops.truncate(Integer.MAX_VALUE, Integer.MAX_VALUE).contains(""))
  }

  test("test string uncapitalize") {
    assert(noneString.ops.uncapitalize.isEmpty)

    assert(FOO_CAP.ops.uncapitalize.contains(FOO_UNCAP), "uncapitalize(String) failed")
    assert(FOO_UNCAP.ops.uncapitalize.contains(FOO_UNCAP), "uncapitalize(string) failed")
    assert("".ops.uncapitalize.contains(""), "uncapitalize(empty-string) failed")
    assert(Some("X").ops.uncapitalize.contains("x"), "uncapitalize(single-char-string) failed")

    // Examples from uncapitalize Javadoc
    assert("cat".ops.uncapitalize.contains("cat"))
    assert("Cat".ops.uncapitalize.contains("cat"))
    assert("CAT".ops.uncapitalize.contains("cAT"))
  }

  test("test string unwrap with wrap token") {
    assert(noneString.ops.unwrap(nullString).isEmpty)
    assert(nullString.ops.unwrap(CharUtils.NUL).isEmpty)
    assert(nullString.ops.unwrap('1').isEmpty)

    assert(Some("abc").ops.unwrap(noneString).contains("abc"))
    assert("a".ops.unwrap(Some("a")).contains("a"))
    assert("aa".ops.unwrap("a").contains(""))
    assert("\'abc\'".ops.unwrap('\'').contains("abc"))
    assert(Some("AabcA").ops.unwrap('A').contains("abc"))
    assert("AAabcAA".ops.unwrap('A').contains("AabcA"))
    assert("abc".ops.unwrap('b').contains("abc"))
    assert("#A".ops.unwrap('#').contains("#A"))
    assert("A#".ops.unwrap('#').contains("A#"))
    assert("AABAA".ops.unwrap('A').contains("ABA"))
  }

  test("test string unwrap with wrap token string") {
    assert(nullString.ops.unwrap(noneString).isEmpty)
    assert(noneString.ops.unwrap("").isEmpty)
    assert(nullString.ops.unwrap(Some("1")).isEmpty)

    assert("abc".ops.unwrap(nullString).contains("abc"))
    assert("abc".ops.unwrap("").contains("abc"))
    assert(Some("a").ops.unwrap("a").contains("a"))
    assert("ababa".ops.unwrap(Some("aba")).contains("ababa"))
    assert("aa".ops.unwrap("a").contains(""))
    assert("\'abc\'".ops.unwrap("\'").contains("abc"))
    assert("\"abc\"".ops.unwrap("\"").contains("abc"))
    assert(Some("\"abc\"xyz\"").ops.unwrap("\"").contains("abc\"xyz"))
    assert("\"abc\"xyz\"\"".ops.unwrap("\"").contains("abc\"xyz\""))
    assert("\"abc\'xyz\'\"".ops.unwrap("\"").contains("abc\'xyz\'"))
    assert("AA\"abc\'xyz\'\"AA".ops.unwrap("AA").contains("\"abc\'xyz\'\""))
    assert("123\"abc\'xyz\'\"123".ops.unwrap("123").contains("\"abc\'xyz\'\""))
    assert("AA\"abc\'xyz\'\"".ops.unwrap("AA").contains("AA\"abc\'xyz\'\""))
    assert("AAA\"abc\'xyz\'\"AAA".ops.unwrap("A").contains("AA\"abc\'xyz\'\"AA"))
    assert("\"abc\'xyz\'\"AA".ops.unwrap("AA").contains("\"abc\'xyz\'\"AA"))
  }

  test("test string upper case") {
    assert(nullString.ops.upperCase.isEmpty)
    assert(noneString.ops.upperCase(Locale.ENGLISH).isEmpty)
    assert(Some("fOo test THING").ops.upperCase.contains("FOO TEST THING"), "upperCase(String) failed")
    assert("".ops.upperCase.contains(""), "upperCase(empty-string) failed")
    assert("fOo test THING".ops.upperCase(Locale.ENGLISH).contains("FOO TEST THING"), "upperCase(String, Locale) failed")
    assert("".ops.upperCase(Locale.ENGLISH).contains(""), "upperCase(empty-string, Locale) failed")
  }

  test("test string wrap with char") {
    assert(nullString.ops.wrap(CharUtils.NUL).isEmpty)
    assert(noneString.ops.wrap('1').isEmpty)

    assert("".ops.wrap(CharUtils.NUL).contains(""))
    assert("ab".ops.wrap('x').contains("xabx"))
    assert(Some("ab").ops.wrap('\"').contains("\"ab\""))
    assert("\"ab\"".ops.wrap('\"').contains("\"\"ab\"\""))
    assert("ab".ops.wrap('\'').contains("'ab'"))
    assert("'abcd'".ops.wrap('\'').contains("''abcd''"))
    assert("\"abcd\"".ops.wrap('\'').contains("'\"abcd\"'"))
    assert("'abcd'".ops.wrap('\"').contains("\"'abcd'\""))
  }

  test("test string wrap with string") {
    assert(noneString.ops.wrap(nullString).isEmpty)
    assert(nullString.ops.wrap("").isEmpty)
    assert(noneString.ops.wrap("1").isEmpty)

    assert(nullString.ops.wrap(noneString).isEmpty)
    assert(Some("").ops.wrap("").contains(""))
    assert("ab".ops.wrap(nullString).contains("ab"))
    assert(Some("ab").ops.wrap(Some("x")).contains("xabx"))
    assert("ab".ops.wrap("\"").contains("\"ab\""))
    assert("\"ab\"".ops.wrap("\"").contains("\"\"ab\"\""))
    assert("ab".ops.wrap(Some("'")).contains("'ab'"))
    assert("'abcd'".ops.wrap("'").contains("''abcd''"))
    assert("\"abcd\"".ops.wrap("'").contains("'\"abcd\"'"))
    assert("'abcd'".ops.wrap("\"").contains("\"'abcd'\""))
  }

  test("test string wrap if missing char") {
    assert(noneString.ops.wrapIfMissing(CharUtils.NUL).isEmpty)
    assert(nullString.ops.wrapIfMissing('1').isEmpty)

    assert(Some("").ops.wrapIfMissing(CharUtils.NUL).contains(""))
    assert("ab".ops.wrapIfMissing('x').contains("xabx"))
    assert("ab".ops.wrapIfMissing('\"').contains("\"ab\""))
    assert("\"ab\"".ops.wrapIfMissing('\"').contains("\"ab\""))
    assert("ab".ops.wrapIfMissing('\'').contains("'ab'"))
    assert("'abcd'".ops.wrapIfMissing('\'').contains("'abcd'"))
    assert("\"abcd\"".ops.wrapIfMissing('\'').contains("'\"abcd\"'"))
    assert(Some("'abcd'").ops.wrapIfMissing('\"').contains("\"'abcd'\""))
    assert("x".ops.wrapIfMissing('/').contains("/x/"))
    assert("x/y/z".ops.wrapIfMissing('/').contains("/x/y/z/"))
    assert("/x/y/z".ops.wrapIfMissing('/').contains("/x/y/z/"))
    assert("x/y/z/".ops.wrapIfMissing('/').contains("/x/y/z/"))

    assert("/".ops.wrapIfMissing('/').contains("/"))
    assert("/x/".ops.wrapIfMissing('/').contains("/x/"))
  }

  test("test string wrap if missing string") {
    assert(nullString.ops.wrapIfMissing(Some("\u0000")).isEmpty)
    assert(noneString.ops.wrapIfMissing("1").isEmpty)

    assert(Some("").ops.wrapIfMissing("\u0000").contains(""))
    assert(Some("ab").ops.wrapIfMissing(Some("x")).contains("xabx"))
    assert("ab".ops.wrapIfMissing("\"").contains("\"ab\""))
    assert("\"ab\"".ops.wrapIfMissing("\"").contains("\"ab\""))
    assert("ab".ops.wrapIfMissing("\'").contains("'ab'"))
    assert("'abcd'".ops.wrapIfMissing("\'").contains("'abcd'"))
    assert("\"abcd\"".ops.wrapIfMissing("\'").contains("'\"abcd\"'"))
    assert(Some("'abcd'").ops.wrapIfMissing("\"").contains("\"'abcd'\""))
    assert("x".ops.wrapIfMissing("/").contains("/x/"))
    assert("x/y/z".ops.wrapIfMissing("/").contains("/x/y/z/"))
    assert("/x/y/z".ops.wrapIfMissing("/").contains("/x/y/z/"))
    assert("x/y/z/".ops.wrapIfMissing(Some("/")).contains("/x/y/z/"))
    assert("/".ops.wrapIfMissing("/").contains("/"))
    assert("/".ops.wrapIfMissing("ab").contains("ab/ab"))

    assert("ab/ab".ops.wrapIfMissing("ab").contains("ab/ab"))
    assert("//x//".ops.wrapIfMissing("//").contains("//x//"))
  }

  test("test string to root lower case") {
    assert(noneString.ops.toRootLowerCase.isEmpty)
    assert(Some("A").ops.toRootLowerCase.contains("a"))
    assert(Some("a").ops.toRootLowerCase.contains("a"))
    val TURKISH = Locale.forLanguageTag("tr")
    // Sanity checks:
    assert("TITLE".toLowerCase(TURKISH) != "title")
    assert("TITLE".toLowerCase(Locale.ROOT) == "title")
    assert("TITLE".ops.toRootLowerCase.contains("title"))
    // Make sure we are not using the default Locale:
    val defaultLocale = Locale.getDefault
    try {
      Locale.setDefault(TURKISH)
      assert("TITLE".ops.toRootLowerCase.contains("title"))
    } finally Locale.setDefault(defaultLocale)
  }

  test("test string to root upper case") {
    assert(noneString.ops.toRootUpperCase.isEmpty)
    assert(Some("a").ops.toRootUpperCase.contains("A"))
    assert("A".ops.toRootUpperCase.contains("A"))
    val TURKISH = Locale.forLanguageTag("tr")
    // Sanity checks:
    assert("TITLE" != "title".toUpperCase(TURKISH))
    assert("TITLE" == "title".toUpperCase(Locale.ROOT))
    assert(Some("title").ops.toRootUpperCase.contains("TITLE"))
    // Make sure we are not using the default Locale:
    val defaultLocale = Locale.getDefault();
    try {
      Locale.setDefault(TURKISH)
      assert("title".ops.toRootUpperCase.contains("TITLE"));
    } finally {
      Locale.setDefault(defaultLocale);
    }
  }

  test("test string georgian") {
    val arrayI = Array[Char]( // Latin Small Letter dotless I
      0x0131.toChar, // Greek Capital Letter Theta
      0x03f4.toChar
    )
    val arrayJ = Array[Char]( // Latin Capital Letter I with dot above
      0x0130.toChar, // Greek Theta Symbol
      0x03d1.toChar
    )
    for (i <- arrayI) {
      for (j <- arrayJ) {
        val si   = String.valueOf(i)
        val sj   = String.valueOf(j)
        val res1 = si.equalsIgnoreCase(sj)
        val ci   = new mutable.StringBuilder(si)
        val cj   = new mutable.StringBuilder(sj)
        var res2 = ci.toString().ops.startsWithIgnoreCase(cj.toString())
        assert(res1 == res2, "si : " + si + " sj : " + sj)
        res2 = ci.toString().ops.endsWithIgnoreCase(cj.toString())
        assert(res1 == res2, "si : " + si + " sj : " + sj)
        res2 = ci.toString.ops.compareIgnoreCase(cj.toString) == 0
        assert(res1 == res2, "si : " + si + " sj : " + sj)
        res2 = ci.toString.ops.indexOfIgnoreCase(cj.toString) == 0
        assert(res1 == res2, "si : " + si + " sj : " + sj)
        res2 = ci.toString.ops.indexOfIgnoreCase(cj.toString) == 0
        assert(res1 == res2, "si : " + si + " sj : " + sj)
      }
    }
  }
}

object StringUtilsSpec {
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
  val WHITESPACE: String     = whitespace
  val NON_WHITESPACE: String = non_whitespace
  val HARD_SPACE: String     = hard_space
  val TRIMMABLE: String      = trimmable
  val NON_TRIMMABLE: String  = non_trimmable

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
}
