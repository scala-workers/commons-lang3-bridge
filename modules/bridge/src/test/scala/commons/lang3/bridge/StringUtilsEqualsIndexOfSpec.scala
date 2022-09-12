package commons.lang3.bridge

import commons.lang3.bridge.StringUtils.ops._
import org.scalatest.funsuite.AnyFunSuite

import java.lang
import java.nio.CharBuffer

/** TODO
  *
  * @author
  *   mars
  * @version 1.0.0
  * @since 2022/09/10
  *   21:25
  */
class StringUtilsEqualsIndexOfSpec extends AnyFunSuite {
  import StringUtilsSpec.{noneString, nullString}

  private val BAR = "bar"

  /** Supplementary character U+20000 See http://www.oracle.com/technetwork/articles/javase/supplementary-142654.html
    */
  private val CharU20000 = "\uD840\uDC00"

  /** Supplementary character U+20001 See http://www.oracle.com/technetwork/articles/javase/supplementary-142654.html
    */
  private val CharU20001 = "\uD840\uDC01"

  private val FOO = "foo"

  private val FOOBAR = "foobar"

  private val FOOBAR_SUB_ARRAY = Array[String]("ob", "ba")

  test("string index of") {
    assert(noneString.ops.indexOf(nullString) == -1)
    assert("".ops.indexOf(' ') == -1)
    assert("aabaabaa".ops.indexOf('a') == 0)
    assert("aabaabaa".ops.indexOf('b') == 2)
  }

  import org.apache.commons.lang3.{StringUtils => Strings}
  test("test string index of char start index") {
    assert(-1 == noneString.ops.indexOf(' ', 0))
    assert(-1 == nullString.ops.indexOf(' ', -1))
    assert(-1 == "".ops.indexOf(' ', 0))
    assert(-1 == Some("").ops.indexOf(' ', -1))
    assert(0 == "aabaabaa".ops.indexOf('a', 0))
    assert(2 == "aabaabaa".ops.indexOf('b', 0))
    assert(5 == "aabaabaa".ops.indexOf('b', 3))
    assert(-1 == "aabaabaa".ops.indexOf('b', 9))
    assert(2 == "aabaabaa".ops.indexOf('b', -1))
  }

  test("test string index of string") {
    assert(-1 == noneString.ops.indexOf(nullString))
    assert(-1 == "".ops.indexOf(noneString))
    assert(0 == Some("").ops.indexOf(""))
    assert(0 == Some("aabaabaa").ops.indexOf("a"))
    assert(2 == "aabaabaa".ops.indexOf(Some("b")))
    assert(1 == "aabaabaa".ops.indexOf("ab"))
    assert(0 == "aabaabaa".ops.indexOf(""))
  }

  test("test string index of string and start position") {
    assert(-1 == nullString.ops.indexOf(noneString, 0))
    assert(-1 == noneString.ops.indexOf(nullString, -1))
    assert(-1 == nullString.ops.indexOf("", 0))
    assert(-1 == nullString.ops.indexOf("", -1))
    assert(-1 == Some("").ops.indexOf(nullString, 0))
    assert(-1 == "".ops.indexOf(nullString, -1))
    assert(0 == "".ops.indexOf("", 0))
    assert(0 == "".ops.indexOf("", -1))
    assert(0 == Some("").ops.indexOf("", 9))
    assert(0 == "abc".ops.indexOf(Some(""), 0))
    assert(0 == "abc".ops.indexOf("", -1))
    assert(3 == "abc".ops.indexOf("", 9))
    assert(3 == "abc".ops.indexOf("", 3))
    assert(0 == "aabaabaa".ops.indexOf("a", 0))
    assert(2 == "aabaabaa".ops.indexOf("b", 0))
    assert(1 == "aabaabaa".ops.indexOf("ab", 0))
    assert(5 == "aabaabaa".ops.indexOf("b", 3))
    assert(-1 == "aabaabaa".ops.indexOf("b", 9))
    assert(2 == "aabaabaa".ops.indexOf("b", -1))
    assert(2 == "aabaabaa".ops.indexOf(Some(""), 2))

    // Test that startIndex works correctly, i.e. cannot match before startIndex
    assert(7 == "12345678".ops.indexOf("8", 5))
    assert(7 == "12345678".ops.indexOf(Some("8"), 6))
    assert(7 == "12345678".ops.indexOf("8", 7)) // 7 is last index

    assert(-1 == Some("12345678").ops.indexOf("8", 8))

  }

  test("test string index of any char") {
    assert(-1 == noneString.ops.indexOfAny(null.asInstanceOf[Array[Char]]: _*))
    assert(-1 == noneString.ops.indexOfAny(new Array[Char](0): _*))
    assert(-1 == nullString.ops.indexOfAny('a', 'b'))

    assert(-1 == "".ops.indexOfAny(null.asInstanceOf[Array[Char]]: _*))
    assert(-1 == "".ops.indexOfAny(new Array[Char](0): _*))
    assert(-1 == Some("").ops.indexOfAny('a', 'b'))

    assert(-1 == "zzabyycdxx".ops.indexOfAny(null.asInstanceOf[Array[Char]]: _*))
    assert(-1 == Some("zzabyycdxx").ops.indexOfAny(new Array[Char](0): _*))
    assert(0 == "zzabyycdxx".ops.indexOfAny('z', 'a'))
    assert(3 == "zzabyycdxx".ops.indexOfAny('b', 'y'))
    assert(-1 == "ab".ops.indexOfAny('z'))
  }

  test("test string index of any chars with supplementary chars") {
    assert(0 == (CharU20000 + CharU20001).ops.indexOfAny(CharU20000.toCharArray: _*))
    assert(2 == Some(CharU20000 + CharU20001).ops.indexOfAny(CharU20001.toCharArray: _*))
    assert(0 == CharU20000.ops.indexOfAny(CharU20000.toCharArray: _*))
    assert(-1 == CharU20000.ops.indexOfAny(CharU20001.toCharArray: _*))
  }

  test("test string index of any bug string") {
    assert(-1 == nullString.ops.indexOfAnyBut(nullString))
    assert(-1 == noneString.ops.indexOfAnyBut(""))
    assert(-1 == nullString.ops.indexOfAnyBut(Some("ab")))

    assert(-1 == Some("").ops.indexOfAnyBut(noneString))
    assert(-1 == "".ops.indexOfAnyBut(""))
    assert(-1 == "".ops.indexOfAnyBut(Some("ab")))

    assert(-1 == "zzabyycdxx".ops.indexOfAnyBut(noneString))
    assert(-1 == Some("zzabyycdxx").ops.indexOfAnyBut(""))
    assert(3 == "zzabyycdxx".ops.indexOfAnyBut("za"))
    assert(0 == "zzabyycdxx".ops.indexOfAnyBut(Some("by")))
    assert(0 == "ab".ops.indexOfAnyBut("z"))
  }

  test("test string index of any but string with supplement chars") {
    assert(2 == Some(CharU20000 + CharU20001).ops.indexOfAnyBut(CharU20000))
    assert(0 == (CharU20000 + CharU20001).ops.indexOfAnyBut(Some(CharU20001)))
    assert(-1 == CharU20000.ops.indexOfAnyBut(CharU20000))
    assert(0 == CharU20000.ops.indexOfAnyBut(CharU20001))
  }

  test("test string index of ignore case") {
    assert(-1 == noneString.ops.indexOfIgnoreCase(nullString))
    assert(-1 == nullString.ops.indexOfIgnoreCase(Some("")))
    assert(-1 == "".ops.indexOfIgnoreCase(noneString))
    assert(0 == Some("").ops.indexOfIgnoreCase(""))
    assert(0 == "aabaabaa".ops.indexOfIgnoreCase("a"))
    assert(0 == "aabaabaa".ops.indexOfIgnoreCase("A"))
    assert(2 == Some("aabaabaa").ops.indexOfIgnoreCase("b"))
    assert(2 == "aabaabaa".ops.indexOfIgnoreCase("B"))
    assert(1 == "aabaabaa".ops.indexOfIgnoreCase(Some("ab")))
    assert(1 == "aabaabaa".ops.indexOfIgnoreCase("AB"))
    assert(0 == "aabaabaa".ops.indexOfIgnoreCase(""))
  }

  test("test string index of ignore case with start position") {
    assert(1 == "aabaabaa".ops.indexOfIgnoreCase("AB", -1))
    assert(1 == Some("aabaabaa").ops.indexOfIgnoreCase("AB", 0))
    assert(1 == "aabaabaa".ops.indexOfIgnoreCase("AB", 1))
    assert(4 == "aabaabaa".ops.indexOfIgnoreCase("AB", 2))
    assert(4 == "aabaabaa".ops.indexOfIgnoreCase(Some("AB"), 3))
    assert(4 == "aabaabaa".ops.indexOfIgnoreCase("AB", 4))
    assert(-1 == "aabaabaa".ops.indexOfIgnoreCase("AB", 5))
    assert(-1 == "aabaabaa".ops.indexOfIgnoreCase("AB", 6))
    assert(-1 == ("aabaabaa").ops.indexOfIgnoreCase("AB", 7))
    assert(-1 == "aabaabaa".ops.indexOfIgnoreCase("AB", 8))
    assert(1 == "aab".ops.indexOfIgnoreCase("AB", 1))
    assert(5 == "aabaabaa".ops.indexOfIgnoreCase("", 5))
    assert(-1 == "ab".ops.indexOfIgnoreCase("AAB", 0))
    assert(-1 == "aab".ops.indexOfIgnoreCase("AAB", 1))
    assert(-1 == "abc".ops.indexOfIgnoreCase("", 9))
  }

  test("test string last index of char") {
    assert(-1 == noneString.ops.lastIndexOf(' '))
    assert(-1 == Some("").ops.lastIndexOf(' '))
    assert(7 == "aabaabaa".ops.lastIndexOf('a'))
    assert(5 == Some("aabaabaa").ops.lastIndexOf('b'))
  }

  test("test string last index of char from start position") {
    assert(-1 == noneString.ops.lastIndexOf(' ', 0))
    assert(-1 == nullString.ops.lastIndexOf(' ', -1))
    assert(-1 == "".ops.lastIndexOf(' ', 0))
    assert(-1 == Some("").ops.lastIndexOf(' ', -1))
    assert(7 == "aabaabaa".ops.lastIndexOf('a', 8))
    assert(5 == "aabaabaa".ops.lastIndexOf('b', 8))
    assert(2 == "aabaabaa".ops.lastIndexOf('b', 3))
    assert(5 == Some("aabaabaa").ops.lastIndexOf('b', 9))
    assert(-1 == "aabaabaa".ops.lastIndexOf('b', -1))
    assert(0 == "aabaabaa".ops.lastIndexOf('a', 0))

    // LANG-1300 addition test
    val CODE_POINT                       = 0x2070e
    var builder: java.lang.StringBuilder = new lang.StringBuilder
    builder.appendCodePoint(CODE_POINT)
    assert(0 == builder.toString.ops.lastIndexOf(CODE_POINT, 0))
    builder.appendCodePoint(CODE_POINT)
    assert(0 == builder.toString.ops.lastIndexOf(CODE_POINT, 0))
    assert(0 == builder.toString.ops.lastIndexOf(CODE_POINT, 1))
    assert(2 == builder.toString.ops.lastIndexOf(CODE_POINT, 2))

    builder.append("aaaaa")
    assert(2 == builder.toString.ops.lastIndexOf(CODE_POINT, 4))
    // inner branch on the supplementary character block
    val tmp: Array[Char] = Array(55361.toChar)
    builder = new lang.StringBuilder()
    builder.append(tmp)
    assert(-1 == builder.toString.ops.lastIndexOf(CODE_POINT, 0))
    builder.appendCodePoint(CODE_POINT)
    assert(-1 == builder.toString.ops.lastIndexOf(CODE_POINT, 0))
    assert(1 == builder.toString.ops.lastIndexOf(CODE_POINT, 1))
    assert(-1 == builder.toString.ops.lastIndexOf(CODE_POINT, 0))
    assert(1 == builder.toString.ops.lastIndexOf(CODE_POINT, 1))
    assert(-1 == CharBuffer.wrap("[%{.c.0rro").toString.ops.lastIndexOf(-1738.toChar, 982))

  }

  test("test string last index of string") {
    assert(-1 == noneString.ops.lastIndexOf(nullString))
    assert(-1 == "".ops.lastIndexOf(noneString))
    assert(-1 == "".ops.lastIndexOf("a"))
    assert(0 == Some("").ops.lastIndexOf(""))
    assert(8 == "aabaabaa".ops.lastIndexOf(""))
    assert(7 == "aabaabaa".ops.lastIndexOf(Some("a")))
    assert(5 == "aabaabaa".ops.lastIndexOf("b"))
    assert(4 == "aabaabaa".ops.lastIndexOf("ab"))

  }

  test("test last index of string with start position") {
    assert(-1 == noneString.ops.lastIndexOf(nullString, 0))
    assert(-1 == nullString.ops.lastIndexOf(noneString, -1))
    assert(-1 == noneString.ops.lastIndexOf("", 0))
    assert(-1 == nullString.ops.lastIndexOf(Some(""), -1))
    assert(-1 == "".ops.lastIndexOf(nullString, 0))
    assert(-1 == "".ops.lastIndexOf(noneString, -1))
    assert(0 == Some("").ops.lastIndexOf(Some(""), 0))
    assert(-1 == "".ops.lastIndexOf("", -1))
    assert(0 == "".ops.lastIndexOf("", 9))
    assert(0 == "abc".ops.lastIndexOf("", 0))
    assert(-1 == "abc".ops.lastIndexOf("", -1))
    assert(3 == "abc".ops.lastIndexOf("", 9))
    assert(7 == "aabaabaa".ops.lastIndexOf("a", 8))
    assert(5 == "aabaabaa".ops.lastIndexOf("b", 8))
    assert(4 == "aabaabaa".ops.lastIndexOf("ab", 8))
    assert(2 == "aabaabaa".ops.lastIndexOf("b", 3))
    assert(5 == "aabaabaa".ops.lastIndexOf("b", 9))
    assert(-1 == "aabaabaa".ops.lastIndexOf("b", -1))
    assert(-1 == "aabaabaa".ops.lastIndexOf("b", 0))
    assert(0 == "aabaabaa".ops.lastIndexOf("a", 0))
    assert(-1 == "aabaabaa".ops.lastIndexOf("a", -1))

    // Test that fromIndex works correctly, i.e. cannot match after fromIndex
    assert(7 == Some("12345678").ops.lastIndexOf("8", 9))
    assert(7 == "12345678".ops.lastIndexOf("8", 8))
    assert(7 == "12345678".ops.lastIndexOf("8", 7)) // 7 is last index

    assert(-1 == "12345678".ops.lastIndexOf("8", 6))

    assert(-1 == "aabaabaa".ops.lastIndexOf("b", 1))
    assert(2 == "aabaabaa".ops.lastIndexOf("b", 2))
    assert(2 == "aabaabaa".ops.lastIndexOf(Some("ba"), 2))
    assert(2 == "aabaabaa".ops.lastIndexOf("ba", 3))

  }

  test("test string last index of any strings") {
    assert(-1 == noneString.ops.lastIndexOfAny(null.asInstanceOf[CharSequence])) // test both types of ...

    assert(-1 == noneString.ops.lastIndexOfAny(null.asInstanceOf[Array[CharSequence]]: _*)) // ... varargs invocation

    assert(-1 == noneString.ops.lastIndexOfAny(FOOBAR_SUB_ARRAY: _*))
    assert(-1 == FOOBAR.ops.lastIndexOfAny(null.asInstanceOf[CharSequence]))
    assert(-1 == Some(FOOBAR).ops.lastIndexOfAny(null.asInstanceOf[Array[CharSequence]]: _*))

    assert(3 == FOOBAR.ops.lastIndexOfAny(FOOBAR_SUB_ARRAY: _*))
    assert(-1 == FOOBAR.ops.lastIndexOfAny(new Array[String](0): _*))
    assert(-1 == noneString.ops.lastIndexOfAny(new Array[String](0): _*))
    assert(-1 == Some("").ops.lastIndexOfAny(new Array[Option[String]](0): _*))
    assert(-1 == FOOBAR.ops.lastIndexOfAny(Array[String]("llll"): _*))
    assert(6 == FOOBAR.ops.lastIndexOfAny(Array[String](""): _*))
    assert(0 == "".ops.lastIndexOfAny(Array[String](""): _*))
    assert(-1 == "".ops.lastIndexOfAny(Array[String]("a"): _*))
    assert(-1 == "".ops.lastIndexOfAny(Array[String](null): _*))
    assert(-1 == FOOBAR.ops.lastIndexOfAny(Array[String](null): _*))
    assert(-1 == noneString.ops.lastIndexOfAny(Array[String](null): _*))
  }

  test("test string last index of ignore case") {
    assert(-1 == noneString.ops.lastIndexOfIgnoreCase(nullString))
    assert(-1 == Some("").ops.lastIndexOfIgnoreCase(noneString))
    assert(-1 == nullString.ops.lastIndexOfIgnoreCase(""))
    assert(-1 == "".ops.lastIndexOfIgnoreCase(Some("a")))
    assert(0 == "".ops.lastIndexOfIgnoreCase(""))
    assert(8 == "aabaabaa".ops.lastIndexOfIgnoreCase(""))
    assert(7 == "aabaabaa".ops.lastIndexOfIgnoreCase("a"))
    assert(7 == "aabaabaa".ops.lastIndexOfIgnoreCase("A"))
    assert(5 == "aabaabaa".ops.lastIndexOfIgnoreCase("b"))
    assert(5 == "aabaabaa".ops.lastIndexOfIgnoreCase("B"))
    assert(4 == "aabaabaa".ops.lastIndexOfIgnoreCase("ab"))
    assert(4 == "aabaabaa".ops.lastIndexOfIgnoreCase("AB"))
    assert(-1 == "ab".ops.lastIndexOfIgnoreCase("AAB"))
    assert(0 == "aab".ops.lastIndexOfIgnoreCase("AAB"))
  }

  test("test string last index of ignore case with start position") {
    assert(-1 == nullString.ops.lastIndexOfIgnoreCase(nullString, 0))
    assert(-1 == noneString.ops.lastIndexOfIgnoreCase(noneString, -1))
    assert(-1 == nullString.ops.lastIndexOfIgnoreCase("", 0))
    assert(-1 == nullString.ops.lastIndexOfIgnoreCase(Some(""), -1))
    assert(-1 == "".ops.lastIndexOfIgnoreCase(noneString, 0))
    assert(-1 == "".ops.lastIndexOfIgnoreCase(nullString, -1))
    assert(0 == Some("").ops.lastIndexOfIgnoreCase("", 0))
    assert(-1 == Some("").ops.lastIndexOfIgnoreCase("", -1))
    assert(0 == "".ops.lastIndexOfIgnoreCase(Some(""), 9))
    assert(0 == "abc".ops.lastIndexOfIgnoreCase("", 0))
    assert(-1 == "abc".ops.lastIndexOfIgnoreCase("", -1))
    assert(3 == "abc".ops.lastIndexOfIgnoreCase("", 9))
    assert(7 == "aabaabaa".ops.lastIndexOfIgnoreCase("A", 8))
    assert(5 == "aabaabaa".ops.lastIndexOfIgnoreCase("B", 8))
    assert(4 == "aabaabaa".ops.lastIndexOfIgnoreCase("AB", 8))
    assert(2 == "aabaabaa".ops.lastIndexOfIgnoreCase("B", 3))
    assert(5 == "aabaabaa".ops.lastIndexOfIgnoreCase("B", 9))
    assert(-1 == "aabaabaa".ops.lastIndexOfIgnoreCase("B", -1))
    assert(-1 == "aabaabaa".ops.lastIndexOfIgnoreCase("B", 0))
    assert(0 == "aabaabaa".ops.lastIndexOfIgnoreCase("A", 0))
    assert(1 == "aab".ops.lastIndexOfIgnoreCase("AB", 1))
  }

  test("test last ordinal index of") {
    assert(-1 == noneString.ops.lastOrdinalIndexOf("*", 42))
    assert(-1 == Some("*").ops.lastOrdinalIndexOf(nullString, 42))
    assert(0 == "".ops.lastOrdinalIndexOf("", 42))
    assert(7 == "aabaabaa".ops.lastOrdinalIndexOf("a", 1))
    assert(6 == "aabaabaa".ops.lastOrdinalIndexOf("a", 2))
    assert(5 == Some("aabaabaa").ops.lastOrdinalIndexOf("b", 1))
    assert(2 == "aabaabaa".ops.lastOrdinalIndexOf("b", 2))
    assert(4 == "aabaabaa".ops.lastOrdinalIndexOf(Some("ab"), 1))
    assert(1 == "aabaabaa".ops.lastOrdinalIndexOf("ab", 2))
    assert(8 == "aabaabaa".ops.lastOrdinalIndexOf("", 1))
    assert(8 == "aabaabaa".ops.lastOrdinalIndexOf("", 2))
  }

  test("test ordinal index of") {
    assert(-1 == nullString.ops.ordinalIndexOf(noneString, Integer.MIN_VALUE))
    assert(-1 == "".ops.ordinalIndexOf(nullString, Integer.MIN_VALUE))
    assert(-1 == "".ops.ordinalIndexOf("", Integer.MIN_VALUE))
    assert(-1 == "aabaabaa".ops.ordinalIndexOf("a", Integer.MIN_VALUE))
    assert(-1 == "aabaabaa".ops.ordinalIndexOf("b", Integer.MIN_VALUE))
    assert(-1 == Some("aabaabaa").ops.ordinalIndexOf("ab", Integer.MIN_VALUE))
    assert(-1 == "aabaabaa".ops.ordinalIndexOf("", Integer.MIN_VALUE))

    assert(-1 == noneString.ops.ordinalIndexOf(nullString, -1))
    assert(-1 == "".ops.ordinalIndexOf(noneString, -1))
    assert(-1 == "".ops.ordinalIndexOf("", -1))
    assert(-1 == "aabaabaa".ops.ordinalIndexOf("a", -1))
    assert(-1 == "aabaabaa".ops.ordinalIndexOf("b", -1))
    assert(-1 == "aabaabaa".ops.ordinalIndexOf("ab", -1))
    assert(-1 == "aabaabaa".ops.ordinalIndexOf("", -1))

    assert(-1 == nullString.ops.ordinalIndexOf(nullString, 0))
    assert(-1 == "".ops.ordinalIndexOf(noneString, 0))
    assert(-1 == "".ops.ordinalIndexOf("", 0))
    assert(-1 == "aabaabaa".ops.ordinalIndexOf("a", 0))
    assert(-1 == "aabaabaa".ops.ordinalIndexOf("b", 0))
    assert(-1 == "aabaabaa".ops.ordinalIndexOf("ab", 0))
    assert(-1 == "aabaabaa".ops.ordinalIndexOf("", 0))

    assert(-1 == nullString.ops.ordinalIndexOf(nullString, 1))
    assert(-1 == "".ops.ordinalIndexOf(noneString, 1))
    assert(0 == "".ops.ordinalIndexOf("", 1))
    assert(0 == "aabaabaa".ops.ordinalIndexOf("a", 1))
    assert(2 == "aabaabaa".ops.ordinalIndexOf("b", 1))
    assert(1 == "aabaabaa".ops.ordinalIndexOf("ab", 1))
    assert(0 == "aabaabaa".ops.ordinalIndexOf("", 1))

    assert(-1 == nullString.ops.ordinalIndexOf(noneString, 2))
    assert(-1 == "".ops.ordinalIndexOf(nullString, 2))
    assert(0 == "".ops.ordinalIndexOf(Some(""), 2))
    assert(1 == "aabaabaa".ops.ordinalIndexOf("a", 2))
    assert(5 == "aabaabaa".ops.ordinalIndexOf("b", 2))
    assert(4 == "aabaabaa".ops.ordinalIndexOf("ab", 2))
    assert(0 == "aabaabaa".ops.ordinalIndexOf("", 2))

    assert(-1 == noneString.ops.ordinalIndexOf(noneString, Integer.MAX_VALUE))
    assert(-1 == "".ops.ordinalIndexOf(nullString, Integer.MAX_VALUE))
    assert(0 == "".ops.ordinalIndexOf("", Integer.MAX_VALUE))
    assert(-1 == "aabaabaa".ops.ordinalIndexOf("a", Integer.MAX_VALUE))
    assert(-1 == "aabaabaa".ops.ordinalIndexOf("b", Integer.MAX_VALUE))
    assert(-1 == "aabaabaa".ops.ordinalIndexOf("ab", Integer.MAX_VALUE))
    assert(0 == "aabaabaa".ops.ordinalIndexOf("", Integer.MAX_VALUE))

    assert(-1 == "aaaaaaaaa".ops.ordinalIndexOf("a", 0))
    assert(0 == "aaaaaaaaa".ops.ordinalIndexOf("a", 1))
    assert(1 == "aaaaaaaaa".ops.ordinalIndexOf("a", 2))
    assert(2 == "aaaaaaaaa".ops.ordinalIndexOf("a", 3))
    assert(3 == "aaaaaaaaa".ops.ordinalIndexOf("a", 4))
    assert(4 == "aaaaaaaaa".ops.ordinalIndexOf("a", 5))
    assert(5 == "aaaaaaaaa".ops.ordinalIndexOf("a", 6))
    assert(6 == Some("aaaaaaaaa").ops.ordinalIndexOf(Some("a"), 7))
    assert(7 == "aaaaaaaaa".ops.ordinalIndexOf("a", 8))
    assert(8 == "aaaaaaaaa".ops.ordinalIndexOf("a", 9))
    assert(-1 == "aaaaaaaaa".ops.ordinalIndexOf("a", 10))

    // match at each possible position
    assert(0 == "aaaaaa".ops.ordinalIndexOf("aa", 1))
    assert(1 == "aaaaaa".ops.ordinalIndexOf("aa", 2))
    assert(2 == Some("aaaaaa").ops.ordinalIndexOf("aa", 3))
    assert(3 == "aaaaaa".ops.ordinalIndexOf("aa", 4))
    assert(4 == "aaaaaa".ops.ordinalIndexOf("aa", 5))
    assert(-1 == "aaaaaa".ops.ordinalIndexOf("aa", 6))

    assert(0 == "ababab".ops.ordinalIndexOf("aba", 1))
    assert(2 == "ababab".ops.ordinalIndexOf(Some("aba"), 2))
    assert(-1 == "ababab".ops.ordinalIndexOf("aba", 3))

    assert(0 == "abababab".ops.ordinalIndexOf("abab", 1))
    assert(2 == "abababab".ops.ordinalIndexOf("abab", 2))
    assert(4 == "abababab".ops.ordinalIndexOf("abab", 3))
    assert(-1 == "abababab".ops.ordinalIndexOf("abab", 4))
  }

  test("test lang 1193") {
    assert(0 == "abc".ops.ordinalIndexOf("ab", 1))
  }

  test("test lang 1241 1") {
    assert(0 == "abaabaab".ops.ordinalIndexOf("ab", 1))
    assert(3 == "abaabaab".ops.ordinalIndexOf("ab", 2))
    assert(6 == "abaabaab".ops.ordinalIndexOf("ab", 3))
  }

  test("test lang 1241 2") {
    //                                          0 2 4
    assert(0 == "abababa".ops.ordinalIndexOf("aba", 1))
    assert(2 == "abababa".ops.ordinalIndexOf("aba", 2))
    assert(4 == "abababa".ops.ordinalIndexOf("aba", 3))
    assert(0 == "abababab".ops.ordinalIndexOf("abab", 1))
    assert(2 == "abababab".ops.ordinalIndexOf("abab", 2))
    assert(4 == "abababab".ops.ordinalIndexOf("abab", 3))
  }
}
