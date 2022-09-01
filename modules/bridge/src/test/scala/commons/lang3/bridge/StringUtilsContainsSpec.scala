package commons.lang3.bridge

import org.scalatest.funsuite.AnyFunSuite

import java.util.Locale

/** TODO
  *
  * @author
  *   mars
  * @version 1.0.0
  * @since 2022/08/28
  *   01:18
  */
class StringUtilsContainsSpec extends AnyFunSuite {
  val nullString: String         = null
  val nullChars: Array[Char]     = null
  val noneString: Option[String] = None

  import commons.lang3.bridge.StringUtils.ops._

  /** Supplementary character U+20000 See http://www.oracle.com/technetwork/articles/javase/supplementary-142654.html
    */
  private val CharU20000 = "\uD840\uDC00"

  /** Supplementary character U+20001 See http://www.oracle.com/technetwork/articles/javase/supplementary-142654.html
    */
  private val CharU20001 = "\uD840\uDC01"

  /** Incomplete supplementary character U+20000, high surrogate only. See
    * http://www.oracle.com/technetwork/articles/javase/supplementary-142654.html
    */
  private val CharUSuppCharHigh = "\uDC00"

  /** Incomplete supplementary character U+20000, low surrogate only. See
    * http://www.oracle.com/technetwork/articles/javase/supplementary-142654.html
    */
  private val CharUSuppCharLow = "\uD840"

  test("test string contains char") {

    assert(!nullString.ops.contains(' '))
    assert(!"".ops.contains(' '))

    assert(!"".ops.contains(nullString))
    assert(!nullString.ops.contains(nullString))

    assert("abc".ops.contains('a'))
    assert("abc".ops.contains('b'))
    assert("abc".ops.contains('c'))
    assert(!"abc".ops.contains('z'))
  }

  test("test option string contains char ") {
    assert(!noneString.ops.contains(' '))
    assert(!Some("").ops.contains(' '))

    assert(!Some("").ops.contains(None))

    assert(!None.ops.contains(None))

    assert(Some("abc").ops.contains('a'))
    assert(Some("abc").ops.contains('b'))
    assert(Some("abc").ops.contains('c'))
    assert(!Some("abc").ops.contains('z'))

  }

  test("test string contains string") {
    assert(!nullString.ops.contains(nullString))
    assert(!nullString.ops.contains(""))
    assert(!nullString.ops.contains("a"))
    assert(!"".ops.contains(nullString))
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

    assert(!nullString.ops.containsAny(nullString))
    assert(!nullString.ops.containsAny(new Array[Char](0): _*))
    assert(!nullString.ops.containsAny('a', 'b'))

    assert(!"".ops.containsAny(nullString))
    assert(!"".ops.containsAny(new Array[Char](0): _*))
    assert(!"".ops.containsAny('a', 'b'))

    assert(!"zzabyycdxx".ops.containsAny(nullString))
    assert(!"zzabyycdxx".ops.containsAny(new Array[Char](0): _*))
    assert("zzabyycdxx".ops.containsAny('z', 'a'))
    assert("zzabyycdxx".ops.containsAny("b", "y"))
    assert("zzabyycdxx".ops.containsAny('z', 'y'))
    assert(!"ab".ops.containsAny(new Array[Char]('z'): _*))
  }

  test("test option string contains any string char array") {

    assert(!nullString.ops.containsAny(nullString))
    assert(!nullString.ops.containsAny(new Array[Char](0): _*))
    assert(!nullString.ops.containsAny('a', 'b'))

    assert(!Some("").ops.containsAny(nullString))
    assert(!Some("").ops.containsAny(new Array[Char](0): _*))
    assert(!Some("").ops.containsAny('a', 'b'))

    assert(!Some("zzabyycdxx").ops.containsAny(nullString))
    assert(!Some("zzabyycdxx").ops.containsAny(new Array[Char](0): _*))
    assert(Some("zzabyycdxx").ops.containsAny("z", "a"))
    assert(Some("zzabyycdxx").ops.containsAny('b', 'y'))
    assert(Some("zzabyycdxx").ops.containsAny('z', 'y'))
    assert(!Some("ab").ops.containsAny(new Array[Char]('z'): _*))
  }

  test("test contains any string or char array with bad supplement chars") {
    assert(!CharUSuppCharHigh.ops.containsAny(CharU20001.toCharArray: _*))
    assert(!("abc" + CharUSuppCharHigh + "xyz").ops.containsAny(CharU20001.toCharArray: _*))
    assert(CharUSuppCharLow.indexOf(CharU20001) == -1)
    assert(!Some(CharUSuppCharLow).ops.containsAny(CharU20001.toCharArray: _*))
    assert(!CharU20001.ops.containsAny(CharUSuppCharHigh.toCharArray: _*))
    assert(CharU20001.indexOf(CharUSuppCharLow) == 0)
    assert(Some(CharU20001).ops.containsAny(CharUSuppCharLow.toCharArray: _*))
  }

  test("test contains any string or char array with supplement chars") {
    assert((CharU20000 + CharU20001).ops.containsAny(CharU20000.toCharArray: _*))
    assert(("a" + CharU20000 + CharU20001).ops.containsAny("a".toCharArray: _*))
    assert((CharU20000 + "a" + CharU20001).ops.containsAny("a".toCharArray: _*))
    assert((CharU20000 + CharU20001 + "a").ops.containsAny("a".toCharArray: _*))
    assert(Some(CharU20000 + CharU20001).ops.containsAny(CharU20001.toCharArray: _*))
    assert(CharU20000.ops.containsAny(CharU20000.toCharArray: _*))
    // Sanity check:
    assert(-1 == CharU20000.indexOf(CharU20001))
    assert(0 == CharU20000.indexOf(CharU20001.charAt(0)))
    assert(-1 == CharU20000.indexOf(CharU20001.charAt(1)))
    // Test:
    assert(!Some(CharU20000).ops.containsAny(CharU20001.toCharArray: _*))
    assert(!CharU20001.ops.containsAny(CharU20000.toCharArray: _*))
  }

  test("test string contains any string") {
    assert(!nullString.ops.containsAny(nullString))
    assert(!nullString.ops.containsAny(""))
    assert(!nullString.ops.containsAny("ab"))

    assert(!"".ops.containsAny(nullString))
    assert(!"".ops.containsAny(""))
    assert(!"".ops.containsAny(""))
    assert(!"".ops.containsAny("ab"))

    assert(!"zzabyycdxx".ops.containsAny(nullString))
    assert(!"zzabyycdxx".ops.containsAny(""))
    assert("zzabyycdxx".ops.containsAny("za"))
    assert("zzabyycdxx".ops.containsAny("by"))
    assert("zzabyycdxx".ops.containsAny("zy"))
    assert(!"ab".ops.containsAny("z"))

  }

  test("test option string contains any string") {
    assert(!noneString.ops.containsAny(nullString))
    assert(!noneString.ops.containsAny(""))
    assert(!noneString.ops.containsAny("ab"))

    assert(!Some("").ops.containsAny(nullString))
    assert(!Some("").ops.containsAny(""))
    assert(!Some("").ops.containsAny("ab"))

    assert(!Some("zzabyycdxx").ops.containsAny(nullString))
    assert(!Some("zzabyycdxx").ops.containsAny(""))
    assert(Some("zzabyycdxx").ops.containsAny(Some("za")))
    assert(Some("zzabyycdxx").ops.containsAny("by"))
    assert(Some("zzabyycdxx").ops.containsAny("zy"))
    assert(!Some("ab").ops.containsAny("z"))
  }

  test("test string contains any string array") {
    assert(!nullString.ops.containsAny(nullString))
    assert(!nullString.ops.containsAny(new Array[String](0): _*))
    assert(!noneString.ops.containsAny(Array[String]("hello"): _*))
    assert(!Some("").ops.containsAny(noneString))
    assert(!"".ops.containsAny(new Array[String](0): _*))
    assert(!Some("").ops.containsAny(Array[String]("hello"): _*))
    assert(!Some("hello, goodbye").ops.containsAny(noneString))
    assert(!"hello, goodbye".ops.containsAny(new Array[String](0): _*))
    assert("hello, goodbye".ops.containsAny(Array[String]("hello", "goodbye"): _*))
    assert(Some("hello, goodbye").ops.containsAny(Array[String]("hello", "Goodbye"): _*))
    assert(!"hello, goodbye".ops.containsAny(Array[String]("Hello", "Goodbye"): _*))
    assert(!Some("hello, goodbye").ops.containsAny(Array[String]("Hello", null): _*))
    assert(!"hello, null".ops.containsAny(Array[String]("Hello", null): _*))
    // Javadoc examples:
    assert("abcd".ops.containsAny("ab", null))
    assert(Some("abcd").ops.containsAny("ab", "cd"))
    assert("abc".ops.containsAny("d", "abc"))
  }

  test("test string contains any ignore case string array") {
    assert(!(nullString.ops.containsAnyIgnoreCase(nullString)))
    assert(!nullString.ops.containsAnyIgnoreCase(new Array[String](0): _*))
    assert(!noneString.ops.containsAnyIgnoreCase(Array[String]("hello"): _*))
    assert(!"".ops.containsAnyIgnoreCase(nullString))
    assert(!Some("").ops.containsAnyIgnoreCase(new Array[String](0): _*))
    assert(!Some("").ops.containsAnyIgnoreCase(Array[String]("hello"): _*))
    assert(!Some("hello, goodbye").ops.containsAnyIgnoreCase(nullString))
    assert(!"hello, goodbye".ops.containsAnyIgnoreCase(new Array[String](0): _*))
    assert("hello, goodbye".ops.containsAnyIgnoreCase(Array[String]("hello", "goodbye"): _*))
    assert("hello, goodbye".ops.containsAnyIgnoreCase(Array[String]("hello", "Goodbye"): _*))
    assert(Some("hello, goodbye").ops.containsAnyIgnoreCase(Array[String]("Hello", "Goodbye"): _*))
    assert("hello, goodbye".ops.containsAnyIgnoreCase(Array[String]("Hello", null): _*))
    assert("hello, null".ops.containsAnyIgnoreCase(Array[String]("Hello", null): _*))
    // Javadoc examples:
    assert("abcd".ops.containsAnyIgnoreCase("ab", null))
    assert(Some("abcd").ops.containsAnyIgnoreCase("ab", "cd"))
    assert("abc".ops.containsAnyIgnoreCase("d", "abc"))
  }

  test("test string contains any string with bad supplementary chars") {
    assert(!CharUSuppCharHigh.ops.containsAny(CharU20001))
    assert(-1 == CharUSuppCharLow.indexOf(CharU20001))
    assert(!Some(CharUSuppCharLow).ops.containsAny(CharU20001))
    assert(!CharU20001.ops.containsAny(CharUSuppCharHigh))
    assert(0 == CharU20001.indexOf(CharUSuppCharLow))
    assert(CharU20001.ops.containsAnyIgnoreCase(CharUSuppCharLow))
  }

  test("test string contains any string with supplementary chars") {
    assert(Some(CharU20000 + CharU20001).ops.containsAny(CharU20000))
    assert((CharU20000 + CharU20001).ops.containsAny(Some(CharU20001)))
    assert(CharU20000.ops.containsAny(CharU20000))
    // Sanity check:
    assert(-1 == CharU20000.indexOf(CharU20001))
    assert(0 == CharU20000.indexOf(CharU20001.charAt(0)))
    assert(-1 == CharU20000.indexOf(CharU20001.charAt(1)))
    // Test:
    assert(!CharU20000.ops.containsAny(CharU20001))
    assert(!CharU20001.ops.containsAny(CharU20000))
  }

  test("test string contains ignore case locale independence") {
    val locales = Array(Locale.ENGLISH, new Locale("tr"), Locale.GERMAN)

    val tdata = Array(Array("i", "I"), Array("I", "i"), Array("\u03C2", "\u03C3"), Array("\u03A3", "\u03C2"), Array("\u03A3", "\u03C3"))

    val fdata = Array(Array("\u00DF", "SS"))

    for (testLocale <- locales) {
      Locale.setDefault(testLocale)
      for (j <- tdata.indices) {
        assert(
          tdata(j)(0).ops.containsIgnoreCase(tdata(j)(1)),
          Locale.getDefault.toString + ": " + j + " " + tdata(j)(0) + " " + tdata(j)(1)
        )
      }
      for (j <- fdata.indices) {
        assert(
          !fdata(j)(0).ops.containsIgnoreCase(fdata(j)(1)),
          Locale.getDefault.toString + ": " + j + " " + fdata(j)(0) + " " + fdata(j)(1)
        )
      }
    }
  }

  test("test string contains ignore case string") {
    assert(!nullString.ops.containsIgnoreCase(nullString))

    // Null tests
    assert(!noneString.ops.containsIgnoreCase(""))
    assert(!noneString.ops.containsIgnoreCase("a"))
    assert(!nullString.ops.containsIgnoreCase(Some("abc")))

    assert(!"".ops.containsIgnoreCase(nullString))
    assert(!Some("a").ops.containsIgnoreCase(nullString))
    assert(!"abc".ops.containsIgnoreCase(noneString))

    // Match len = 0
    assert("".ops.containsIgnoreCase(""))
    assert(Some("a").ops.containsIgnoreCase(Some("")))
    assert("abc".ops.containsIgnoreCase(""))

    // Match len = 1
    assert(!"".ops.containsIgnoreCase(Some("a")))
    assert(Some("a").ops.containsIgnoreCase("a"))
    assert("abc".ops.containsIgnoreCase("a"))
    assert(!Some("").ops.containsIgnoreCase("A"))
    assert("a".ops.containsIgnoreCase("A"))
    assert("abc".ops.containsIgnoreCase("A"))

    // Match len > 1
    assert(!"".ops.containsIgnoreCase("abc"))
    assert(!"a".ops.containsIgnoreCase(Some("abc")))
    assert(Some("xabcz").ops.containsIgnoreCase("abc"))
    assert(!"".ops.containsIgnoreCase(Some("ABC")))
    assert(!"a".ops.containsIgnoreCase("ABC"))
    assert("xabcz".ops.containsIgnoreCase("ABC"))
  }

  test("test string contains none char array") {
    val str1              = "a"
    val str2              = "b"
    val str3              = "ab."
    val chars1            = Array('b')
    val chars2            = Array('.')
    val chars3            = Array('c', 'd')
    val emptyChars        = new Array[Char](0)
    val noc: Option[Char] = None

    assert(nullString.ops.containsNone(noc))
    assert("".ops.containsNone(noc))
    assert(nullString.ops.containsNone(emptyChars: _*))
    assert(Some(str1).ops.containsNone(emptyChars: _*))
    assert("".ops.containsNone(emptyChars: _*))
    assert("".ops.containsNone(chars1: _*))
    assert(Some(str1).ops.containsNone(chars1: _*))
    assert(str1.ops.containsNone(chars2: _*))
    assert(str1.ops.containsNone(chars3: _*))
    assert(!str2.ops.containsNone(chars1: _*))
    assert(str2.ops.containsNone(chars2: _*))
    assert(str2.ops.containsNone(chars3: _*))
    assert(!Some(str3).ops.containsNone(chars1: _*))
    assert(!str3.ops.containsNone(chars2: _*))
    assert(str3.ops.containsNone(chars3: _*))
  }

  test("test string contains none of char array with bad supplementary chars") {
    assert(CharUSuppCharHigh.ops.containsNone(CharU20001.toCharArray: _*))
    assert(-1 == CharUSuppCharLow.indexOf(CharU20001))
    assert(CharUSuppCharLow.ops.containsNone(CharU20001.toCharArray: _*))
    assert(-1 == CharU20001.indexOf(CharUSuppCharHigh))
    assert(Some(CharU20001).ops.containsNone(CharUSuppCharHigh.toCharArray: _*))
    assert(0 == CharU20001.indexOf(CharUSuppCharLow))
    assert(!CharU20001.ops.containsNone(CharUSuppCharLow.toCharArray.map(Some(_)): _*))
  }

  test("test string contains none of char array with supplementary chars") {
    assert(!(CharU20000 + CharU20001).ops.containsNone(CharU20000.toCharArray: _*))
    assert(!Some(CharU20000 + CharU20001).ops.containsNone(CharU20001.toCharArray: _*))
    assert(!CharU20000.ops.containsNone(CharU20000.toCharArray: _*))
    // Sanity check:
    assert(-1 == CharU20000.indexOf(CharU20001))
    assert(0 == CharU20000.indexOf(CharU20001.charAt(0)))
    assert(-1 == CharU20000.indexOf(CharU20001.charAt(1)))
    // Test:
    assert(CharU20000.ops.containsNone(CharU20001.toCharArray: _*))
    assert(CharU20001.ops.containsNone(CharU20000.toCharArray.map(Some(_)): _*))
  }

  test("test string contains none string") {
    val str1   = "a"
    val str2   = "b"
    val str3   = "ab."
    val chars1 = "b"
    val chars2 = "."
    val chars3 = "cd"
    assert(nullString.ops.containsNone(nullString))
    assert(Some("").ops.containsNone(nullString))
    assert(nullString.ops.containsNone(""))
    assert(Some(str1).ops.containsNone(""))
    assert(Some("").ops.containsNone(Some("")))
    assert("".ops.containsNone(Some(chars1)))
    assert(str1.ops.containsNone(chars1))
    assert(str1.ops.containsNone(chars2))
    assert(str1.ops.containsNone(chars3))
    assert(!Some(str2).ops.containsNone(chars1))
    assert(Some(str2).ops.containsNone(Some(chars2)))
    assert(str2.ops.containsNone(chars3))
    assert(!str3.ops.containsNone(chars1))
    assert(!str3.ops.containsNone(Some(chars2)))
    assert(str3.ops.containsNone(chars3))
  }

  test("test string contains none string with bad supplementary chars") {
    // Test edge case: 1/2 of a (broken) supplementary char
    assert(CharUSuppCharHigh.ops.containsNone(CharU20001))
    assert(-1 == CharUSuppCharLow.indexOf(CharU20001))
    assert(CharUSuppCharLow.ops.containsNone(Some(CharU20001)))
    assert(-1 == CharU20001.indexOf(CharUSuppCharHigh))
    assert(Some(CharU20001).ops.containsNone(CharUSuppCharHigh))
    assert(0 == CharU20001.indexOf(CharUSuppCharLow))
    assert(!CharU20001.ops.containsNone(CharUSuppCharLow))
  }

  test("test string contains none string with supplementary chars") {
    assert(!(CharU20000 + CharU20001).ops.containsNone(CharU20000))
    assert(!Some(CharU20000 + CharU20001).ops.containsNone(Some(CharU20001)))
    assert(!CharU20000.ops.containsNone(CharU20000))
    // Sanity check:
    assert(-1 == CharU20000.indexOf(CharU20001))
    assert(0 == CharU20000.indexOf(CharU20001.charAt(0)))
    assert(-1 == CharU20000.indexOf(CharU20001.charAt(1)))
    // Test:
    assert(CharU20000.ops.containsNone(CharU20001))
    assert(CharU20001.ops.containsNone(CharU20000))
  }

  test("test string contains only char array") {
    val str1       = "a"
    val str2       = "b"
    val str3       = "ab"
    val chars1     = Array('b')
    val chars2     = Array('a')
    val chars3     = Array('a', 'b')
    val emptyChars = new Array[Char](0)
    assert(!nullString.ops.containsOnly(noneString))
    assert(!Some("").ops.containsOnly(nullString))
    assert(!nullString.ops.containsOnly(emptyChars: _*))
    assert(!str1.ops.containsOnly(emptyChars: _*))
    assert(Some("").ops.containsOnly(emptyChars: _*))
    assert("".ops.containsOnly(chars1: _*))
    assert(!Some(str1).ops.containsOnly(chars1: _*))
    assert(str1.ops.containsOnly(chars2: _*))
    assert(str1.ops.containsOnly(chars3: _*))
    assert(str2.ops.containsOnly(chars1: _*))
    assert(!Some(str2).ops.containsOnly(chars2.map(Option(_)): _*))
    assert(str2.ops.containsOnly(chars3.map(Option(_)): _*))
    assert(!str3.ops.containsOnly(chars1: _*))
    assert(!str3.ops.containsOnly(chars2: _*))
    assert(str3.ops.containsOnly(chars3: _*))
  }

  test("test string contains only string") {
    val str1   = "a"
    val str2   = "b"
    val str3   = "ab"
    val chars1 = "b"
    val chars2 = "a"
    val chars3 = "ab"
    assert(!noneString.ops.containsOnly(noneString))
    assert(!Some("").ops.containsOnly(nullString))
    assert(!noneString.ops.containsOnly(""))
    assert(!Some(str1).ops.containsOnly(Some("")))
    assert("".ops.containsOnly(""))
    assert("".ops.containsOnly(chars1))
    assert(!str1.ops.containsOnly(Some(chars1)))
    assert(str1.ops.containsOnly(chars2))
    assert(str1.ops.containsOnly(chars3))
    assert(str2.ops.containsOnly(chars1))
    assert(!str2.ops.containsOnly(chars2))
    assert(str2.ops.containsOnly(chars3))
    assert(!str3.ops.containsOnly(chars1))
    assert(!Some(str3).ops.containsOnly(Some(chars2)))
    assert(str3.ops.containsOnly(chars3))
  }

  test("test string contains whitespace") {
    assert(!"".ops.containsWhitespace)
    assert(Some(" ").ops.containsWhitespace)
    assert(!"a".ops.containsWhitespace)
    assert("a ".ops.containsWhitespace)
    assert(" a".ops.containsWhitespace)
    assert(Some("a\t").ops.containsWhitespace)
    assert("\n".ops.containsWhitespace)
  }
}
