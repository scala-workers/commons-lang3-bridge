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

  import commons.lang3.bridge.StringUtils.bridge._

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

  test("test string contains ignore case locale independence"){
    val locales = Array(Locale.ENGLISH, new Locale("tr"), Locale.GERMAN)

    val tdata = Array(Array("i", "I"), Array("I", "i"), Array("\u03C2", "\u03C3"), Array("\u03A3", "\u03C2"), Array("\u03A3", "\u03C3"))

    val fdata = Array(Array("\u00DF", "SS"))

    for (testLocale <- locales) {
      Locale.setDefault(testLocale)
      for (j <- 0 until tdata.length) {
        assert(tdata(j)(0).ops.containsIgnoreCase(tdata(j)(1)), Locale.getDefault + ": " + j + " " + tdata(j)(0) + " " + tdata(j)(1))
      }
      for (j <- 0 until fdata.length) {
        assertFalse(StringUtils.containsIgnoreCase(fdata(j)(0), fdata(j)(1)), Locale.getDefault + ": " + j + " " + fdata(j)(0) + " " + fdata(j)(1))
      }
    }
  }
}
