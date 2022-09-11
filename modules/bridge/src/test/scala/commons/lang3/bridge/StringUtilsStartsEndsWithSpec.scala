package commons.lang3.bridge

import commons.lang3.bridge.StringUtils.ops.StringOptExt
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable

/** TODO
  *
  * @author
  *   mars
  * @version 1.0.0
  * @since 2022/09/11
  *   17:08
  */
class StringUtilsStartsEndsWithSpec extends AnyFunSuite {
  private val foo    = "foo"
  private val bar    = "bar"
  private val foobar = "foobar"
  private val FOO    = "FOO"
  private val BAR    = "BAR"
  private val FOOBAR = "FOOBAR"

  import StringUtilsSpec.{noneString, nullString}

  test("test string starts with") {
    assert(nullString.ops.startsWith(nullString), "startsWith(null, null)")
    assert(!FOOBAR.ops.startsWith(noneString), "startsWith(FOOBAR, null)")
    assert(!nullString.ops.startsWith(FOO), "startsWith(null, FOO)")
    assert(FOOBAR.ops.startsWith(""), "startsWith(FOOBAR, \"\")")

    assert(foobar.ops.startsWith(foo), "startsWith(foobar, foo)")
    assert(FOOBAR.ops.startsWith(FOO), "startsWith(FOOBAR, FOO)")
    assert(!foobar.ops.startsWith(FOO), "startsWith(foobar, FOO)")
    assert(!FOOBAR.ops.startsWith(foo), "startsWith(FOOBAR, foo)")

    assert(!foo.ops.startsWith(foobar), "startsWith(foo, foobar)")
    assert(!bar.ops.startsWith(foobar), "startsWith(foo, foobar)")

    assert(!foobar.ops.startsWith(bar), "startsWith(foobar, bar)")
    assert(!FOOBAR.ops.startsWith(BAR), "startsWith(FOOBAR, BAR)")
    assert(!foobar.ops.startsWith(BAR), "startsWith(foobar, BAR)")
    assert(!FOOBAR.ops.startsWith(bar), "startsWith(FOOBAR, bar)")
  }

  test("test string starts with ignore case") {
    assert(noneString.ops.startsWithIgnoreCase(nullString), "startsWithIgnoreCase(null, null)")
    assert(!FOOBAR.ops.startsWithIgnoreCase(noneString), "startsWithIgnoreCase(FOOBAR, null)")
    assert(!nullString.ops.startsWithIgnoreCase(FOO), "startsWithIgnoreCase(null, FOO)")
    assert(FOOBAR.ops.startsWithIgnoreCase(""), "startsWithIgnoreCase(FOOBAR, \"\")")

    assert(foobar.ops.startsWithIgnoreCase(foo), "startsWithIgnoreCase(foobar, foo)")
    assert(FOOBAR.ops.startsWithIgnoreCase(FOO), "startsWithIgnoreCase(FOOBAR, FOO)")
    assert(foobar.ops.startsWithIgnoreCase(FOO), "startsWithIgnoreCase(foobar, FOO)")
    assert(FOOBAR.ops.startsWithIgnoreCase(foo), "startsWithIgnoreCase(FOOBAR, foo)")

    assert(!foo.ops.startsWithIgnoreCase(foobar), "startsWithIgnoreCase(foo, foobar)")
    assert(!bar.ops.startsWithIgnoreCase(foobar), "startsWithIgnoreCase(foo, foobar)")

    assert(!foobar.ops.startsWithIgnoreCase(bar), "startsWithIgnoreCase(foobar, bar)")
    assert(!FOOBAR.ops.startsWithIgnoreCase(BAR), "startsWithIgnoreCase(FOOBAR, BAR)")
    assert(!foobar.ops.startsWithIgnoreCase(BAR), "startsWithIgnoreCase(foobar, BAR)")
    assert(!FOOBAR.ops.startsWithIgnoreCase(bar), "startsWithIgnoreCase(FOOBAR, bar)")
  }

  test("test starts with any") {
    assert(!nullString.ops.startsWithAny(null.asInstanceOf[Array[String]]: _*))
    assert(!noneString.ops.startsWithAny("abc"))
    assert(!"abcxyz".ops.startsWithAny(null.asInstanceOf[Array[String]]: _*))
    assert(!"abcxyz".ops.startsWithAny[String]())
    assert("abcxyz".ops.startsWithAny("abc"))
    assert("abcxyz".ops.startsWithAny(null, "xyz", "abc"))
    assert(!Some("abcxyz").ops.startsWithAny(null, "xyz", "abcd"))
    assert(Some("abcxyz").ops.startsWithAny(Some("")))
    assert(!"abcxyz".ops.startsWithAny(null, "xyz", "ABCX"))
    assert(!"ABCXYZ".ops.startsWithAny(null, "xyz", "abc"))

    assert(
      "abcxyz".ops.startsWithAny(new mutable.StringBuilder("xyz"), new StringBuffer("abc")),
      "StringUtils.startsWithAny(abcxyz, StringBuilder(xyz), StringBuffer(abc))"
    )

  }

  test("test ends with") {
    assert(nullString.ops.endsWith(noneString), "endsWith(null, null)")
    assert(!FOOBAR.ops.endsWith(nullString), "endsWith(FOOBAR, null)")
    assert(!noneString.ops.endsWith(FOO), "endsWith(null, FOO)")
    assert(FOOBAR.ops.endsWith(""), "endsWith(FOOBAR, \"\")")

    assert(!foobar.ops.endsWith(foo), "endsWith(foobar, foo)")
    assert(!FOOBAR.ops.endsWith(FOO), "endsWith(FOOBAR, FOO)")
    assert(!foobar.ops.endsWith(FOO), "endsWith(foobar, FOO)")
    assert(!FOOBAR.ops.endsWith(foo), "endsWith(FOOBAR, foo)")

    assert(!foo.ops.endsWith(foobar), "endsWith(foo, foobar)")
    assert(!bar.ops.endsWith(foobar), "endsWith(foo, foobar)")

    assert(foobar.ops.endsWith(bar), "endsWith(foobar, bar)")
    assert(FOOBAR.ops.endsWith(BAR), "endsWith(FOOBAR, BAR)")
    assert(!foobar.ops.endsWith(BAR), "endsWith(foobar, BAR)")
    assert(!FOOBAR.ops.endsWith(bar), "endsWith(FOOBAR, bar)")

    // "alpha, beta, gamma, delta".endsWith("delta")
    assert("\u03B1\u03B2\u03B3\u03B4".ops.endsWith("\u03B4"), "endsWith(\u03B1\u03B2\u03B3\u03B4, \u03B4)")
    // "alpha, beta, gamma, delta".endsWith("gamma, DELTA")
    assert(!Some("\u03B1\u03B2\u03B3\u03B4").ops.endsWith("\u03B3\u0394"), "endsWith(\u03B1\u03B2\u03B3\u03B4, \u03B3\u0394)")
  }

  test("test ends with ignore case") {
    assert(nullString.ops.endsWithIgnoreCase(noneString), "endsWithIgnoreCase(null, null)")
    assert(!FOOBAR.ops.endsWithIgnoreCase(nullString), "endsWithIgnoreCase(FOOBAR, null)")
    assert(!nullString.ops.endsWithIgnoreCase(FOO), "endsWithIgnoreCase(null, FOO)")
    assert(FOOBAR.ops.endsWithIgnoreCase(""), "endsWithIgnoreCase(FOOBAR, \"\")")

    assert(!foobar.ops.endsWithIgnoreCase(foo), "endsWithIgnoreCase(foobar, foo)")
    assert(!FOOBAR.ops.endsWithIgnoreCase(FOO), "endsWithIgnoreCase(FOOBAR, FOO)")
    assert(!foobar.ops.endsWithIgnoreCase(FOO), "endsWithIgnoreCase(foobar, FOO)")
    assert(!FOOBAR.ops.endsWithIgnoreCase(foo), "endsWithIgnoreCase(FOOBAR, foo)")

    assert(!foo.ops.endsWithIgnoreCase(foobar), "endsWithIgnoreCase(foo, foobar)")
    assert(!bar.ops.endsWithIgnoreCase(Some(foobar)), "endsWithIgnoreCase(foo, foobar)")

    assert(foobar.ops.endsWithIgnoreCase(bar), "endsWithIgnoreCase(foobar, bar)")
    assert(Some(FOOBAR).ops.endsWithIgnoreCase(BAR), "endsWithIgnoreCase(FOOBAR, BAR)")
    assert(foobar.ops.endsWithIgnoreCase(BAR), "endsWithIgnoreCase(foobar, BAR)")
    assert(FOOBAR.ops.endsWithIgnoreCase(bar), "endsWithIgnoreCase(FOOBAR, bar)")

    // javadoc
    assert("abcdef".ops.endsWithIgnoreCase("def"))
    assert("ABCDEF".ops.endsWithIgnoreCase("def"))
    assert(!Some("ABCDEF").ops.endsWithIgnoreCase("cde"))

    // "alpha, beta, gamma, delta".endsWith("DELTA")
    assert("\u03B1\u03B2\u03B3\u03B4".ops.endsWithIgnoreCase("\u0394"), "endsWith(\u03B1\u03B2\u03B3\u03B4, \u0394)")
    // "alpha, beta, gamma, delta".endsWith("GAMMA")
    assert(!"\u03B1\u03B2\u03B3\u03B4".ops.endsWithIgnoreCase("\u0393"), "endsWith(\u03B1\u03B2\u03B3\u03B4, \u0393)")
  }

  test("test ends with any") {
    assert(!nullString.ops.endsWithAny(null.asInstanceOf[String]), "StringUtils.endsWithAny(null, null)")
    assert(!noneString.ops.endsWithAny("abc"), "StringUtils.endsWithAny(null, new String[] {abc})")
    assert(!"abcxyz".ops.endsWithAny(null.asInstanceOf[String]), "StringUtils.endsWithAny(abcxyz, null)")
    assert("abcxyz".ops.endsWithAny(""), "StringUtils.endsWithAny(abcxyz, new String[] {\"\"})")
    assert("abcxyz".ops.endsWithAny("xyz"), "StringUtils.endsWithAny(abcxyz, new String[] {xyz})")
    assert("abcxyz".ops.endsWithAny(null, "xyz", "abc"), "StringUtils.endsWithAny(abcxyz, new String[] {null, xyz, abc})")
    assert(!"defg".ops.endsWithAny(null, "xyz", "abc"), "StringUtils.endsWithAny(defg, new String[] {null, xyz, abc})")
    assert("abcXYZ".ops.endsWithAny("def", "XYZ"))
    assert(!"abcXYZ".ops.endsWithAny("def", "xyz"))
    assert("abcXYZ".ops.endsWithAny(Some("def"), Some("YZ")))

    /*
     * Type null of the last argument to method endsWithAny(CharSequence, CharSequence...)
     * doesn't exactly match the vararg parameter type.
     * Cast to CharSequence[] to confirm the non-varargs invocation,
     * or pass individual arguments of type CharSequence for a varargs invocation.
     *
     * assertFalse(StringUtils.endsWithAny("abcXYZ", null)); // replace with specific types to avoid warning
     */
    assert(!Some("abcXYZ").ops.endsWithAny(null.asInstanceOf[CharSequence]))
    assert(!"abcXYZ".ops.endsWithAny(null.asInstanceOf[Array[CharSequence]]: _*))
    assert("abcXYZ".ops.endsWithAny(""))

    assert(
      "abcxyz".ops.endsWithAny(new mutable.StringBuilder("abc"), new StringBuffer("xyz")),
      "StringUtils.endsWithAny(abcxyz, StringBuilder(abc), StringBuffer(xyz))"
    )

  }
}
