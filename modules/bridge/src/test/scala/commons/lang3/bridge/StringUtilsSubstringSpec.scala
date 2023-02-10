package commons.lang3.bridge

import commons.lang3.bridge.StringUtils.ops.StringOptExt
import org.scalatest.funsuite.AnyFunSuite

/** TODO
  *
  * @author
  *   mars
  * @version 1.0.0
  * @since 2022/09/11
  *   18:14
  */
class StringUtilsSubstringSpec extends AnyFunSuite {
  private val FOO      = "foo"
  private val BAR      = "bar"
  private val BAZ      = "baz"
  private val FOOBAR   = "foobar"
  private val SENTENCE = "foo bar baz"

  import StringUtilsSpec.{noneString, nullString}
  test("test substring with start") {
    assert(noneString.ops.substring(0).isEmpty)
    assert(Some("").ops.substring(0).contains(""))
    assert("".ops.substring(2).contains(""))

    assert(SENTENCE.ops.substring(80).contains(""))
    assert(SENTENCE.ops.substring(8).contains(BAZ))
    assert(SENTENCE.ops.substring(-3).contains(BAZ))
    assert(SENTENCE.ops.substring(0).contains(SENTENCE))
    assert(Some("abc").ops.substring(-4).contains("abc"))
    assert("abc".ops.substring(-3).contains("abc"))
    assert("abc".ops.substring(-2).contains("bc"))
    assert("abc".ops.substring(-1).contains("c"))
    assert("abc".ops.substring(0).contains("abc"))
    assert("abc".ops.substring(1).contains("bc"))
    assert(Some("abc").ops.substring(2).contains("c"))
    assert("abc".ops.substring(3).contains(""))
    assert("abc".ops.substring(4).contains(""))
  }

  test("test substring with start and end") {
    assert(nullString.ops.substring(0, 0).isEmpty)
    assert(noneString.ops.substring(1, 2).isEmpty)
    assert("".ops.substring(0, 0).contains(""))
    assert(Some("").ops.substring(1, 2).contains(""))
    assert("".ops.substring(-2, -1).contains(""))

    assert("" == SENTENCE.ops.substring(8, 6).get)
    assert(FOO == SENTENCE.ops.substring(0, 3).get)
    assert("o" == SENTENCE.ops.substring(-9, 3).get)
    assert(FOO == SENTENCE.ops.substring(0, -8).get)
    assert("o" == SENTENCE.ops.substring(-9, -8).get)
    assert(SENTENCE == SENTENCE.ops.substring(0, 80).get)
    assert("" == SENTENCE.ops.substring(2, 2).get)
    assert("b" == "abc".ops.substring(-2, -1).get)
  }

  test("test string left") {
    assert(noneString.ops.left(-1).isEmpty)
    assert(nullString.ops.left(0).isEmpty)
    assert(noneString.ops.left(2).isEmpty)

    assert("".ops.left(-1).contains(""))
    assert("".ops.left(0).contains(""))
    assert("".ops.left(2).contains(""))

    assert(FOOBAR.ops.left(-1).contains(""))
    assert(FOOBAR.ops.left(0).contains(""))
    assert(FOOBAR.ops.left(3).contains(FOO))
    assert(FOOBAR.ops.left(80).contains(FOOBAR))
  }

  test("test string right") {
    assert(nullString.ops.right(-1).isEmpty)
    assert(noneString.ops.right(0).isEmpty)
    assert(nullString.ops.right(2).isEmpty)

    assert("".ops.right(-1).contains(""))
    assert("".ops.right(0).contains(""))
    assert("".ops.right(2).contains(""))

    assert(FOOBAR.ops.right(-1).contains(""))
    assert(FOOBAR.ops.right(0).contains(""))
    assert(Some(FOOBAR).ops.right(3).contains(BAR))
    assert(FOOBAR.ops.right(80).contains(FOOBAR))
  }

  test("test string mid") {
    assert(nullString.ops.mid(-1, 0).isEmpty)
    assert(noneString.ops.mid(0, -1).isEmpty)
    assert(nullString.ops.mid(3, 0).isEmpty)
    assert(noneString.ops.mid(3, 2).isEmpty)

    assert("".ops.mid(0, -1).contains(""))
    assert("".ops.mid(0, 0).contains(""))
    assert("".ops.mid(0, 2).contains(""))

    assert(FOOBAR.ops.mid(3, -1).contains(""))
    assert(FOOBAR.ops.mid(3, 0).contains(""))
    assert(FOOBAR.ops.mid(3, 1).contains("b"))
    assert(FOOBAR.ops.mid(0, 3).contains(FOO))
    assert(FOOBAR.ops.mid(3, 3).contains(BAR))
    assert(FOOBAR.ops.mid(0, 80).contains(FOOBAR))
    assert(FOOBAR.ops.mid(3, 80).contains(BAR))
    assert(FOOBAR.ops.mid(9, 3).contains(""))
    assert(FOOBAR.ops.mid(-1, 3).contains(FOO))
  }

  test("test string substring before separator char") {
    assert("fooXXbarXXbaz".ops.substringBefore('X').contains("foo"))

    assert(nullString.ops.substringBefore(0).isEmpty)
    assert(noneString.ops.substringBefore('X').isEmpty)
    assert("".ops.substringBefore(0).contains(""))
    assert("".ops.substringBefore('X').contains(""))

    assert(Some("foo").ops.substringBefore(0).contains("foo"))
    assert("foo".ops.substringBefore('b').contains("foo"))
    assert("foot".ops.substringBefore('o').contains("f"))
    assert("abc".ops.substringBefore('a').contains(""))
    assert("abcba".ops.substringBefore('b').contains("a"))
    assert("abc".ops.substringBefore('c').contains("ab"))
    assert("abc".ops.substringBefore(0).contains("abc"))
  }

  test("test string substring before separator string") {
    assert(Some("fooXXbarXXbaz").ops.substringBefore("XX").contains("foo"))

    assert(noneString.ops.substringBefore(noneString).isEmpty)
    assert(nullString.ops.substringBefore("").isEmpty)
    assert(noneString.ops.substringBefore("XX").isEmpty)
    assert("".ops.substringBefore(nullString).contains(""))
    assert("".ops.substringBefore("").contains(""))
    assert("".ops.substringBefore("XX").contains(""))

    assert(Some("foo").ops.substringBefore[String](null).contains("foo"))
    assert("foo".ops.substringBefore("b").contains("foo"))
    assert("foot".ops.substringBefore("o").contains("f"))
    assert("abc".ops.substringBefore("a").contains(""))
    assert("abcba".ops.substringBefore("b").contains("a"))
    assert("abc".ops.substringBefore("c").contains("ab"))
    assert("abc".ops.substringBefore("").contains(""))
    assert("abc".ops.substringBefore("X").contains("abc"))
  }

  test("test string substring after separator string") {
    assert("fooXXbarXXbaz".ops.substringAfter("XX").contains("barXXbaz"))

    assert(noneString.ops.substringAfter[String](null).isEmpty)
    assert(noneString.ops.substringAfter("").isEmpty)
    assert(nullString.ops.substringAfter("XX").isEmpty)
    assert("".ops.substringAfter[String](null).contains(""))
    assert("".ops.substringAfter("").contains(""))
    assert(Some("").ops.substringAfter(Some("XX")).contains(""))

    assert("foo".ops.substringAfter(noneString).contains(""))
    assert("foot".ops.substringAfter("o").contains("ot"))
    assert("abc".ops.substringAfter(Some("a")).contains("bc"))
    assert(Some("abcba").ops.substringAfter("b").contains("cba"))
    assert("abc".ops.substringAfter("c").contains(""))
    assert("abc".ops.substringAfter("").contains("abc"))
    assert("abc".ops.substringAfter("d").contains(""))
  }

  test("test string substring after separator char") {
    assert(noneString.ops.substringAfter(0).isEmpty)
    assert(nullString.ops.substringAfter('X').isEmpty)
    assert("".ops.substringAfter(0).contains(""))
    assert(Some("").ops.substringAfter('X').contains(""))

    assert("foo".ops.substringAfter(0).contains(""))
    assert("foot".ops.substringAfter('o').contains("ot"))
    assert("abc".ops.substringAfter('a').contains("bc"))
    assert("abcba".ops.substringAfter('b').contains("cba"))
    assert("abc".ops.substringAfter('c').contains(""))
    assert("abc".ops.substringAfter('d').contains(""))
  }

  test("test string substring before last separator") {
    assert("fooXXbarXXbaz".ops.substringBeforeLast("XX").contains("fooXXbar"))

    assert(nullString.ops.substringBeforeLast(noneString).isEmpty)
    assert(noneString.ops.substringBeforeLast("").isEmpty)
    assert(nullString.ops.substringBeforeLast("XX").isEmpty)
    assert("".ops.substringBeforeLast(noneString).contains(""))
    assert(Some("").ops.substringBeforeLast("").contains(""))
    assert("".ops.substringBeforeLast(Some("XX")).contains(""))

    assert("foo".ops.substringBeforeLast(nullString).contains("foo"))
    assert(Some("foo").ops.substringBeforeLast("b").contains("foo"))
    assert("foo".ops.substringBeforeLast("o").contains("fo"))
    assert("abc\r\n".ops.substringBeforeLast("d").contains("abc\r\n"))
    assert("abcdabc".ops.substringBeforeLast("d").contains("abc"))
    assert("abcdabcd".ops.substringBeforeLast(Some("d")).contains("abcdabc"))
    assert("abc".ops.substringBeforeLast("b").contains("a"))
    assert("abc \n".ops.substringBeforeLast("\n").contains("abc "))
    assert("a".ops.substringBeforeLast(noneString).contains("a"))
    assert("a".ops.substringBeforeLast("").contains("a"))
    assert("a".ops.substringBeforeLast("a").contains(""))
  }

  test("test string substring after last separator") {
    assert("fooXXbarXXbaz".ops.substringAfterLast("XX").contains("baz"))

    assert(nullString.ops.substringAfterLast(nullString).isEmpty)
    assert(nullString.ops.substringAfterLast(Some("")).isEmpty)
    assert(noneString.ops.substringAfterLast("XX").isEmpty)
    assert("".ops.substringAfterLast(noneString).contains(""))
    assert("".ops.substringAfterLast("").contains(""))
    assert("".ops.substringAfterLast("a").contains(""))

    assert("foo".ops.substringAfterLast(nullString).contains(""))
    assert("foo".ops.substringAfterLast("b").contains(""))
    assert(Some("foot").ops.substringAfterLast("o").contains("t"))
    assert("abc".ops.substringAfterLast(Some("a")).contains("bc"))
    assert("abcba".ops.substringAfterLast("b").contains("a"))
    assert("abc".ops.substringAfterLast("c").contains(""))
    assert("".ops.substringAfterLast("d").contains(""))
    assert("abc".ops.substringAfterLast("").contains(""))
  }

  test("test string substring after last separator char") {
    assert(nullString.ops.substringAfterLast(0).isEmpty)
    assert(noneString.ops.substringAfterLast('X').isEmpty)
    assert("".ops.substringAfterLast(0).contains(""))
    assert(Some("").ops.substringAfterLast('a').contains(""))

    assert("foo".ops.substringAfterLast(0).contains(""))
    assert("foo".ops.substringAfterLast('b').contains(""))
    assert(Some("foot").ops.substringAfterLast('o').contains("t"))
    assert("abc".ops.substringAfterLast('a').contains("bc"))
    assert("abcba".ops.substringAfterLast('b').contains("a"))
    assert("abc".ops.substringAfterLast('c').contains(""))
    assert("".ops.substringAfterLast('d').contains(""))
  }

  test("test string substring between tag") {
    assert(noneString.ops.substringBetween("tag").isEmpty)
    assert(Some("").ops.substringBetween("").contains(""))
    assert("".ops.substringBetween(Some("abc")).isEmpty)
    assert("    ".ops.substringBetween(" ").contains(""))
    assert("abc".ops.substringBetween(noneString).isEmpty)
    assert("abc".ops.substringBetween("").contains(""))
    assert("abc".ops.substringBetween(Some("a")).isEmpty)
    assert(Some("abca").ops.substringBetween("a").contains("bc"))
    assert("abcabca".ops.substringBetween("a").contains("bc"))
    assert("\nbar\n".ops.substringBetween("\n").contains("bar"))
  }

  test("test string substring between open and close string") {
    assert(noneString.ops.substringBetween("", "").isEmpty)
    assert("".ops.substringBetween(nullString, "").isEmpty)
    assert(Some("").ops.substringBetween(Some(""), noneString).isEmpty)
    assert("".ops.substringBetween("", "").contains(""))
    assert("foo".ops.substringBetween("", "").contains(""))
    assert("foo".ops.substringBetween(Some(""), Some("]")).isEmpty)
    assert("foo".ops.substringBetween("[", "]").isEmpty)
    assert("    ".ops.substringBetween(" ", "  ").contains(""))
    assert("<foo>bar</foo>".ops.substringBetween("<foo>", "</foo>").contains("bar"))
  }

  test("test strings substring between open and close") {
    var results = "[one], [two], [three]".ops.substringsBetween("[", "]").get
    assert(3 == results.length)
    assert("one" == results(0))
    assert("two" == results(1))
    assert("three" == results(2))

    results = "[one], [two], three".ops.substringsBetween(Some("["), Some("]")).get
    assert(2 == results.length)
    assert("one" == results(0))
    assert("two" == results(1))

    results = "[one], [two], three]".ops.substringsBetween("[", "]").get
    assert(2 == results.length)
    assert("one" == results(0))
    assert("two" == results(1))

    results = "[one], two], three]".ops.substringsBetween("[", "]").get
    assert(1 == results.length)
    assert("one" == results(0))

    results = "one], two], [three]".ops.substringsBetween("[", "]").get
    assert(1 == results.length)
    assert("three" == results(0))

    // 'ab hello ba' will match, but 'ab non ba' won't
    // this is because the 'a' is shared between the two and can't be matched twice
    results = "aabhellobabnonba".ops.substringsBetween("ab", "ba").get
    assert(1 == results.length)
    assert("hello" == results(0))

    results = "one, two, three".ops.substringsBetween("[", "]").orNull
    assert(results == null)

    results = "[one, two, three".ops.substringsBetween("[", "]").orNull
    assert(results == null)

    results = "one, two, three]".ops.substringsBetween("[", "]").orNull
    assert(results == null)

    results = "[one], [two], [three]".ops.substringsBetween("[", null).orNull
    assert(results == null)

    results = "[one], [two], [three]".ops.substringsBetween(noneString, Some("]")).orNull
    assert(results == null)

    results = "[one], [two], [three]".ops.substringsBetween("", "").orNull
    assert(results == null)

    results = noneString.ops.substringsBetween("[", "]").orNull
    assert(results == null)

    results = "".ops.substringsBetween("[", "]").get
    assert(0 == results.length)
  }

  test("test string count matches with string") {
    assert(0 == nullString.ops.countMatches(noneString))
    assert(0 == Some("blah").ops.countMatches(nullString))
    assert(0 == nullString.ops.countMatches("DD"))

    assert(0 == "x".ops.countMatches(""))
    assert(0 == "".ops.countMatches(""))

    assert(3 == "one long someone sentence of one".ops.countMatches("one"))
    assert(0 == "one long someone sentence of one".ops.countMatches("two"))
    assert(4 == "oooooooooooo".ops.countMatches("ooo"))
    assert(0 == noneString.ops.countMatches("?"))
    assert(0 == "".ops.countMatches(Some("?")))
    assert(0 == "abba".ops.countMatches(noneString))
    assert(0 == "abba".ops.countMatches(""))
    assert(2 == Some("abba").ops.countMatches("a"))
    assert(1 == "abba".ops.countMatches("ab"))
    assert(0 == "abba".ops.countMatches("xxx"))
    assert(1 == "ababa".ops.countMatches("aba"))
  }

  test("test string count matches with char") {
    assert(0 == nullString.ops.countMatches('D'))
    assert(5 == "one long someone sentence of one".ops.countMatches(' '))
    assert(6 == "one long someone sentence of one".ops.countMatches('o'))
    assert(4 == "oooooooooooo".ops.countMatches("ooo"))
  }
}
