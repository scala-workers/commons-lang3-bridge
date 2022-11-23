package commons.lang3.bridge

import commons.lang3.bridge.StringUtils.ops.StringOptExt
import commons.lang3.bridge.StringUtilsSpec._
import org.scalatest.funsuite.AnyFunSuite

/** TODO
  *
  * @author
  *   mars
  * @version 1.0.0
  * @since 2022/09/11
  *   23:26
  */
class StringUtilsTrimStripSpec extends AnyFunSuite {
  private val FOO = "foo"

  test("test trim") {
    assert(FOO == (FOO + "  ").ops.trim.get)
    assert(FOO == (" " + FOO + "  ").ops.trim.get)
    assert(FOO == (" " + FOO).ops.trim.get)
    assert(FOO == (FOO + "").ops.trim.get)
    assert(" \t\r\n\b ".ops.trim.contains(""))
    assert(TRIMMABLE.ops.trim.contains(""))
    assert(NON_TRIMMABLE == NON_TRIMMABLE.ops.trim.get)
    assert(Some("").ops.trim.contains(""))
    assert(noneString.ops.trim.isEmpty)
  }

  test("test string trim to null") {
    assert(FOO == (FOO + "  ").ops.trimToNone.get)
    assert(FOO == (" " + FOO + "  ").ops.trimToNone.get)
    assert(FOO == (" " + FOO).ops.trimToNone.get)
    assert(FOO == (FOO + "").ops.trimToNone.get)
    assert(Some(" \t\r\n\b ").ops.trimToNone.isEmpty)
    assert(TRIMMABLE.ops.trimToNone.isEmpty)
    assert(NON_TRIMMABLE == NON_TRIMMABLE.ops.trimToNone.get)
    assert("".ops.trimToNone.isEmpty)
    assert(noneString.ops.trimToNone.isEmpty)
  }

  test("test string trim to empty") {
    assert(FOO == (FOO + "  ").ops.trimToEmpty.get)
    assert(FOO == (" " + FOO + "  ").ops.trimToEmpty.get)
    assert(FOO == Some(" " + FOO).ops.trimToEmpty.get)
    assert(FOO == (FOO + "").ops.trimToEmpty.get)
    assert("" == " \t\r\n\b ".ops.trimToEmpty.get)
    assert("" == TRIMMABLE.ops.trimToEmpty.get)
    assert(NON_TRIMMABLE == NON_TRIMMABLE.ops.trimToEmpty.get)
    assert("" == "".ops.trimToEmpty.get)
    assert("" == noneString.ops.trimToEmpty.get)
  }

  test("test string strip") {
    assert(nullString.ops.strip.isEmpty)
    assert(Some("").ops.strip.contains(""))
    assert("        ".ops.strip.contains(""))
    assert("  abc  ".ops.strip.contains("abc"))
    assert(NON_WHITESPACE == (WHITESPACE + NON_WHITESPACE + WHITESPACE).ops.strip.get)
  }

  test("test string strip to null") {
    assert(noneString.ops.stripToNone.isEmpty)
    assert("".ops.stripToNone.isEmpty)
    assert(Some("        ").ops.stripToNone.isEmpty)
    assert(WHITESPACE.ops.stripToNone.isEmpty)
    assert("  ab c  ".ops.stripToNone.contains("ab c"))
    assert((WHITESPACE + NON_WHITESPACE + WHITESPACE).ops.stripToNone.contains(NON_WHITESPACE))
  }

  test("test strip to empty") {
    assert("" == noneString.ops.stripToEmpty.get)
    assert("" == Some("").ops.stripToEmpty.get)
    assert("" == "        ".ops.stripToEmpty.get)
    assert(WHITESPACE.ops.stripToEmpty.contains(""))
    assert("  ab c  ".ops.stripToEmpty.contains("ab c"))
    assert((WHITESPACE + NON_WHITESPACE + WHITESPACE).ops.stripToEmpty.contains(NON_WHITESPACE))
  }

  test("test string strip with strip chars") {
    // null strip
    assert(nullString.ops.strip(nullString).isEmpty)
    assert("".ops.strip(nullString).contains(""))
    assert("        ".ops.strip(noneString).contains(""))
    assert("  abc  ".ops.strip(noneString).contains("abc"))
    assert(Some(WHITESPACE + NON_WHITESPACE + WHITESPACE).ops.strip[String](null).contains(NON_WHITESPACE))

    // "" strip
    assert(nullString.ops.strip("").isEmpty)
    assert(Some("").ops.strip("").contains(""))
    assert("        ".ops.strip("").contains("        "))
    assert(Some("  abc  ").ops.strip(Some("")).contains("  abc  "))
    assert(WHITESPACE.ops.strip("").contains(WHITESPACE))

    // " " strip
    assert(noneString.ops.strip(" ").isEmpty)
    assert("".ops.strip(" ").contains(""))
    assert("        ".ops.strip(" ").contains(""))
    assert("  abc  ".ops.strip(" ").contains("abc"))

    // "ab" strip
    assert(nullString.ops.strip("ab").isEmpty)
    assert("".ops.strip("ab").contains(""))
    assert("        ".ops.strip("ab").contains("        "))
    assert("  abc  ".ops.strip("ab").contains("  abc  "))
    assert("abcabab".ops.strip("ab").contains("c"))
    assert(WHITESPACE.ops.strip("").contains(WHITESPACE))
  }

  test("test string strip start with strip chars string") {
    // null stripStart
    assert(noneString.ops.stripStart(nullString).isEmpty)
    assert("".ops.stripStart(nullString).contains(""))
    assert(Some("        ").ops.stripStart(nullString).contains(""))
    assert("  abc  ".ops.stripStart(noneString).contains("abc  "))
    assert((WHITESPACE + NON_WHITESPACE + WHITESPACE).ops.stripStart(nullString).contains(NON_WHITESPACE + WHITESPACE))

    // "" stripStart
    assert(noneString.ops.stripStart("").isEmpty)
    assert("".ops.stripStart("").contains(""))
    assert(Some("        ").ops.stripStart("").contains("        "))
    assert("  abc  ".ops.stripStart("").contains("  abc  "))
    assert(WHITESPACE.ops.stripStart("").contains(WHITESPACE))

    // " " stripStart
    assert(nullString.ops.stripStart(" ").isEmpty)
    assert("".ops.stripStart(" ").contains(""))
    assert(Some("        ").ops.stripStart(" ").contains(""))
    assert("  abc  ".ops.stripStart(" ").contains("abc  "))

    // "ab" stripStart
    assert(nullString.ops.stripStart("ab").isEmpty)
    assert("".ops.stripStart("ab").contains(""))
    assert("        ".ops.stripStart("ab").contains("        "))
    assert("  abc  ".ops.stripStart(Some("ab")).contains("  abc  "))
    assert("abcabab".ops.stripStart("ab").contains("cabab"))
    assert(Some(WHITESPACE).ops.stripStart("").contains(WHITESPACE))
  }

  test("test string strip end with strip chars string") {
    // null stripEnd
    assert(nullString.ops.stripEnd(nullString).isEmpty)
    assert("".ops.stripEnd(nullString).contains(""))
    assert("        ".ops.stripEnd(noneString).contains(""))
    assert("  abc  ".ops.stripEnd(null).contains("  abc"))
    assert(Some(WHITESPACE + NON_WHITESPACE + WHITESPACE).ops.stripEnd(noneString).contains(WHITESPACE + NON_WHITESPACE))

    // "" stripEnd
    assert(nullString.ops.stripEnd("").isEmpty)
    assert(Some("").ops.stripEnd("").contains(""))
    assert("        ".ops.stripEnd(Some("")).contains("        "))
    assert(Some("  abc  ").ops.stripEnd("").contains("  abc  "))
    assert(WHITESPACE.ops.stripEnd("").contains(WHITESPACE))

    // " " stripEnd
    assert(nullString.ops.stripEnd(" ").isEmpty)
    assert("".ops.stripEnd(" ").contains(""))
    assert("        ".ops.stripEnd(" ").contains(""))
    assert("  abc  ".ops.stripEnd(" ").contains("  abc"))

    // "ab" stripEnd
    assert(noneString.ops.stripEnd("ab").isEmpty)
    assert("".ops.stripEnd("ab").contains(""))
    assert("        ".ops.stripEnd(Some("ab")).contains("        "))
    assert("  abc  ".ops.stripEnd(Some("ab")).contains("  abc  "))
    assert(Some("abcabab").ops.stripEnd("ab").contains("abc"))
    assert(WHITESPACE.ops.stripEnd("").contains(WHITESPACE))
  }

  test("test string strip accents") {
    val cue = "\u00C7\u00FA\u00EA"
    assert(cue.ops.stripAccents.contains("Cue"), "Failed to strip accents from " + cue)

    val lots =
      "\u00C0\u00C1\u00C2\u00C3\u00C4\u00C5\u00C7\u00C8\u00C9" + "\u00CA\u00CB\u00CC\u00CD\u00CE\u00CF\u00D1\u00D2\u00D3" + "\u00D4\u00D5\u00D6\u00D9\u00DA\u00DB\u00DC\u00DD"
    assert(lots.ops.stripAccents.contains("AAAAAACEEEEIIIINOOOOOUUUUY"), "Failed to strip accents from " + lots)

    assert(nullString.ops.stripAccents.isEmpty, "Failed null safety")
    assert(Some("").ops.stripAccents.contains(""), "Failed empty String")
    assert("control".ops.stripAccents.contains("control"), "Failed to handle non-accented text")
    assert("\u00E9clair".ops.stripAccents.contains("eclair"), "Failed to handle easy example")
    assert(
      ("\u0104\u0141\u00D3\u015A\u017B\u0179\u0106\u0143 " + "\u0105\u0142\u00F3\u015B\u017C\u017A\u0107\u0144").ops.stripAccents
        .contains("ALOSZZCN aloszzcn")
    )
  }
}
