package commons.lang3.scala

import java.util.function.Supplier
import scala.language.implicitConversions


/**
 * 封装 Apache Commons 的字符串工具函数，简化代码
 *
 * @author liuxin
 * @version 1.0.0
 * @since 2022/08/22 18:58
 */
object StringUtils {

  object bridge {

    import commons.lang3.scala.ToStringOpt

    implicit val stringMappingImplicit: ToStringOpt[String] = ToStringOpt(i => Option.apply[String](i))
    implicit val stringOptMappingImplicit: ToStringOpt[Option[String]] = ToStringOpt(identity)

    import org.apache.commons.lang3.{StringUtils => Strings}

    implicit class StringOptExt[T: ToStringOpt](x: T) {

      private def optFunc: ToStringOpt[T] = summon

      def strOpt: Option[String] = optFunc(x)

      val ops: StringCommons[T] = new StringCommons(x)
    }

  }

  class StringHelper(str: String) {

    import org.apache.commons.lang3.{StringUtils => Strings}

    def abbreviate(maxWidth: Int): String = Strings.abbreviate(str, maxWidth)

    def abbreviate(offset: Int, maxWidth: Int): String = Strings.abbreviate(str, offset, maxWidth)

    def abbreviate(abbrevMarker: String, maxWidth: Int): String =
      Strings.abbreviate(str, abbrevMarker, maxWidth)

    def abbreviate(abbrevMarker: String, offset: Int, maxWidth: Int): String =
      Strings.abbreviate(str, abbrevMarker, offset, maxWidth)

    def abbreviateMiddle(middle: String, length: Int): String =
      Strings.abbreviateMiddle(str, middle, length)

    def appendIfMissing(suffix: CharSequence, suffixes: CharSequence*): String =
      Strings.appendIfMissing(str, suffix, suffixes: _*)

    def appendIfMissingIgnoreCase(suffix: CharSequence, suffixes: CharSequence*): String =
      Strings.appendIfMissingIgnoreCase(str, suffix, suffixes: _*)

    def capitalize: String = Strings.capitalize(str)

    def center(size: Int): String = Strings.center(str, size)

    def center(size: Int, padChar: Char): String = Strings.center(str, size, padChar)

    def center(size: Int, padStr: String): String = Strings.center(str, size, padStr)

    def chomp: String = Strings.chomp(str)

    def chop: String = Strings.chop(str)

    def compare(other: String): Int = Strings.compare(str, other)

    def compare(other: String, nullIsNull: Boolean): Int = Strings.compare(str, other, nullIsNull)

    def compareIgnoreCase(other: String): Int = Strings.compareIgnoreCase(str, other)

    def compareIgnoreCase(other: String, nullIsLess: Boolean): Int =
      Strings.compareIgnoreCase(str, other, nullIsLess)


    def containsAnyChar(searchChars: Char*): Boolean = Strings.containsAny(str, searchChars: _*)

    def containsAny(searchChars: CharSequence): Boolean = Strings.containsAny(str, searchChars)

    def containsAny(searchCharSequences: CharSequence*): Boolean = Strings.containsAny(str, searchCharSequences: _*)

    def containsAnyIgnoreCase(searchCharSequences: CharSequence*): Boolean =
      Strings.containsAnyIgnoreCase(str, searchCharSequences: _*)

    def containsIgnoreCase(searchStr: CharSequence): Boolean = Strings.containsIgnoreCase(str, searchStr)

    def containsNone(searchChars: Char*): Boolean = Strings.containsNone(str, searchChars: _*)

    def containsNone(invalidChars: String): Boolean = Strings.containsNone(str, invalidChars)

    def containsOnly(valid: Char*): Boolean = Strings.containsOnly(str, valid: _*)

    def containsOnly(validChars: String): Boolean = Strings.containsOnly(str, validChars)

    def countMatches(ch: Char): Int = Strings.countMatches(str, ch)

    def countMatches(sub: CharSequence): Int = Strings.countMatches(str, sub)

    def defaultIfBlank(defaultStr: String): String = Strings.defaultIfBlank(str, defaultStr)

    def defaultIfEmpty(defaultStr: String): String = Strings.defaultIfEmpty(str, defaultStr)

    def defaultString: String = Strings.defaultString(str)

    def defaultString(defaultStr: String): String = Strings.defaultString(str, defaultStr)

    def deleteWhitespace: String = Strings.deleteWhitespace(str)

    def difference(other: String): String = Strings.difference(str, other)

    def endsWith(suffix: CharSequence): Boolean = Strings.endsWith(str, suffix)

    def endsWithAny(searchStrings: CharSequence*): Boolean = Strings.endsWithAny(str, searchStrings: _*)

    def endWithIgnoreCase(suffix: CharSequence): Boolean =
      Strings.endsWithIgnoreCase(str, suffix)

    def equals(other: String): Boolean = Strings.equals(str, other)

    def equalsAny(searchStrings: String*): Boolean = Strings.equalsAny(str, searchStrings: _*)

    def equalsAnyIgnoreCase(searchStrings: String*): Boolean = Strings.equalsAnyIgnoreCase(str, searchStrings: _*)

    def equalsIgnoreCase(other: String): Boolean = Strings.equalsIgnoreCase(str, other)

    def getDigits: String = Strings.getDigits(str)

    def getIfBlank(defaultSupplier: Supplier[String]): String = Strings.getIfBlank[String](str, defaultSupplier)

    def getIfEmpty(defaultSupplier: Supplier[String]): String = Strings.getIfBlank[String](str, defaultSupplier)

    def indexOf(searchSeq: CharSequence): Int = Strings.indexOf(str, searchSeq)

    def indexOf(searchSeq: CharSequence, startPos: Int): Int = Strings.indexOf(str, searchSeq, startPos)

    def indexOf(searchChar: Char): Int = Strings.indexOf(str, searchChar)

    def indexOf(searchChar: Char, startPos: Int): Int = Strings.indexOf(str, searchChar, startPos)

    def indexOfAnyChar(searchChars: Char*): Int = Strings.indexOfAny(str, searchChars: _*)

    def indexOfAny(searchStrs: CharSequence*): Int = Strings.indexOfAny(str, searchStrs: _*)

    def indexOfAny(searchChars: String): Int = Strings.indexOfAny(str, searchChars)

    def indexOfAnyCharBut(searchChars: Char*): Int = Strings.indexOfAnyBut(str, searchChars: _*)

    def indexOfAnyBut(searchChars: CharSequence): Int = Strings.indexOfAnyBut(str, searchChars)

    def indexOfDifference(other: String): Int = Strings.indexOfDifference(str, other)

    def indexOfDifference(searchStr: CharSequence): Int = Strings.indexOfDifference(str, searchStr)

    def indexOfIgnoreCase(searchStr: CharSequence, startPos: Int): Int =
      Strings.indexOfIgnoreCase(str, searchStr, startPos)


  }

}
