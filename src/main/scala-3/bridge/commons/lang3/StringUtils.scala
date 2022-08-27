package bridge.commons.lang3


/**
 * 封装 Apache Commons 的字符串工具函数，简化代码
 *
 * @author liuxin
 * @version 1.0.0
 * @since 2022/08/22 18:58
 */
object StringUtils {

  import org.apache.commons.lang3.{StringUtils => Strings}

  implicit class StringOps(x: String) {
    def abbreviate(str: String, maxWidth: Int): String = Strings.abbreviate(str, maxWidth)

    def abbreviate(str: String, offset: Int, maxWidth: Int): String = Strings.abbreviate(str, offset, maxWidth)

    def abbreviate(str: String, abbrevMarker: String, maxWidth: Int): String =
      Strings.abbreviate(str, abbrevMarker, maxWidth)

    def abbreviate(str: String, abbrevMarker: String, offset: Int, maxWidth: Int): String =
      Strings.abbreviate(str, abbrevMarker, offset, maxWidth)



    def isBlank: Boolean = Strings.isBlank(x)

    def isNotBlank: Boolean = Strings.isNotBlank(x)

    def isNoneBlank: Boolean = Strings.isNoneBlank(x)

    def isEmpty: Boolean = Strings.isEmpty(x)

    def isNotEmpty: Boolean = Strings.isNotEmpty(x)

    def isNoneEmpty: Boolean = Strings.isNoneEmpty(x)

  }
}
