package commons.lang3.bridge

/** 封装 Apache Commons 的字符串工具函数，简化代码
  *
  * @author
  *   liuxin
  * @version 1.0.0
  * @since 2022/08/22
  *   18:58
  */
object StringUtils {

  object ops {

    import net.scalax.simple.adt.{TypeAdt => Adt}

    implicit class StringOptExt[T: Adt.CoProducts2[*, String, Option[String]]](x: T) {
      val ops: StringCommons[T] = new StringCommons(x)
    }

  }

}
