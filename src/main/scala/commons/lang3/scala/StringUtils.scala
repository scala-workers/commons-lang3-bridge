package commons.lang3.scala

/**
 * 封装 Apache Commons 的字符串工具函数，简化代码
 *
 * @author liuxin
 * @version 1.0.0
 * @since 2022/08/22 18:58
 */
object StringUtils {

  object bridge {

    implicit val stringMappingImplicit: ToStringOpt[String] = ToStringOpt(i => Option.apply[String](i))
    implicit val stringOptMappingImplicit: ToStringOpt[Option[String]] = ToStringOpt(identity)

    implicit class StringOptExt[T: ToStringOpt](x: T) {

      private def optFunc: ToStringOpt[T] = implicitly

      def strOpt: Option[String] = optFunc(x)

      val ops: StringCommons[T] = new StringCommons(x)
    }

  }
}
