package commons.lang3.scala

/**
 * TODO
 *
 * @author mars
 * @version 1.0.0
 * @since 2022/08/28 21:02
 */
trait ToStringOpt[-T] extends TypeMapping[T, Option[String]] {
  override def apply(i: T): Option[String]
}

object ToStringOpt {
  def apply[S](func: S => Option[String]): ToStringOpt[S] = (i: S) => func(i)

}