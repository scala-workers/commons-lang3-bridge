package commons.scala.bridge.lang3

/**
 * TODO
 *
 * @author mars
 * @version 1.0.0
 * @since 2022/08/28 02:48
 */
trait TypeMapping[-I, +O] extends (I => O) {
  override def apply(i: I): O
}

object TypeMapping {
  def apply[S, T](func: S => T): scala.TypeMapping[S, T] = new TypeMapping[S, T] {
    override def apply(i: S): T = func(i)
  }
}