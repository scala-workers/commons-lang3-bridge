package commons.lang3.scala

/**
 * TODO
 *
 * @author mars
 * @version 1.0.0
 * @since 2022/08/28 02:48
 */
trait TypeMapping[-I, +O] {
  def apply(i: I): O
}

object TypeMapping {
  def apply[S, T](func: S => T): TypeMapping[S, T] = func(_)
}