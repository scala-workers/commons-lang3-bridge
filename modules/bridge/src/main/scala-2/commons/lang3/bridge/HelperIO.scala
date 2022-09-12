package commons.lang3.bridge

/** TODO
  *
  * @author
  *   djx314
  * @version 1.0.0
  * @since 2022/08/28
  *   02:48
  */
abstract class HelperIO[Input] {
  type Out
  def helper(index: Int, value: Any): Out
}

object HelperIO extends impl.HelperIOImplicit {
  type Aux[I, O] = HelperIO[I] { type Out = O }
}
