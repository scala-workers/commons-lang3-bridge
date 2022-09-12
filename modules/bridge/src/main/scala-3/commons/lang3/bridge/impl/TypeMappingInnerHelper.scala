package commons.lang3.bridge.impl

import commons.lang3.bridge.TypeMapping
import commons.lang3.bridge.HelperIO

package inner:

  class CusInnerApply[O[_] <: Tuple](index: Int, value: Any):
    inline def fold[U](inline funcCol: O[U]): U = funcCol.drop(index - 1).asInstanceOf[NonEmptyTuple].head.asInstanceOf[Any => U](value)
  end CusInnerApply

end inner

trait HelperIOImplicit:
  import inner.CusInnerApply as InnerApply

  given [E, A <: Tuple]: HelperIO.Aux[TypeMapping[E, A], InnerApply[[t] =>> Tuple.Map[A, [x] =>> (x => t)]]] =
    new HelperIO[TypeMapping[E, A]]:
      override type Out = InnerApply[[t] =>> Tuple.Map[A, [x] =>> (x => t)]]
      override def helper(index: Int, value: Any): InnerApply[[t] =>> Tuple.Map[A, [x] =>> (x => t)]] = new InnerApply(index, value)

end HelperIOImplicit
