package commons.lang3.bridge

package impl:

  class CusInnerApply[O[_] <: Tuple](index: Int, value: Any):
    inline def fold[U](inline funcCol: O[U]): U = funcCol.drop(index - 1).asInstanceOf[NonEmptyTuple].head.asInstanceOf[Any => U](value)
  end CusInnerApply

end impl

object TypeMappingInnerHelper:

  import impl.CusInnerApply as InnerApply

  extension [I, T <: Tuple](inline mapping: TypeMapping[I, T])
    inline def input(inline i: I): InnerApply[[t] =>> Tuple.Map[T, [x] =>> (x => t)]] = new InnerApply(index = mapping.index, value = i)

end TypeMappingInnerHelper
