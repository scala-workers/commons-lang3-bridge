package commons.lang3.bridge

package impl:

  class CusInnerApply[O[_] <: Tuple](index: Int, value: Any):
    def fold[U](func: O[U]): U =
      func.drop(index - 1).asInstanceOf[NonEmptyTuple].head.asInstanceOf[Any => U](value)
  end CusInnerApply

end impl

object TypeMappingInnerHelper:

  import impl.CusInnerApply as InnerApply

  extension [I, T <: Tuple](mapping: TypeMapping[I, T])
    @inline def input(i: I): InnerApply[[t] =>> Tuple.Map[T, [x] =>> (x => t)]] = new InnerApply(index = mapping.index, value = i)

end TypeMappingInnerHelper
