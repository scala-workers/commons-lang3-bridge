package commons.lang3.bridge.impl

import commons.lang3.bridge.TypeMapping

final class FetchMappingAply[F[_] <: TypeMapping[_, _]]:
  type TakeTuple[T <: TypeMapping[_, _]] <: Tuple = T match
    case TypeMapping[a, b] =>
      b match
        case c *: d     => c *: d
        case EmptyTuple => EmptyTuple
  end TakeTuple

  import inner.CusInnerApply as InnerApply

  inline final def input[T](inline data: T)(using inline mapping: F[T]): InnerApply[[t] =>> Tuple.Map[TakeTuple[F[T]], [x] =>> (x => t)]] =
    new InnerApply(index = mapping.index, value = data)
  end input

end FetchMappingAply

object FetchMappingAply:
  private val value: FetchMappingAply[TypeMapping[*, Any]]       = new FetchMappingAply[TypeMapping[*, Any]]
  inline def get[F[_] <: TypeMapping[_, _]]: FetchMappingAply[F] = value.asInstanceOf[FetchMappingAply[F]]
end FetchMappingAply

package inner:
  final class CusInnerApply[O[_] <: Tuple](index: Int, value: Any):
    inline def fold[U](inline funcCol: O[U]): U = funcCol.drop(index - 1).asInstanceOf[NonEmptyTuple].head.asInstanceOf[Any => U](value)
  end CusInnerApply
end inner
