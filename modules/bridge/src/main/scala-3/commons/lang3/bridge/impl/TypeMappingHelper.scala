package commons.lang3.bridge.impl

import commons.lang3.bridge.TypeMapping
import scala.language.experimental.erasedDefinitions

final class FetchMappingAply[F[_] <: TypeMapping[_, _]]:
  type TakeTuple[T <: TypeMapping[_, _]] <: Tuple = T match
    case TypeMapping[a, b] =>
      b match
        case c *: d     => c *: d
        case EmptyTuple => EmptyTuple
  end TakeTuple

  inline final def input[T](inline data: T): InnerApply[[t] =>> Tuple.Map[TakeTuple[F[T]], [x] =>> (x => t)], F[T]] = InnerApply(data)
end FetchMappingAply

final class InnerApply[O[_] <: Tuple, Mapping <: TypeMapping[_, _]](value: Any) extends AnyVal:
  inline def fold[U](inline funcCol: O[U])(using inline mapping: Mapping): U =
    funcCol.productElement(mapping.index - 1).asInstanceOf[Any => Any](value).asInstanceOf[U]
end InnerApply
