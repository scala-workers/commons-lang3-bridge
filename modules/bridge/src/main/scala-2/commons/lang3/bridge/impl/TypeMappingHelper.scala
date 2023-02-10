package commons.lang3.bridge.impl

import commons.lang3.bridge.{HelperIO, TypeMapping}

final class TakeTuple[T <: TypeMapping[_, _]] {
  type Target
}
object TakeTuple {
  type Aux[T <: TypeMapping[_, _], U] = TakeTuple[T] { type Target = U }
  @inline implicit def takeTupleImplicit[A, U]: Aux[TypeMapping[A, U], U] = null
}

final class FetchMappingApply[F[_] <: TypeMapping[_, _]] {
  @inline def input[T, V, Out](data: T)(implicit mapping: F[T], t: TakeTuple.Aux[F[T], V], helperIO: HelperIO.Aux[V, Out]): Out =
    helperIO.helper(mapping.index, data)
}
object FetchMappingApply {
  @inline private val value: FetchMappingApply[TypeMapping[*, Any]] = new FetchMappingApply[TypeMapping[*, Any]]
  @inline def get[F[_] <: TypeMapping[_, _]]: FetchMappingApply[F]  = value.asInstanceOf[FetchMappingApply[F]]
}
