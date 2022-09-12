package commons.lang3.bridge.impl

import commons.lang3.bridge.{HelperIO, TypeMapping}

final class TakeTuple[T <: TypeMapping[_, _]] {
  type Target
}
object TakeTuple {
  @inline private val value: TakeTuple[TypeMapping[Any, Any]] = new TakeTuple
  type Aux[T <: TypeMapping[_, _], U] = TakeTuple[T] { type Target = U }
  @inline implicit def takeTupleImplicit[A, U]: Aux[TypeMapping[A, U], U] = value.asInstanceOf[Aux[TypeMapping[A, U], U]]
}

final class FetchMappingAply[F[_] <: TypeMapping[_, _]] {
  @inline def input[T, V, Out](data: T)(implicit mapping: F[T], t: TakeTuple.Aux[F[T], V], helperIO: HelperIO.Aux[V, Out]): Out =
    helperIO.helper(mapping.index, data)
}
object FetchMappingAply {
  @inline private val value: FetchMappingAply[TypeMapping[*, Any]] = new FetchMappingAply[TypeMapping[*, Any]]
  @inline def get[F[_] <: TypeMapping[_, _]]: FetchMappingAply[F]  = value.asInstanceOf[FetchMappingAply[F]]
}
