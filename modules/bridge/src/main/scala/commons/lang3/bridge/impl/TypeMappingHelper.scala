package commons.lang3.bridge.impl

import commons.lang3.bridge.{HelperIO, TypeMapping}

trait FetchMappingAply[F[_] <: TypeMapping[_, _]] {
  @inline def input[T, Out](data: T)(implicit mapping: F[T], helperIO: HelperIO.Aux[F[T], Out]): Out = helperIO.helper(mapping.index, data)
}

object FetchMappingAply {
  private object value extends FetchMappingAply[({ type F[_] = TypeMapping[Any, Any] })#F]
  @inline def get[F[_] <: TypeMapping[_, _]]: FetchMappingAply[F] = value.asInstanceOf[FetchMappingAply[F]]
}
