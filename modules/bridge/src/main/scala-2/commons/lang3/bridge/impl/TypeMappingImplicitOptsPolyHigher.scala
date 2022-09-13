package commons.lang3.bridge.impl

import commons.lang3.bridge.TypeMapping

trait TypeMappingImplicitOptsPolyHigher {
  @inline def getMapping[F[_] <: TypeMapping[_, _]]: FetchMappingAply[F] = FetchMappingAply.get
}
