package commons.lang3.bridge.impl
import commons.lang3.bridge.TypeMapping

trait TypeMappingImplicitOptsPolyHigher extends TypeMappingImplicitOptsPolyLower with MappingApply:
  inline given [A, B <: A, T <: Tuple]: TypeMapping[B, A *: T] = TypeMapping(1)
end TypeMappingImplicitOptsPolyHigher

trait TypeMappingImplicitOptsPolyLower:
  inline given [A, B, T <: Tuple](using inline mapping: TypeMapping[B, T]): TypeMapping[B, A *: T] = TypeMapping(mapping.index + 1)
end TypeMappingImplicitOptsPolyLower

trait MappingApply:
  inline def getMapping[F[_] <: TypeMapping[_, _]]: FetchMappingAply[F] = null
end MappingApply
