package commons.lang3.bridge.impl
import commons.lang3.bridge.TypeMapping

trait TypeMappingImplicitOptsPolyHigher extends TypeMappingImplicitOptsPolyLower:
  inline given [A, B <: A, T <: Tuple]: TypeMapping[B, A *: T] = new TypeMapping(1)
end TypeMappingImplicitOptsPolyHigher

trait TypeMappingImplicitOptsPolyLower:
  inline given [A, B, T <: Tuple](using inline mapping: TypeMapping[B, T]): TypeMapping[B, A *: T] = new TypeMapping(mapping.index + 1)
end TypeMappingImplicitOptsPolyLower
