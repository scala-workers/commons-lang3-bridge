package commons.lang3.bridge.impl

import commons.lang3.bridge.TypeMapping

sealed trait HlistTypeMapping

final class HlistTypeMappingPositive[Current, Tail <: HlistTypeMapping] extends HlistTypeMapping
object HlistTypeMappingPositive extends HlistTypeMappingPositiveLower {
  @inline implicit def hlistTypeMappingPositiveImplicit1[B <: A, A, Tail <: HlistTypeMapping]
    : TypeMapping[B, HlistTypeMappingPositive[A, Tail]] =
    new TypeMapping(1)
}
trait HlistTypeMappingPositiveLower {
  @inline implicit def hlistTypeMappingPositiveImplicitLower[A, B, Tail <: HlistTypeMapping](implicit
    tailMapping: TypeMapping[B, Tail]
  ): TypeMapping[B, HlistTypeMappingPositive[A, Tail]] = new TypeMapping(tailMapping.index + 1)
}

final class HlistTypeMappingZero extends HlistTypeMapping
