package commons.lang3.bridge.impl

import commons.lang3.bridge.TypeMapping

sealed trait HlistTypeMapping

trait HlistTypeMappingPositive[Current, Tail <: HlistTypeMapping] extends HlistTypeMapping
object HlistTypeMappingPositive extends HlistTypeMappingPositiveLower {
  implicit def hlistTypeMappingPositiveImplicit1[B <: A, A, Tail <: HlistTypeMapping]: TypeMapping[B, HlistTypeMappingPositive[A, Tail]] =
    new TypeMapping(1)
}
trait HlistTypeMappingPositiveLower {
  implicit def hlistTypeMappingPositiveImplicitLower[A, B, Tail <: HlistTypeMapping](implicit
    tailMapping: TypeMapping[B, Tail]
  ): TypeMapping[B, HlistTypeMappingPositive[A, Tail]] = new TypeMapping(tailMapping.index + 1)
}

class HlistTypeMappingZero extends HlistTypeMapping
