package commons.lang3.scala

trait TypeMappingImplicitOptsPoly1 extends TypeMappingImplicitOptsPoly2 {

  implicit def typeMappingImplicit2IDept[I1, I2]: TypeMapping[I1, (I1, I2)] = TypeMappingInstances.get(1)

  implicit def typeMappingImplicit2I1[I1, I2]: TypeMapping[I2, (I1, I2)] = TypeMappingInstances.get(2)

  implicit def typeMappingImplicit3I1[I1, I2, I3]: TypeMapping[I3, (I1, I2, I3)] = TypeMappingInstances.get(3)

  implicit def typeMappingImplicit3Dept[I4, I1, I2, I3](implicit
    deptMapping: TypeMapping[I4, (I1, I2, I3)]
  ): TypeMapping[I4, (I1, I2, I3)] = deptMapping.asInstanceOf

  implicit def typeMappingImplicit4I1[I1, I2, I3, I4]: TypeMapping[I4, (I1, I2, I3, I4)] = TypeMappingInstances.get(4)

  implicit def typeMappingImplicit4Dept[I5, I1, I2, I3, I4](implicit
    deptMapping: TypeMapping[I5, (I1, I2, I3, I4)]
  ): TypeMapping[I5, (I1, I2, I3, I4)] = deptMapping.asInstanceOf

  implicit def typeMappingImplicit5I1[I1, I2, I3, I4, I5]: TypeMapping[I5, (I1, I2, I3, I4, I5)] = TypeMappingInstances.get(5)

  implicit def typeMappingImplicit5Dept[I6, I1, I2, I3, I4, I5](implicit
    deptMapping: TypeMapping[I6, (I1, I2, I3, I4, I5)]
  ): TypeMapping[I6, (I1, I2, I3, I4, I5)] = deptMapping.asInstanceOf

  implicit def typeMappingImplicit6I1[I1, I2, I3, I4, I5, I6]: TypeMapping[I6, (I1, I2, I3, I4, I5, I6)] = TypeMappingInstances.get(6)

  implicit def typeMappingImplicit6Dept[I7, I1, I2, I3, I4, I5, I6](implicit
    deptMapping: TypeMapping[I7, (I1, I2, I3, I4, I5, I6)]
  ): TypeMapping[I7, (I1, I2, I3, I4, I5, I6)] = deptMapping.asInstanceOf

  implicit def typeMappingImplicit7I1[I1, I2, I3, I4, I5, I6, I7]: TypeMapping[I7, (I1, I2, I3, I4, I5, I6, I7)] =
    TypeMappingInstances.get(7)

  implicit def typeMappingImplicit7Dept[I8, I1, I2, I3, I4, I5, I6, I7](implicit
    deptMapping: TypeMapping[I8, (I1, I2, I3, I4, I5, I6, I7)]
  ): TypeMapping[I8, (I1, I2, I3, I4, I5, I6, I7)] = deptMapping.asInstanceOf

  implicit def typeMappingImplicit8I1[I1, I2, I3, I4, I5, I6, I7, I8]: TypeMapping[I8, (I1, I2, I3, I4, I5, I6, I7, I8)] =
    TypeMappingInstances.get(8)

  implicit def typeMappingImplicit8Dept[I9, I1, I2, I3, I4, I5, I6, I7, I8](implicit
    deptMapping: TypeMapping[I9, (I1, I2, I3, I4, I5, I6, I7, I8)]
  ): TypeMapping[I9, (I1, I2, I3, I4, I5, I6, I7, I8)] = deptMapping.asInstanceOf

  implicit def typeMappingImplicit9I1[I1, I2, I3, I4, I5, I6, I7, I8, I9]: TypeMapping[I9, (I1, I2, I3, I4, I5, I6, I7, I8, I9)] =
    TypeMappingInstances.get(9)

  implicit def typeMappingImplicit9Dept[I10, I1, I2, I3, I4, I5, I6, I7, I8, I9](implicit
    deptMapping: TypeMapping[I10, (I1, I2, I3, I4, I5, I6, I7, I8, I9)]
  ): TypeMapping[I10, (I1, I2, I3, I4, I5, I6, I7, I8, I9)] = deptMapping.asInstanceOf

  implicit def typeMappingImplicit10I1[I1, I2, I3, I4, I5, I6, I7, I8, I9, I10]
    : TypeMapping[I10, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10)] = TypeMappingInstances.get(10)

  implicit def typeMappingImplicit10Dept[I11, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10](implicit
    deptMapping: TypeMapping[I11, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10)]
  ): TypeMapping[I11, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10)] = deptMapping.asInstanceOf

  implicit def typeMappingImplicit11I1[I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11]
    : TypeMapping[I11, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11)] = TypeMappingInstances.get(11)

  implicit def typeMappingImplicit11Dept[I12, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11](implicit
    deptMapping: TypeMapping[I12, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11)]
  ): TypeMapping[I12, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11)] = deptMapping.asInstanceOf

  implicit def typeMappingImplicit12I1[I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12]
    : TypeMapping[I12, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12)] = TypeMappingInstances.get(12)

  implicit def typeMappingImplicit12Dept[I13, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12](implicit
    deptMapping: TypeMapping[I13, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12)]
  ): TypeMapping[I13, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12)] = deptMapping.asInstanceOf

  implicit def typeMappingImplicit13I1[I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13]
    : TypeMapping[I13, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13)] = TypeMappingInstances.get(13)

  implicit def typeMappingImplicit13Dept[I14, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13](implicit
    deptMapping: TypeMapping[I14, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13)]
  ): TypeMapping[I14, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13)] = deptMapping.asInstanceOf

  implicit def typeMappingImplicit14I1[I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14]
    : TypeMapping[I14, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14)] = TypeMappingInstances.get(14)

  implicit def typeMappingImplicit14Dept[I15, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14](implicit
    deptMapping: TypeMapping[I15, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14)]
  ): TypeMapping[I15, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14)] = deptMapping.asInstanceOf

  implicit def typeMappingImplicit15I1[I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15]
    : TypeMapping[I15, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15)] = TypeMappingInstances.get(15)

  implicit def typeMappingImplicit15Dept[I16, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15](implicit
    deptMapping: TypeMapping[I16, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15)]
  ): TypeMapping[I16, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15)] = deptMapping.asInstanceOf

  implicit def typeMappingImplicit16I1[I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16]
    : TypeMapping[I16, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16)] = TypeMappingInstances.get(16)

  implicit def typeMappingImplicit16Dept[I17, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16](implicit
    deptMapping: TypeMapping[I17, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16)]
  ): TypeMapping[I17, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16)] = deptMapping.asInstanceOf

  implicit def typeMappingImplicit17I1[I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17]
    : TypeMapping[I17, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17)] = TypeMappingInstances.get(17)

  implicit def typeMappingImplicit17Dept[I18, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17](implicit
    deptMapping: TypeMapping[I18, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17)]
  ): TypeMapping[I18, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17)] = deptMapping.asInstanceOf

  implicit def typeMappingImplicit18I1[I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18]
    : TypeMapping[I18, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18)] = TypeMappingInstances.get(18)

  implicit def typeMappingImplicit18Dept[I19, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18](implicit
    deptMapping: TypeMapping[I19, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18)]
  ): TypeMapping[I19, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18)] = deptMapping.asInstanceOf

  implicit def typeMappingImplicit19I1[I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19]
    : TypeMapping[I19, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19)] =
    TypeMappingInstances.get(19)

  implicit def typeMappingImplicit19Dept[I20, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19](implicit
    deptMapping: TypeMapping[I20, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19)]
  ): TypeMapping[I20, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19)] = deptMapping.asInstanceOf

  implicit def typeMappingImplicit20I1[I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19, I20]
    : TypeMapping[I20, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19, I20)] =
    TypeMappingInstances.get(20)

  implicit def typeMappingImplicit20Dept[I21, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19, I20](
    implicit deptMapping: TypeMapping[I21, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19, I20)]
  ): TypeMapping[I21, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19, I20)] =
    deptMapping.asInstanceOf

  implicit def typeMappingImplicit21I1[I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19, I20, I21]
    : TypeMapping[I21, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19, I20, I21)] =
    TypeMappingInstances.get(21)

  implicit def typeMappingImplicit21Dept[
    I22,
    I1,
    I2,
    I3,
    I4,
    I5,
    I6,
    I7,
    I8,
    I9,
    I10,
    I11,
    I12,
    I13,
    I14,
    I15,
    I16,
    I17,
    I18,
    I19,
    I20,
    I21
  ](implicit
    deptMapping: TypeMapping[I22, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19, I20, I21)]
  ): TypeMapping[I22, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19, I20, I21)] =
    deptMapping.asInstanceOf

  implicit def typeMappingImplicit22I1[I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19, I20, I21, I22]
    : TypeMapping[I22, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19, I20, I21, I22)] =
    TypeMappingInstances.get(22)

  implicit def typeMappingImplicit22Dept[
    I23,
    I1,
    I2,
    I3,
    I4,
    I5,
    I6,
    I7,
    I8,
    I9,
    I10,
    I11,
    I12,
    I13,
    I14,
    I15,
    I16,
    I17,
    I18,
    I19,
    I20,
    I21,
    I22
  ](implicit
    deptMapping: TypeMapping[I23, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19, I20, I21, I22)]
  ): TypeMapping[I23, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19, I20, I21, I22)] =
    deptMapping.asInstanceOf

}
