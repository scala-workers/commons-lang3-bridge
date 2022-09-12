package commons.lang3.bridge.impl
import commons.lang3.bridge.TypeMapping

trait TypeMappingAlias {

  type TypeOptions1[A, I1]        = TypeMapping[A, (I1)]
  type TypeOptions1F[F[_], A, I1] = TypeMapping[F[A], (I1)]

  type TypeOptions2[A, I1, I2]        = TypeMapping[A, (I1, I2)]
  type TypeOptions2F[F[_], A, I1, I2] = TypeMapping[F[A], (I1, I2)]

  type TypeOptions3[A, I1, I2, I3]        = TypeMapping[A, (I1, I2, I3)]
  type TypeOptions3F[F[_], A, I1, I2, I3] = TypeMapping[F[A], (I1, I2, I3)]

  type TypeOptions4[A, I1, I2, I3, I4]        = TypeMapping[A, (I1, I2, I3, I4)]
  type TypeOptions4F[F[_], A, I1, I2, I3, I4] = TypeMapping[F[A], (I1, I2, I3, I4)]

  type TypeOptions5[A, I1, I2, I3, I4, I5]        = TypeMapping[A, (I1, I2, I3, I4, I5)]
  type TypeOptions5F[F[_], A, I1, I2, I3, I4, I5] = TypeMapping[F[A], (I1, I2, I3, I4, I5)]

  type TypeOptions6[A, I1, I2, I3, I4, I5, I6]        = TypeMapping[A, (I1, I2, I3, I4, I5, I6)]
  type TypeOptions6F[F[_], A, I1, I2, I3, I4, I5, I6] = TypeMapping[F[A], (I1, I2, I3, I4, I5, I6)]

  type TypeOptions7[A, I1, I2, I3, I4, I5, I6, I7]        = TypeMapping[A, (I1, I2, I3, I4, I5, I6, I7)]
  type TypeOptions7F[F[_], A, I1, I2, I3, I4, I5, I6, I7] = TypeMapping[F[A], (I1, I2, I3, I4, I5, I6, I7)]

  type TypeOptions8[A, I1, I2, I3, I4, I5, I6, I7, I8]        = TypeMapping[A, (I1, I2, I3, I4, I5, I6, I7, I8)]
  type TypeOptions8F[F[_], A, I1, I2, I3, I4, I5, I6, I7, I8] = TypeMapping[F[A], (I1, I2, I3, I4, I5, I6, I7, I8)]

  type TypeOptions9[A, I1, I2, I3, I4, I5, I6, I7, I8, I9]        = TypeMapping[A, (I1, I2, I3, I4, I5, I6, I7, I8, I9)]
  type TypeOptions9F[F[_], A, I1, I2, I3, I4, I5, I6, I7, I8, I9] = TypeMapping[F[A], (I1, I2, I3, I4, I5, I6, I7, I8, I9)]

  type TypeOptions10[A, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10]        = TypeMapping[A, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10)]
  type TypeOptions10F[F[_], A, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10] = TypeMapping[F[A], (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10)]

  type TypeOptions11[A, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11] = TypeMapping[A, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11)]
  type TypeOptions11F[F[_], A, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11] =
    TypeMapping[F[A], (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11)]

  type TypeOptions12[A, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12] =
    TypeMapping[A, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12)]
  type TypeOptions12F[F[_], A, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12] =
    TypeMapping[F[A], (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12)]

  type TypeOptions13[A, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13] =
    TypeMapping[A, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13)]
  type TypeOptions13F[F[_], A, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13] =
    TypeMapping[F[A], (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13)]

  type TypeOptions14[A, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14] =
    TypeMapping[A, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14)]
  type TypeOptions14F[F[_], A, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14] =
    TypeMapping[F[A], (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14)]

  type TypeOptions15[A, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15] =
    TypeMapping[A, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15)]
  type TypeOptions15F[F[_], A, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15] =
    TypeMapping[F[A], (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15)]

  type TypeOptions16[A, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16] =
    TypeMapping[A, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16)]
  type TypeOptions16F[F[_], A, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16] =
    TypeMapping[F[A], (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16)]

  type TypeOptions17[A, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17] =
    TypeMapping[A, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17)]
  type TypeOptions17F[F[_], A, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17] =
    TypeMapping[F[A], (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17)]

  type TypeOptions18[A, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18] =
    TypeMapping[A, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18)]
  type TypeOptions18F[F[_], A, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18] =
    TypeMapping[F[A], (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18)]

  type TypeOptions19[A, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19] =
    TypeMapping[A, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19)]
  type TypeOptions19F[F[_], A, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19] =
    TypeMapping[F[A], (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19)]

  type TypeOptions20[A, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19, I20] =
    TypeMapping[A, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19, I20)]
  type TypeOptions20F[F[_], A, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19, I20] =
    TypeMapping[F[A], (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19, I20)]

  type TypeOptions21[A, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19, I20, I21] =
    TypeMapping[A, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19, I20, I21)]
  type TypeOptions21F[F[_], A, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19, I20, I21] =
    TypeMapping[F[A], (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19, I20, I21)]

  type TypeOptions22[A, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19, I20, I21, I22] =
    TypeMapping[A, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19, I20, I21, I22)]
  type TypeOptions22F[F[_], A, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19, I20, I21, I22] =
    TypeMapping[F[A], (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19, I20, I21, I22)]

}
