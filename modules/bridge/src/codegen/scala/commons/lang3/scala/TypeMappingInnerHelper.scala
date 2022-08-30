package commons.lang3.scala

object TypeMappingInnerHelper extends TypeMappingInnerHelperPoly {

  implicit class typeMappingImplicit2Poly[I, I1, I2](val mapping: TypeMapping[I, (I1, I2)]) extends AnyVal {
    def ops[T](func1: I1 => T, func2: I2 => T): I => T = i => {
      val setValue = mapping.setValue(i)
      setValue._1 match {

        case 1 => func1(setValue._2.asInstanceOf[I1])

        case 2 => func2(setValue._2.asInstanceOf[I2])

      }
    }
  }

  implicit class typeMappingImplicit3Poly[I, I1, I2, I3](val mapping: TypeMapping[I, (I1, I2, I3)]) extends AnyVal {
    def ops[T](func1: I1 => T, func2: I2 => T, func3: I3 => T): I => T = i => {
      val setValue = mapping.setValue(i)
      setValue._1 match {

        case 1 => func1(setValue._2.asInstanceOf[I1])

        case 2 => func2(setValue._2.asInstanceOf[I2])

        case 3 => func3(setValue._2.asInstanceOf[I3])

      }
    }
  }

  implicit class typeMappingImplicit4Poly[I, I1, I2, I3, I4](val mapping: TypeMapping[I, (I1, I2, I3, I4)]) extends AnyVal {
    def ops[T](func1: I1 => T, func2: I2 => T, func3: I3 => T, func4: I4 => T): I => T = i => {
      val setValue = mapping.setValue(i)
      setValue._1 match {

        case 1 => func1(setValue._2.asInstanceOf[I1])

        case 2 => func2(setValue._2.asInstanceOf[I2])

        case 3 => func3(setValue._2.asInstanceOf[I3])

        case 4 => func4(setValue._2.asInstanceOf[I4])

      }
    }
  }

  implicit class typeMappingImplicit5Poly[I, I1, I2, I3, I4, I5](val mapping: TypeMapping[I, (I1, I2, I3, I4, I5)]) extends AnyVal {
    def ops[T](func1: I1 => T, func2: I2 => T, func3: I3 => T, func4: I4 => T, func5: I5 => T): I => T = i => {
      val setValue = mapping.setValue(i)
      setValue._1 match {

        case 1 => func1(setValue._2.asInstanceOf[I1])

        case 2 => func2(setValue._2.asInstanceOf[I2])

        case 3 => func3(setValue._2.asInstanceOf[I3])

        case 4 => func4(setValue._2.asInstanceOf[I4])

        case 5 => func5(setValue._2.asInstanceOf[I5])

      }
    }
  }

  implicit class typeMappingImplicit6Poly[I, I1, I2, I3, I4, I5, I6](val mapping: TypeMapping[I, (I1, I2, I3, I4, I5, I6)]) extends AnyVal {
    def ops[T](func1: I1 => T, func2: I2 => T, func3: I3 => T, func4: I4 => T, func5: I5 => T, func6: I6 => T): I => T = i => {
      val setValue = mapping.setValue(i)
      setValue._1 match {

        case 1 => func1(setValue._2.asInstanceOf[I1])

        case 2 => func2(setValue._2.asInstanceOf[I2])

        case 3 => func3(setValue._2.asInstanceOf[I3])

        case 4 => func4(setValue._2.asInstanceOf[I4])

        case 5 => func5(setValue._2.asInstanceOf[I5])

        case 6 => func6(setValue._2.asInstanceOf[I6])

      }
    }
  }

  implicit class typeMappingImplicit7Poly[I, I1, I2, I3, I4, I5, I6, I7](val mapping: TypeMapping[I, (I1, I2, I3, I4, I5, I6, I7)])
      extends AnyVal {
    def ops[T](func1: I1 => T, func2: I2 => T, func3: I3 => T, func4: I4 => T, func5: I5 => T, func6: I6 => T, func7: I7 => T): I => T =
      i => {
        val setValue = mapping.setValue(i)
        setValue._1 match {

          case 1 => func1(setValue._2.asInstanceOf[I1])

          case 2 => func2(setValue._2.asInstanceOf[I2])

          case 3 => func3(setValue._2.asInstanceOf[I3])

          case 4 => func4(setValue._2.asInstanceOf[I4])

          case 5 => func5(setValue._2.asInstanceOf[I5])

          case 6 => func6(setValue._2.asInstanceOf[I6])

          case 7 => func7(setValue._2.asInstanceOf[I7])

        }
      }
  }

  implicit class typeMappingImplicit8Poly[I, I1, I2, I3, I4, I5, I6, I7, I8](val mapping: TypeMapping[I, (I1, I2, I3, I4, I5, I6, I7, I8)])
      extends AnyVal {
    def ops[T](
      func1: I1 => T,
      func2: I2 => T,
      func3: I3 => T,
      func4: I4 => T,
      func5: I5 => T,
      func6: I6 => T,
      func7: I7 => T,
      func8: I8 => T
    ): I => T = i => {
      val setValue = mapping.setValue(i)
      setValue._1 match {

        case 1 => func1(setValue._2.asInstanceOf[I1])

        case 2 => func2(setValue._2.asInstanceOf[I2])

        case 3 => func3(setValue._2.asInstanceOf[I3])

        case 4 => func4(setValue._2.asInstanceOf[I4])

        case 5 => func5(setValue._2.asInstanceOf[I5])

        case 6 => func6(setValue._2.asInstanceOf[I6])

        case 7 => func7(setValue._2.asInstanceOf[I7])

        case 8 => func8(setValue._2.asInstanceOf[I8])

      }
    }
  }

  implicit class typeMappingImplicit9Poly[I, I1, I2, I3, I4, I5, I6, I7, I8, I9](
    val mapping: TypeMapping[I, (I1, I2, I3, I4, I5, I6, I7, I8, I9)]
  ) extends AnyVal {
    def ops[T](
      func1: I1 => T,
      func2: I2 => T,
      func3: I3 => T,
      func4: I4 => T,
      func5: I5 => T,
      func6: I6 => T,
      func7: I7 => T,
      func8: I8 => T,
      func9: I9 => T
    ): I => T = i => {
      val setValue = mapping.setValue(i)
      setValue._1 match {

        case 1 => func1(setValue._2.asInstanceOf[I1])

        case 2 => func2(setValue._2.asInstanceOf[I2])

        case 3 => func3(setValue._2.asInstanceOf[I3])

        case 4 => func4(setValue._2.asInstanceOf[I4])

        case 5 => func5(setValue._2.asInstanceOf[I5])

        case 6 => func6(setValue._2.asInstanceOf[I6])

        case 7 => func7(setValue._2.asInstanceOf[I7])

        case 8 => func8(setValue._2.asInstanceOf[I8])

        case 9 => func9(setValue._2.asInstanceOf[I9])

      }
    }
  }

  implicit class typeMappingImplicit10Poly[I, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10](
    val mapping: TypeMapping[I, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10)]
  ) extends AnyVal {
    def ops[T](
      func1: I1 => T,
      func2: I2 => T,
      func3: I3 => T,
      func4: I4 => T,
      func5: I5 => T,
      func6: I6 => T,
      func7: I7 => T,
      func8: I8 => T,
      func9: I9 => T,
      func10: I10 => T
    ): I => T = i => {
      val setValue = mapping.setValue(i)
      setValue._1 match {

        case 1 => func1(setValue._2.asInstanceOf[I1])

        case 2 => func2(setValue._2.asInstanceOf[I2])

        case 3 => func3(setValue._2.asInstanceOf[I3])

        case 4 => func4(setValue._2.asInstanceOf[I4])

        case 5 => func5(setValue._2.asInstanceOf[I5])

        case 6 => func6(setValue._2.asInstanceOf[I6])

        case 7 => func7(setValue._2.asInstanceOf[I7])

        case 8 => func8(setValue._2.asInstanceOf[I8])

        case 9 => func9(setValue._2.asInstanceOf[I9])

        case 10 => func10(setValue._2.asInstanceOf[I10])

      }
    }
  }

  implicit class typeMappingImplicit11Poly[I, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11](
    val mapping: TypeMapping[I, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11)]
  ) extends AnyVal {
    def ops[T](
      func1: I1 => T,
      func2: I2 => T,
      func3: I3 => T,
      func4: I4 => T,
      func5: I5 => T,
      func6: I6 => T,
      func7: I7 => T,
      func8: I8 => T,
      func9: I9 => T,
      func10: I10 => T,
      func11: I11 => T
    ): I => T = i => {
      val setValue = mapping.setValue(i)
      setValue._1 match {

        case 1 => func1(setValue._2.asInstanceOf[I1])

        case 2 => func2(setValue._2.asInstanceOf[I2])

        case 3 => func3(setValue._2.asInstanceOf[I3])

        case 4 => func4(setValue._2.asInstanceOf[I4])

        case 5 => func5(setValue._2.asInstanceOf[I5])

        case 6 => func6(setValue._2.asInstanceOf[I6])

        case 7 => func7(setValue._2.asInstanceOf[I7])

        case 8 => func8(setValue._2.asInstanceOf[I8])

        case 9 => func9(setValue._2.asInstanceOf[I9])

        case 10 => func10(setValue._2.asInstanceOf[I10])

        case 11 => func11(setValue._2.asInstanceOf[I11])

      }
    }
  }

  implicit class typeMappingImplicit12Poly[I, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12](
    val mapping: TypeMapping[I, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12)]
  ) extends AnyVal {
    def ops[T](
      func1: I1 => T,
      func2: I2 => T,
      func3: I3 => T,
      func4: I4 => T,
      func5: I5 => T,
      func6: I6 => T,
      func7: I7 => T,
      func8: I8 => T,
      func9: I9 => T,
      func10: I10 => T,
      func11: I11 => T,
      func12: I12 => T
    ): I => T = i => {
      val setValue = mapping.setValue(i)
      setValue._1 match {

        case 1 => func1(setValue._2.asInstanceOf[I1])

        case 2 => func2(setValue._2.asInstanceOf[I2])

        case 3 => func3(setValue._2.asInstanceOf[I3])

        case 4 => func4(setValue._2.asInstanceOf[I4])

        case 5 => func5(setValue._2.asInstanceOf[I5])

        case 6 => func6(setValue._2.asInstanceOf[I6])

        case 7 => func7(setValue._2.asInstanceOf[I7])

        case 8 => func8(setValue._2.asInstanceOf[I8])

        case 9 => func9(setValue._2.asInstanceOf[I9])

        case 10 => func10(setValue._2.asInstanceOf[I10])

        case 11 => func11(setValue._2.asInstanceOf[I11])

        case 12 => func12(setValue._2.asInstanceOf[I12])

      }
    }
  }

  implicit class typeMappingImplicit13Poly[I, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13](
    val mapping: TypeMapping[I, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13)]
  ) extends AnyVal {
    def ops[T](
      func1: I1 => T,
      func2: I2 => T,
      func3: I3 => T,
      func4: I4 => T,
      func5: I5 => T,
      func6: I6 => T,
      func7: I7 => T,
      func8: I8 => T,
      func9: I9 => T,
      func10: I10 => T,
      func11: I11 => T,
      func12: I12 => T,
      func13: I13 => T
    ): I => T = i => {
      val setValue = mapping.setValue(i)
      setValue._1 match {

        case 1 => func1(setValue._2.asInstanceOf[I1])

        case 2 => func2(setValue._2.asInstanceOf[I2])

        case 3 => func3(setValue._2.asInstanceOf[I3])

        case 4 => func4(setValue._2.asInstanceOf[I4])

        case 5 => func5(setValue._2.asInstanceOf[I5])

        case 6 => func6(setValue._2.asInstanceOf[I6])

        case 7 => func7(setValue._2.asInstanceOf[I7])

        case 8 => func8(setValue._2.asInstanceOf[I8])

        case 9 => func9(setValue._2.asInstanceOf[I9])

        case 10 => func10(setValue._2.asInstanceOf[I10])

        case 11 => func11(setValue._2.asInstanceOf[I11])

        case 12 => func12(setValue._2.asInstanceOf[I12])

        case 13 => func13(setValue._2.asInstanceOf[I13])

      }
    }
  }

  implicit class typeMappingImplicit14Poly[I, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14](
    val mapping: TypeMapping[I, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14)]
  ) extends AnyVal {
    def ops[T](
      func1: I1 => T,
      func2: I2 => T,
      func3: I3 => T,
      func4: I4 => T,
      func5: I5 => T,
      func6: I6 => T,
      func7: I7 => T,
      func8: I8 => T,
      func9: I9 => T,
      func10: I10 => T,
      func11: I11 => T,
      func12: I12 => T,
      func13: I13 => T,
      func14: I14 => T
    ): I => T = i => {
      val setValue = mapping.setValue(i)
      setValue._1 match {

        case 1 => func1(setValue._2.asInstanceOf[I1])

        case 2 => func2(setValue._2.asInstanceOf[I2])

        case 3 => func3(setValue._2.asInstanceOf[I3])

        case 4 => func4(setValue._2.asInstanceOf[I4])

        case 5 => func5(setValue._2.asInstanceOf[I5])

        case 6 => func6(setValue._2.asInstanceOf[I6])

        case 7 => func7(setValue._2.asInstanceOf[I7])

        case 8 => func8(setValue._2.asInstanceOf[I8])

        case 9 => func9(setValue._2.asInstanceOf[I9])

        case 10 => func10(setValue._2.asInstanceOf[I10])

        case 11 => func11(setValue._2.asInstanceOf[I11])

        case 12 => func12(setValue._2.asInstanceOf[I12])

        case 13 => func13(setValue._2.asInstanceOf[I13])

        case 14 => func14(setValue._2.asInstanceOf[I14])

      }
    }
  }

  implicit class typeMappingImplicit15Poly[I, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15](
    val mapping: TypeMapping[I, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15)]
  ) extends AnyVal {
    def ops[T](
      func1: I1 => T,
      func2: I2 => T,
      func3: I3 => T,
      func4: I4 => T,
      func5: I5 => T,
      func6: I6 => T,
      func7: I7 => T,
      func8: I8 => T,
      func9: I9 => T,
      func10: I10 => T,
      func11: I11 => T,
      func12: I12 => T,
      func13: I13 => T,
      func14: I14 => T,
      func15: I15 => T
    ): I => T = i => {
      val setValue = mapping.setValue(i)
      setValue._1 match {

        case 1 => func1(setValue._2.asInstanceOf[I1])

        case 2 => func2(setValue._2.asInstanceOf[I2])

        case 3 => func3(setValue._2.asInstanceOf[I3])

        case 4 => func4(setValue._2.asInstanceOf[I4])

        case 5 => func5(setValue._2.asInstanceOf[I5])

        case 6 => func6(setValue._2.asInstanceOf[I6])

        case 7 => func7(setValue._2.asInstanceOf[I7])

        case 8 => func8(setValue._2.asInstanceOf[I8])

        case 9 => func9(setValue._2.asInstanceOf[I9])

        case 10 => func10(setValue._2.asInstanceOf[I10])

        case 11 => func11(setValue._2.asInstanceOf[I11])

        case 12 => func12(setValue._2.asInstanceOf[I12])

        case 13 => func13(setValue._2.asInstanceOf[I13])

        case 14 => func14(setValue._2.asInstanceOf[I14])

        case 15 => func15(setValue._2.asInstanceOf[I15])

      }
    }
  }

  implicit class typeMappingImplicit16Poly[I, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16](
    val mapping: TypeMapping[I, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16)]
  ) extends AnyVal {
    def ops[T](
      func1: I1 => T,
      func2: I2 => T,
      func3: I3 => T,
      func4: I4 => T,
      func5: I5 => T,
      func6: I6 => T,
      func7: I7 => T,
      func8: I8 => T,
      func9: I9 => T,
      func10: I10 => T,
      func11: I11 => T,
      func12: I12 => T,
      func13: I13 => T,
      func14: I14 => T,
      func15: I15 => T,
      func16: I16 => T
    ): I => T = i => {
      val setValue = mapping.setValue(i)
      setValue._1 match {

        case 1 => func1(setValue._2.asInstanceOf[I1])

        case 2 => func2(setValue._2.asInstanceOf[I2])

        case 3 => func3(setValue._2.asInstanceOf[I3])

        case 4 => func4(setValue._2.asInstanceOf[I4])

        case 5 => func5(setValue._2.asInstanceOf[I5])

        case 6 => func6(setValue._2.asInstanceOf[I6])

        case 7 => func7(setValue._2.asInstanceOf[I7])

        case 8 => func8(setValue._2.asInstanceOf[I8])

        case 9 => func9(setValue._2.asInstanceOf[I9])

        case 10 => func10(setValue._2.asInstanceOf[I10])

        case 11 => func11(setValue._2.asInstanceOf[I11])

        case 12 => func12(setValue._2.asInstanceOf[I12])

        case 13 => func13(setValue._2.asInstanceOf[I13])

        case 14 => func14(setValue._2.asInstanceOf[I14])

        case 15 => func15(setValue._2.asInstanceOf[I15])

        case 16 => func16(setValue._2.asInstanceOf[I16])

      }
    }
  }

  implicit class typeMappingImplicit17Poly[I, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17](
    val mapping: TypeMapping[I, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17)]
  ) extends AnyVal {
    def ops[T](
      func1: I1 => T,
      func2: I2 => T,
      func3: I3 => T,
      func4: I4 => T,
      func5: I5 => T,
      func6: I6 => T,
      func7: I7 => T,
      func8: I8 => T,
      func9: I9 => T,
      func10: I10 => T,
      func11: I11 => T,
      func12: I12 => T,
      func13: I13 => T,
      func14: I14 => T,
      func15: I15 => T,
      func16: I16 => T,
      func17: I17 => T
    ): I => T = i => {
      val setValue = mapping.setValue(i)
      setValue._1 match {

        case 1 => func1(setValue._2.asInstanceOf[I1])

        case 2 => func2(setValue._2.asInstanceOf[I2])

        case 3 => func3(setValue._2.asInstanceOf[I3])

        case 4 => func4(setValue._2.asInstanceOf[I4])

        case 5 => func5(setValue._2.asInstanceOf[I5])

        case 6 => func6(setValue._2.asInstanceOf[I6])

        case 7 => func7(setValue._2.asInstanceOf[I7])

        case 8 => func8(setValue._2.asInstanceOf[I8])

        case 9 => func9(setValue._2.asInstanceOf[I9])

        case 10 => func10(setValue._2.asInstanceOf[I10])

        case 11 => func11(setValue._2.asInstanceOf[I11])

        case 12 => func12(setValue._2.asInstanceOf[I12])

        case 13 => func13(setValue._2.asInstanceOf[I13])

        case 14 => func14(setValue._2.asInstanceOf[I14])

        case 15 => func15(setValue._2.asInstanceOf[I15])

        case 16 => func16(setValue._2.asInstanceOf[I16])

        case 17 => func17(setValue._2.asInstanceOf[I17])

      }
    }
  }

  implicit class typeMappingImplicit18Poly[I, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18](
    val mapping: TypeMapping[I, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18)]
  ) extends AnyVal {
    def ops[T](
      func1: I1 => T,
      func2: I2 => T,
      func3: I3 => T,
      func4: I4 => T,
      func5: I5 => T,
      func6: I6 => T,
      func7: I7 => T,
      func8: I8 => T,
      func9: I9 => T,
      func10: I10 => T,
      func11: I11 => T,
      func12: I12 => T,
      func13: I13 => T,
      func14: I14 => T,
      func15: I15 => T,
      func16: I16 => T,
      func17: I17 => T,
      func18: I18 => T
    ): I => T = i => {
      val setValue = mapping.setValue(i)
      setValue._1 match {

        case 1 => func1(setValue._2.asInstanceOf[I1])

        case 2 => func2(setValue._2.asInstanceOf[I2])

        case 3 => func3(setValue._2.asInstanceOf[I3])

        case 4 => func4(setValue._2.asInstanceOf[I4])

        case 5 => func5(setValue._2.asInstanceOf[I5])

        case 6 => func6(setValue._2.asInstanceOf[I6])

        case 7 => func7(setValue._2.asInstanceOf[I7])

        case 8 => func8(setValue._2.asInstanceOf[I8])

        case 9 => func9(setValue._2.asInstanceOf[I9])

        case 10 => func10(setValue._2.asInstanceOf[I10])

        case 11 => func11(setValue._2.asInstanceOf[I11])

        case 12 => func12(setValue._2.asInstanceOf[I12])

        case 13 => func13(setValue._2.asInstanceOf[I13])

        case 14 => func14(setValue._2.asInstanceOf[I14])

        case 15 => func15(setValue._2.asInstanceOf[I15])

        case 16 => func16(setValue._2.asInstanceOf[I16])

        case 17 => func17(setValue._2.asInstanceOf[I17])

        case 18 => func18(setValue._2.asInstanceOf[I18])

      }
    }
  }

  implicit class typeMappingImplicit19Poly[I, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19](
    val mapping: TypeMapping[I, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19)]
  ) extends AnyVal {
    def ops[T](
      func1: I1 => T,
      func2: I2 => T,
      func3: I3 => T,
      func4: I4 => T,
      func5: I5 => T,
      func6: I6 => T,
      func7: I7 => T,
      func8: I8 => T,
      func9: I9 => T,
      func10: I10 => T,
      func11: I11 => T,
      func12: I12 => T,
      func13: I13 => T,
      func14: I14 => T,
      func15: I15 => T,
      func16: I16 => T,
      func17: I17 => T,
      func18: I18 => T,
      func19: I19 => T
    ): I => T = i => {
      val setValue = mapping.setValue(i)
      setValue._1 match {

        case 1 => func1(setValue._2.asInstanceOf[I1])

        case 2 => func2(setValue._2.asInstanceOf[I2])

        case 3 => func3(setValue._2.asInstanceOf[I3])

        case 4 => func4(setValue._2.asInstanceOf[I4])

        case 5 => func5(setValue._2.asInstanceOf[I5])

        case 6 => func6(setValue._2.asInstanceOf[I6])

        case 7 => func7(setValue._2.asInstanceOf[I7])

        case 8 => func8(setValue._2.asInstanceOf[I8])

        case 9 => func9(setValue._2.asInstanceOf[I9])

        case 10 => func10(setValue._2.asInstanceOf[I10])

        case 11 => func11(setValue._2.asInstanceOf[I11])

        case 12 => func12(setValue._2.asInstanceOf[I12])

        case 13 => func13(setValue._2.asInstanceOf[I13])

        case 14 => func14(setValue._2.asInstanceOf[I14])

        case 15 => func15(setValue._2.asInstanceOf[I15])

        case 16 => func16(setValue._2.asInstanceOf[I16])

        case 17 => func17(setValue._2.asInstanceOf[I17])

        case 18 => func18(setValue._2.asInstanceOf[I18])

        case 19 => func19(setValue._2.asInstanceOf[I19])

      }
    }
  }

  implicit class typeMappingImplicit20Poly[I, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19, I20](
    val mapping: TypeMapping[I, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19, I20)]
  ) extends AnyVal {
    def ops[T](
      func1: I1 => T,
      func2: I2 => T,
      func3: I3 => T,
      func4: I4 => T,
      func5: I5 => T,
      func6: I6 => T,
      func7: I7 => T,
      func8: I8 => T,
      func9: I9 => T,
      func10: I10 => T,
      func11: I11 => T,
      func12: I12 => T,
      func13: I13 => T,
      func14: I14 => T,
      func15: I15 => T,
      func16: I16 => T,
      func17: I17 => T,
      func18: I18 => T,
      func19: I19 => T,
      func20: I20 => T
    ): I => T = i => {
      val setValue = mapping.setValue(i)
      setValue._1 match {

        case 1 => func1(setValue._2.asInstanceOf[I1])

        case 2 => func2(setValue._2.asInstanceOf[I2])

        case 3 => func3(setValue._2.asInstanceOf[I3])

        case 4 => func4(setValue._2.asInstanceOf[I4])

        case 5 => func5(setValue._2.asInstanceOf[I5])

        case 6 => func6(setValue._2.asInstanceOf[I6])

        case 7 => func7(setValue._2.asInstanceOf[I7])

        case 8 => func8(setValue._2.asInstanceOf[I8])

        case 9 => func9(setValue._2.asInstanceOf[I9])

        case 10 => func10(setValue._2.asInstanceOf[I10])

        case 11 => func11(setValue._2.asInstanceOf[I11])

        case 12 => func12(setValue._2.asInstanceOf[I12])

        case 13 => func13(setValue._2.asInstanceOf[I13])

        case 14 => func14(setValue._2.asInstanceOf[I14])

        case 15 => func15(setValue._2.asInstanceOf[I15])

        case 16 => func16(setValue._2.asInstanceOf[I16])

        case 17 => func17(setValue._2.asInstanceOf[I17])

        case 18 => func18(setValue._2.asInstanceOf[I18])

        case 19 => func19(setValue._2.asInstanceOf[I19])

        case 20 => func20(setValue._2.asInstanceOf[I20])

      }
    }
  }

  implicit class typeMappingImplicit21Poly[
    I,
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
  ](val mapping: TypeMapping[I, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19, I20, I21)])
      extends AnyVal {
    def ops[T](
      func1: I1 => T,
      func2: I2 => T,
      func3: I3 => T,
      func4: I4 => T,
      func5: I5 => T,
      func6: I6 => T,
      func7: I7 => T,
      func8: I8 => T,
      func9: I9 => T,
      func10: I10 => T,
      func11: I11 => T,
      func12: I12 => T,
      func13: I13 => T,
      func14: I14 => T,
      func15: I15 => T,
      func16: I16 => T,
      func17: I17 => T,
      func18: I18 => T,
      func19: I19 => T,
      func20: I20 => T,
      func21: I21 => T
    ): I => T = i => {
      val setValue = mapping.setValue(i)
      setValue._1 match {

        case 1 => func1(setValue._2.asInstanceOf[I1])

        case 2 => func2(setValue._2.asInstanceOf[I2])

        case 3 => func3(setValue._2.asInstanceOf[I3])

        case 4 => func4(setValue._2.asInstanceOf[I4])

        case 5 => func5(setValue._2.asInstanceOf[I5])

        case 6 => func6(setValue._2.asInstanceOf[I6])

        case 7 => func7(setValue._2.asInstanceOf[I7])

        case 8 => func8(setValue._2.asInstanceOf[I8])

        case 9 => func9(setValue._2.asInstanceOf[I9])

        case 10 => func10(setValue._2.asInstanceOf[I10])

        case 11 => func11(setValue._2.asInstanceOf[I11])

        case 12 => func12(setValue._2.asInstanceOf[I12])

        case 13 => func13(setValue._2.asInstanceOf[I13])

        case 14 => func14(setValue._2.asInstanceOf[I14])

        case 15 => func15(setValue._2.asInstanceOf[I15])

        case 16 => func16(setValue._2.asInstanceOf[I16])

        case 17 => func17(setValue._2.asInstanceOf[I17])

        case 18 => func18(setValue._2.asInstanceOf[I18])

        case 19 => func19(setValue._2.asInstanceOf[I19])

        case 20 => func20(setValue._2.asInstanceOf[I20])

        case 21 => func21(setValue._2.asInstanceOf[I21])

      }
    }
  }

  implicit class typeMappingImplicit22Poly[
    I,
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
  ](val mapping: TypeMapping[I, (I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19, I20, I21, I22)])
      extends AnyVal {
    def ops[T](
      func1: I1 => T,
      func2: I2 => T,
      func3: I3 => T,
      func4: I4 => T,
      func5: I5 => T,
      func6: I6 => T,
      func7: I7 => T,
      func8: I8 => T,
      func9: I9 => T,
      func10: I10 => T,
      func11: I11 => T,
      func12: I12 => T,
      func13: I13 => T,
      func14: I14 => T,
      func15: I15 => T,
      func16: I16 => T,
      func17: I17 => T,
      func18: I18 => T,
      func19: I19 => T,
      func20: I20 => T,
      func21: I21 => T,
      func22: I22 => T
    ): I => T = i => {
      val setValue = mapping.setValue(i)
      setValue._1 match {

        case 1 => func1(setValue._2.asInstanceOf[I1])

        case 2 => func2(setValue._2.asInstanceOf[I2])

        case 3 => func3(setValue._2.asInstanceOf[I3])

        case 4 => func4(setValue._2.asInstanceOf[I4])

        case 5 => func5(setValue._2.asInstanceOf[I5])

        case 6 => func6(setValue._2.asInstanceOf[I6])

        case 7 => func7(setValue._2.asInstanceOf[I7])

        case 8 => func8(setValue._2.asInstanceOf[I8])

        case 9 => func9(setValue._2.asInstanceOf[I9])

        case 10 => func10(setValue._2.asInstanceOf[I10])

        case 11 => func11(setValue._2.asInstanceOf[I11])

        case 12 => func12(setValue._2.asInstanceOf[I12])

        case 13 => func13(setValue._2.asInstanceOf[I13])

        case 14 => func14(setValue._2.asInstanceOf[I14])

        case 15 => func15(setValue._2.asInstanceOf[I15])

        case 16 => func16(setValue._2.asInstanceOf[I16])

        case 17 => func17(setValue._2.asInstanceOf[I17])

        case 18 => func18(setValue._2.asInstanceOf[I18])

        case 19 => func19(setValue._2.asInstanceOf[I19])

        case 20 => func20(setValue._2.asInstanceOf[I20])

        case 21 => func21(setValue._2.asInstanceOf[I21])

        case 22 => func22(setValue._2.asInstanceOf[I22])

      }
    }
  }

}

trait TypeMappingInnerHelperPoly {
  implicit class typeMappingImplicit1Poly[I, I1](mapping: TypeMapping[I, I1]) {
    def ops: I => I1 = (i: I) => i.asInstanceOf
  }
}
