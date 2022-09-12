package commons.lang3.bridge.impl

object InnerTypeMappingClass {

  class CusInnerApply2[I1, I2](index: Int, value: Any) {
    @inline def fold[T](func1: I1 => T, func2: I2 => T): T = index match {
      case 1 => func1(value.asInstanceOf[I1])
      case 2 => func2(value.asInstanceOf[I2])
    }
  }

  class CusInnerApply3[I1, I2, I3](index: Int, value: Any) {
    @inline def fold[T](func1: I1 => T, func2: I2 => T, func3: I3 => T): T = index match {
      case 1 => func1(value.asInstanceOf[I1])
      case 2 => func2(value.asInstanceOf[I2])
      case 3 => func3(value.asInstanceOf[I3])
    }
  }

  class CusInnerApply4[I1, I2, I3, I4](index: Int, value: Any) {
    @inline def fold[T](func1: I1 => T, func2: I2 => T, func3: I3 => T, func4: I4 => T): T = index match {
      case 1 => func1(value.asInstanceOf[I1])
      case 2 => func2(value.asInstanceOf[I2])
      case 3 => func3(value.asInstanceOf[I3])
      case 4 => func4(value.asInstanceOf[I4])
    }
  }

  class CusInnerApply5[I1, I2, I3, I4, I5](index: Int, value: Any) {
    @inline def fold[T](func1: I1 => T, func2: I2 => T, func3: I3 => T, func4: I4 => T, func5: I5 => T): T = index match {
      case 1 => func1(value.asInstanceOf[I1])
      case 2 => func2(value.asInstanceOf[I2])
      case 3 => func3(value.asInstanceOf[I3])
      case 4 => func4(value.asInstanceOf[I4])
      case 5 => func5(value.asInstanceOf[I5])
    }
  }

  class CusInnerApply6[I1, I2, I3, I4, I5, I6](index: Int, value: Any) {
    @inline def fold[T](func1: I1 => T, func2: I2 => T, func3: I3 => T, func4: I4 => T, func5: I5 => T, func6: I6 => T): T = index match {
      case 1 => func1(value.asInstanceOf[I1])
      case 2 => func2(value.asInstanceOf[I2])
      case 3 => func3(value.asInstanceOf[I3])
      case 4 => func4(value.asInstanceOf[I4])
      case 5 => func5(value.asInstanceOf[I5])
      case 6 => func6(value.asInstanceOf[I6])
    }
  }

  class CusInnerApply7[I1, I2, I3, I4, I5, I6, I7](index: Int, value: Any) {
    @inline def fold[T](func1: I1 => T, func2: I2 => T, func3: I3 => T, func4: I4 => T, func5: I5 => T, func6: I6 => T, func7: I7 => T): T =
      index match {
        case 1 => func1(value.asInstanceOf[I1])
        case 2 => func2(value.asInstanceOf[I2])
        case 3 => func3(value.asInstanceOf[I3])
        case 4 => func4(value.asInstanceOf[I4])
        case 5 => func5(value.asInstanceOf[I5])
        case 6 => func6(value.asInstanceOf[I6])
        case 7 => func7(value.asInstanceOf[I7])
      }
  }

  class CusInnerApply8[I1, I2, I3, I4, I5, I6, I7, I8](index: Int, value: Any) {
    @inline def fold[T](
      func1: I1 => T,
      func2: I2 => T,
      func3: I3 => T,
      func4: I4 => T,
      func5: I5 => T,
      func6: I6 => T,
      func7: I7 => T,
      func8: I8 => T
    ): T = index match {
      case 1 => func1(value.asInstanceOf[I1])
      case 2 => func2(value.asInstanceOf[I2])
      case 3 => func3(value.asInstanceOf[I3])
      case 4 => func4(value.asInstanceOf[I4])
      case 5 => func5(value.asInstanceOf[I5])
      case 6 => func6(value.asInstanceOf[I6])
      case 7 => func7(value.asInstanceOf[I7])
      case 8 => func8(value.asInstanceOf[I8])
    }
  }

  class CusInnerApply9[I1, I2, I3, I4, I5, I6, I7, I8, I9](index: Int, value: Any) {
    @inline def fold[T](
      func1: I1 => T,
      func2: I2 => T,
      func3: I3 => T,
      func4: I4 => T,
      func5: I5 => T,
      func6: I6 => T,
      func7: I7 => T,
      func8: I8 => T,
      func9: I9 => T
    ): T = index match {
      case 1 => func1(value.asInstanceOf[I1])
      case 2 => func2(value.asInstanceOf[I2])
      case 3 => func3(value.asInstanceOf[I3])
      case 4 => func4(value.asInstanceOf[I4])
      case 5 => func5(value.asInstanceOf[I5])
      case 6 => func6(value.asInstanceOf[I6])
      case 7 => func7(value.asInstanceOf[I7])
      case 8 => func8(value.asInstanceOf[I8])
      case 9 => func9(value.asInstanceOf[I9])
    }
  }

  class CusInnerApply10[I1, I2, I3, I4, I5, I6, I7, I8, I9, I10](index: Int, value: Any) {
    @inline def fold[T](
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
    ): T = index match {
      case 1  => func1(value.asInstanceOf[I1])
      case 2  => func2(value.asInstanceOf[I2])
      case 3  => func3(value.asInstanceOf[I3])
      case 4  => func4(value.asInstanceOf[I4])
      case 5  => func5(value.asInstanceOf[I5])
      case 6  => func6(value.asInstanceOf[I6])
      case 7  => func7(value.asInstanceOf[I7])
      case 8  => func8(value.asInstanceOf[I8])
      case 9  => func9(value.asInstanceOf[I9])
      case 10 => func10(value.asInstanceOf[I10])
    }
  }

  class CusInnerApply11[I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11](index: Int, value: Any) {
    @inline def fold[T](
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
    ): T = index match {
      case 1  => func1(value.asInstanceOf[I1])
      case 2  => func2(value.asInstanceOf[I2])
      case 3  => func3(value.asInstanceOf[I3])
      case 4  => func4(value.asInstanceOf[I4])
      case 5  => func5(value.asInstanceOf[I5])
      case 6  => func6(value.asInstanceOf[I6])
      case 7  => func7(value.asInstanceOf[I7])
      case 8  => func8(value.asInstanceOf[I8])
      case 9  => func9(value.asInstanceOf[I9])
      case 10 => func10(value.asInstanceOf[I10])
      case 11 => func11(value.asInstanceOf[I11])
    }
  }

  class CusInnerApply12[I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12](index: Int, value: Any) {
    @inline def fold[T](
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
    ): T = index match {
      case 1  => func1(value.asInstanceOf[I1])
      case 2  => func2(value.asInstanceOf[I2])
      case 3  => func3(value.asInstanceOf[I3])
      case 4  => func4(value.asInstanceOf[I4])
      case 5  => func5(value.asInstanceOf[I5])
      case 6  => func6(value.asInstanceOf[I6])
      case 7  => func7(value.asInstanceOf[I7])
      case 8  => func8(value.asInstanceOf[I8])
      case 9  => func9(value.asInstanceOf[I9])
      case 10 => func10(value.asInstanceOf[I10])
      case 11 => func11(value.asInstanceOf[I11])
      case 12 => func12(value.asInstanceOf[I12])
    }
  }

  class CusInnerApply13[I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13](index: Int, value: Any) {
    @inline def fold[T](
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
    ): T = index match {
      case 1  => func1(value.asInstanceOf[I1])
      case 2  => func2(value.asInstanceOf[I2])
      case 3  => func3(value.asInstanceOf[I3])
      case 4  => func4(value.asInstanceOf[I4])
      case 5  => func5(value.asInstanceOf[I5])
      case 6  => func6(value.asInstanceOf[I6])
      case 7  => func7(value.asInstanceOf[I7])
      case 8  => func8(value.asInstanceOf[I8])
      case 9  => func9(value.asInstanceOf[I9])
      case 10 => func10(value.asInstanceOf[I10])
      case 11 => func11(value.asInstanceOf[I11])
      case 12 => func12(value.asInstanceOf[I12])
      case 13 => func13(value.asInstanceOf[I13])
    }
  }

  class CusInnerApply14[I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14](index: Int, value: Any) {
    @inline def fold[T](
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
    ): T = index match {
      case 1  => func1(value.asInstanceOf[I1])
      case 2  => func2(value.asInstanceOf[I2])
      case 3  => func3(value.asInstanceOf[I3])
      case 4  => func4(value.asInstanceOf[I4])
      case 5  => func5(value.asInstanceOf[I5])
      case 6  => func6(value.asInstanceOf[I6])
      case 7  => func7(value.asInstanceOf[I7])
      case 8  => func8(value.asInstanceOf[I8])
      case 9  => func9(value.asInstanceOf[I9])
      case 10 => func10(value.asInstanceOf[I10])
      case 11 => func11(value.asInstanceOf[I11])
      case 12 => func12(value.asInstanceOf[I12])
      case 13 => func13(value.asInstanceOf[I13])
      case 14 => func14(value.asInstanceOf[I14])
    }
  }

  class CusInnerApply15[I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15](index: Int, value: Any) {
    @inline def fold[T](
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
    ): T = index match {
      case 1  => func1(value.asInstanceOf[I1])
      case 2  => func2(value.asInstanceOf[I2])
      case 3  => func3(value.asInstanceOf[I3])
      case 4  => func4(value.asInstanceOf[I4])
      case 5  => func5(value.asInstanceOf[I5])
      case 6  => func6(value.asInstanceOf[I6])
      case 7  => func7(value.asInstanceOf[I7])
      case 8  => func8(value.asInstanceOf[I8])
      case 9  => func9(value.asInstanceOf[I9])
      case 10 => func10(value.asInstanceOf[I10])
      case 11 => func11(value.asInstanceOf[I11])
      case 12 => func12(value.asInstanceOf[I12])
      case 13 => func13(value.asInstanceOf[I13])
      case 14 => func14(value.asInstanceOf[I14])
      case 15 => func15(value.asInstanceOf[I15])
    }
  }

  class CusInnerApply16[I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16](index: Int, value: Any) {
    @inline def fold[T](
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
    ): T = index match {
      case 1  => func1(value.asInstanceOf[I1])
      case 2  => func2(value.asInstanceOf[I2])
      case 3  => func3(value.asInstanceOf[I3])
      case 4  => func4(value.asInstanceOf[I4])
      case 5  => func5(value.asInstanceOf[I5])
      case 6  => func6(value.asInstanceOf[I6])
      case 7  => func7(value.asInstanceOf[I7])
      case 8  => func8(value.asInstanceOf[I8])
      case 9  => func9(value.asInstanceOf[I9])
      case 10 => func10(value.asInstanceOf[I10])
      case 11 => func11(value.asInstanceOf[I11])
      case 12 => func12(value.asInstanceOf[I12])
      case 13 => func13(value.asInstanceOf[I13])
      case 14 => func14(value.asInstanceOf[I14])
      case 15 => func15(value.asInstanceOf[I15])
      case 16 => func16(value.asInstanceOf[I16])
    }
  }

  class CusInnerApply17[I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17](index: Int, value: Any) {
    @inline def fold[T](
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
    ): T = index match {
      case 1  => func1(value.asInstanceOf[I1])
      case 2  => func2(value.asInstanceOf[I2])
      case 3  => func3(value.asInstanceOf[I3])
      case 4  => func4(value.asInstanceOf[I4])
      case 5  => func5(value.asInstanceOf[I5])
      case 6  => func6(value.asInstanceOf[I6])
      case 7  => func7(value.asInstanceOf[I7])
      case 8  => func8(value.asInstanceOf[I8])
      case 9  => func9(value.asInstanceOf[I9])
      case 10 => func10(value.asInstanceOf[I10])
      case 11 => func11(value.asInstanceOf[I11])
      case 12 => func12(value.asInstanceOf[I12])
      case 13 => func13(value.asInstanceOf[I13])
      case 14 => func14(value.asInstanceOf[I14])
      case 15 => func15(value.asInstanceOf[I15])
      case 16 => func16(value.asInstanceOf[I16])
      case 17 => func17(value.asInstanceOf[I17])
    }
  }

  class CusInnerApply18[I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18](index: Int, value: Any) {
    @inline def fold[T](
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
    ): T = index match {
      case 1  => func1(value.asInstanceOf[I1])
      case 2  => func2(value.asInstanceOf[I2])
      case 3  => func3(value.asInstanceOf[I3])
      case 4  => func4(value.asInstanceOf[I4])
      case 5  => func5(value.asInstanceOf[I5])
      case 6  => func6(value.asInstanceOf[I6])
      case 7  => func7(value.asInstanceOf[I7])
      case 8  => func8(value.asInstanceOf[I8])
      case 9  => func9(value.asInstanceOf[I9])
      case 10 => func10(value.asInstanceOf[I10])
      case 11 => func11(value.asInstanceOf[I11])
      case 12 => func12(value.asInstanceOf[I12])
      case 13 => func13(value.asInstanceOf[I13])
      case 14 => func14(value.asInstanceOf[I14])
      case 15 => func15(value.asInstanceOf[I15])
      case 16 => func16(value.asInstanceOf[I16])
      case 17 => func17(value.asInstanceOf[I17])
      case 18 => func18(value.asInstanceOf[I18])
    }
  }

  class CusInnerApply19[I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19](index: Int, value: Any) {
    @inline def fold[T](
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
    ): T = index match {
      case 1  => func1(value.asInstanceOf[I1])
      case 2  => func2(value.asInstanceOf[I2])
      case 3  => func3(value.asInstanceOf[I3])
      case 4  => func4(value.asInstanceOf[I4])
      case 5  => func5(value.asInstanceOf[I5])
      case 6  => func6(value.asInstanceOf[I6])
      case 7  => func7(value.asInstanceOf[I7])
      case 8  => func8(value.asInstanceOf[I8])
      case 9  => func9(value.asInstanceOf[I9])
      case 10 => func10(value.asInstanceOf[I10])
      case 11 => func11(value.asInstanceOf[I11])
      case 12 => func12(value.asInstanceOf[I12])
      case 13 => func13(value.asInstanceOf[I13])
      case 14 => func14(value.asInstanceOf[I14])
      case 15 => func15(value.asInstanceOf[I15])
      case 16 => func16(value.asInstanceOf[I16])
      case 17 => func17(value.asInstanceOf[I17])
      case 18 => func18(value.asInstanceOf[I18])
      case 19 => func19(value.asInstanceOf[I19])
    }
  }

  class CusInnerApply20[I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19, I20](index: Int, value: Any) {
    @inline def fold[T](
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
    ): T = index match {
      case 1  => func1(value.asInstanceOf[I1])
      case 2  => func2(value.asInstanceOf[I2])
      case 3  => func3(value.asInstanceOf[I3])
      case 4  => func4(value.asInstanceOf[I4])
      case 5  => func5(value.asInstanceOf[I5])
      case 6  => func6(value.asInstanceOf[I6])
      case 7  => func7(value.asInstanceOf[I7])
      case 8  => func8(value.asInstanceOf[I8])
      case 9  => func9(value.asInstanceOf[I9])
      case 10 => func10(value.asInstanceOf[I10])
      case 11 => func11(value.asInstanceOf[I11])
      case 12 => func12(value.asInstanceOf[I12])
      case 13 => func13(value.asInstanceOf[I13])
      case 14 => func14(value.asInstanceOf[I14])
      case 15 => func15(value.asInstanceOf[I15])
      case 16 => func16(value.asInstanceOf[I16])
      case 17 => func17(value.asInstanceOf[I17])
      case 18 => func18(value.asInstanceOf[I18])
      case 19 => func19(value.asInstanceOf[I19])
      case 20 => func20(value.asInstanceOf[I20])
    }
  }

  class CusInnerApply21[I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19, I20, I21](
    index: Int,
    value: Any
  ) {
    @inline def fold[T](
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
    ): T = index match {
      case 1  => func1(value.asInstanceOf[I1])
      case 2  => func2(value.asInstanceOf[I2])
      case 3  => func3(value.asInstanceOf[I3])
      case 4  => func4(value.asInstanceOf[I4])
      case 5  => func5(value.asInstanceOf[I5])
      case 6  => func6(value.asInstanceOf[I6])
      case 7  => func7(value.asInstanceOf[I7])
      case 8  => func8(value.asInstanceOf[I8])
      case 9  => func9(value.asInstanceOf[I9])
      case 10 => func10(value.asInstanceOf[I10])
      case 11 => func11(value.asInstanceOf[I11])
      case 12 => func12(value.asInstanceOf[I12])
      case 13 => func13(value.asInstanceOf[I13])
      case 14 => func14(value.asInstanceOf[I14])
      case 15 => func15(value.asInstanceOf[I15])
      case 16 => func16(value.asInstanceOf[I16])
      case 17 => func17(value.asInstanceOf[I17])
      case 18 => func18(value.asInstanceOf[I18])
      case 19 => func19(value.asInstanceOf[I19])
      case 20 => func20(value.asInstanceOf[I20])
      case 21 => func21(value.asInstanceOf[I21])
    }
  }

  class CusInnerApply22[I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19, I20, I21, I22](
    index: Int,
    value: Any
  ) {
    @inline def fold[T](
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
    ): T = index match {
      case 1  => func1(value.asInstanceOf[I1])
      case 2  => func2(value.asInstanceOf[I2])
      case 3  => func3(value.asInstanceOf[I3])
      case 4  => func4(value.asInstanceOf[I4])
      case 5  => func5(value.asInstanceOf[I5])
      case 6  => func6(value.asInstanceOf[I6])
      case 7  => func7(value.asInstanceOf[I7])
      case 8  => func8(value.asInstanceOf[I8])
      case 9  => func9(value.asInstanceOf[I9])
      case 10 => func10(value.asInstanceOf[I10])
      case 11 => func11(value.asInstanceOf[I11])
      case 12 => func12(value.asInstanceOf[I12])
      case 13 => func13(value.asInstanceOf[I13])
      case 14 => func14(value.asInstanceOf[I14])
      case 15 => func15(value.asInstanceOf[I15])
      case 16 => func16(value.asInstanceOf[I16])
      case 17 => func17(value.asInstanceOf[I17])
      case 18 => func18(value.asInstanceOf[I18])
      case 19 => func19(value.asInstanceOf[I19])
      case 20 => func20(value.asInstanceOf[I20])
      case 21 => func21(value.asInstanceOf[I21])
      case 22 => func22(value.asInstanceOf[I22])
    }
  }

}
