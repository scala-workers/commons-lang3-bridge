package commons.lang3

import commons.lang3.scala.{TypeMappingInnerHelper => helper}

package object scala {

  import helper._

  type StrToOpt[T] = TypeMapping[T, (String, Option[String])]

  def strToOpt[T: StrToOpt](t: T): Option[String] = {
    val mapping = implicitly[StrToOpt[T]]
    mapping.ops(t)(Option(_), identity)
  }

  // ↓ Simple to delete
  {
    type cusToList[T] = TypeMapping[T, (Char, String, None.type, Some[String], Option[String])]

    def strOpt[T: cusToList](t: T): List[Char] = {
      val mapping = implicitly[cusToList[T]]
      mapping.ops(t)(
        { s =>
          println("这个是 Char")
          List(s)
        },
        { s =>
          println("这个是 String")
          s.toList
        },
        t => {
          println("这个是 None")
          List.empty[Char]
        },
        s => {
          println("这个是 Some")
          s.toList.flatten
        },
        s => {
          println("这个是 Option")
          s.toList.flatten
        }
      )
    }

    println(strOpt('2'))               // 这个是 Char
    println(strOpt("sdfwer"))          // 这个是 String
    println(strOpt(Some("3424")))      // 这个是 Some
    println(strOpt(Option("5964954"))) // 这个是 Option
    println(strOpt(None))              // 这个是 None
    println(strOpt(Option.empty))      // 这个是 Option

// 这个是 Char
// List(2)
// 这个是 String
// List(s, d, f, w, e, r)
// 这个是 Some
// List(3, 4, 2, 4)
// 这个是 Option
// List(5, 9, 6, 4, 9, 5, 4)
// 这个是 None
// List()
// 这个是 Option
// List()

  }

}
