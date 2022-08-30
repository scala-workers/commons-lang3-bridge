package commons.lang3.scala

/** TODO
  *
  * @author
  *   mars
  * @version 1.0.0
  * @since 2022/08/28
  *   02:48
  */
trait TypeGetter[Sum] {
  def typeTransaform: Array[Any => Sum]
  def get[T](i: Int): T => Sum = (s: T) => typeTransaform(i)(s)
}

object TypeGetter {
  def apply[Sum](arr: Array[Any => Sum]): TypeGetter[Sum] = new TypeGetter[Sum] {
    override val typeTransaform: Array[Any => Sum] = arr
  }
}
