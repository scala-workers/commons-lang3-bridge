package commons.lang3.scala

/** TODO
  *
  * @author
  *   mars
  * @version 1.0.0
  * @since 2022/08/28
  *   02:48
  */
trait TypeMapping[Input, Sum] {
  def index: Int
  def setValue(any: Any): (Int, Any) = (index, any)
}

object TypeMapping extends TypeMappingImplicitOptsPolyHigher {
  def apply[Input, Sum](i: Int): TypeMapping[Input, Sum] = new TypeMapping[Input, Sum] {
    override def index: Int = i
  }
}

trait TypeMappingImplicitOptsPoly3 {
  implicit def typeMappingImplicit1Poly3Identity[I <: I1, I1]: TypeMapping[I, I1] = TypeMappingInstances.get(1)
}

private[scala] object TypeMappingInstances {

  private var typeMappingSumMap: Map[Int, TypeMapping[Any, Any]] = Map.empty

  def get[Input, Sum](i: Int): TypeMapping[Input, Sum] = {
    val opt = typeMappingSumMap.get(i)
    if (opt.isEmpty) {
      val typeMappingToPush: TypeMapping[Any, Any] = TypeMapping(i)
      typeMappingSumMap = typeMappingSumMap + ((i, typeMappingToPush))
      typeMappingToPush.asInstanceOf[TypeMapping[Input, Sum]]
    } else
      opt.get.asInstanceOf[TypeMapping[Input, Sum]]
  }

}
