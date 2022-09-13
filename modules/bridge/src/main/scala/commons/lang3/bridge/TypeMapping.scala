package commons.lang3.bridge

/** TODO
  *
  * @author
  *   mars
  * @version 1.0.0
  * @since 2022/08/28
  *   02:48
  */
class TypeMapping[Input, Sum](val index: Int) extends AnyVal

object TypeMapping extends impl.TypeMappingImplicitOptsPolyHigher {
  object alias extends impl.TypeMappingAlias
}
