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

object TypeMapping extends TypeMappingImplicitOptsPolyHigher {
  @inline def apply[Input, Sum](i: Int): TypeMapping[Input, Sum]               = new TypeMapping(i)
  @inline def getMapping[F[_] <: TypeMapping[_, _], A](implicit f: F[A]): F[A] = f
}

trait TypeMappingImplicitOptsPoly3 {
  implicit def typeMappingImplicit1Poly3Identity[I <: I1, I1]: TypeMapping[I, I1] = TypeMapping(1)
}
