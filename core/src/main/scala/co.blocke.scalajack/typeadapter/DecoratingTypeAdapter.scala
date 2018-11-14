package co.blocke.scalajack
package typeadapter

import scala.reflect.ClassTag

trait DecoratingTypeAdapter[T] extends TypeAdapter[T] {

  def decorated: TypeAdapter[T]

  override def maybeAs[U <: TypeAdapter[_]: ClassTag]: Option[U] = super.maybeAs[U] orElse decorated.maybeAs[U]
}