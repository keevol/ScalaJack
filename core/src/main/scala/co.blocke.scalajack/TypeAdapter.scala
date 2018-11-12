package co.blocke.scalajack

import scala.reflect.ClassTag

/**
 * TypeAdapter includes two matching patterns you can use when you extend trait TypeAdapter for your
 * custom adapters.  The two matching behaviors are '===' and '=:='.
 *
 * This difference is because =:= matches children.  Consider:
 *
 *    type Phone = String
 *    case class( name:String, phone:Phone )
 *
 * With =:= both name and phone (String and Phone) will match a TypeAdapter derived from =:=.
 * This is actually what you want if you haven't overridden Phone with its own TypeAdapter... it should default
 * to the TypeAdapter of its base type.
 *
 * But... if you did provide an override PhoneTypeAdapter you want the matching to be strict, so we use  ===
 * in this case.  With strict matching String != Phone.
 *
 */
object TypeAdapter {

  abstract class ===[X](implicit ttFactory: TypeTag[X]) extends TypeAdapterFactory.===[X] with TypeAdapter[X] {
    override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[X]): TypeAdapter[X] = this
  }

  abstract class =:=[X](implicit ttFactory: TypeTag[X]) extends TypeAdapterFactory.=:=[X] with TypeAdapter[X] {
    override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[X]): TypeAdapter[X] = this
  }

  def apply[IR, T](irTransceiver: IRTransceiver[IR]): TypeAdapter[T] = Fixed(irTransceiver)

  private case class Fixed[IR, T](override val irTransceiver: IRTransceiver[IR]) extends TypeAdapter[T]

}

trait TypeAdapter[T] {

  self =>

  def is[U <: TypeAdapter[_]: ClassTag]: Boolean =
    maybeAs[U].isDefined

  def as[U <: TypeAdapter[_]: ClassTag]: U =
    maybeAs[U].getOrElse(throw new RuntimeException(s"$self is not an instance of ${implicitly[ClassTag[U]].runtimeClass}"))

  def maybeAs[U <: TypeAdapter[_]: ClassTag]: Option[U] = {
    val runtimeClass = implicitly[ClassTag[U]].runtimeClass
    try {
      Some(runtimeClass.cast(self).asInstanceOf[U])
    } catch {
      case _: ClassCastException =>
        None
    }
  }

  val irTransceiver: IRTransceiver[T] = new IRTransceiver[T] {}

  /*
  val deserializer: Deserializer[T] = new Deserializer[T] {
    override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[T] =
      throw new NotImplementedError(s"$self.deserializer.deserialize")
  }

  val serializer: Serializer[T] = new Serializer[T] {
    override def serialize[AST, S](tagged: TypeTagged[T])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
      throw new NotImplementedError(s"$self.serializer.serialize")
  }
  */

  def defaultValue: Option[T] = None

  def resolved: TypeAdapter[T] = this
}

