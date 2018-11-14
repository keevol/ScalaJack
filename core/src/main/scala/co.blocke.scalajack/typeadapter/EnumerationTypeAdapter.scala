package co.blocke.scalajack
package typeadapter

object EnumerationTypeAdapter extends TypeAdapterFactory.FromClassSymbol {

  override def typeAdapterOf[T](classSymbol: ClassSymbol, next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    if (tt.tpe.typeSymbol.fullName == "scala.Enumeration.Value") {
      // Can't use tpe <:< because Enumeration has no companion object
      val erasedEnumClassName = tt.tpe.toString match {
        case raw if (raw.endsWith(".Value")) => raw.replace(".Value", "$")
        case raw                             => raw.dropRight(raw.length - raw.lastIndexOf('.')) + "$"
      }
      val enum = Class.forName(erasedEnumClassName).getField(scala.reflect.NameTransformer.MODULE_INSTANCE_NAME).get(null).asInstanceOf[Enumeration]
      EnumerationTypeAdapter(new EnumerationIRTransceiver(enum), enum).asInstanceOf[TypeAdapter[T]]
    } else {
      next.typeAdapterOf[T]
    }

}

case class EnumerationTypeAdapter[E <: Enumeration](override val irTransceiver: IRTransceiver[E#Value], enum: E) extends TypeAdapter[E#Value]

class EnumerationIRTransceiver[E <: Enumeration](enumeration: E)(implicit tt: TypeTag[E#Value]) extends IRTransceiver[E#Value] {
  self =>

  private val enumerationName: String = enumeration.getClass.getName
  private val enumerationValueType: Type = tt.tpe

  override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[E#Value] =
    ir match {
      case IRString(name) =>
        ReadResult(path)(TypeTagged(enumeration.withName(name), enumerationValueType), {
          case _: NoSuchElementException =>
            ReadError.Malformed(s"Enumeration $enumerationName does not contain a value named $name", reportedBy = self)
        })

      case IRLong(index) =>
        ReadResult(path)(TypeTagged(enumeration(index.intValue), enumerationValueType), {
          case _: NoSuchElementException =>
            ReadError.Malformed(s"Enumeration $enumerationName does not contain a value at index $index", reportedBy = self)
        })

      case IRNull() =>
        ReadSuccess(TypeTagged(null, enumerationValueType))

      case _ =>
        ReadFailure(path, ReadError.Unexpected("Expected a JSON string or int", reportedBy = self))
    }

  override def write[IR](tagged: TypeTagged[E#Value])(implicit ops: OpsBase[IR], guidance: SerializationGuidance): WriteResult[IR] =
    tagged match {
      case TypeTagged(null)             => WriteSuccess(IRNull())
      case TypeTagged(enumerationValue) => WriteSuccess(IRString(enumerationValue.toString))
    }
}
