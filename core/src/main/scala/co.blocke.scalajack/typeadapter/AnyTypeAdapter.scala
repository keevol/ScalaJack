package co.blocke.scalajack
package typeadapter

object AnyTypeAdapter extends TypeAdapterFactory {

  override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    if (tt.tpe =:= typeOf[Any]) {
      val typeTypeAdapter = context.typeAdapterOf[Type]
      val memberNameTypeAdapter = context.typeAdapterOf[MemberName]
      val mapTypeAdapter = context.typeAdapterOf[Map[Any, Any]]
      val listTypeAdapter = context.typeAdapterOf[List[Any]]
      val stringTypeAdapter = context.typeAdapterOf[String]
      val booleanTypeAdapter = context.typeAdapterOf[Boolean]

      AnyTypeAdapter(
        new AnyIRTransceiver(
          typeTypeAdapter.irTransceiver,
          mapTypeAdapter.irTransceiver,
          listTypeAdapter.irTransceiver,
          stringTypeAdapter.irTransceiver,
          new NumberIRTransceiver(),
          booleanTypeAdapter.irTransceiver,
          context),
        typeTypeAdapter,
        memberNameTypeAdapter,
        mapTypeAdapter,
        listTypeAdapter,
        stringTypeAdapter,
        booleanTypeAdapter,
        context).asInstanceOf[TypeAdapter[T]]
    } else {
      next.typeAdapterOf[T]
    }

}

case class AnyTypeAdapter(
    override val irTransceiver: IRTransceiver[Any],
    typeTypeAdapter:            TypeAdapter[Type],
    memberNameTypeAdapter:      TypeAdapter[MemberName],
    mapTypeAdapter:             TypeAdapter[Map[Any, Any]],
    listTypeAdapter:            TypeAdapter[List[Any]],
    stringTypeAdapter:          TypeAdapter[String],
    booleanTypeAdapter:         TypeAdapter[Boolean],
    context:                    Context) extends TypeAdapter.=:=[Any]

class AnyIRTransceiver(
    typeIRTransceiver:    IRTransceiver[Type],
    mapIRTransceiver:     IRTransceiver[Map[Any, Any]],
    listIRTransceiver:    IRTransceiver[List[Any]],
    stringIRTransceiver:  IRTransceiver[String],
    numberIRTransceiver:  IRTransceiver[java.lang.Number],
    booleanIRTransceiver: IRTransceiver[Boolean],
    context:              Context) extends IRTransceiver[Any] {

  self =>

  private val nullTypeTagged = TypeTagged(null, typeOf[Any])
  private val StringType: Type = typeOf[String]
  private val TypeType: Type = typeOf[Type]

  override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[Any] =
    ir match {
      // For map keys of type Any, all of these will be string.  We need to test and see if we can deserialize a specific type or not.
      // If not, stay with the string, otherwise use the more specific type.
      case IRString(s) if (guidance.isMapKey) =>
        try {
          ops.deserialize(path, s.asInstanceOf[WIRE]).mapToReadResult(path, (dsIR: IR) => context.typeAdapterOf[Any].irTransceiver.read(path, dsIR)) match {
            case success: ReadSuccess[_] => success
            case _                       => stringIRTransceiver.read(path, ir)
          }
        } catch {
          // Nope... no embedded typed thing found... must be a plain 'ol String
          case _: Throwable => stringIRTransceiver.read(path, ir)
        }

      case IRMap(fields) =>
        import scala.collection.mutable.{ Builder, Map }
        val builder: Builder[(Any, Any), Map[Any, Any]] = scala.collection.mutable.Map.newBuilder
        fields.map {
          case (kIR, vIR) =>
            builder += ((this.read(path, kIR).get, this.read(path, vIR).get))
        }
        ReadSuccess(TypeTagged(builder.result(), typeOf[Map[Any, Any]]))

      case IRObject(fields) =>
        val concreteTypeFieldName = context.defaultHint
        fields.find { case (name, _) => name == concreteTypeFieldName }.map(_._2) match {
          case Some(concreteTypeIR) =>
            val ReadSuccess(TypeTagged(concreteType)) = typeIRTransceiver.read(path \ concreteTypeFieldName, concreteTypeIR)
            context.typeAdapter(concreteType).irTransceiver.read(path, ir)
          case None =>
            mapIRTransceiver.read(path, ir)
        }

      case IRArray(_) =>
        listIRTransceiver.read(path, ir)

      case IRString(_) =>
        stringIRTransceiver.read(path, ir)

      case IRBoolean(_) =>
        booleanIRTransceiver.read(path, ir)

      case IRDouble(_) | IRDecimal(_) | IRInt(_) | IRLong(_) =>
        numberIRTransceiver.read(path, ir)

      case IRNull() =>
        ReadSuccess(nullTypeTagged)

      case _ => ReadFailure(path, ReadError.Unexpected(s"Given value is of unknown type: $ir", reportedBy = self))
    }

  override def write[IR, WIRE](tagged: TypeTagged[Any])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
    tagged match {
      case TypeTagged(null) =>
        WriteSuccess(IRNull())

      case TypeTagged(_: String) =>
        stringIRTransceiver.write(tagged.asInstanceOf[TypeTagged[String]])

      case TypeTagged(enum: Enumeration#Value) =>
        stringIRTransceiver.write(TypeTagged(enum.toString, StringType))

      case TypeTagged(_: List[_]) =>
        listIRTransceiver.write(tagged.asInstanceOf[TypeTagged[List[Any]]])

      case TypeTagged(_: Map[_, _]) =>
        mapIRTransceiver.write(tagged.asInstanceOf[TypeTagged[Map[Any, Any]]])

      case TypeTagged(value) =>
        val valueType = staticClass(value.getClass.getName).toType

        val valueIRTransceiver = context.irTransceiver(valueType).asInstanceOf[IRTransceiver[Any]]

        valueIRTransceiver.write(tagged) map {
          case IRObject(fields) =>
            val WriteSuccess(typeIR) = typeIRTransceiver.write(TypeTagged(valueType, TypeType))
            IRObject(("_hint", typeIR) +: fields)
          case json => json
        }
    }

}