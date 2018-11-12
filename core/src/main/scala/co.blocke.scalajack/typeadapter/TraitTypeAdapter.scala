package co.blocke.scalajack
package typeadapter

import scala.collection.mutable

class TraitIRTransceiver[T](
    typeFieldName:     MemberName, // hint label
    typeIRTransceiver: IRTransceiver[Type], // Ignored for modified versions, as it is set by concrete type
    f:                 Option[BijectiveFunction[String, Type]] = None // optional string->type map (hint modifier)
)(implicit tt: TypeTag[T], context: Context) extends IRTransceiver[T] {

  self =>

  // f is None for global TraitSerializer.  It is set in ScalaJack for specific type modifiers set using withHindModifiers()

  private val polymorphicType: Type = tt.tpe
  private val populatedConcreteTypeCache = new mutable.WeakHashMap[Type, Type]
  private val TypeType: Type = typeOf[Type]
  private val stringTransceiver = context.typeAdapterOf[String].irTransceiver
  private val nullTypeTagged: TypeTagged[T] = TypeTagged[T](null.asInstanceOf[T], polymorphicType)

  private def populateConcreteType(concreteType: Type): Type =
    populatedConcreteTypeCache.getOrElseUpdate(concreteType, Reflection.populateChildTypeArgs(polymorphicType, concreteType))

  override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[T] =
    ir match {
      case IRNull() => ReadSuccess(nullTypeTagged)

      case IRObject(fields) =>
        var maybeConcreteType: Option[Type] = None
        fields.foreach {
          case (fieldName, fieldValueIR) =>
            if (fieldName == typeFieldName) {
              maybeConcreteType = f.map(_.apply(ops.unapplyString(fieldValueIR).get)).orElse {
                val ReadSuccess(TypeTagged(concreteType)) = typeIRTransceiver.read(path \ fieldName, fieldValueIR)
                Some(concreteType)
              }
            }
        }

        maybeConcreteType match {
          case Some(concreteType) =>
            val populatedConcreteType = populateConcreteType(concreteType)
            context.irTransceiver(populatedConcreteType).read(path, ir).asInstanceOf[ReadResult[T]]

          case None =>
            throw new java.lang.IllegalStateException(s"""Could not find type field named "$typeFieldName"\n""" /* FIXME + reader.showError()*/ )
        }

      case IRString(s) if (guidance.isMapKey) =>
        context.typeAdapterOf[T].irTransceiver.read(Path.Root, ops.deserialize(s.asInstanceOf[WIRE]).get)

      case _ =>
        ReadFailure(path, ReadError.Unexpected("Expected a JSON object", reportedBy = self))
    }

  override def write[IR](tagged: TypeTagged[T])(implicit ops: OpsBase[IR], guidance: SerializationGuidance): WriteResult[IR] =
    tagged match {
      case TypeTagged(null) => WriteSuccess(IRNull())

      case TypeTagged(value) =>
        // TODO figure out a better way to infer the type (perhaps infer the type arguments?)
        val concreteType = classSymbol(value.getClass).toType
        val populatedConcreteType = populateConcreteType(concreteType)

        context.irTransceiver(populatedConcreteType).asInstanceOf[IRTransceiver[T]].write(tagged) map {
          case IRObject(concreteFields) =>
            val WriteSuccess(typeIR) = f.map(bij =>
              stringTransceiver.write(TypeTagged(bij.unapply(concreteType), TypeType))).getOrElse(
              typeIRTransceiver.write(TypeTagged(concreteType, TypeType))
            )
            IRObject((typeFieldName, typeIR) +: concreteFields)
          case json => json
        }
    }
}

// This should come *after* SealedTraitTypeAdapter in the context factory list, as all sealed traits are
// also traits, and this factory would pick them all up, hiding the sealed ones.
//
case class TraitTypeAdapterFactory(hintLabel: String, specificType: Option[Type] = None) extends TypeAdapterFactory.FromClassSymbol {

  override def typeAdapterOf[T](classSymbol: ClassSymbol, next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    if (specificType.map(_ == tt.tpe).getOrElse(true) && classSymbol.isTrait) {
      val typeTypeAdapter = context.typeAdapterOf[Type]

      TraitTypeAdapter(
        new TraitIRTransceiver[T](hintLabel, typeTypeAdapter.irTransceiver),
        tt.tpe)
    } else {
      next.typeAdapterOf[T]
    }

}

case class TraitTypeAdapter[T](
    override val irTransceiver: IRTransceiver[T],
    polymorphicType:            Type
) extends TypeAdapter[T]