package co.blocke.scalajack
package typeadapter

object TypeSerializer {
  def typeToTypeName(tpe: Type): String = tpe.typeSymbol.fullName
}

object TypeDeserializer {
  def typeNameToType(typeName: String): Type =
    try {
      staticClass(typeName).toType
    } catch {
      case e: ScalaReflectionException =>
        throw new ClassNotFoundException(s"""Unable to find class named "$typeName"\n""", e)
    }
}

class TypeIRTransceiver(
    typeToTypeName: Type => String = TypeSerializer.typeToTypeName,
    typeNameToType: String => Type = TypeDeserializer.typeNameToType
) extends IRTransceiver[Type] {

  private val TypeType: Type = typeOf[Type]

  override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[Type] =
    ir match {
      // --- Not sure null is a thing related to type serialization...
      //      case IRNull()           => ReadSuccess(nullTypeTagged)
      case IRString(typeName) => ReadSuccess(TypeTagged(typeNameToType(typeName), TypeType))
    }

  override def write[IR](tagged: TypeTagged[Type])(implicit ops: OpsBase[IR], guidance: SerializationGuidance): WriteResult[IR] =
    tagged match {
      // Is there such a thing as a null type?  Not sure Scala lets you set a type to null...
      //      case TypeTagged(null) => SerializationSuccess(AstNull())
      case TypeTagged(tpe) => WriteSuccess(IRString(typeToTypeName(tpe)))
    }
}

object TypeTypeAdapter extends TypeAdapterFactory {

  override def typeAdapterOf[Type](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[Type]): TypeAdapter[Type] = {
    if (tt.tpe =:= typeOf[Type]) {
      TypeTypeAdapter(
        new TypeIRTransceiver,
        tt.mirror).asInstanceOf[TypeAdapter[Type]]
    } else {
      next.typeAdapterOf[Type]
    }
  }

}

case class TypeTypeAdapter(override val irTransceiver: IRTransceiver[Type], mirror: Mirror, typeModifier: Option[HintModifier] = None) extends TypeAdapter[Type]
