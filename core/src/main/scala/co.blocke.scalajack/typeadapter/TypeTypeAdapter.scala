package co.blocke.scalajack
package typeadapter

object TypeTypeIRTransceiver {
  def typeNameToType(typeName: String): Type =
    try {
      staticClass(typeName).toType
    } catch {
      case e: ScalaReflectionException =>
        println("oops: " + typeName) // Why did this work ok pre-refactor?  What's the difference???
        throw new ClassNotFoundException(s"""Unable to find class named "$typeName"\n""", e)
    }

  def typeToTypeName(tpe: Type): String = tpe.typeSymbol.fullName
}

class TypeTypeIRTransceiver(
    typeToTypeName: Type => String = TypeTypeIRTransceiver.typeToTypeName,
    typeNameToType: String => Type = TypeTypeIRTransceiver.typeNameToType) extends IRTransceiver[Type] {

  private val TypeType: Type = typeOf[Type]

  override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[Type] =
    ir match {
      // --- Not sure null is a thing related to type serialization...
      //      case AstNull()           => DeserializationSuccess(nullTypeTagged)
      case IRString(typeName) => ReadSuccess(TypeTagged(typeNameToType(typeName), TypeType))
    }

  override def write[IR, WIRE](tagged: TypeTagged[Type])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
    tagged match {
      // Is there such a thing as a null type?  Not sure Scala lets you set a type to null...
      //      case TypeTagged(null) => SerializationSuccess(AstNull())
      case TypeTagged(tpe) => WriteSuccess(IRString(typeToTypeName(tpe)))
    }
}

object TypeTypeAdapter extends TypeAdapterFactory {

  override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] = {
    if (tt.tpe =:= typeOf[Type]) {
      TypeTypeAdapter(
        new TypeTypeIRTransceiver(),
        tt.mirror).asInstanceOf[TypeAdapter[T]]
    } else {
      next.typeAdapterOf[T]
    }
  }

}

case class TypeTypeAdapter(override val irTransceiver: IRTransceiver[Type], mirror: Mirror, typeModifier: Option[HintModifier] = None) extends TypeAdapter[Type]
