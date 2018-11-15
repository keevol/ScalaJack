package co.blocke.scalajack
package typeadapter

object DerivedValueClassTypeAdapter extends TypeAdapterFactory.FromClassSymbol {

  override def typeAdapterOf[T](classSymbol: ClassSymbol, next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    if (classSymbol.isDerivedValueClass) {
      val tpe = tt.tpe

      val constructorSymbol = classSymbol.primaryConstructor.asMethod
      val constructorMirror = reflectClass(classSymbol).reflectConstructor(constructorSymbol)

      val (parameter :: Nil) :: Nil = constructorSymbol.paramLists
      val parameterName = parameter.name.encodedName.toString
      val accessorMethodSymbol = tpe.member(TermName(parameterName)).asMethod
      val accessorMethod = Reflection.methodToJava(accessorMethodSymbol)

      //      type Source = Any
      //      type Derived = T

      type Derived = T
      val derivedTypeTag: TypeTag[Derived] = tt.asInstanceOf[TypeTag[Derived]]

      type Source = Any
      val valueType = parameter.infoIn(tpe).substituteTypes(tpe.typeConstructor.typeParams, tpe.typeArgs)
      val sourceTypeTag: TypeTag[Source] = TypeTags.of[Source](valueType)

      val valueTypeAdapter = context.typeAdapter(valueType).asInstanceOf[TypeAdapter[Source]]

      def wrap(source: Source): Derived = constructorMirror.apply(source).asInstanceOf[Derived]

      def unwrap(wrapped: Derived): Source = accessorMethod.invoke(wrapped)

      implicit val z = sourceTypeTag
      DerivedValueClassAdapter[Derived, Source](
        new DerivedValueClassIRTransceiver[Derived, Source](valueTypeAdapter.irTransceiver, unwrap, wrap)(derivedTypeTag, sourceTypeTag))
    } else {
      next.typeAdapterOf[T]
    }
}

case class DerivedValueClassAdapter[DerivedValueClass, Value](
    override val irTransceiver: IRTransceiver[DerivedValueClass]) extends TypeAdapter[DerivedValueClass]

class DerivedValueClassIRTransceiver[Derived, Source](
    sourceTransceiver: IRTransceiver[Source],
    unwrap:            Derived => Source,
    derive:            Source => Derived)(implicit derivedTypeTag: TypeTag[Derived], sourceTypeTag: TypeTag[Source]) extends IRTransceiver[Derived] {

  private val derivedType: Type = derivedTypeTag.tpe
  private val sourceType: Type = sourceTypeTag.tpe
  override def toString: String = s"DerivedValueClassIRTransceiver[${derivedTypeTag.tpe}, ${sourceTypeTag.tpe}]"

  override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[Derived] =
    sourceTransceiver.read(path, ir) map {
      case TypeTagged(source) =>
        val derived = derive(source)
        TypeTagged(derived, derivedType)
    }

  override def write[IR, WIRE](tagged: TypeTagged[Derived])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
    tagged match {
      case TypeTagged(derived) =>
        val source = unwrap(derived)
        sourceTransceiver.write(TypeTagged(source, sourceType))
    }

}