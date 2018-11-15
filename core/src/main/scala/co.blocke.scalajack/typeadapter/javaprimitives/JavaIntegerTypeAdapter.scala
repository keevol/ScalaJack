package co.blocke.scalajack
package typeadapter
package javaprimitives

object JavaIntegerTypeAdapter extends TypeAdapterFactory.=:=[java.lang.Integer] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[java.lang.Integer]): TypeAdapter[java.lang.Integer] = {
    val intTypeAdapter = context.typeAdapterOf[Int]
    new JavaIntegerTypeAdapter(
      new IRTransceiver[java.lang.Integer] {
        private val BoxedIntType: Type = typeOf[java.lang.Integer]

        override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[java.lang.Integer] =
          ir match {
            case IRNull() =>
              ReadSuccess(TypeTagged(null, BoxedIntType))

            case _ =>
              intTypeAdapter.irTransceiver.read(path, ir) map {
                case TypeTagged(intValue) => TypeTagged(java.lang.Integer.valueOf(intValue), BoxedIntType)
              }
          }

        override def write[IR, WIRE](tagged: TypeTagged[java.lang.Integer])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
          tagged match {
            case TypeTagged(null)  => WriteSuccess(IRNull())
            case TypeTagged(boxed) => intTypeAdapter.irTransceiver.write(TypeTagged(boxed.intValue))
          }
      })
  }

}

class JavaIntegerTypeAdapter(override val irTransceiver: IRTransceiver[java.lang.Integer]) extends TypeAdapter[java.lang.Integer]
