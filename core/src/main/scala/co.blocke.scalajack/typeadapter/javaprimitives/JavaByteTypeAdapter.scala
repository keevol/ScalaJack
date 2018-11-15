package co.blocke.scalajack
package typeadapter
package javaprimitives

object JavaByteTypeAdapter extends TypeAdapterFactory.=:=[java.lang.Byte] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[java.lang.Byte]): TypeAdapter[java.lang.Byte] = {
    val byteTypeAdapter = context.typeAdapterOf[Byte]
    new JavaByteTypeAdapter(
      new IRTransceiver[java.lang.Byte] {

        private val BoxedByteType: Type = typeOf[java.lang.Byte]

        override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[java.lang.Byte] =
          ir match {
            case IRNull() =>
              ReadSuccess(TypeTagged(null, BoxedByteType))

            case _ =>
              byteTypeAdapter.irTransceiver.read(path, ir) map {
                case TypeTagged(byteValue) => TypeTagged(java.lang.Byte.valueOf(byteValue), BoxedByteType)
              }
          }

        override def write[IR, WIRE](tagged: TypeTagged[java.lang.Byte])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
          tagged match {
            case TypeTagged(null)  => WriteSuccess(IRNull())
            case TypeTagged(boxed) => byteTypeAdapter.irTransceiver.write(TypeTagged(boxed.byteValue))
          }
      })
  }

}

class JavaByteTypeAdapter(override val irTransceiver: IRTransceiver[java.lang.Byte]) extends TypeAdapter.=:=[java.lang.Byte]
