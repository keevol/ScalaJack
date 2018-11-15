package co.blocke.scalajack
package typeadapter
package javaprimitives

object JavaCharacterTypeAdapter extends TypeAdapterFactory.=:=[java.lang.Character] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[java.lang.Character]): TypeAdapter[java.lang.Character] = {
    val charTypeAdapter = context.typeAdapterOf[Char]
    new JavaCharacterTypeAdapter(
      new IRTransceiver[java.lang.Character] {

        private val BoxedCharType: Type = typeOf[java.lang.Character]

        override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[java.lang.Character] =
          ir match {
            case IRNull() =>
              ReadSuccess(TypeTagged(null, BoxedCharType))

            case _ =>
              charTypeAdapter.irTransceiver.read(path, ir) map {
                case TypeTagged(charValue) => TypeTagged(java.lang.Character.valueOf(charValue), BoxedCharType)
              }
          }

        override def write[IR, WIRE](tagged: TypeTagged[java.lang.Character])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
          tagged match {
            case TypeTagged(null)  => WriteSuccess(IRNull())
            case TypeTagged(boxed) => charTypeAdapter.irTransceiver.write(TypeTagged(boxed.charValue))
          }
      })
  }

}

class JavaCharacterTypeAdapter(override val irTransceiver: IRTransceiver[java.lang.Character]) extends TypeAdapter.=:=[java.lang.Character]
