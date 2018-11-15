package co.blocke.scalajack
package typeadapter
package javaprimitives

object JavaBooleanTypeAdapter extends TypeAdapterFactory.=:=[java.lang.Boolean] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[java.lang.Boolean]): TypeAdapter[java.lang.Boolean] = {
    val booleanTypeAdapter = context.typeAdapterOf[Boolean]
    new JavaBooleanTypeAdapter(
      new IRTransceiver[java.lang.Boolean] {
        private val BoxedBooleanType: Type = typeOf[java.lang.Boolean]

        override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[java.lang.Boolean] =
          ir match {
            case IRNull() =>
              ReadSuccess(TypeTagged(null, BoxedBooleanType))

            case _ =>
              booleanTypeAdapter.irTransceiver.read(path, ir) map {
                case TypeTagged(booleanValue) => TypeTagged(java.lang.Boolean.valueOf(booleanValue), BoxedBooleanType)
              }
          }

        override def write[IR, WIRE](tagged: TypeTagged[java.lang.Boolean])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
          tagged match {
            case TypeTagged(null)  => WriteSuccess(IRNull())
            case TypeTagged(boxed) => booleanTypeAdapter.irTransceiver.write(TypeTagged(boxed.booleanValue))
          }
      })
  }

}

class JavaBooleanTypeAdapter(override val irTransceiver: IRTransceiver[java.lang.Boolean]) extends TypeAdapter[java.lang.Boolean]