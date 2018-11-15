package co.blocke.scalajack
package typeadapter
package javaprimitives
import java.lang

object JavaFloatTypeAdapter extends TypeAdapterFactory.=:=[java.lang.Float] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[java.lang.Float]): TypeAdapter[java.lang.Float] = {
    val floatTypeAdapter = context.typeAdapterOf[Float]
    new JavaFloatTypeAdapter(
      new IRTransceiver[lang.Float] {
        private val BoxedFloatType: Type = typeOf[java.lang.Float]

        override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[lang.Float] =
          ir match {
            case IRNull() =>
              ReadSuccess(TypeTagged(null, BoxedFloatType))

            case _ =>
              floatTypeAdapter.irTransceiver.read(path, ir) map {
                case TypeTagged(floatValue) => TypeTagged(java.lang.Float.valueOf(floatValue), BoxedFloatType)
              }
          }

        override def write[IR, WIRE](tagged: TypeTagged[lang.Float])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
          tagged match {
            case TypeTagged(null)  => WriteSuccess(IRNull())
            case TypeTagged(boxed) => floatTypeAdapter.irTransceiver.write(TypeTagged(boxed.floatValue))
          }
      }
    )
  }

}

class JavaFloatTypeAdapter(override val irTransceiver: IRTransceiver[lang.Float]) extends TypeAdapter.=:=[java.lang.Float]
