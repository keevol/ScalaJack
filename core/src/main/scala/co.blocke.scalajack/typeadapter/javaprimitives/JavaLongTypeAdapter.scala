package co.blocke.scalajack
package typeadapter
package javaprimitives
import java.lang

object JavaLongTypeAdapter extends TypeAdapterFactory.=:=[java.lang.Long] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[java.lang.Long]): TypeAdapter[java.lang.Long] = {
    val longTypeAdapter = context.typeAdapterOf[Long]
    new JavaLongTypeAdapter(
      new IRTransceiver[lang.Long] {

        private val BoxedLongType: Type = typeOf[java.lang.Long]

        override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[lang.Long] =
          ir match {
            case IRNull() =>
              ReadSuccess(TypeTagged(null, BoxedLongType))

            case _ =>
              longTypeAdapter.irTransceiver.read(path, ir) map {
                case TypeTagged(longValue) => TypeTagged(java.lang.Long.valueOf(longValue), BoxedLongType)
              }
          }

        override def write[IR, WIRE](tagged: TypeTagged[lang.Long])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
          tagged match {
            case TypeTagged(null)  => WriteSuccess(IRNull())
            case TypeTagged(boxed) => longTypeAdapter.irTransceiver.write(TypeTagged(boxed.longValue))
          }
      })
  }

}

class JavaLongTypeAdapter(override val irTransceiver: IRTransceiver[lang.Long]) extends TypeAdapter[java.lang.Long]