package co.blocke.scalajack
package typeadapter
package javaprimitives
import java.lang

object JavaShortTypeAdapter extends TypeAdapterFactory.=:=[java.lang.Short] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[java.lang.Short]): TypeAdapter[java.lang.Short] = {
    val shortTypeAdapter = context.typeAdapterOf[Short]
    new JavaShortTypeAdapter(
      new IRTransceiver[lang.Short] {
        private val BoxedShortType: Type = typeOf[java.lang.Short]

        override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[lang.Short] =
          ir match {
            case IRNull() =>
              ReadSuccess(TypeTagged(null, BoxedShortType))

            case _ =>
              shortTypeAdapter.irTransceiver.read(path, ir) map {
                case TypeTagged(shortValue) => TypeTagged(java.lang.Short.valueOf(shortValue), BoxedShortType)
              }
          }

        override def write[IR, WIRE](tagged: TypeTagged[lang.Short])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
          tagged match {
            case TypeTagged(null)  => WriteSuccess(IRNull())
            case TypeTagged(boxed) => shortTypeAdapter.irTransceiver.write(TypeTagged(boxed.shortValue))
          }
      }
    )
  }

}

class JavaShortTypeAdapter(override val irTransceiver: IRTransceiver[lang.Short]) extends TypeAdapter[java.lang.Short]
