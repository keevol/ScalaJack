package co.blocke.scalajack
package typeadapter
package javaprimitives

import java.lang

object JavaDoubleTypeAdapter extends TypeAdapterFactory.=:=[java.lang.Double] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[java.lang.Double]): TypeAdapter[java.lang.Double] = {
    val doubleTypeAdapter = context.typeAdapterOf[Double]
    new JavaDoubleTypeAdapter(
      new IRTransceiver[lang.Double] {

        private val BoxedDoubleType: Type = typeOf[java.lang.Double]

        override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[lang.Double] =
          ir match {
            case IRNull() =>
              ReadSuccess(TypeTagged(null, BoxedDoubleType))

            case _ =>
              doubleTypeAdapter.irTransceiver.read(path, ir) map {
                case TypeTagged(doubleValue) => TypeTagged(java.lang.Double.valueOf(doubleValue), BoxedDoubleType)
              }
          }

        override def write[IR, WIRE](tagged: TypeTagged[lang.Double])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
          tagged match {
            case TypeTagged(null)  => WriteSuccess(IRNull())
            case TypeTagged(boxed) => doubleTypeAdapter.irTransceiver.write(TypeTagged(boxed.doubleValue))
          }
      })
  }

}

class JavaDoubleTypeAdapter(override val irTransceiver: IRTransceiver[java.lang.Double]) extends TypeAdapter.=:=[java.lang.Double]
