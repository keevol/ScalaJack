package co.blocke.scalajack
package typeadapter
package javaprimitives

object JavaBigDecimalTypeAdapter extends TypeAdapterFactory.=:=[java.math.BigDecimal] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[java.math.BigDecimal]): TypeAdapter[java.math.BigDecimal] = {
    val scalaBigDecimalTypeAdapter = context.typeAdapterOf[scala.math.BigDecimal]
    new JavaBigDecimalTypeAdapter(
      new IRTransceiver[java.math.BigDecimal] {
        private val ScalaBigDecimalType: Type = typeOf[scala.math.BigDecimal]
        private val JavaBigDecimalType: Type = typeOf[java.math.BigDecimal]

        override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[java.math.BigDecimal] =
          scalaBigDecimalTypeAdapter.irTransceiver.read(path, ir) map {
            case TypeTagged(null)            => TypeTagged(null, JavaBigDecimalType)
            case TypeTagged(scalaBigDecimal) => TypeTagged(scalaBigDecimal.bigDecimal, JavaBigDecimalType)
          }

        override def write[IR, WIRE](tagged: TypeTagged[java.math.BigDecimal])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
          tagged match {
            case TypeTagged(null)           => scalaBigDecimalTypeAdapter.irTransceiver.write(TypeTagged(null, ScalaBigDecimalType))
            case TypeTagged(javaBigDecimal) => scalaBigDecimalTypeAdapter.irTransceiver.write(TypeTagged(scala.math.BigDecimal(javaBigDecimal), ScalaBigDecimalType))
          }
      })
  }

}

class JavaBigDecimalTypeAdapter(override val irTransceiver: IRTransceiver[java.math.BigDecimal]) extends TypeAdapter.=:=[java.math.BigDecimal]