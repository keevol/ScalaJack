package co.blocke.scalajack
package typeadapter
package javaprimitives
import java.math.BigInteger

object JavaBigIntegerTypeAdapter extends TypeAdapterFactory.=:=[java.math.BigInteger] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[java.math.BigInteger]): TypeAdapter[java.math.BigInteger] = {
    val scalaBigIntTypeAdapter = context.typeAdapterOf[scala.math.BigInt]
    new JavaBigIntegerTypeAdapter(
      new IRTransceiver[BigInteger] {
        private val JavaBigIntegerType: Type = typeOf[java.math.BigInteger]
        private val ScalaBigIntType: Type = typeOf[scala.math.BigInt]

        override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[java.math.BigInteger] =
          scalaBigIntTypeAdapter.irTransceiver.read(path, ir) map {
            case TypeTagged(null)        => TypeTagged(null, JavaBigIntegerType)
            case TypeTagged(scalaBigInt) => TypeTagged(scalaBigInt.bigInteger, JavaBigIntegerType)
          }

        override def write[IR, WIRE](tagged: TypeTagged[java.math.BigInteger])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
          tagged match {
            case TypeTagged(null)           => scalaBigIntTypeAdapter.irTransceiver.write(TypeTagged(null, ScalaBigIntType))
            case TypeTagged(javaBigInteger) => scalaBigIntTypeAdapter.irTransceiver.write(TypeTagged(scala.math.BigInt(javaBigInteger), ScalaBigIntType))
          }
      })
  }

}

class JavaBigIntegerTypeAdapter(override val irTransceiver: IRTransceiver[BigInteger]) extends TypeAdapter[java.math.BigInteger]