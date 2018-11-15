package co.blocke.scalajack
package typeadapter
package javaprimitives

object JavaNumberTypeAdapter extends TypeAdapterFactory.=:=[java.lang.Number] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[java.lang.Number]): TypeAdapter[Number] =
    new JavaNumberTypeAdapter(
      new IRTransceiver[java.lang.Number] {

        self =>

        private val BoxedNumberType: Type = typeOf[java.lang.Number]
        private val BoxedDoubleType: Type = typeOf[java.lang.Double]
        private val BoxedLongType: Type = typeOf[java.lang.Long]
        private val JavaBigDecimalType: Type = typeOf[java.math.BigDecimal]
        private val JavaBigIntegerType: Type = typeOf[java.math.BigInteger]

        // Bizzare set of magic to try to "fix" the precision slop when moving from Float->Double (prints extra digits in JSON)
        private def capFloat(f: Float): Double = {
          val d = f.toString.toDouble
          val diff = f.toDouble - d
          f - diff
        }

        override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[java.lang.Number] =
          ir match {
            case IRNull() => ReadSuccess(TypeTagged(null, BoxedNumberType))
            case IRDecimal(scalaBigDecimal) if (!scalaBigDecimal.isDecimalDouble) => ReadSuccess(TypeTagged(scalaBigDecimal.bigDecimal, JavaBigDecimalType))
            case IRDecimal(scalaBigDecimal) => ReadSuccess(TypeTagged(java.lang.Double.valueOf(scalaBigDecimal.doubleValue), BoxedDoubleType))
            case IRDouble(doubleValue) => ReadSuccess(TypeTagged(java.lang.Double.valueOf(doubleValue), BoxedDoubleType))
            case IRInt(scalaBigInt) => ReadSuccess(TypeTagged(scalaBigInt.bigInteger, JavaBigIntegerType))
            case IRLong(longValue) => ReadSuccess(TypeTagged(java.lang.Long.valueOf(longValue), BoxedLongType))
            case IRString(s) if (guidance.isMapKey) => this.read(path, ops.deserialize(s.asInstanceOf[WIRE]).get)(ops, guidance = guidance.copy(isMapKey = false))
            case IRString(_) => ReadFailure(path, ReadError.Unsupported("Expected a JSON number", reportedBy = self))
            case _ => ReadFailure(path, ReadError.Unsupported("Expected a JSON number", reportedBy = self))
          }

        override def write[IR, WIRE](tagged: TypeTagged[java.lang.Number])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
          tagged match {
            case TypeTagged(null)                                 => WriteSuccess(IRNull())
            case TypeTagged(boxedByte: java.lang.Byte)            => WriteSuccess(IRLong(boxedByte.longValue))
            case TypeTagged(boxedDouble: java.lang.Double)        => WriteSuccess(IRDouble(boxedDouble.doubleValue))
            case TypeTagged(boxedFloat: java.lang.Float)          => WriteSuccess(IRDouble(capFloat(boxedFloat.floatValue)))
            case TypeTagged(boxedInt: java.lang.Integer)          => WriteSuccess(IRLong(boxedInt.longValue))
            case TypeTagged(boxedLong: java.lang.Long)            => WriteSuccess(IRLong(boxedLong.longValue))
            case TypeTagged(boxedShort: java.lang.Short)          => WriteSuccess(IRLong(boxedShort.longValue))
            case TypeTagged(javaBigInteger: java.math.BigInteger) => WriteSuccess(IRInt(scala.math.BigInt(javaBigInteger)))
            case TypeTagged(javaBigDecimal: java.math.BigDecimal) => WriteSuccess(IRDecimal(scala.math.BigDecimal(javaBigDecimal)))
          }
      }
    )

}

class JavaNumberTypeAdapter(override val irTransceiver: IRTransceiver[java.lang.Number]) extends TypeAdapter.=:=[java.lang.Number]
