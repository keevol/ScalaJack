package co.blocke.scalajack
package typeadapter

import java.lang.Number

trait NumberIRReader extends IRReader[Number] {

  self: IRTransceiver[Number] =>

  private val BoxedNumberType: Type = typeOf[Number]
  private val BoxedDoubleType: Type = typeOf[Double]
  private val BoxedLongType: Type = typeOf[Long]
  private val ScalaBigDecimalType: Type = typeOf[scala.math.BigDecimal]
  private val ScalaBigIntegerType: Type = typeOf[scala.math.BigInt]

  override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[Number] =
    ir match {
      case IRNull() => ReadSuccess(TypeTagged(null, BoxedNumberType))
      case IRDecimal(scalaBigDecimal) if (!scalaBigDecimal.isDecimalDouble) => ReadSuccess(TypeTagged(scalaBigDecimal, ScalaBigDecimalType))
      case IRDecimal(scalaBigDecimal) => ReadSuccess(TypeTagged(java.lang.Double.valueOf(scalaBigDecimal.doubleValue()), BoxedDoubleType))
      case IRDouble(doubleValue) => ReadSuccess(TypeTagged(java.lang.Double.valueOf(doubleValue), BoxedDoubleType))
      case IRInt(scalaBigInt) => ReadSuccess(TypeTagged(scalaBigInt, ScalaBigIntegerType))
      case IRLong(longValue) => ReadSuccess(TypeTagged(java.lang.Long.valueOf(longValue), BoxedLongType))
      case IRString(s) if (guidance.isMapKey) => this.read(path, ops.deserialize(s.asInstanceOf[WIRE]).get)(ops, guidance = guidance.copy(isMapKey = false))
      case _ => ReadFailure(path, ReadError.Unsupported("Expected a IR number", reportedBy = self))
    }
}

class NumberIRTransceiver() extends IRTransceiver[Number] with NumberIRReader // no writer for this one!
