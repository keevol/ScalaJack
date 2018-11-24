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
      case IRDecimal(scalaBigDecimal) if (!scalaBigDecimal.isDecimalDouble) => ReadSuccess(TypeTagged(scalaBigDecimal, ScalaBigDecimalType))
      case IRDecimal(scalaBigDecimal) => ReadSuccess(TypeTagged(java.lang.Double.valueOf(scalaBigDecimal.doubleValue()), BoxedDoubleType))
      case IRDouble(doubleValue) => ReadSuccess(TypeTagged(java.lang.Double.valueOf(doubleValue), BoxedDoubleType))
      case IRInt(scalaBigInt) => ReadSuccess(TypeTagged(scalaBigInt, ScalaBigIntegerType))
      case IRLong(longValue) => ReadSuccess(TypeTagged(java.lang.Long.valueOf(longValue), BoxedLongType))

      // Coverage turned off here.  NumberIRTransceiver is currently only used for reading Double, Decimal, Int, or Long
      //     for Any-typed values.  Therefore it's *currently* impossible to receive an IRNull, IRString, or _ (anything that's
      //     not a Decimal, Double, Int, or Long.
      // $COVERAGE-OFF$
      case IRNull() => ReadSuccess(TypeTagged(null, BoxedNumberType))
      case IRString(s) if (guidance.isMapKey) =>
        try {
          ops.deserialize(path, s.asInstanceOf[WIRE]).mapToReadResult(path, (dsIR: IR) => this.read(path, dsIR)(ops, guidance = guidance.copy(isMapKey = false)))
        } catch {
          case t: Throwable => ReadFailure(path, ReadError.ExceptionThrown(t))
        }
      case _ => ReadFailure(path, ReadError.Unsupported("Expected a IR number", reportedBy = self))
      // $COVERAGE-ON$
    }
}

class NumberIRTransceiver() extends IRTransceiver[Number] with NumberIRReader // no writer for this one!
