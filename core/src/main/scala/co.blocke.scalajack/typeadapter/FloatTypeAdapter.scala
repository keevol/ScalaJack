package co.blocke.scalajack
package typeadapter

object FloatTypeAdapter extends TypeAdapter.=:=[Float] {
  override val irTransceiver: IRTransceiver[Float] = new IRTransceiver[Float] {

    self =>

    override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[Float] =
      ir match {
        case IRDecimal(bigDecimal) if bigDecimal.isDecimalFloat =>
          ReadResult(path)(TypeTagged(bigDecimal.toFloat), {
            case e: ArithmeticException =>
              ReadError.Malformed(e, reportedBy = self)
          })
        case IRDecimal(_) => ReadFailure(path, ReadError.Unexpected("Float value out of range", reportedBy = self))

        case IRDouble(doubleValue) =>
          ReadResult(path)(TypeTagged(doubleValue.toFloat), {
            case e: ArithmeticException =>
              ReadError.Malformed(e, reportedBy = self)
          })

        case IRLong(longValue) if (longValue >= java.lang.Float.MIN_VALUE && longValue <= java.lang.Float.MAX_VALUE) =>
          ReadResult(path)(TypeTagged(longValue.toFloat), {
            case e: ArithmeticException =>
              ReadError.Malformed(e, reportedBy = self)
          })
        case IRLong(_) => ReadFailure(path, ReadError.Unexpected("Float value out of range", reportedBy = self))

        case IRInt(intValue) =>
          ReadResult(path)(TypeTagged(intValue.toFloat), {
            case e: ArithmeticException =>
              ReadError.Malformed(e, reportedBy = self)
          })

        case IRString(s) if (guidance.isMapKey) =>
          try {
            ops.deserialize(s.asInstanceOf[WIRE]).mapToReadResult(path, (dsIR: IR) => this.read(path, dsIR)(ops, guidance = guidance.copy(isMapKey = false)))
          } catch {
            case t: Throwable => ReadFailure(path, ReadError.ExceptionThrown(t))
          }

        case _ =>
          ReadFailure(path, ReadError.Unexpected("Expected a JSON number", reportedBy = self))
      }

    // Bizzare set of magic to try to "fix" the precision slop when moving from Float->Double (prints extra digits in JSON)
    private def capFloat(f: Float): Double = {
      val d = f.toString.toDouble
      val diff = f.toDouble - d
      f - diff
    }

    override def write[IR, WIRE](tagged: TypeTagged[Float])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
      WriteSuccess(IRDouble(capFloat(tagged.get.floatValue())))
  }
}
