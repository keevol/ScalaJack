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
              // $COVERAGE-OFF$Not sure how to trigger this! Here for extra safety, really.
              ReadError.Malformed(e, reportedBy = self)
            // $COVERAGE-ON$
          })
        // $COVERAGE-OFF$Not sure how to trigger this! Here for extra safety, really.
        case IRDecimal(_) => ReadFailure(path, ReadError.Unexpected("Float value out of range", reportedBy = self))
        // $COVERAGE-ON$

        case IRDouble(doubleValue) =>
          ReadResult(path)(TypeTagged(doubleValue.toFloat), {
            case e: ArithmeticException =>
              // $COVERAGE-OFF$Not sure how to trigger this! Here for extra safety, really.
              ReadError.Malformed(e, reportedBy = self)
            // $COVERAGE-ON$
          })

        case IRLong(longValue) if (longValue >= java.lang.Float.MIN_VALUE && longValue <= java.lang.Float.MAX_VALUE) =>
          ReadResult(path)(TypeTagged(longValue.toFloat), {
            case e: ArithmeticException =>
              // $COVERAGE-OFF$Not sure how to trigger this! Here for extra safety, really.
              ReadError.Malformed(e, reportedBy = self)
            // $COVERAGE-ON$
          })
        // $COVERAGE-OFF$Not sure how to trigger this! Here for extra safety, really.
        case IRLong(_) => ReadFailure(path, ReadError.Unexpected("Float value out of range", reportedBy = self))
        // $COVERAGE-ON$

        case IRInt(intValue) =>
          ReadResult(path)(TypeTagged(intValue.toFloat), {
            case e: ArithmeticException =>
              // $COVERAGE-OFF$Not sure how to trigger this! Here for extra safety, really.
              ReadError.Malformed(e, reportedBy = self)
            // $COVERAGE-ON$
          })

        case IRString(s) if (guidance.isMapKey) =>
          try {
            ops.deserialize(s.asInstanceOf[WIRE]).mapToReadResult(path, (dsIR: IR) => this.read(path, dsIR)(ops, guidance = guidance.copy(isMapKey = false)))
          } catch {
            // $COVERAGE-OFF$Not sure how to trigger this! Here for extra safety, really.
            case t: Throwable => ReadFailure(path, ReadError.ExceptionThrown(t))
            // $COVERAGE-ON$
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
