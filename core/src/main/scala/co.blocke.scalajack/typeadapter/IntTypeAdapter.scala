package co.blocke.scalajack
package typeadapter

object IntTypeAdapter extends TypeAdapter.=:=[Int] {
  override val irTransceiver: IRTransceiver[Int] = new IRTransceiver[Int] {

    self =>

    import NumberConverters._
    override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[Int] =
      ir match {
        case IRLong(longValue) if (longValue >= -2147483648 && longValue <= 2147483647) => ReadResult(path)(TypeTagged(longValue.toIntExact))
        case IRLong(_) => ReadFailure(path, ReadError.Unexpected("Int value out of range", reportedBy = self))
        case IRInt(bigInt) => ReadSuccess(TypeTagged(bigInt.intValue))
        case IRString(s) if (guidance.isMapKey) =>
          try {
            ops.deserialize(s.asInstanceOf[WIRE]).mapToReadResult(path, (dsIR: IR) => this.read(path, dsIR)(ops, guidance = guidance.copy(isMapKey = false)))
          } catch {
            // $COVERAGE-OFF$Not sure how to trigger this! Here for extra safety, really.
            case t: Throwable => ReadFailure(path, ReadError.ExceptionThrown(t))
            // $COVERAGE-ON$
          }
        case _ => ReadFailure(path, ReadError.Unexpected(s"Expected a JSON int, not $ir", reportedBy = self))
      }

    override def write[IR, WIRE](tagged: TypeTagged[Int])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
      WriteSuccess(IRInt(tagged.get.intValue))
  }
}
