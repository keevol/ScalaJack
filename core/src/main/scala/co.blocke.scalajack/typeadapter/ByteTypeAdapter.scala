package co.blocke.scalajack
package typeadapter

object ByteTypeAdapter extends TypeAdapter.=:=[Byte] {

  override val irTransceiver: IRTransceiver[Byte] = new IRTransceiver[Byte] {

    self =>

    override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[Byte] =
      ir match {
        case IRInt(intValue) if (intValue >= -128 && intValue <= 127) => ReadSuccess(TypeTagged(intValue.byteValue))
        case IRInt(_) => ReadFailure(path, ReadError.Unexpected("Byte value out of range", reportedBy = self))
        case IRLong(longValue) if (longValue >= -128 && longValue <= 127) => ReadSuccess(TypeTagged(longValue.byteValue))
        case IRLong(_) => ReadFailure(path, ReadError.Unexpected("Byte value out of range", reportedBy = self))
        case IRString(s) if (guidance.isMapKey) =>
          try {
            ops.deserialize(s.asInstanceOf[WIRE]).mapToReadResult(path, (dsIR: IR) => this.read(path, dsIR)(ops, guidance = guidance.copy(isMapKey = false)))
          } catch {
            // $COVERAGE-OFF$Not sure how to trigger this! Here for extra safety, really.
            case t: Throwable => ReadFailure(path, ReadError.ExceptionThrown(t))
            // $COVERAGE-ON$
          }
        case _ =>
          ReadFailure(path, ReadError.Unexpected("Expected a JSON number (byte)", reportedBy = self))
      }

    override def write[IR, WIRE](tagged: TypeTagged[Byte])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
      WriteSuccess(IRInt(tagged.get.intValue))
  }
}
