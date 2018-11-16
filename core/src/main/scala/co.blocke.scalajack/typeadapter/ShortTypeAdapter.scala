package co.blocke.scalajack
package typeadapter

object ShortTypeAdapter extends TypeAdapter.=:=[Short] {
  override val irTransceiver: IRTransceiver[Short] = new IRTransceiver[Short] {

    self =>

    import NumberConverters._
    override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[Short] =
      ir match {
        case IRLong(longValue) if (longValue >= -32768 && longValue <= 32767) => ReadSuccess(TypeTagged(longValue.toShortExact))
        case IRLong(_) => ReadFailure(path, ReadError.Unexpected("Short value out of range", reportedBy = self))
        case IRInt(bigInt) if (bigInt >= -32768 && bigInt <= 32767) => ReadSuccess(TypeTagged(bigInt.toShortExact))
        case IRInt(_) => ReadFailure(path, ReadError.Unexpected("Short value out of range", reportedBy = self))
        case IRString(s) if (guidance.isMapKey) =>
          ops.deserialize(s.asInstanceOf[WIRE]).mapToReadResult(path, (dsIR: IR) => this.read(path, dsIR)(ops, guidance = guidance.copy(isMapKey = false)))
        case _ => ReadFailure(path, ReadError.Unexpected(s"Expected a JSON number (short), not $ir", reportedBy = self))
      }

    override def write[IR, WIRE](tagged: TypeTagged[Short])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
      WriteSuccess(IRInt(tagged.get.intValue))
  }
}
