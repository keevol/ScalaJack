package co.blocke.scalajack
package typeadapter

object LongTypeAdapter extends TypeAdapter.=:=[Long] {
  override val irTransceiver: IRTransceiver[Long] = new IRTransceiver[Long] {

    self =>

    override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[Long] =
      ir match {
        case IRLong(x) => ReadSuccess(TypeTagged(x))
        case IRString(s) if (guidance.isMapKey) =>
          try {
            ops.deserialize(s.asInstanceOf[WIRE]).mapToReadResult(path, (dsIR: IR) => this.read(path, dsIR)(ops, guidance = guidance.copy(isMapKey = false)))
          } catch {
            case t: Throwable => ReadFailure(path, ReadError.ExceptionThrown(t))
          }
        case _ =>
          ReadFailure(path, ReadError.Unexpected("Expected a JSON number (long)", reportedBy = self))
      }

    override def write[IR, WIRE](tagged: TypeTagged[Long])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
      WriteSuccess(IRLong(tagged.get.longValue()))
  }
}