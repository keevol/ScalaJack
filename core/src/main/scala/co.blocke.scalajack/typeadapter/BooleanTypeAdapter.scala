package co.blocke.scalajack
package typeadapter

object BooleanTypeAdapter extends TypeAdapter.=:=[Boolean] {
  override val irTransceiver: IRTransceiver[Boolean] = new IRTransceiver[Boolean] { self =>

    override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[Boolean] =
      ir match {
        case IRBoolean(booleanValue) => ReadSuccess(TypeTagged(booleanValue))
        case IRString(s) if (guidance.isMapKey) =>
          try {
            ops.deserialize(s.asInstanceOf[WIRE]).mapToReadResult(path, (dsIR: IR) => this.read(path, dsIR)(ops, guidance = guidance.copy(isMapKey = false)))
          } catch {
            case t: Throwable => ReadFailure(path, ReadError.ExceptionThrown(t))
          }
        case _ => ReadFailure(path, ReadError.Unexpected("Expected a JSON boolean", reportedBy = self))
      }

    override def write[IR, WIRE](tagged: TypeTagged[Boolean])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
      WriteSuccess(IRBoolean(tagged.get.booleanValue()))
  }
}
