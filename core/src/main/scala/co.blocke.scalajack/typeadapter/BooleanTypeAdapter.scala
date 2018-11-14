package co.blocke.scalajack
package typeadapter

object BooleanTypeAdapter extends TypeAdapter.=:=[Boolean] {
  override val irTransceiver: IRTransceiver[Boolean] = new IRTransceiver[Boolean] { self =>

    override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[Boolean] =
      ir match {
        case IRBoolean(booleanValue)            => ReadSuccess(TypeTagged(booleanValue))
        case IRString(s) if (guidance.isMapKey) => this.read(path, ops.deserialize(s.asInstanceOf[WIRE]).get)(ops, guidance = guidance.copy(isMapKey = false))
        case _                                  => ReadFailure(path, ReadError.Unexpected("Expected a JSON boolean", reportedBy = self))
      }

    override def write[IR, WIRE](tagged: TypeTagged[Boolean])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
      WriteSuccess(IRBoolean(tagged.get.booleanValue()))
  }
}
