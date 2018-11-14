package co.blocke.scalajack
package typeadapter

object LongTypeAdapter extends TypeAdapter.=:=[Long] {
  override val irTransceiver: IRTransceiver[Long] = new IRTransceiver[Long] {

    self =>

    override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[Long] =
      ir match {
        case IRLong(x)                          => ReadSuccess(TypeTagged(x))
        case IRString(s) if (guidance.isMapKey) => this.read(path, ops.deserialize(s.asInstanceOf[WIRE]).get)(ops, guidance = guidance.copy(isMapKey = false))
        case _ =>
          ReadFailure(path, ReadError.Unexpected("Expected a JSON number (long)", reportedBy = self))
      }

    override def write[IR, WIRE](tagged: TypeTagged[Long])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
      WriteSuccess(IRLong(tagged.get.longValue()))
  }
}