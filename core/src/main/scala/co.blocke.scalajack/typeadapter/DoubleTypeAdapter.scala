package co.blocke.scalajack
package typeadapter

object DoubleTypeAdapter extends TypeAdapter.=:=[Double] {
  override val irTransceiver: IRTransceiver[Double] = new IRTransceiver[Double] {

    self =>

    import NumberConverters._
    override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[Double] =
      ir match {
        case IRDecimal(x)          => ReadResult(path)(TypeTagged(x.toDoubleExact))
        case IRDouble(doubleValue) => ReadSuccess(TypeTagged(doubleValue))
        case IRString(s) if (guidance.isMapKey) =>
          try {
            ops.deserialize(s.asInstanceOf[WIRE]).mapToReadResult(path, (dsIR: IR) => this.read(path, dsIR)(ops, guidance = guidance.copy(isMapKey = false)))
          } catch {
            case t: Throwable => ReadFailure(path, ReadError.ExceptionThrown(t))
          }
        // TODO handle other JSON types
        case _ => ReadFailure(path, ReadError.Unexpected(s"Expected a JSON number, not $ir", reportedBy = self))
      }

    override def write[IR, WIRE](tagged: TypeTagged[Double])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
      WriteSuccess(IRDouble(tagged.get.doubleValue()))
  }
}
