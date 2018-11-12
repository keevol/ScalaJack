package co.blocke.scalajack

class DeferredIRTransceiverReference[T](resolve: () => IRTransceiver[T]) extends IRTransceiver[T] {

  private lazy val resolved: IRTransceiver[T] = resolve()

  override def read[IR](path: Path, ir: IR)(implicit ops: OpsBase[IR], guidance: SerializationGuidance): ReadResult[T] =
    resolved.read[IR](path, ir)

  override def write[IR](tagged: TypeTagged[T])(implicit ops: OpsBase[IR], guidance: SerializationGuidance): WriteResult[IR] =
    resolved.write[IR](tagged)
}
