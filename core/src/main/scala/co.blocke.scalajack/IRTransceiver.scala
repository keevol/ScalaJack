package co.blocke.scalajack

trait IRTransceiver[T] {

  def read[IR](path: Path, ir: IR)(implicit ops: OpsBase[IR], guidance: SerializationGuidance): ReadResult[T] =
    ReadFailure(path, ReadError.Unsupported("read() is not implemented on base IRTransceiver", this))

  def readFromNothing[IR](path: Path)(implicit ops: OpsBase[IR]): ReadResult[T] =
    ReadFailure(path, ReadError.Unsupported("readFromNothing() is not implemented on base IRTransceiver", this))

  def write[IR](tagged: TypeTagged[T])(implicit ops: OpsBase[IR], guidance: SerializationGuidance): WriteResult[IR] =
    WriteFailure(WriteError.Unsupported("write() is not implemented on base IRTransceiver"))
}
