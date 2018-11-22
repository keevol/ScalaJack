package co.blocke.scalajack

trait IRReader[T] {

  this: IRTransceiver[T] =>

  def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[T] =
    ReadFailure(path, ReadError.Unsupported("read() is not implemented on base IRTransceiver", this))

  def readFromNothing[IR, WIRE](path: Path)(implicit ops: Ops[IR, WIRE]): ReadResult[T] =
    ReadFailure(path, ReadError.Unsupported("readFromNothing() is not implemented on base IRTransceiver", this))
}

trait IRWriter[T] {
  def write[IR, WIRE](tagged: TypeTagged[T])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
    WriteFailure(WriteError.Unsupported("write() is not implemented on base IRTransceiver"))
}

trait IRTransceiver[T] extends IRReader[T] with IRWriter[T]

object NoTransceiver extends IRTransceiver[Nothing]
