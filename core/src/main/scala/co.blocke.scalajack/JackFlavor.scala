package co.blocke.scalajack

trait JackFlavor[IR, WIRE] {
  implicit val guidance: SerializationGuidance
  implicit val ops: Ops[IR, WIRE]

  def render[T](instance: T)(implicit tt: TypeTag[T]): WIRE

  // No exceptions on failure -- Left return on Either for failures
  def readSafely[T](wire: WIRE)(implicit tt: TypeTag[T]): Either[ReadFailure, T]

  def read[T](wire: WIRE)(implicit tt: TypeTag[T]): T =
    readSafely[T](wire) match {
      case Right(x) => x
      case Left(x)  => throw new ReadException(x)
    }

  def parse(wire: WIRE): DeserializationResult[IR]
  def emit(ir: IR): WIRE

  def materialize[T](ir: IR)(implicit tt: TypeTag[T]): ReadResult[T]
  def dematerialize[T](t: T)(implicit tt: TypeTag[T]): WriteResult[IR]
}
