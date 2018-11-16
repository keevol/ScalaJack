package co.blocke.scalajack
package typeadapter

class FallBackIRTransceiver[T](primary: IRTransceiver[T], secondary: IRTransceiver[T]) extends IRTransceiver[T] {

  override def readFromNothing[IR, WIRE](path: Path)(implicit ops: Ops[IR, WIRE]): ReadResult[T] =
    primary.readFromNothing(path) match {
      case primarySuccess @ ReadSuccess(_) => primarySuccess
      case ReadFailure(primaryErrors) =>
        secondary.readFromNothing(path) match {
          case secondarySuccess @ ReadSuccess(_) => secondarySuccess
          case ReadFailure(secondaryErrors) =>
            ReadFailure(primaryErrors ++ secondaryErrors)
        }
    }

  override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[T] =
    primary.read(path, ir) match {
      case primarySuccess @ ReadSuccess(_) => primarySuccess
      case ReadFailure(primaryErrors) =>
        secondary.read(path, ir) match {
          case secondarySuccess @ ReadSuccess(_) => secondarySuccess
          case ReadFailure(secondaryErrors) =>
            ReadFailure(primaryErrors ++ secondaryErrors)
        }
    }

  // NOTE: No write() for this transceiver... it's never used, so just default.
}

case class FallbackTypeAdapter[T](primaryTypeAdapter: TypeAdapter[T], secondaryTypeAdapter: TypeAdapter[T]) extends TypeAdapter[T] {
  override val irTransceiver = new FallBackIRTransceiver(primaryTypeAdapter.irTransceiver, secondaryTypeAdapter.irTransceiver)
}
