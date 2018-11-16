package co.blocke.scalajack
package typeadapter

import scala.util.control.NonFatal

class FallBackIRTransceiver[T](primary: IRTransceiver[T], secondary: IRTransceiver[T]) extends IRTransceiver[T] {

  override def readFromNothing[IR, WIRE](path: Path)(implicit ops: Ops[IR, WIRE]): ReadResult[T] =
    readMeFromNothing(primary, path) match {
      case primarySuccess @ ReadSuccess(_) =>
        primarySuccess

      case ReadFailure(primaryErrors) =>
        readMeFromNothing(secondary, path) match {
          case secondarySuccess @ ReadSuccess(_) =>
            secondarySuccess

          case ReadFailure(secondaryErrors) =>
            ReadFailure(primaryErrors ++ secondaryErrors)
        }
    }

  private def readMe[IR, WIRE](xceiver: IRTransceiver[T], path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[T] =
    try xceiver.read(path, ir) catch {
      case NonFatal(e) =>
        ReadFailure(path, ReadError.ExceptionThrown(e))
    }
  private def readMeFromNothing[IR, WIRE](xceiver: IRTransceiver[T], path: Path)(implicit ops: Ops[IR, WIRE]): ReadResult[T] =
    try xceiver.readFromNothing(path) catch {
      case NonFatal(e) =>
        ReadFailure(path, ReadError.ExceptionThrown(e))
    }

  override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[T] =
    readMe(primary, path, ir) match {
      case primarySuccess @ ReadSuccess(_) =>
        primarySuccess

      case ReadFailure(primaryErrors) =>
        readMe(secondary, path, ir) match {
          case secondarySuccess @ ReadSuccess(_) =>
            secondarySuccess

          case ReadFailure(secondaryErrors) =>
            ReadFailure(primaryErrors ++ secondaryErrors)
        }
    }

  // NOTE: No write() for this transceiver... it's never used, so just default.
}

case class FallbackTypeAdapter[T](primaryTypeAdapter: TypeAdapter[T], secondaryTypeAdapter: TypeAdapter[T]) extends TypeAdapter[T] {
  override val irTransceiver = new FallBackIRTransceiver(primaryTypeAdapter.irTransceiver, secondaryTypeAdapter.irTransceiver)
}
