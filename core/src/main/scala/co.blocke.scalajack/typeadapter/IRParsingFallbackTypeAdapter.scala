package co.blocke.scalajack
package typeadapter

import scala.util.{ Failure, Success, Try }

object IRParsingFallbackTypeAdapter extends TypeAdapterFactory {

  override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] = {
    val nextTypeAdapter = next.typeAdapterOf[T]

    new IRParsingFallbackTypeAdapter[T](
      new IRTransceiver[T] {
        val next: IRTransceiver[T] = nextTypeAdapter.irTransceiver

        override def readFromNothing[IR, WIRE](path: Path)(implicit ops: Ops[IR, WIRE]): ReadResult[T] =
          next.readFromNothing(path)

        override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[T] =
          next.read(path, ir) match {
            case readSuccess @ ReadSuccess(_) =>
              readSuccess

            case readFailure @ ReadFailure(errors) if readFailure.isUnexpected(path) =>
              ir match {
                case IRString(string) if (guidance.isMapKey || guidance.secondLookParsing) =>
                  Try(ops.deserialize(path, string.asInstanceOf[WIRE]).get) match {
                    case Success(fallbackAst) =>
                      next.read(path, fallbackAst) match {
                        case readSuccess @ ReadSuccess(_) => readSuccess
                        case ReadFailure(_)               => ReadFailure(errors)
                        // Note: We don't accumulate errors here because the initial error is considered ok
                        // for map keys or second look parsing.
                      }

                    case Failure(_) => readFailure
                  }

                case _ => readFailure
              }

            case readFailure @ ReadFailure(_) => readFailure
          }

        override def write[IR, WIRE](tagged: TypeTagged[T])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
          next.write(tagged)

      }, nextTypeAdapter)
  }

}

class IRParsingFallbackTypeAdapter[T](override val irTransceiver: IRTransceiver[T], override val decorated: TypeAdapter[T]) extends DecoratingTypeAdapter[T]