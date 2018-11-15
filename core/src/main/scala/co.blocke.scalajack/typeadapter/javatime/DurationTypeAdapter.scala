package co.blocke.scalajack
package typeadapter
package javatime

import java.time.Duration
import java.time.format.DateTimeParseException

object DurationTypeAdapter extends TypeAdapter.=:=[Duration] {

  val CUSTOM_LABEL = "Duration" // Stored as a String (ISO-8601)

  override val irTransceiver = new IRTransceiver[Duration] {
    self =>

    private val DurationType: Type = typeOf[Duration]

    override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[Duration] =
      ir match {
        case IRString(durationString) =>
          ReadResult(path)(TypeTagged(Duration.parse(durationString), DurationType), {
            case e: DateTimeParseException =>
              ReadError.Malformed(e, reportedBy = self)
          })

        case IRCustom((CUSTOM_LABEL, IRString(durationString))) =>
          ReadResult(path)(TypeTagged(Duration.parse(durationString), DurationType), {
            case e: DateTimeParseException =>
              ReadError.Malformed(e, reportedBy = self)
          })

        case IRNull() =>
          ReadSuccess(TypeTagged(null, DurationType))

        case _ =>
          ReadFailure(path, ReadError.Unsupported("Expected a JSON string", reportedBy = self))
      }

    override def write[IR, WIRE](tagged: TypeTagged[Duration])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
      tagged match {
        case TypeTagged(null) => WriteSuccess(IRNull())
        case TypeTagged(x)    => WriteSuccess(IRCustom(CUSTOM_LABEL, IRString(x.toString)))
      }
  }
}
