package co.blocke.scalajack
package typeadapter
package javatime

import java.time.LocalTime
import java.time.format.{ DateTimeFormatter, DateTimeParseException }

object LocalTimeTypeAdapter extends LocalTimeTypeAdapter(DateTimeFormatter.ISO_LOCAL_TIME)

class LocalTimeTypeAdapter(formatter: DateTimeFormatter) extends TypeAdapter.=:=[LocalTime] {
  val CUSTOM_LABEL = "LocalTime" // Stored as a String (ISO-8601)

  override val irTransceiver = new IRTransceiver[LocalTime] {
    self =>

    private val LocalTimeType: Type = typeOf[LocalTime]

    override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[LocalTime] =
      ir match {
        case IRString(localTimeString) =>
          ReadResult(path)(TypeTagged(LocalTime.parse(localTimeString, formatter), LocalTimeType), {
            case e: DateTimeParseException =>
              ReadError.Malformed(e, reportedBy = self)
          })

        case IRCustom((CUSTOM_LABEL, IRString(localTimeString))) =>
          ReadResult(path)(TypeTagged(LocalTime.parse(localTimeString, formatter), LocalTimeType), {
            case e: DateTimeParseException =>
              ReadError.Malformed(e, reportedBy = self)
          })

        case IRNull() =>
          ReadSuccess(TypeTagged(null, LocalTimeType))

        case _ =>
          ReadFailure(path, ReadError.Unsupported("Expected a JSON string", reportedBy = self))
      }

    override def write[IR, WIRE](tagged: TypeTagged[LocalTime])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
      tagged match {
        case TypeTagged(null) => WriteSuccess(IRNull())
        case TypeTagged(x)    => WriteSuccess(IRCustom(CUSTOM_LABEL, IRString(x.format(formatter))))
      }

  }
}
