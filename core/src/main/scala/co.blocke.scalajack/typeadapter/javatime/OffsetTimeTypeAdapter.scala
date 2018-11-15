package co.blocke.scalajack
package typeadapter
package javatime

import java.time.OffsetTime
import java.time.format.{ DateTimeFormatter, DateTimeParseException }

object OffsetTimeTypeAdapter extends OffsetTimeTypeAdapter(DateTimeFormatter.ISO_OFFSET_TIME)

class OffsetTimeTypeAdapter(formatter: DateTimeFormatter) extends TypeAdapter.=:=[OffsetTime] {

  val CUSTOM_LABEL = "OffsetTime" // Stored as a String (ISO-8601)

  override val irTransceiver = new IRTransceiver[OffsetTime] {
    self =>

    private val OffsetTimeType: Type = typeOf[OffsetTime]

    override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[OffsetTime] =
      ir match {
        case IRString(offsetTimeString) =>
          ReadResult(path)(TypeTagged(OffsetTime.parse(offsetTimeString, formatter), OffsetTimeType), {
            case e: DateTimeParseException =>
              ReadError.Malformed(e, reportedBy = self)
          })

        case IRCustom((CUSTOM_LABEL, IRString(offsetTimeString))) =>
          ReadResult(path)(TypeTagged(OffsetTime.parse(offsetTimeString, formatter), OffsetTimeType), {
            case e: DateTimeParseException =>
              ReadError.Malformed(e, reportedBy = self)
          })

        case IRNull() =>
          ReadSuccess(TypeTagged(null, OffsetTimeType))

        case _ =>
          ReadFailure(path, ReadError.Unsupported("Expected a JSON string", reportedBy = self))
      }

    override def write[IR, WIRE](tagged: TypeTagged[OffsetTime])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
      tagged match {
        case TypeTagged(null) => WriteSuccess(IRNull())
        case TypeTagged(x)    => WriteSuccess(IRCustom(CUSTOM_LABEL, IRString(x.format(formatter))))
      }
  }
}
