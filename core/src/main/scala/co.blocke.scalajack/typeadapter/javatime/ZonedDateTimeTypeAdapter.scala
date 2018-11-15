package co.blocke.scalajack
package typeadapter
package javatime

import java.time.ZonedDateTime
import java.time.format.{ DateTimeFormatter, DateTimeParseException }

object ZonedDateTimeTypeAdapter extends ZonedDateTimeTypeAdapter(DateTimeFormatter.ISO_ZONED_DATE_TIME)

class ZonedDateTimeTypeAdapter(formatter: DateTimeFormatter) extends TypeAdapter.=:=[ZonedDateTime] {

  val CUSTOM_LABEL = "ZonedDateTime" // Stored as a String (ISO-8601)

  override val irTransceiver = new IRTransceiver[ZonedDateTime] {
    self =>

    private val ZonedDateTimeType: Type = typeOf[ZonedDateTime]

    override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[ZonedDateTime] =
      ir match {
        case IRNull() => ReadSuccess(TypeTagged(null, ZonedDateTimeType))
        case IRString(zonedDateTimeString) =>
          ReadResult(path)(TypeTagged(ZonedDateTime.parse(zonedDateTimeString, formatter), ZonedDateTimeType), {
            case e: DateTimeParseException =>
              ReadError.Malformed(e, reportedBy = self)
          })

        case IRCustom((CUSTOM_LABEL, IRString(zonedDateTimeString))) =>
          ReadResult(path)(TypeTagged(ZonedDateTime.parse(zonedDateTimeString, formatter), ZonedDateTimeType), {
            case e: DateTimeParseException =>
              ReadError.Malformed(e, reportedBy = self)
          })

        case _ =>
          ReadFailure(path, ReadError.Unsupported("Expected a JSON string", reportedBy = self))
      }

    override def write[IR, WIRE](tagged: TypeTagged[ZonedDateTime])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
      tagged match {
        case TypeTagged(null) => WriteSuccess(IRNull())
        case TypeTagged(x)    => WriteSuccess(IRString(x.format(formatter)))
      }
  }
}
