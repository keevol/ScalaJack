package co.blocke.scalajack
package typeadapter
package javatime

import java.time.{ LocalDateTime, ZonedDateTime }
import java.time.format.{ DateTimeFormatter, DateTimeParseException }

object LocalDateTimeTypeAdapter extends LocalDateTimeTypeAdapter(DateTimeFormatter.ISO_LOCAL_DATE_TIME)

class LocalDateTimeTypeAdapter(formatter: DateTimeFormatter) extends TypeAdapter.=:=[LocalDateTime] {
  val CUSTOM_LABEL = "LocalDateTime" // Stored as a String (ISO-8601)

  override val irTransceiver = new IRTransceiver[LocalDateTime] {
    self =>

    private val LocalDateTimeType: Type = typeOf[LocalDateTime]

    override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[LocalDateTime] =
      ir match {
        case IRString(localDateTimeString) =>
          ReadResult(path)(TypeTagged(LocalDateTime.parse(localDateTimeString, formatter), LocalDateTimeType), {
            case e: DateTimeParseException =>
              ReadError.Malformed(e, reportedBy = self)
          })

        case IRCustom((CUSTOM_LABEL, IRString(localDateTimeString))) =>
          ReadResult(path)(TypeTagged(LocalDateTime.parse(localDateTimeString, formatter), LocalDateTimeType), {
            case e: DateTimeParseException =>
              ReadError.Malformed(e, reportedBy = self)
          })
        case IRCustom((ZonedDateTimeTypeAdapter.CUSTOM_LABEL, IRString(zonedDateTimeString))) =>
          ReadResult(path)({
            val zoned = ZonedDateTime.parse(zonedDateTimeString, DateTimeFormatter.ISO_ZONED_DATE_TIME)
            TypeTagged(zoned.toLocalDateTime, LocalDateTimeType)
          }, {
            case e: DateTimeParseException => ReadError.Malformed(e, reportedBy = self)
          })

        case IRNull() =>
          ReadSuccess(TypeTagged(null, LocalDateTimeType))

        case _ =>
          ReadFailure(path, ReadError.Unsupported("Expected a JSON string", reportedBy = self))
      }

    override def write[IR, WIRE](tagged: TypeTagged[LocalDateTime])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
      tagged match {
        case TypeTagged(null) => WriteSuccess(IRNull())
        case TypeTagged(x)    => WriteSuccess(IRCustom(CUSTOM_LABEL, IRString(x.format(formatter))))
      }

  }
}
