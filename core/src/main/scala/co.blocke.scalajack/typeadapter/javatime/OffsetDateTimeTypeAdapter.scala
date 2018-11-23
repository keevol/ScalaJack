package co.blocke.scalajack
package typeadapter
package javatime

import java.time.{ ZonedDateTime, OffsetDateTime }
import java.time.format.{ DateTimeFormatter, DateTimeParseException }

object OffsetDateTimeTypeAdapter extends OffsetDateTimeTypeAdapter(DateTimeFormatter.ISO_OFFSET_DATE_TIME)

class OffsetDateTimeTypeAdapter(formatter: DateTimeFormatter) extends TypeAdapter.=:=[OffsetDateTime] {

  val CUSTOM_LABEL = "OffsetDateTime" // Stored as a String (ISO-8601)

  override val irTransceiver = new IRTransceiver[OffsetDateTime] {
    self =>

    private val OffsetDateTimeType: Type = typeOf[OffsetDateTime]

    override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[OffsetDateTime] =
      ir match {
        case IRString(offsetDateTimeString) =>
          ReadResult(path)(TypeTagged(OffsetDateTime.parse(offsetDateTimeString, formatter), OffsetDateTimeType), {
            case e: DateTimeParseException =>
              ReadError.Malformed(e, reportedBy = self)
          })

        case IRCustom((CUSTOM_LABEL, IRString(offsetDateTimeString))) =>
          ReadResult(path)(TypeTagged(OffsetDateTime.parse(offsetDateTimeString, formatter), OffsetDateTimeType), {
            case e: DateTimeParseException =>
              ReadError.Malformed(e, reportedBy = self)
          })
        case IRCustom((ZonedDateTimeTypeAdapter.CUSTOM_LABEL, IRString(zonedDateTimeString))) =>
          ReadResult(path)({
            val zoned = ZonedDateTime.parse(zonedDateTimeString, DateTimeFormatter.ISO_ZONED_DATE_TIME)
            TypeTagged(zoned.toOffsetDateTime, OffsetDateTimeType)
          }, {
            case e: DateTimeParseException =>
              ReadError.Malformed(e, reportedBy = self)
          })

        case IRNull() =>
          ReadSuccess(TypeTagged(null, OffsetDateTimeType))

        case x =>
          println(x)
          ReadFailure(path, ReadError.Unsupported("Expected a JSON string", reportedBy = self))
      }

    override def write[IR, WIRE](tagged: TypeTagged[OffsetDateTime])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
      tagged match {
        case TypeTagged(null) => WriteSuccess(IRNull())
        case TypeTagged(x)    => WriteSuccess(IRCustom(CUSTOM_LABEL, IRString(x.format(formatter))))
      }
  }
}
