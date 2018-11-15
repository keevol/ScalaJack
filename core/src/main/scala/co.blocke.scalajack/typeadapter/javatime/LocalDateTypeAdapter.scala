package co.blocke.scalajack
package typeadapter
package javatime

import java.time.LocalDate
import java.time.format.{ DateTimeFormatter, DateTimeParseException }

object LocalDateTypeAdapter extends LocalDateTypeAdapter(DateTimeFormatter.ISO_LOCAL_DATE)

class LocalDateTypeAdapter(formatter: DateTimeFormatter) extends TypeAdapter.=:=[LocalDate] {
  val CUSTOM_LABEL = "LocalDate" // Stored as a String (ISO-8601)

  override val irTransceiver = new IRTransceiver[LocalDate] {
    self =>

    private val LocalDateType: Type = typeOf[LocalDate]

    override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[LocalDate] =
      ir match {
        case IRString(localDateString) =>
          ReadResult(path)(TypeTagged(LocalDate.parse(localDateString, formatter), LocalDateType), {
            case e: DateTimeParseException =>
              ReadError.Malformed(e, reportedBy = self)
          })

        case IRCustom((CUSTOM_LABEL, IRString(localDateString))) =>
          ReadResult(path)(TypeTagged(LocalDate.parse(localDateString, formatter), LocalDateType), {
            case e: DateTimeParseException =>
              ReadError.Malformed(e, reportedBy = self)
          })

        case IRNull() =>
          ReadSuccess(TypeTagged(null, LocalDateType))

        case _ =>
          ReadFailure(path, ReadError.Unsupported("Expected a JSON string", reportedBy = self))
      }

    override def write[IR, WIRE](tagged: TypeTagged[LocalDate])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
      tagged match {
        case TypeTagged(null) => WriteSuccess(IRNull())
        case TypeTagged(x)    => WriteSuccess(IRCustom(CUSTOM_LABEL, IRString(x.format(formatter))))
      }
  }
}