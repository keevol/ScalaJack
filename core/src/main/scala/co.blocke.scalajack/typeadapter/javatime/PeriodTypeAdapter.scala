package co.blocke.scalajack
package typeadapter
package javatime

import java.time.Period
import java.time.format.DateTimeParseException

object PeriodTypeAdapter extends TypeAdapter.=:=[Period] {

  val CUSTOM_LABEL = "Period" // Stored as a String (ISO-8601)

  override val irTransceiver = new IRTransceiver[Period] {
    self =>

    private val PeriodType: Type = typeOf[Period]

    override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[Period] =
      ir match {
        case IRString(periodString) =>
          ReadResult(path)(TypeTagged(Period.parse(periodString), PeriodType), {
            case e: DateTimeParseException =>
              ReadError.Malformed(e, reportedBy = self)
          })

        case IRCustom((CUSTOM_LABEL, IRString(periodString))) =>
          ReadResult(path)(TypeTagged(Period.parse(periodString), PeriodType), {
            case e: DateTimeParseException =>
              ReadError.Malformed(e, reportedBy = self)
          })

        case IRNull() =>
          ReadSuccess(TypeTagged(null, PeriodType))

        case _ =>
          ReadFailure(path, ReadError.Unsupported("Expected a JSON string", reportedBy = self))
      }

    override def write[IR, WIRE](tagged: TypeTagged[Period])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
      tagged match {
        case TypeTagged(null) => WriteSuccess(IRNull())
        case TypeTagged(x)    => WriteSuccess(IRCustom(CUSTOM_LABEL, IRString(x.toString)))
      }
  }
}
