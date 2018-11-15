package co.blocke.scalajack
package typeadapter
package javatime

import java.time.Instant
import java.time.format.DateTimeParseException

object InstantTypeAdapter extends TypeAdapter.=:=[Instant] {
  val CUSTOM_LABEL = "Instant" // Stored as Array[epocSec,nano]

  override val irTransceiver = new IRTransceiver[Instant] {
    self =>

    private val InstantType: Type = typeOf[Instant]

    override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[Instant] =
      ir match {
        case IRString(x) =>
          ReadResult(path)(TypeTagged(Instant.parse(x), InstantType), {
            case e: DateTimeParseException =>
              ReadError.Malformed(e, reportedBy = self)
          })

        case IRCustom((CUSTOM_LABEL, IRArray(epocnano))) =>
          val Seq(IRLong(epocSec), IRInt(nano)) = epocnano
          ReadSuccess(TypeTagged(Instant.ofEpochSecond(epocSec, nano.intValue()), InstantType))

        case IRNull() =>
          ReadSuccess(TypeTagged(null, InstantType))

        case _ =>
          ReadFailure(path, ReadError.Unsupported("Expected a JSON string", reportedBy = self))
      }

    override def write[IR, WIRE](tagged: TypeTagged[Instant])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
      tagged match {
        case TypeTagged(null) => WriteSuccess(IRNull())
        case TypeTagged(x)    => WriteSuccess(IRCustom(CUSTOM_LABEL, IRArray(Seq(IRLong(x.getEpochSecond), IRInt(x.getNano)))))
      }
  }
}
