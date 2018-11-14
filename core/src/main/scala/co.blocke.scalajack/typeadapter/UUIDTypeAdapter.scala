package co.blocke.scalajack
package typeadapter

import java.util.UUID

object UUIDTypeAdapter extends TypeAdapter.=:=[UUID] {
  override val irTransceiver: IRTransceiver[UUID] = new IRTransceiver[UUID] {

    self =>

    private val UUIDType: Type = typeOf[UUID]

    override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[UUID] =
      ir match {
        case IRNull() => ReadSuccess(TypeTagged(null, UUIDType))
        case IRString(x) => ReadResult(path)(TypeTagged(UUID.fromString(x), UUIDType), {
          case e: IllegalArgumentException => ReadError.Malformed(e, reportedBy = self)
        })
        case _ => ReadFailure(path, ReadError.Unexpected("Expected a JSON string", reportedBy = self))
      }

    override def write[IR, WIRE](tagged: TypeTagged[UUID])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
      tagged match {
        case TypeTagged(null) => WriteSuccess(IRNull())
        case TypeTagged(uuid) => WriteSuccess(IRString(uuid.toString))
      }
  }
}
