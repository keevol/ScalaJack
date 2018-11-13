package co.blocke.scalajack
package typeadapter

object StringTypeAdapter extends TypeAdapter.=:=[String] {
  override val irTransceiver: IRTransceiver[String] = new IRTransceiver[String] {

    self =>

    private val StringType: Type = typeOf[String]

    override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[String] =
      ir match {
        case IRNull()        => ReadSuccess(TypeTagged(null, StringType))
        case IRString(value) => ReadSuccess(TypeTagged(value, StringType))
        case _               => ReadFailure(path, ReadError.Unexpected("Expected a JSON string", reportedBy = self))
      }

    override def write[IR](tagged: TypeTagged[String])(implicit ops: OpsBase[IR], guidance: SerializationGuidance): WriteResult[IR] =
      tagged match {
        case TypeTagged(null)  => WriteSuccess(IRNull())
        case TypeTagged(value) => WriteSuccess(IRString(value))
      }
  }
}
