package co.blocke.scalajack
package typeadapter

object CharTypeAdapter extends TypeAdapter.=:=[Char] {

  override val irTransceiver: IRTransceiver[Char] = new IRTransceiver[Char] {

    self =>

    override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[Char] =
      ir match {
        case IRString(string) if string.length == 1 => ReadSuccess(TypeTagged(string.charAt(0)))
        case IRString(string)                       => ReadFailure(path, ReadError.Malformed(s"Expected a char (JSON string of length 1), not $string", reportedBy = self))
        case _                                      => ReadFailure(path, ReadError.Unexpected("Expected a char (JSON string of length 1)", reportedBy = self))
      }

    override def write[IR, WIRE](tagged: TypeTagged[Char])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
      WriteSuccess(IRString("" + tagged.get))
  }
}
