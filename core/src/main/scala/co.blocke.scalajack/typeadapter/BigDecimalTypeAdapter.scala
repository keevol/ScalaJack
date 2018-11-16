package co.blocke.scalajack
package typeadapter

object BigDecimalTypeAdapter extends TypeAdapter.=:=[BigDecimal] {
  override val irTransceiver: IRTransceiver[BigDecimal] = new IRTransceiver[BigDecimal] {

    self =>

    private val BigDecimalType: Type = typeOf[BigDecimal]
    private val taggedNull: TypeTagged[BigDecimal] = TypeTagged[BigDecimal](null, BigDecimalType)

    override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[BigDecimal] =
      ir match {
        case IRNull()     => ReadSuccess(taggedNull)
        case IRDecimal(x) => ReadSuccess(TypeTagged(x, BigDecimalType))
        case IRDouble(x)  => ReadResult(path)(TypeTagged(BigDecimal(x), BigDecimalType))
        case IRInt(x)     => ReadResult(path)(TypeTagged(BigDecimal(x), BigDecimalType))
        case IRLong(x)    => ReadResult(path)(TypeTagged(BigDecimal(x), BigDecimalType))
        case IRString(s) if (guidance.isMapKey) =>
          ops.deserialize(s.asInstanceOf[WIRE]).mapToReadResult(path, (dsIR: IR) => this.read(path, dsIR)(ops, guidance = guidance.copy(isMapKey = false)))
        case _ => ReadFailure(path, ReadError.Unexpected(s"Expected a JSON number, not $ir", reportedBy = self))
      }

    override def write[IR, WIRE](tagged: TypeTagged[BigDecimal])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
      tagged match {
        case TypeTagged(null)       => WriteSuccess(IRNull())
        case TypeTagged(bigDecimal) => WriteSuccess(IRDecimal(bigDecimal))
      }
  }
}
