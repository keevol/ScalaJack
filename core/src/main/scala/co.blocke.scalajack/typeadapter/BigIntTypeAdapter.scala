package co.blocke.scalajack
package typeadapter

object BigIntTypeAdapter extends TypeAdapter.=:=[BigInt] {
  override val irTransceiver: IRTransceiver[BigInt] = new IRTransceiver[BigInt] {

    self =>

    private val BigIntType: Type = typeOf[BigInt]

    override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[BigInt] =
      ir match {
        case IRNull()           => ReadSuccess(TypeTagged(null, BigIntType))
        case IRLong(longValue)  => ReadSuccess(TypeTagged(BigInt(longValue), BigIntType))
        case IRInt(scalaBigInt) => ReadSuccess(TypeTagged(scalaBigInt, BigIntType))

        case IRDecimal(scalaBigDecimal) =>
          ReadResult(path)(TypeTagged(BigInt(scalaBigDecimal.bigDecimal.toBigIntegerExact), BigIntType), {
            case e: ArithmeticException =>
              ReadError.Malformed(e, reportedBy = self)
          })

        case IRDouble(doubleValue) =>
          BigDecimal.apply(doubleValue).toBigIntExact match {
            case Some(x) => ReadSuccess(TypeTagged(x, typeOf[BigInt]))
            case None    => ReadFailure.apply(path, ReadError.Malformed(s"Can't create a BigInt from $doubleValue", self))
          }

        case IRString(s) if (guidance.isMapKey) => this.read(path, ops.deserialize(s.asInstanceOf[WIRE]).get)(ops, guidance = guidance.copy(isMapKey = false))

        case _                                  => ReadFailure(path, ReadError.Unexpected("Expected a JSON number (integer value)", reportedBy = self))
      }

    override def write[IR, WIRE](tagged: TypeTagged[BigInt])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
      tagged match {
        case TypeTagged(null)   => WriteSuccess(IRNull())
        case TypeTagged(bigInt) => WriteSuccess(IRInt(bigInt))
      }
  }
}
