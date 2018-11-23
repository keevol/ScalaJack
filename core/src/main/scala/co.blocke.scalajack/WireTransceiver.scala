package co.blocke.scalajack

trait WireSerializer[IR, WIRE] {
  this: Ops[IR, WIRE] =>
  def serialize(ir: IR, sj: ScalaJackLike[_, _]): WIRE
}

trait WireDeserializer[IR, WIRE] {
  this: Ops[IR, WIRE] =>
  def deserialize(path: Path, wire: WIRE): DeserializationResult[IR]
}

trait WireTransceiver[IR, WIRE] extends WireSerializer[IR, WIRE] with WireDeserializer[IR, WIRE] {
  this: Ops[IR, WIRE] =>
}
