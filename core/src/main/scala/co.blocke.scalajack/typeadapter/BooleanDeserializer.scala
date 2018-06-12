package co.blocke.scalajack
package typeadapter

class BooleanDeserializer extends Deserializer[Boolean] {

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[Boolean] =
    json match {
      case JsonBoolean(booleanValue) => DeserializationSuccess(TypeTagged(booleanValue))
      case _                         => DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON boolean"))
    }

}