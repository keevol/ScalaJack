package co.blocke.scalajack
package mongo
package typeadapter

object BsonObjectIdTypeAdapter extends TypeAdapter.===[ObjectId] {
  override val serializer: Serializer[ObjectId] = new BsonObjectIdSerializer()
  override val deserializer: Deserializer[ObjectId] = new BsonObjectIdDeserializer()
}
