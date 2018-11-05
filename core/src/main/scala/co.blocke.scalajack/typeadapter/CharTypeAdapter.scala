package co.blocke.scalajack
package typeadapter

object CharTypeAdapter extends TypeAdapter.=:=[Char] {
  override val deserializer: Deserializer[Char] = new CharDeserializer
  override val serializer: Serializer[Char] = new CharSerializer
}
