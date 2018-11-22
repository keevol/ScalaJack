package co.blocke.scalajack
package mongo
package typeadapter

import java.time.OffsetDateTime

import scala.reflect.runtime.universe.TypeTag

object MongoOffsetDateTimeTypeAdapter extends TypeAdapterFactory.=:=[OffsetDateTime] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[OffsetDateTime]): TypeAdapter[OffsetDateTime] =
    MongoOffsetDateTimeTypeAdapter()

}

case class MongoOffsetDateTimeTypeAdapter() extends TypeAdapter[OffsetDateTime] {
  override val serializer: Serializer[OffsetDateTime] = new MongoOffsetDateTimeSerializer()
  override val deserializer: Deserializer[OffsetDateTime] = new MongoOffsetDateTimeDeserializer()
}
