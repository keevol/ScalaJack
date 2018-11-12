package co.blocke.scalajack
package mongo
package typeadapter

import org.bson.BsonDateTime
import java.time._

class MongoOffsetDateTimeDeserializer() extends Deserializer[OffsetDateTime] {

  self =>

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[OffsetDateTime] =
    ast match {
      case AstNull() => DeserializationSuccess(TypeTagged(null, typeOf[OffsetDateTime]))
      case bsonDateTime: BsonDateTime =>
        val result = ZonedDateTime.ofInstant(Instant.ofEpochMilli(bsonDateTime.getValue()), ZoneId.systemDefault()).toOffsetDateTime
        DeserializationSuccess(TypeTagged(result, typeOf[OffsetDateTime]))
      case _ =>
        DeserializationFailure(path, ReadError.Unexpected("Expected a OffsetDateTime value", reportedBy = self))
    }

}
