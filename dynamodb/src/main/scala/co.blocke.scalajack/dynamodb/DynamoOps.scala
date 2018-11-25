package co.blocke.scalajack
package dynamodb

import co.blocke.scalajack.json.{ Json4sOps, Json4sOpsBase }
import com.amazonaws.services.dynamodbv2.document.Item
import org.json4s.JsonAST.JValue

object DynamoOps extends Ops[JValue, Item] with Json4sOpsBase with DynamoWireTransceiver

trait DynamoWireTransceiver extends WireSerializer[JValue, Item] with WireDeserializer[JValue, Item] {
  this: Ops[JValue, Item] =>

  def serialize(ir: JValue, sj: ScalaJackLike[_, _]): Item =
    Item.fromJSON(Json4sOps.serialize(ir, sj))

  def deserialize(path: Path, wire: Item): DeserializationResult[JValue] =
    Json4sOps.deserialize(path, wire.toJSON())
}
