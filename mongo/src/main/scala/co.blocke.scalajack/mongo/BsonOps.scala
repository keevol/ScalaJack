package co.blocke.scalajack
package mongo

import org.bson._
import co.blocke.scalajack.json.Json4sOpsBase
import org.json4s.JsonAST.JValue

object BsonOps extends Ops[JValue, BsonValue] with Json4sOpsBase with BsonDeserializer[JValue] with BsonSerializer[JValue]
