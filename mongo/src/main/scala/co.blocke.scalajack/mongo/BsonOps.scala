package co.blocke.scalajack
package mongo

import scala.collection.JavaConverters._
import org.bson.types.Decimal128
import org.bson._
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.MongoClient

import co.blocke.scalajack.json.{ Json4sOps, Json4sOpsBase }
import org.json4s.JsonAST.JValue

//trait BsonParser extends Parser[Document] {
//  def _parse[AST](source: Document)(implicit ops: AstOps[AST, BsonValue]): Option[AST] =
//    Some(source.toBsonDocument(classOf[BsonDocument], MongoClient.DEFAULT_CODEC_REGISTRY).asInstanceOf[AST])
//}
//
//trait BsonRenderer extends Renderer[BsonValue] {
//  def _renderCompact[AST](ast: AST, sj: ScalaJackLike[_, _])(implicit ops: AstOps[AST, Document]): Document =
//    Document(ast.asInstanceOf[BsonValue].asDocument)
//}

object BsonOps extends Ops[JValue, BsonValue] with Json4sOpsBase with BsonDeserializer[JValue] with BsonSerializer[JValue]
