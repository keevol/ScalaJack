package co.blocke.scalajack
package mongo

import org.bson._
import scala.collection.JavaConverters._

trait BsonRenderer extends Renderer[BsonValue] {
  def _renderCompact[AST](ast: AST, sj: ScalaJackLike[_, _])(implicit ops: AstOps[AST, BsonValue]): BsonValue =
    ast match {
      case AstCustom(a) => a
      case AstArray(a) =>
        val bsonVals = ops.mapArrayElements(ast.asInstanceOf[ops.ArrayElements], (_, element) => _renderCompact(element, sj))
        new BsonArray(bsonVals.asJava)
      case AstBoolean(a) => new BsonBoolean(a)
      case AstDecimal(a) => new BsonDouble(a.doubleValue())
      case AstDouble(a)  => new BsonDouble(a)
      case AstInt(a)     => new BsonInt32(a)
      case AstLong(a)    => new BsonInt64(a)
      case AstNull()     => new BsonNull()
      case AstObject(a) =>
        val bsonVals = ops.map(ast.asInstanceOf[ops.ObjectFields], (name, value) => new BsonElement(name, _renderCompact(value, sj)))
        new BsonDocument(bsonVals.asJava)
      case AstString(a) => new BsonString(a)
    }
}

object AstCustom {
  final def unapply[AST, S](ast: AST)(implicit ops: AstOps[AST, S]): Option[BsonValue] = ast match {
    case AstArray(a) =>
      ops.getArrayElement(a.asInstanceOf[ops.ArrayElements], 0) match {
        case Some(AstString(s)) if s == "ASTCustom" => ops.getArrayElement(a.asInstanceOf[ops.ArrayElements], 1) match {

          case Some(AstString(s)) if s == "ObjectId" => ops.getArrayElement(a.asInstanceOf[ops.ArrayElements], 2) match {
            case Some(AstString(s)) => Some(new BsonObjectId(new org.bson.types.ObjectId(s)))
            case _                  => None
          }

          case _ => None
          //        case Some(AstString(s)) if s == "BsonBinary" => ops.getArrayElement(a.asInstanceOf[ops.ArrayElements],1) match {
          //          case Some(AstString(s)) => Some(new BsonObjectId( new org.bson.types.ObjectId(s) ))
          //          case _ => None
          //        }
        }
        case _ => None
      }
  }
}

// ObjectId --> AstObject("ASTCustom",Name,Value) --> BsonObjectId
