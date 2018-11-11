package co.blocke.scalajack
package mongo

import org.bson._
import org.apache.commons.codec.binary.Hex

trait BsonParser extends Parser[BsonValue] {
  def _parse[AST](source: BsonValue)(implicit ops: AstOps[AST, BsonValue]): Option[AST] =
    source match {
      case b: BsonArray =>
        val vvv = b.getValues.toArray.asInstanceOf[Array[BsonValue]]
        Some(ops.applyArray(vvv.map(v => _parse(v).getOrElse(AstNull())).toList))
      case b: BsonBinary =>
        Some(AstString(Hex.encodeHexString(b.getData())))
      case BsonNull        => Some(AstNull())
      case b: BsonBoolean  => Some(AstBoolean(b.getValue()))
      case b: BsonDateTime => Some(AstLong(b.getValue))
      case b: BsonDocument =>
        val entries = b.entrySet().toArray.asInstanceOf[Array[java.util.Map.Entry[String, BsonValue]]]
          .map(e => (e.getKey, _parse(e.getValue).getOrElse(AstNull()))).toList
        Some(ops.applyObject(entries))
      case b: BsonDouble   => Some(AstDouble(b.getValue()))
      case b: BsonInt32    => Some(AstInt(b.getValue()))
      case b: BsonInt64    => Some(AstLong(b.getValue()))
      case b: BsonObjectId => Some(AstString(b.getValue.toHexString))
      case b: BsonString   => Some(AstString(b.getValue()))
    }
}
