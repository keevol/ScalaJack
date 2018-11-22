package co.blocke.scalajack
package mongo

import org.bson._
import org.apache.commons.codec.binary.Hex

trait BsonDeserializer[IR] extends WireDeserializer[IR, BsonValue] {

  this: Ops[IR, BsonValue] =>

  override def deserialize(wire: BsonValue): DeserializationResult[IR] =
    wire match {
      case b: BsonArray =>
        val vvv = b.getValues.toArray.asInstanceOf[Array[BsonValue]]
        DeserializationSuccess(IRArray(vvv.map(v => deserialize(v).getOrElse(IRNull())).toSeq))
      case b: BsonBinary =>
        DeserializationSuccess(IRString(Hex.encodeHexString(b.getData())))
      case BsonNull        => DeserializationSuccess(IRNull())
      case b: BsonBoolean  => DeserializationSuccess(IRBoolean(b.getValue()))
      case b: BsonDateTime => DeserializationSuccess(IRLong(b.getValue))
      case b: BsonDocument =>
        val entries = b.entrySet().toArray.asInstanceOf[Array[java.util.Map.Entry[String, BsonValue]]]
          .map(e => (e.getKey, deserialize(e.getValue).getOrElse(IRNull()))).toList
        DeserializationSuccess(applyObject(entries))
      case b: BsonDouble => DeserializationSuccess(IRDouble(b.getValue()))
      case b: BsonInt32  => DeserializationSuccess(IRInt(b.getValue()))
      case b: BsonInt64  => DeserializationSuccess(IRLong(b.getValue()))
      case b: BsonString => DeserializationSuccess(IRString(b.getValue()))

      // BSON Custom Types
      //      case b: BsonObjectId => DeserializationSuccess(IRCustom(b.getValue.toHexString))
    }
}
