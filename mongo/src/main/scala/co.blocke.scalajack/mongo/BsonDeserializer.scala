package co.blocke.scalajack
package mongo

import org.bson._
import org.apache.commons.codec.binary.Hex
import scala.collection.JavaConverters._
import mongo.typeadapter.BsonObjectIdTypeAdapter

trait BsonDeserializer[IR] extends WireDeserializer[IR, BsonValue] {

  this: Ops[IR, BsonValue] =>

  override def deserialize(path: Path, wire: BsonValue): DeserializationResult[IR] =
    wire match {
      case b: BsonArray =>
        val (worked, broken) = b.getValues.asScala.zipWithIndex.map { case (item, index) => deserialize(path \ index, item) }
          .partition(_ match {
            case _: DeserializationSuccess[IR] => true
            case _                             => false
          })
        if (broken.isEmpty)
          DeserializationSuccess(IRArray(worked.map(_.get)))
        else {
          val flattened = broken.foldRight(Seq.empty[(Path, ReadError)]) {
            case (res, acc) =>
              acc ++ res.asInstanceOf[DeserializationFailure[IR]].errors
          }.toList
          DeserializationFailure(flattened)
        }

      case b: BsonBinary   => DeserializationSuccess(IRString(Hex.encodeHexString(b.getData())))
      case _: BsonNull     => DeserializationSuccess(IRNull())
      case b: BsonBoolean  => DeserializationSuccess(IRBoolean(b.getValue()))
      case b: BsonDateTime => DeserializationSuccess(IRLong(b.getValue))

      case b: BsonDocument =>
        val entries = b.entrySet().asScala.map(entry => (entry.getKey, deserialize(path \ entry.getKey, entry.getValue)))
        val errors = entries.filter {
          case (_, DeserializationFailure(_)) => true
          case _                              => false
        }
        if (errors.isEmpty) {
          val fixed = entries.map { case (n, v) => (n, v.get) }.toList
          DeserializationSuccess(IRObject(fixed))
        } else {
          val errDetails = errors.map { case (_, v) => v.asInstanceOf[DeserializationFailure[IR]].errors }.flatten.toList
          DeserializationFailure(errDetails)
        }

      case b: BsonDouble   => DeserializationSuccess(IRDouble(b.getValue()))
      case b: BsonInt32    => DeserializationSuccess(IRInt(b.getValue()))
      case b: BsonInt64    => DeserializationSuccess(IRLong(b.getValue()))
      case b: BsonString   => DeserializationSuccess(IRString(b.getValue()))

      // BSON Custom Types
      case b: BsonObjectId => DeserializationSuccess(IRCustom(BsonObjectIdTypeAdapter.CUSTOM_LABEL, IRString(b.getValue().toHexString)))

      // Catch-all for anything unsupported
      case u               => DeserializationFailure(path, ReadError.Unsupported(s"BSON type $u is currently unsupported.", NoTransceiver))
    }
}
