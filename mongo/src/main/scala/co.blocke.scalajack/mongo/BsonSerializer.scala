package co.blocke.scalajack
package mongo

import org.mongodb.scala.bson._

import scala.collection.JavaConverters._
import mongo.typeadapter._

trait BsonSerializer[IR] extends WireSerializer[IR, BsonValue] {

  this: Ops[IR, BsonValue] =>

  override def serialize(ir: IR, sj: ScalaJackLike[_, _]): BsonValue =
    ir match {
      case IRCustom(label, ir) =>
        label match {
          case BsonObjectIdTypeAdapter.CUSTOM_LABEL =>
            val IRString(hex) = ir
            BsonObjectId(new ObjectId(hex))
        }
      case IRArray(a) =>
        val bsonVals = mapArrayElements[BsonValue](ir, elementPair => serialize(elementPair._1, sj))
        new BsonArray(bsonVals.asJava)
      case IRBoolean(a) => new BsonBoolean(a)
      case IRDecimal(a) => new BsonDouble(a.doubleValue())
      case IRDouble(a)  => new BsonDouble(a)
      case IRInt(a)     => new BsonInt32(a)
      case IRLong(a)    => new BsonInt64(a)
      case IRNull()     => new BsonNull()
      case IRMap(m) =>
        // Figure out if all keys are strings, and if so treat it as an object (BsonDocument), otherwise serialize
        // as key/value pairs.
        val allStrings = m.foldRight(true) {
          case (a, b) =>
            if (IRString.unapply(a._1).isEmpty)
              false
            else
              true
        }
        if (allStrings) // promote to object/document
          new BsonDocument(m.map { case (k, v) => new BsonElement(IRString.unapply(k).get, serialize(v, sj)) }.asJava)
        else // key/value pairs
          new BsonArray(m.map { case (k, v) => new BsonArray(List(serialize(k, sj), serialize(v, sj)).asJava) }.asJava)
      case IRObject(a) =>
        new BsonDocument(mapObjectFields(ir, (name, value) => new BsonElement(name, serialize(value, sj))).asJava)
      case IRString(a) => new BsonString(a)
    }
}
