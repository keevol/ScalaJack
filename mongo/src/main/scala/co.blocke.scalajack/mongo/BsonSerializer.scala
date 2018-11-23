package co.blocke.scalajack
package mongo

import java.time._
import java.time.format.DateTimeFormatter

import org.mongodb.scala.bson._
import co.blocke.scalajack.typeadapter.javatime._
import co.blocke.scalajack.typeadapter.BinaryTypeAdapter

import scala.collection.JavaConverters._
import mongo.typeadapter._
import org.apache.commons.codec.binary.Hex

trait BsonSerializer[IR] extends WireSerializer[IR, BsonValue] {

  this: Ops[IR, BsonValue] =>

  override def serialize(ir: IR, sj: ScalaJackLike[_, _]): BsonValue =
    ir match {
      case IRMap(m) =>
        // Figure out if all keys are strings, and if so treat it as an object (BsonDocument), otherwise serialize
        // as key/value pairs.
        val allStrings = m.foldRight(true) {
          case (a, _) =>
            if (IRString.unapply(a._1).isEmpty)
              false
            else
              true
        }
        if (allStrings) // promote to object/document
          new BsonDocument(m.map { case (k, v) => new BsonElement(IRString.unapply(k).get, serialize(v, sj)) }.asJava)
        else // key/value pairs
          new BsonArray(m.map { case (k, v) => new BsonArray(List(serialize(k, sj), serialize(v, sj)).asJava) }.asJava)

      case IRCustom(label, ir) =>
        label match {
          case BinaryTypeAdapter.CUSTOM_LABEL =>
            val IRString(hex) = ir
            BsonBinary(Hex.decodeHex(hex))
          case BsonObjectIdTypeAdapter.CUSTOM_LABEL =>
            val IRString(hex) = ir
            BsonObjectId(hex)
          case ZonedDateTimeTypeAdapter.CUSTOM_LABEL =>
            val IRString(zonedDateTimeString) = ir
            val normalized = ZonedDateTime.parse(zonedDateTimeString, DateTimeFormatter.ISO_ZONED_DATE_TIME)
            BsonDateTime(normalized.toInstant.toEpochMilli)
          case OffsetDateTimeTypeAdapter.CUSTOM_LABEL =>
            val IRString(dateTimeString) = ir
            val normalized = OffsetDateTime.parse(dateTimeString, DateTimeFormatter.ISO_OFFSET_DATE_TIME)
            BsonDateTime(normalized.toZonedDateTime.withZoneSameInstant(ZoneId.of("UTC")).toInstant.toEpochMilli)
          case OffsetTimeTypeAdapter.CUSTOM_LABEL =>
            val IRString(dateTimeString) = ir
            val normalized = OffsetTime.parse(dateTimeString, DateTimeFormatter.ISO_OFFSET_TIME)
            BsonDateTime(normalized.atDate(LocalDate.now()).toZonedDateTime.withZoneSameInstant(ZoneId.of("UTC")).toInstant.toEpochMilli)
          case LocalDateTimeTypeAdapter.CUSTOM_LABEL =>
            val IRString(dateTimeString) = ir
            val normalized = LocalDateTime.parse(dateTimeString, DateTimeFormatter.ISO_LOCAL_DATE_TIME)
            println("Normal: " + normalized)
            val z = BsonDateTime(normalized.atZone(ZoneId.of("UTC")).toInstant.toEpochMilli)
            println("z: " + z)
            z
          case LocalDateTypeAdapter.CUSTOM_LABEL =>
            val IRString(dateTimeString) = ir
            val normalized = LocalDate.parse(dateTimeString, DateTimeFormatter.ISO_LOCAL_DATE)
            BsonDateTime(normalized.atStartOfDay().atZone(ZoneId.of("UTC")).toInstant.toEpochMilli)
          case LocalTimeTypeAdapter.CUSTOM_LABEL =>
            val IRString(dateTimeString) = ir
            val normalized = LocalTime.parse(dateTimeString, DateTimeFormatter.ISO_LOCAL_TIME)
            BsonDateTime(normalized.atDate(LocalDate.now()).atZone(ZoneId.of("UTC")).toInstant.toEpochMilli)
        }

      case IRArray(_) =>
        val bsonVals = mapArrayElements[BsonValue](ir, elementPair => serialize(elementPair._1, sj))
        new BsonArray(bsonVals.asJava)
      case IRBoolean(a) => new BsonBoolean(a)
      case IRDecimal(a) => new BsonDouble(a.doubleValue())
      case IRDouble(a)  => new BsonDouble(a)
      case IRInt(a)     => new BsonInt32(a.intValue)
      case IRLong(a)    => new BsonInt64(a)
      case IRNull()     => new BsonNull()
      case IRObject(_) =>
        new BsonDocument(mapObjectFields(ir, (name, value) => new BsonElement(name, serialize(value, sj))).asJava)
      case IRString(a) => new BsonString(a)
    }
}
