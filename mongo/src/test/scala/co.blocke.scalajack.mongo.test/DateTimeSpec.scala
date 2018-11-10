package co.blocke.scalajack
package mongo
package test

import java.time._
import org.mongodb.scala.bson._
import typeadapter._
import org.scalatest.Matchers._
import org.scalatest.FunSpec

case class When(d: MongoTime)

class DateTimeSpec extends FunSpec {

  val sj = ScalaJack(MongoFlavor())

  describe("-----------------------------------\n:  Mongo Dates & Times (MongoDB)  :\n-----------------------------------") {
    it("OffsetDateTime support") {
      val t = OffsetDateTime.parse("2018-10-30T18:04:58.874-05:00")
      val thing = JodaThing("Foo", t, List(t, t, null.asInstanceOf[OffsetDateTime]), Some(t))
      val dbo = sj.render(thing)
      dbo.toJson should equal("""{ "name" : "Foo", "dt" : { "$date" : 1540940698874 }, "many" : [{ "$date" : 1540940698874 }, { "$date" : 1540940698874 }, null], "maybe" : { "$date" : 1540940698874 } }""")
      val b = sj.read[JodaThing](dbo)
      b should equal(thing)

      implicit val ops = sj.ops
      implicit val g = sj.guidance
      val ta = sj.context.typeAdapterOf[OffsetDateTime].as[MongoOffsetDateTimeTypeAdapter].deserializer
      ta.deserialize(Path.Root, AstInt(50)).toString should be("DeserializationFailure(Vector(($,Expected a OffsetDateTime value (reported by: co.blocke.scalajack.mongo.typeadapter.MongoOffsetDateTimeDeserializer))))")
    }
    it("ZonedDateTime") {
      implicit val ops = sj.ops
      implicit val g = sj.guidance
      val ta = sj.context.typeAdapterOf[ZonedDateTime].as[MongoZonedDateTimeTypeAdapter].deserializer
      ta.deserialize(Path.Root, AstInt(50)).toString should be("DeserializationFailure(Vector(($,Expected a ZonedDateTime value (reported by: co.blocke.scalajack.mongo.typeadapter.MongoZonedDateTimeDeserializer))))")
    }
    it("DateTime") {
      val w = When(LocalDateTime.parse( // Parse into an object representing a date with a time-of-day but without time zone and without offset-from-UTC.
        "2014/10/29 18:10:45" // Convert input string to comply with standard ISO 8601 format.
          .replace(" ", "T") // Replace SPACE in the middle with a `T`.
          .replace("/", "-") // Replace SLASH in the middle with a `-`.
      )
        .atZone( // Apply a time zone to provide the context needed to determine an actual moment.
          ZoneId.of("Europe/Oslo") // Specify the time zone you are certain was intended for that input.
        ) // Returns a `ZonedDateTime` object.
        .toInstant() // Adjust into UTC.
        .toEpochMilli())
      val doc = sj.render(w)
      sj.read[When](doc) should be(w)

      val sjJson = ScalaJack()
      val ast = sj.dematerialize(w)
      println(ast)
      //  def transform[ASTA, ASTB, SRCA, SRCB](source: ASTA)(implicit sourceOps: AstOps[ASTA, SRCA], targetOps: AstOps[ASTB, SRCB]): ASTB =
      //      val ast = AstValue.transform[BsonValue, Document, org.json4s.JsonAST.JValue, String](doc)(sj.ops, sjJson.ops)
      val ast2 = AstValue.transform(ast)(sj.ops, sjJson.ops)
      println(ast2)
    }
  }
}
