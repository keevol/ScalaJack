package co.blocke.scalajack
package mongo
package test

import java.time._

import org.scalatest.Matchers._
import org.scalatest.FunSpec
import org.mongodb.scala.bson.BsonDocument

case class When1(d: LocalDateTime)
case class When2(d: LocalDate)
case class When3(d: LocalTime)
case class When4(d: OffsetDateTime)
case class When5(d: OffsetTime)
case class When6(d: ZonedDateTime)

class DateTimeSpec extends FunSpec {

  val sj = ScalaJack(MongoFlavor())

  describe("-----------------------------------\n:  Mongo Dates & Times (MongoDB)  :\n-----------------------------------") {
    it("LocalDateTime") {
      val w = When1(LocalDateTime.parse("2007-12-03T10:15:30"))
      val dbo = sj.render(w).asInstanceOf[BsonDocument]
      dbo.toJson should be("""{ "d" : { "$date" : 1196676930000 } }""")
      sj.read[When1](dbo) should be(w)
    }
    it("LocalDate") {
      val w = When2(LocalDate.parse("2007-12-03"))
      val dbo = sj.render(w).asInstanceOf[BsonDocument]
      dbo.toJson should be("""{ "d" : { "$date" : 1196640000000 } }""")
      sj.read[When2](dbo) should be(w)
    }
    it("LocalTime") {
      val w = When3(LocalTime.parse("10:15:30"))
      val dbo = sj.render(w).asInstanceOf[BsonDocument]
      val timeLong = LocalTime.parse("10:15:30").atDate(LocalDate.now()).atZone(ZoneId.of("UTC")).toInstant.toEpochMilli
      dbo.toJson should be(s"""{ "d" : { "$$date" : $timeLong } }""")
      sj.read[When3](dbo) should be(w)
    }
    it("OffsetDateTime") {
      val w = When4(OffsetDateTime.parse("2007-12-03T10:15:30+01:00"))
      val dbo = sj.render(w).asInstanceOf[BsonDocument]
      dbo.toJson should be("""{ "d" : { "$date" : 1196673330000 } }""")
      sj.read[When4](dbo).d.isEqual(w.d) should be(true)
    }
    it("OffsetTime") {
      val w = When5(OffsetTime.parse("10:15:30+01:00"))
      val dbo = sj.render(w).asInstanceOf[BsonDocument]
      val timeLong = OffsetTime.parse("10:15:30+01:00").atDate(LocalDate.now()).toZonedDateTime.withZoneSameInstant(ZoneId.of("UTC")).toInstant.toEpochMilli
      dbo.toJson should be(s"""{ "d" : { "$$date" : $timeLong } }""")
      sj.read[When5](dbo).d.isEqual(w.d) should be(true)
    }
    it("ZonedDateTime") {
      val w = When6(ZonedDateTime.parse("2007-12-03T10:15:30+01:00[Europe/Paris]"))
      val dbo = sj.render(w).asInstanceOf[BsonDocument]
      dbo.toJson should be("""{ "d" : { "$date" : 1196673330000 } }""")
      sj.read[When6](dbo).d.isEqual(w.d) should be(true)
    }
  }
}
