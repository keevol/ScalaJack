package co.blocke.scalajack
package mongo
package test

import org.bson.types.ObjectId
import org.scalatest.FunSpec
import org.scalatest.Matchers._
import org.mongodb.scala.bson._
import co.blocke.scalajack.typeadapter.BinaryTypeAdapter

case class Lifecycle(a: String, b: Boolean, c: Array[Byte])

class PlugTestHoles extends FunSpec {

  val sj = ScalaJack(MongoFlavor())

  implicit val ops = BsonOps
  implicit val g = SerializationGuidance()

  describe("Plug Holes in Test Coverage (via Coveralls)") {
    it("BsonObjectIdTypeAdapter") {
      val ta = sj.context.typeAdapterOf[ObjectId].resolved
      ta.irTransceiver.read(Path.Root, IRLong(5)).toString should be("""ReadFailure(Vector(($,Expected a Bson ObjectId value (reported by: co.blocke.scalajack.mongo.typeadapter.BsonObjectIdTypeAdapter$$anon$1))))""")
    }
    it("BsonDeserializer") {
      val bson = BsonDocument(List(("a", BsonNull()), ("b", BsonBoolean(true)), ("c", BsonBinary("foo".getBytes))))
      val ir = sj.parse(bson)
      ir.toString should be("DeserializationSuccess(JObject(List((a,JNull), (c,JString(666f6f)), (b,JBool(true)))))")

      val bson2 = BsonSymbol(scala.Symbol("symbol"))
      val bson3 = BsonArray(List(bson2, bson2, bson2))
      sj.parse(bson3).toString should be("""DeserializationFailure(List(($[2],BSON type symbol is currently unsupported. (reported by: co.blocke.scalajack.NoTransceiver$)), ($[1],BSON type symbol is currently unsupported. (reported by: co.blocke.scalajack.NoTransceiver$)), ($[0],BSON type symbol is currently unsupported. (reported by: co.blocke.scalajack.NoTransceiver$))))""")
    }
    it("MongoFlavor") {
      sj.secondLookParsing should be(false)
      sj.withSecondLookParsing().secondLookParsing should be(true)
    }
    it("BsonSerializer") {
      val map: Map[Any, String] = Map(true -> "a", 123 -> "b", "Fred" -> "c")
      val ir = (sj.dematerialize(map)).get
      sj.emit(ir).toString should be("""BsonArray{values=[BsonArray{values=[BsonBoolean{value=true}, BsonString{value='a'}]}, BsonArray{values=[BsonInt32{value=123}, BsonString{value='b'}]}, BsonArray{values=[BsonString{value='Fred'}, BsonString{value='c'}]}]}""")
      val ir2 = IRCustom(BinaryTypeAdapter.CUSTOM_LABEL, IRString("a0a0a0"))
      sj.emit(ir2).toString should be("""BsonBinary{type=0, data=[-96, -96, -96]}""")
      val ir3 = IRDecimal(BigDecimal(123.45))
      sj.emit(ir3).toString should be("BsonDouble{value=123.45}")
    }
    it("Lifecycle") {
      val inst = Lifecycle(null, false, "test".getBytes())
      val one = sj.dematerialize(inst)
      one.toString should be("WriteSuccess(JObject(List((a,JNull), (b,JBool(false)), (c,JString(74657374)))))")
      val two = sj.materialize[Lifecycle](one.get).get
      two.c.toSeq should be(inst.c.toSeq)

      val ir = one.get
      val bson = sj.emit(ir).asInstanceOf[BsonDocument]
      bson.toJson should be("""{ "a" : null, "b" : false, "c" : "74657374" }""")
      sj.parse(bson).get should be(ir)
    }
  }
}
