package co.blocke.scalajack
package mongo
package test

import org.bson.types.ObjectId
import org.scalatest.FunSpec
import org.scalatest.Matchers._
import org.mongodb.scala.bson._

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
    }
    it("MongoFlavor") {
      sj.secondLookParsing should be(false)
      sj.withSecondLookParsing().secondLookParsing should be(true)
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
    /*
    it("Ast Transform (transform from one AST to another one") {
      implicit val ops = Json4sOps
      val nums: List[JValue] = List(AstInt(1), AstInt(2), AstInt(3))
      val objstuff: List[(String, JValue)] = List(("a", AstInt(5)), ("b", AstInt(6)))
      val jsonStuff = List(
        ops.applyArray(nums),
        AstBoolean(true),
        AstDecimal(BigDecimal(123.45)),
        AstDouble(12.34),
        AstInt(5),
        AstLong(5L),
        AstNull(),
        ops.applyObject(objstuff),
        AstString("wow"))
      val result = AstValue.transform[JValue, BsonValue, String, Document](ops.applyArray(jsonStuff))(Json4sOps, BsonOps)
      val wrapper = Document("m" -> result)
      assertResult("""{ "m" : [[1, 2, 3], true, { "$numberDecimal" : "123.45" }, 12.34, 5, { "$numberLong" : "5" }, null, { "a" : 5, "b" : 6 }, "wow"] }""")(wrapper.toJson())
    }
    it("materialize, dematerialize, parseToAST, and emitFromAST") {
      val one = One("Greg", List("a", "b"), List(Two("x", false), Two("y", true)), Two("Nest!", true), Some("wow"), Map("hey" -> 17, "you" -> 21), true, 99123986123L, Num.C, 46)
      sj.materialize[One](sj.dematerialize(one)) should be(one)

      val ast = sj.dematerialize(one)
      sj.parseToAST(sj.emitFromAST(ast)) should be(ast)

      implicit val ops = BsonOps
      ops.isArray(ops.applyArray(List(AstInt(1)))) should be(true)
      ops.isArray(AstInt(1)) should be(false)
      ops.isObject(ops.applyObject(List(("a", AstInt(1))))) should be(true)
      ops.isObject(AstInt(1)) should be(false)
    }
    it("ObjectId") {
      implicit val ops = sj.ops
      implicit val g = sj.guidance
      val ta = sj.context.typeAdapterOf[ObjectId].deserializer
      ta.deserialize(Path.Root, AstInt(5)).toString should be("DeserializationFailure(Vector(($,Expected a Bson ObjectId value (reported by: co.blocke.scalajack.mongo.typeadapter.BsonObjectIdDeserializer))))")

      val ta2 = sj.context.typeAdapterOf[String].deserializer
      ta2.deserialize(Path.Root, AstInt(5)).toString should be("DeserializationFailure(Vector(($,Expected a JSON string (reported by: co.blocke.scalajack.typeadapter.StringDeserializer))))")
    }
    */
  }
}
