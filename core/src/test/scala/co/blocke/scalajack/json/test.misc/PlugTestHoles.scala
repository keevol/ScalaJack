package co.blocke.scalajack
package json
package test.misc

import org.scalatest.FunSpec
import org.scalatest.Matchers._

class PlugTestHoles extends FunSpec {

  val sj = ScalaJack()
  implicit val ops = Json4sOps
  implicit val guidance = SerializationGuidance()

  case class Bogus(name: String)
  class NonCase(age: Int)

  describe("core") {
    it("DeserializationResult") {
      val s = DeserializationSuccess(IRString("Yay"))
      s.errors should be(List.empty)
      val f = DeserializationFailure(ReadError.Missing("foo", sj.context.typeAdapterOf[Byte].resolved.irTransceiver))
      the[UnsupportedOperationException] thrownBy f.get should have message "DeserializationFailure.get not supported"
      assertResult("""DeserializationFailure(Required field foo missing (reported by: co.blocke.scalajack.typeadapter.ByteTypeAdapter$$anon$1))""") { f.toString }
    }
    it("Default IRTransceiver") {
      val t = new IRTransceiver[Int] {}
      assertResult("""ReadFailure(Vector(($,read() is not implemented on base IRTransceiver (reported by: co.blocke.scalajack.json.test.misc.PlugTestHoles$$anon$1))))""") { t.read(Path.Root, IRInt(5)).toString }
      assertResult("""WriteFailure(Unsupported(write() is not implemented on base IRTransceiver))""") { t.write(TypeTagged(5, typeOf[Int])).toString }
    }
    it("IRTransceiverReference") {
      val t1 = new IRTransceiver[Int] {}
      val t2 = new IRTransceiver[Int] {}
      val tr = new IRTransceiverReference(t1)
      val t1trStr = tr.toString
      assertResult(true) { t1trStr.startsWith("""IRTransceiverReference(co.blocke.scalajack.json.test.misc.PlugTestHoles$$anon$2""") }
      assertResult(t1) { tr.referencedTransceiver }
      the[IllegalArgumentException] thrownBy tr.referencedTransceiver_=(null) should have message "requirement failed: Referenced transceiver must not be null"
      tr.referencedTransceiver_=(t2)
      val t2trStr = tr.toString
      assertResult(true) { t2trStr.startsWith("""IRTransceiverReference(co.blocke.scalajack.json.test.misc.PlugTestHoles$$anon$3""") }
      assertResult("""ReadFailure(Vector(($,read() is not implemented on base IRTransceiver (reported by: co.blocke.scalajack.json.test.misc.PlugTestHoles$$anon$3))))""") { tr.read(Path.Root, IRInt(5)).toString }
      assertResult("""WriteFailure(Unsupported(write() is not implemented on base IRTransceiver))""") { tr.write(TypeTagged(5, typeOf[Int])).toString }
    }
    it("LazyTypeAdapter") {
      val z = new LazyTypeAdapter[Int](sj.context, typeOf[Int])
      assertResult(true) { z.resolved.toString.startsWith("""co.blocke.scalajack.typeadapter.IntTypeAdapter""") }
    }
    it("NumberConverters") {
      import NumberConverters._

      val bigD = BigDecimal(12.34)
      val smallF: Float = bigD.toFloatExact
      smallF should be(12.34F)
      the[ArithmeticException] thrownBy BigDecimal(123.4567890).toFloatExact should have message "123.456789 (BigDecimal) cannot be exactly converted to Float (123.45679)"

      val bigD2 = BigDecimal(10)
      val shortOne: Short = bigD2.toShortExact
      shortOne should be(10.toShortExact)
      //        the[ArithmeticException] thrownBy BigDecimal(123456).toShortExact should have message "$bigDecimal (BigDecimal) cannot be exactly converted to Short ($bigDecimalAsShort)"

      123.0D.toFloatExact should be(123.0F)
      the[ArithmeticException] thrownBy 123.456D.toFloatExact should have message "123.456 (Double) cannot be exactly converted to 123.456 (Float)"

      123L.toIntExact should be(123)
      123L.toFloatExact should be(123.0F)
      the[ArithmeticException] thrownBy 12345678901234L.toIntExact should have message "12345678901234 (Long) cannot be exactly converted to Int (1942892530)"
      the[ArithmeticException] thrownBy 12345678901234L.toShortExact should have message "12345678901234 (Long) cannot be exactly converted to Short (12274)"
    }
    it("Path") {
      Path.Unknown.toString should be("???")
      Path.Field(Path.Root, """{"foo":3}""").toString should be("""$.{"foo":3}""")
      val p = Path.Root + "Thing"
      Path.Element(p, 0).toString should be("""$.Thing[0]""")
    }
    it("Reflection") {
      assertResult(typeOf[Null]) { Reflection.inferTypeOf(null) }
    }
    it("ScalaJack") {
      the[ViewException] thrownBy sj.view[Int](null) should have message "Output of view() must be a case class, not Int"
      the[ViewException] thrownBy sj.spliceInto(true, Bogus("hey")) should have message "View must be a case class, not Boolean"
      the[ViewException] thrownBy sj.spliceInto(Bogus("hey"), true) should have message "View must be a case class, not Boolean"
    }
    it("SerializationGuidance") {
      guidance.toString should be(""":: isMapKey: false  isMapValue: false  secondLookParsing: false  inSeq: false""")
    }
    it("TypeAdapter") {
      val ta = sj.context.typeAdapterOf[Int]
      ta.is[TypeAdapter.=:=[Int]] should be(true)
      ta.as[TypeAdapter.=:=[Int]].toString.startsWith("co.blocke.scalajack.typeadapter.IntTypeAdapter") should be(true)
    }
    it("TypeTagged") {
      val tt = TypeTagged(5, typeOf[Int])
      tt.toString should be("""5 as Int""")
    }
    it("WriteResult") {
      val ws = WriteSuccess("ok")
      ws.errors should be(List.empty)
      val wf = WriteFailure[String](WriteError.Unsupported("me"))
      the[UnsupportedOperationException] thrownBy wf.get should have message "WriteFailure.get not supported"
      wf.map[Int](_ => 15) should be(wf.asInstanceOf[WriteResult[Int]])
    }
    it("package") {
      SingleType(typeOf[Int], typeOf[String].typeSymbol) // Not really sure what this does... just exercising the API here.
    }
    describe("typeadapters") {
      describe("javacollections") {
        it("JavaCollectionTypeAdapter") {
          val ta = sj.context.typeAdapterOf[java.util.Vector[String]].irTransceiver
          ta.read(Path.Root, IRBoolean(true)).toString should be("""ReadFailure(Vector(($,Expected a JSON array, not JBool(true) (reported by: co.blocke.scalajack.typeadapter.javacollections.JavaCollectionTypeAdapter$$anon$1))))""")
        }
        it("JavaMapTypeAdapter") {
          val ta = sj.context.typeAdapterOf[java.util.HashMap[String, Int]].irTransceiver
          ta.read(Path.Root, IRBoolean(true)).toString should be("""ReadFailure(Vector(($,Expected a JSON object (reported by: co.blocke.scalajack.typeadapter.javacollections.JavaMapTypeAdapter$$anon$1))))""")
        }
      }
      describe("javaprimitives") {
        it("JavaNumberTypeAdapter") {
          val ta = sj.context.typeAdapterOf[java.lang.Number].irTransceiver
          ta.read(Path.Root, IRDouble(123.456)).toString should be("""ReadSuccess(123.456 as java.lang.Double)""")
          ta.read(Path.Root, IRBoolean(true)).toString should be("""ReadFailure(Vector(($,Expected a JSON number (reported by: co.blocke.scalajack.typeadapter.javaprimitives.JavaNumberTypeAdapter$$anon$1))))""")
        }
      }
    }
  }
}
