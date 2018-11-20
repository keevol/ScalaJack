package co.blocke.scalajack
package json
package test.misc

import org.scalatest.FunSpec
import org.scalatest.Matchers._
import typeadapter._
import java.time._

case class Bogus(name: String)
class NonCase(age: Int)
case class LeftD(name: String, one: Int)
case class RightD(name: String, two: Boolean)
case class VCDouble(vc: Double) extends AnyVal

class PlugTestHoles extends FunSpec {

  val sj = ScalaJack()
  implicit val ops = Json4sOps
  implicit val guidance = SerializationGuidance()

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
    it("Ops") {
      val ir = IRString("wow")
      the[IllegalArgumentException] thrownBy ops.partitionObject(ir, (a: String, i: org.json4s.JValue) => true) should have message "partitionObject() requires IRObject as input"
      // NEED CSV implemented first!!!
      //      implicit val ops = Json4sOps
      //      val nums: List[JValue] = List(AstInt(1), AstInt(2), AstInt(3))
      //      val objstuff: List[(String, JValue)] = List(("a", AstInt(5)), ("b", AstInt(6)))
      //      val jsonStuff = List(
      //        ops.applyArray(nums),
      //        AstBoolean(true),
      //        AstDecimal(BigDecimal(123.45)),
      //        AstDouble(12.34),
      //        AstInt(5),
      //        AstLong(5L),
      //        AstNull(),
      //        ops.applyObject(objstuff),
      //        AstString("wow"))
      //      val result = AstValue.transform[JValue, JValue, String, String](ops.applyArray(jsonStuff))(Json4sOps, CSVOps)
      //      result.toString() should be("""JArray(List(JArray(List(JInt(1), JInt(2), JInt(3))), JBool(true), JDecimal(123.45), JDouble(12.34), JInt(5), JLong(5), JNull, JObject(List((a,JInt(5)), (b,JInt(6)))), JString(wow)))""")
    }
    it("Path") {
      Path.Unknown.toString should be("???")
      Path.Field(Path.Root, """{"foo":3}""").toString should be("""$.{"foo":3}""")
      val p = Path.Root + "Thing"
      Path.Element(p, 0).toString should be("""$.Thing[0]""")
    }
    it("ReadResult") {
      val mt = scala.collection.immutable.Seq.empty
      val re = new ReadException(ReadFailure(mt))
      re.toString should be("""co.blocke.scalajack.ReadException: ReadException(no errors)""")
      val rs = ReadSuccess(TypeTagged("Yay", typeOf[String]))
      rs.isSuccess should be(true)
      val rf = ReadFailure(Path.Root, ReadError.Missing("something", sj.context.typeAdapterOf[Byte].resolved.irTransceiver))
      rf.isFailure should be(true)
      val msg = """ReadException(1 error):
                  |  [$] Required field something missing (reported by: co.blocke.scalajack.typeadapter.ByteTypeAdapter$$anon$1)""".stripMargin
      the[ReadException] thrownBy rf.get should have message msg
    }
    it("Reflection") {
      assertResult(typeOf[Null]) { Reflection.inferTypeOf(null) }
    }
    it("ScalaJack") {
      the[ViewException] thrownBy sj.view[Int](null) should have message "Output of view() must be a case class, not Int"
      the[ViewException] thrownBy sj.spliceInto(true, Bogus("hey")) should have message "View must be a case class, not Boolean"
      the[ViewException] thrownBy sj.spliceInto(Bogus("hey"), true) should have message "Master must be a case class, not Boolean"
    }
    it("SerializationGuidance") {
      guidance.toString should be(""":: isMapKey: false  isMapValue: false  secondLookParsing: false  inSeq: false""")
    }
    it("TypeAdapter") {
      val ta = sj.context.typeAdapterOf[Int]
      ta.is[TypeAdapter.=:=[Int]] should be(true)
      ta.as[TypeAdapter.=:=[Int]].toString.startsWith("co.blocke.scalajack.typeadapter.IntTypeAdapter") should be(true)
      val caught =
        intercept[RuntimeException] {
          ta.as[CaseClassTypeAdapter[Bogus]]
        }
      assert(caught.getMessage.endsWith("is not an instance of class co.blocke.scalajack.typeadapter.CaseClassTypeAdapter"))
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
    describe("json") {
      it("Json4sOps") {
        val arr: ops.ArrayType = IRArray(List(IRInt(1), IRInt(2), IRInt(3))).asInstanceOf[ops.ArrayType]
        ops.getArrayElement(arr, 1).get should be(IRInt(2))
      }
      it("JsonDeserializer") {
        val js = """This is an "easy" \"peasy\" test"""
        ops.deserialize(js).toString should be("""DeserializationSuccess(JString(This is an "easy" \"peasy\" test))""")
      }
      it("JsonDiff") {
        val a = LeftD("Fred", 1)
        val b = RightD("Sally", true)
        val jd = JsonDiff.compare(Path.Root, sj.dematerialize(a).get, sj.dematerialize(b).get)
        jd.toString should be("List(JsonDiff($.name, left: JString(Fred), right: JString(Sally)), JsonDiff($.one, left: JInt(1), right: JNothing), JsonDiff($.two, left: JNothing, right: JBool(true)))")

        implicit val ops = Json4sOps
        val jd2 = JsonDiff.compare(Path.Root, IRString("Foo"), IRString("Bar"))
        jd2.toString should be("List(JsonDiff($, left: JString(Foo), right: JString(Bar)))")
      }
      it("JsonSerializer") {
        val m: Map[Map[Int, String], Boolean] = Map(Map(5 -> "a", 6 -> "b") -> true)
        val sj2 = ScalaJack().isCanonical(false)
        val ir = sj2.dematerialize(m).get
        sj2.emit(ir) should be("""{{5:"a",6:"b"}:true}""")
      }
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
      describe("javatime") {
        it("DurationTypeAdapter") {
          val ir = IRCustom("Duration", IRString("foo"))
          sj.materialize[Duration](ir).toString should be("ReadFailure(Vector(($,Text cannot be parsed to a Duration (reported by: co.blocke.scalajack.typeadapter.javatime.DurationTypeAdapter$$anon$1))))")
        }
        it("LocalDateTimeTypeAdapter") {
          val ir = IRCustom("LocalDateTime", IRString("foo"))
          sj.materialize[LocalDateTime](ir).toString should be("ReadFailure(Vector(($,Text 'foo' could not be parsed at index 0 (reported by: co.blocke.scalajack.typeadapter.javatime.LocalDateTimeTypeAdapter$$anon$1))))")
          val ir2 = IRCustom("LocalDateTime", IRString("2007-12-03T10:15:30"))
          sj.materialize[LocalDateTime](ir2).toString should be("ReadSuccess(2007-12-03T10:15:30 as java.time.LocalDateTime)")
        }
        it("LocalDateTypeAdapter") {
          val ir = IRCustom("LocalDate", IRString("foo"))
          sj.materialize[LocalDate](ir).toString should be("ReadFailure(Vector(($,Text 'foo' could not be parsed at index 0 (reported by: co.blocke.scalajack.typeadapter.javatime.LocalDateTypeAdapter$$anon$1))))")
          val ir2 = IRCustom("LocalDate", IRString("2007-12-03"))
          sj.materialize[LocalDate](ir2).toString should be("ReadSuccess(2007-12-03 as java.time.LocalDate)")
        }
        it("LocalTimeTypeAdapter") {
          val ir = IRCustom("LocalTime", IRString("foo"))
          sj.materialize[LocalTime](ir).toString should be("ReadFailure(Vector(($,Text 'foo' could not be parsed at index 0 (reported by: co.blocke.scalajack.typeadapter.javatime.LocalTimeTypeAdapter$$anon$1))))")
        }
        it("OffsetDateTimeTypeAdapter") {
          val ir = IRCustom("OffsetDateTime", IRString("foo"))
          sj.materialize[OffsetDateTime](ir).toString should be("ReadFailure(Vector(($,Text 'foo' could not be parsed at index 0 (reported by: co.blocke.scalajack.typeadapter.javatime.OffsetDateTimeTypeAdapter$$anon$1))))")
          val ir2 = IRCustom("OffsetDateTime", IRString("2007-12-03T10:15:30+01:00"))
          sj.materialize[OffsetDateTime](ir2).toString should be("ReadSuccess(2007-12-03T10:15:30+01:00 as java.time.OffsetDateTime)")
        }
        it("PeriodTypeAdapter") {
          val ir = IRCustom("Period", IRString("foo"))
          sj.materialize[Period](ir).toString should be("ReadFailure(Vector(($,Text cannot be parsed to a Period (reported by: co.blocke.scalajack.typeadapter.javatime.PeriodTypeAdapter$$anon$1))))")
        }
        it("ZonedDateTimeTypeAdapter") {
          val ir = IRCustom("ZonedDateTime", IRString("foo"))
          sj.materialize[ZonedDateTime](ir).toString should be("ReadFailure(Vector(($,Text 'foo' could not be parsed at index 0 (reported by: co.blocke.scalajack.typeadapter.javatime.ZonedDateTimeTypeAdapter$$anon$1))))")
        }
      }
      it("AnyTypeAdapter") {
        val stuff = Map(List(1, 2, 3) -> true)
        val js = sj.render(stuff)
        val ir = sj.parse(js).get
        sj.materialize[Any](ir).toString should be("""ReadSuccess(Map(List(1, 2, 3) -> true) as Map[Any,Any])""")
        val obj = Bogus("thing")
        val ir2 = sj.parse(sj.render(obj)).get
        sj.materialize[Any](ir2).toString should be("""ReadSuccess(Map(name -> thing) as Map[Any,Any])""")
      }
      it("BigDecimalTypeAdapter") {
        val ir = IRDouble(123.45)
        val x = sj.materialize[BigDecimal](ir)
        x.toString should be("ReadSuccess(123.45 as scala.BigDecimal)")
        val ir2 = IRInt(123)
        val y = sj.materialize[BigDecimal](ir2)
        y.toString should be("ReadSuccess(123 as scala.BigDecimal)")
        val ir3 = IRString("1.2.3")
        val z = sj.materialize[BigDecimal](ir3)
        z.toString should be("ReadFailure(Vector(($,Expected a JSON number, not JString(1.2.3) (reported by: co.blocke.scalajack.typeadapter.BigDecimalTypeAdapter$$anon$1))))")
      }
      it("BigIntTypeAdapter") {
        val ir = IRDouble(123.0)
        val x = sj.materialize[BigInt](ir)
        x.toString should be("ReadSuccess(123 as scala.BigInt)")
        val ir2 = IRDouble(123.4)
        sj.materialize[BigInt](ir2).toString should be("ReadFailure(Vector(($,Can't create a BigInt from 123.4 (reported by: co.blocke.scalajack.typeadapter.BigIntTypeAdapter$$anon$1))))")
      }
      it("ByteTypeAdapter") {
        val ir = IRInt(5)
        sj.materialize[Short](ir).get should be(5)
      }
      it("DerivedValueClassTypeAdapter") {
        val ta = sj.context.typeAdapterOf[VCDouble].resolved
        ta.toString should be("DerivedValueClassAdapter(DerivedValueClassIRTransceiver[co.blocke.scalajack.json.test.misc.VCDouble, scala.Double])")
      }
      it("DoubleTypeAdapter") {
        val ir = IRDouble(12.34)
        sj.materialize[Double](ir).get should be(12.34D)
      }
      it("EnumerationTypeAdapter") {
        val eta = sj.context.typeAdapterOf[Size.Value].irTransceiver
        val ir = IRDouble(12.34)
        eta.read(Path.Root, ir).toString should be("ReadFailure(Vector(($,Expected a JSON string or int (reported by: co.blocke.scalajack.typeadapter.EnumerationIRTransceiver))))")
      }
      it("IntTypeAdapter") {
        val ir = IRLong(15)
        sj.materialize[Int](ir).get should be(15)
      }
      it("OptionTypeAdapter") {
        val ota = sj.context.typeAdapterOf[Option[Int]].as[OptionTypeAdapter[Int]]
        ota.defaultValue should be(Some(None))
        val OptionTypeAdapterNull(a) = ota.as[OptionTypeAdapter[Int]].noneAsNull
        val OptionTypeAdapterEmpty(b) = ota.as[OptionTypeAdapter[Int]].noneAsEmptyString
        a.resolved.getClass.getName should be("co.blocke.scalajack.typeadapter.IntTypeAdapter$")
        b.resolved.getClass.getName should be("co.blocke.scalajack.typeadapter.IntTypeAdapter$")
        a.resolved should be(b.resolved)
        val ir = IRString("")
        sj.materialize[Option[String]](ir).toString should be("ReadSuccess(Some() as scala.Some[String])")
        sj.materialize[Option[Int]](ir).toString should be("ReadSuccess(None as scala.None.type)")
      }
      it("ShortTypeAdapter") {
        val ir = IRInt(5)
        sj.materialize[Byte](ir).get should be(5)
        val ir2 = IRInt(256)
        sj.materialize[Byte](ir2).toString should be("ReadFailure(Vector(($,Byte value out of range (reported by: co.blocke.scalajack.typeadapter.ByteTypeAdapter$$anon$1))))")
      }
      it("TryTypeAdapter") {
        val tts = sj.context.typeAdapterOf[scala.util.Try[Int]].as[TryTypeAdapter[Int]].irTransceiver
        assertThrows[Exception](tts.write(TypeTagged(scala.util.Failure(new Exception("Boom")), typeOf[scala.util.Failure[Int]])))
        tts.read(Path.Root, IRString("boom")).toString should be("""ReadSuccess(Failure(co.blocke.scalajack.typeadapter.TryIRTransceiver$$anon$1: ReadException(1 error):
                                                                           |  [$] Expected a JSON int, not JString(boom) (reported by: co.blocke.scalajack.typeadapter.IntTypeAdapter$$anon$1)) as scala.util.Failure[Int])""".stripMargin)
      }
      it("TupleTypeAdapter") {
        val tup: (Option[Int], String) = (Some(5), "yay")
        val ir = sj.dematerialize(tup).get
        val a = sj.materialize[(Option[Int], String)](ir).get
        a should be(tup)
        val js = """{"a":5}"""
        val msg = """ReadException(1 error):
                    |  [$] Expected a JSON array, not JObject(List((a,JLong(5)))) (reported by: co.blocke.scalajack.typeadapter.TupleIRTransceiver)""".stripMargin
        the[ReadException] thrownBy sj.read[(Option[Int], String)](js) should have message msg
      }
    }
  }
}
