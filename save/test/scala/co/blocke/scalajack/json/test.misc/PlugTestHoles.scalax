package co.blocke.scalajack
package json
package test.misc

import co.blocke.scalajack.typeadapter.javacollections.JavaMapTypeAdapter
import org.scalatest.FunSpec
import org.scalatest.Matchers._
import org.json4s.JsonAST._
import csv.{ CSVFlavor, CSVOps }
import typeadapter._
import typeadapter.javacollections._
import typeadapter.javaprimitives._

case class Bloke(name: String)
case class Strs(a: String, b: String, c: String)
case class BoomCsv(m: List[Int])

case class LeftD(name: String, one: Int)
case class RightD(name: String, two: Boolean)

case class JMap(jm: java.util.Map[String, Int])

class PlugTestHoles extends FunSpec {

  val sj = ScalaJack()
  val sjCsv = ScalaJack(CSVFlavor())

  describe("Plug Holes in Test Coverage (via Coveralls)") {
    describe("csv") {
      it("readSafely") {
        implicit val ops = Json4sOps
        val Left(x) = sjCsv.readSafely[Encapsulated]("123")
        x.toString should be("DeserializationFailure(Vector(($.bar,Required field missing (reported by: co.blocke.scalajack.typeadapter.BooleanDeserializer))))")
        val csv = "123.45,123456789,true"
        sjCsv.read[Strs](csv) should be(Strs("123.45", "123456789", "true"))
        sjCsv.emitFromAST(sjCsv.parseToAST(csv)) should be(csv)
        sjCsv.render(List(1, 2, 3)) should be("1,2,3")
        sjCsv.render("foo") should be("")
        val s = Strs("a", "b", "c")
        sjCsv.materialize[Strs](sjCsv.dematerialize(s)) should be(s)
        val zzz = sjCsv.dematerialize(s).asInstanceOf[ops.ObjectFields]
        val yyy = ops.mapObjectFields(zzz, {
          case (fname, fval) =>
            val fixed = fval match {
              case AstString(t) => ops.applyArray(List(AstString(t)))
              case n            => n
            }
            (fname, fixed)
        })
        assertThrows[ReadException] {
          sjCsv.materialize[Strs](yyy)
        }
        sjCsv.render(Strs(null, "a", "b")) should be(",a,b")
      }
    }
    describe("json") {
      it("JsonDiff") {
        val a = LeftD("Fred", 1)
        val b = RightD("Sally", true)
        val jd = JsonDiff.compare(Path.Root, sj.dematerialize(a), sj.dematerialize(b))
        jd.toString should be("List(JsonDiff($.name, left: JString(Fred), right: JString(Sally)), JsonDiff($.one, left: JInt(1), right: JNothing), JsonDiff($.two, left: JNothing, right: JBool(true)))")

        implicit val ops = Json4sOps
        val jd2 = JsonDiff.compare(Path.Root, AstString("Foo"), AstString("Bar"))
        jd2.toString should be("List(JsonDiff($, left: JString(Foo), right: JString(Bar)))")
      }
      it("JsonParser") {
        val js = """{"name":"Fred \"wildman\" Jones", "one":1}"""
        val z = sj.read[LeftD](js)
        z should be(LeftD("""Fred "wildman" Jones""", 1))
      }
      it("JsonRenderer") {
        val ld = LeftD("Fred {wow} Jones", 5)
        val js = sj.render(ld)
        js should be("{\"name\":\"Fred {wow} Jones\",\"one\":5}")
      }
    }
    describe("core") {
      it("AstOps") {
        implicit val ops = json.Json4sOps
        val nums = List(AstInt(1), AstInt(2), AstInt(3))
        val coll = ops.applyArray(nums)
        val times2 = ops.mapArrayElements(coll.asInstanceOf[ops.ArrayElements], (_, e) => e match {
          case AstInt(n) => n * 2
        })
        times2 should be(List(2, 4, 6))
        Json4sOps.parse("[1,2,3]").toString should be("JArray(List(JLong(1), JLong(2), JLong(3)))")
      }
      it("AAstValue") {
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
        val result = AstValue.transform[JValue, JValue, String, String](ops.applyArray(jsonStuff))(Json4sOps, CSVOps)
        result.toString() should be("""JArray(List(JArray(List(JInt(1), JInt(2), JInt(3))), JBool(true), JDecimal(123.45), JDouble(12.34), JInt(5), JLong(5), JNull, JObject(List((a,JInt(5)), (b,JInt(6)))), JString(wow)))""")
      }
      it("Context") {
        sj.context.deserializerOf[Int].getClass().getName().startsWith("co.blocke.scalajack.typeadapter.AstParsingFallbackDeserializer") should be(true)
        sj.context.serializerOf[Int].getClass().getName().startsWith("co.blocke.scalajack.TermSerializer") should be(true)
        sj.context.deserializer(typeOf[Int]).getClass().getName().startsWith("co.blocke.scalajack.typeadapter.AstParsingFallbackDeserializer") should be(true)
        sj.context.serializer(typeOf[Int]).getClass().getName().startsWith("co.blocke.scalajack.TermSerializer") should be(true)
        sj.context.addTypeAdapterFactories(EitherTypeAdapter)
      }
      it("'No errors' Failure (DeserializationException)") {
        // pick up some other test cases--from DeserializationResult actually, but hey...we're here
        val df = DeserializationFailure(Path.Root)
        the[ReadException] thrownBy df.get should have message "DeserializationException(no errors)"
        val success = DeserializationSuccess(TypeTagged("x", typeOf[String]))
        df.flatMap(_ => success) should be(df.asInstanceOf[DeserializationResult[_]])

        df.isSuccess should be(false)
        val fail = new ReadException(df)
        fail.getMessage should be("DeserializationException(no errors)")
      }
      it("DeferredDeserializerReference") {
        implicit val ops = Json4sOps
        implicit val guidance = SerializationGuidance()
        val deser = Deserializer.constant(TypeTagged("foo", typeOf[String]))
        val deferred = new DeferredDeserializerReference(() => deser)
        deferred.deserializeFromNothing(Path.Root).toString should be("DeserializationSuccess(foo as String)")
      }
      it("DeserializationResult") {
        val z = DeserializationResult.trapExceptions(Path.Root) {
          val x = 0
          val y = 9 / x
          DeserializationSuccess(TypeTagged(y, typeOf[Int]))
        }
        z.toString should be("DeserializationFailure(Vector(($,Exception was thrown: java.lang.ArithmeticException: / by zero (reported by: unknown))))")

        val z2 = DeserializationResult.trapExceptions(Path.Root) {
          throw new ReadException(DeserializationFailure(Path.Root))
          val x = "abc123".toLong
          val y = 1
          DeserializationSuccess(TypeTagged(y, typeOf[Int]))
        }
        z2.toString should be("""DeserializationFailure(Vector())""")

        val success = DeserializationSuccess(TypeTagged(3, typeOf[Int])).flatMap(g => DeserializationSuccess(TypeTagged(g.get * 3, typeOf[Int])))
        success should be(DeserializationSuccess(TypeTagged(9, typeOf[Int])))
        success.isSuccess should be(true)

        val z3 = DeserializationResult(Path.Root) {
          val x = 0
          val y = 9 / x
          TypeTagged(y, typeOf[Int])
        }
        z3.toString should be("DeserializationFailure(Vector(($,Exception was thrown: java.lang.ArithmeticException: / by zero (reported by: unknown))))")
        val z4 = DeserializationResult(Path.Root) {
          val x = 0
          throw new ReadException(DeserializationFailure(Path.Root))
          val y = 9 / x
          TypeTagged(y, typeOf[Int])
        }
        z4.toString should be("DeserializationFailure(Vector())")
      }
      it("Deserializer") {
        implicit val ops = Json4sOps
        implicit val guidance = SerializationGuidance()
        val deser = Deserializer.constant(TypeTagged("foo", typeOf[String]))
        deser.deserializeFromNothing(Path.Root).toString should be("DeserializationSuccess(foo as String)")
        deser.deserialize(Path.Root, AstString("foo")).toString should be("DeserializationFailure(Vector(($,Expected no JSON at path $ because value is constant (reported by: co.blocke.scalajack.Deserializer$$anon$1))))")
      }
      it("DeserializerReference") {
        implicit val ops = Json4sOps
        implicit val guidance = SerializationGuidance()
        val deser = Deserializer.constant(TypeTagged("blah", typeOf[String]))
        val ref = new DeserializerReference(deser)
        ref.toString startsWith ("DeserializerReference(co.blocke.scalajack.Deserializer$$") should be(true)
        ref.referencedDeserializer should be(deser)
        val deser2 = Deserializer.constant(TypeTagged("foo", typeOf[String]))
        ref.referencedDeserializer_=(deser2)
        ref.referencedDeserializer should be(deser2)
        ref.deserializeFromNothing(Path.Root).toString should be("DeserializationSuccess(foo as String)")
        ref.deserialize(Path.Root, AstString("foo")).toString should be("DeserializationFailure(Vector(($,Expected no JSON at path $ because value is constant (reported by: co.blocke.scalajack.Deserializer$$anon$1))))")
        the[IllegalArgumentException] thrownBy ref.referencedDeserializer_=(null) should have message "requirement failed: Referenced deserializer must not be null"
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
        val e = Path.Element(Path.Root, 1)
        e.toString should be("$[1]")
      }
      it("Reflecton") {
        val n = null
        Reflection.inferTypeOf(n).toString should be("Null")
      }
      it("ScalaJack") {
        val m = Bloke("top")
        the[ViewException] thrownBy sj.spliceInto[List[Int], Bloke](List(1, 2, 3), m) should have message "View must be a case class, not scala.List[Int]"
        the[ViewException] thrownBy sj.spliceInto[Bloke, List[Int]](m, List(1, 2, 3)) should have message "Master must be a case class, not scala.List[Int]"
      }
      it("SerializationException") {
        implicit val ops = Json4sOps
        (new SerializationException(SerializationFailure())).toString should be("co.blocke.scalajack.SerializationException: SerializationException(no errors)")
        val msg = """co.blocke.scalajack.SerializationException: SerializationException(2 errors):
                    |  Nothing
                    |  Nothing""".stripMargin
        (new SerializationException(SerializationFailure(SerializationError.Nothing, SerializationError.Nothing))).toString should be(msg)
      }
      it("SerializationGuidance") {
        val g = SerializationGuidance()
        g.toString should be(":: isMapKey: false  isMapValue: false  secondLookParsing: false  inSeq: false")
      }
      it("SerializatioinFailure") {
        implicit val ops = Json4sOps
        val sf = SerializationFailure()
        the[UnsupportedOperationException] thrownBy sf.get should have message "SerializationFailure.get not supported"
        val se = SerializationFailure(SerializationError.ExceptionThrown(new Exception("boom")), SerializationError.ExceptionThrown(new Exception("bam")))
        se.toString should be("SerializationFailure(ExceptionThrown(java.lang.Exception: boom), ExceptionThrown(java.lang.Exception: bam))")
        se.map(_ => 5) == se.asInstanceOf[SerializationFailure[Int]] should be(true)
      }
      it("SerializerReference") {
        implicit val ops = Json4sOps
        implicit val guidance = SerializationGuidance()
        val ser = sj.context.typeAdapterOf[String].serializer
        val ref = new IRTransceiverReference(ser)
        ref.toString.startsWith("SerializerReference(co.blocke.scalajack.TermSerializer@") should be(true)
        ref.referencedSerializer should be(ser)
        ref.referencedSerializer_=(ser)
        ref.serialize(TypeTagged("wow", typeOf[String])).toString should be("SerializationSuccess(JString(wow))")
        the[IllegalArgumentException] thrownBy ref.referencedSerializer_=(null) should have message "requirement failed: Referenced serializer must not be null"
      }
      it("SerializationResult") {
        val ss = SerializationSuccess(1)
        ss.errors.size should be(0)
      }
      it("TypeAdapter") {
        val t = sj.context.typeAdapterOf[String]
        t.is[StringTypeAdapter.type] should be(true)
        t.is[IntTypeAdapter.type] should be(false)
        t.as[StringTypeAdapter.type].getClass().getName() should be("co.blocke.scalajack.typeadapter.StringTypeAdapter$")

        implicit val ops = Json4sOps
        implicit val g = SerializationGuidance()
        val ta = new TypeAdapter[Encapsulated] {}
        assertThrows[scala.NotImplementedError] {
          ta.deserializer.deserialize(Path.Root, AstString("foobar"))
        }
        assertThrows[scala.NotImplementedError] {
          ta.serializer.serialize(TypeTagged(Encapsulated("c", false), typeOf[Encapsulated]))
        }
      }
      it("TypeTagged") {
        val ttb = TypeTagged(true)
        (ttb.tpe == typeOf[Boolean]) should be(true)
        val ttby = TypeTagged(2.toByte)
        (ttby.tpe == typeOf[Byte]) should be(true)
        val ttc = TypeTagged('z')
        (ttc.tpe == typeOf[Char]) should be(true)
        val ttd = TypeTagged(5.2D)
        (ttd.tpe == typeOf[Double]) should be(true)
        val ttf = TypeTagged(1.2F)
        (ttf.tpe == typeOf[Float]) should be(true)
        val tti = TypeTagged(2)
        (tti.tpe == typeOf[Int]) should be(true)
        val ttl = TypeTagged(2L)
        (ttl.tpe == typeOf[Long]) should be(true)
        val tts = TypeTagged(2.toShort)
        (tts.tpe == typeOf[Short]) should be(true)
      }
      it("package") {
        val nqm = No_Quote_Marker
        nqm should be('\u00C5')
      }
    }
    describe("Type Adapters") {
      it("Java Collections") {
        implicit val ops = Json4sOps
        implicit val g = SerializationGuidance()
        val jdser = sj.context.typeAdapterOf[java.util.HashMap[String, Int]].as[JavaMapTypeAdapter[String, Int, java.util.HashMap[String, Int]]].deserializer
        jdser.deserialize(Path.Root, AstString("boom")).toString should be("DeserializationFailure(Vector(($,Expected a JSON object (reported by: co.blocke.scalajack.typeadapter.javacollections.JavaMapDeserializer))))")

        val jdser2 = sj.context.typeAdapterOf[java.util.Vector[String]].as[JavaCollectionTypeAdapter[String, java.util.Vector[String]]].deserializer
        jdser2.deserialize(Path.Root, AstInt(5)).toString should be("DeserializationFailure(Vector(($,Expected a JSON array, not JInt(5) (reported by: co.blocke.scalajack.typeadapter.javacollections.JavaCollectionDeserializer))))")
      }
      it("Java Primitives") {
        implicit val ops = Json4sOps
        implicit val g = SerializationGuidance()
        val tad = sj.context.typeAdapterOf[java.lang.Number].as[JavaNumberTypeAdapter].deserializer
        tad.deserialize(Path.Root, AstDouble(123.45)) should be(DeserializationSuccess(TypeTagged(123.45D, typeOf[java.lang.Double])))
        tad.deserialize(Path.Root, ops.applyArray(List(AstDouble(123.45), AstDouble(123.45)))).toString should be("DeserializationFailure(Vector(($,Expected a JSON number (reported by: co.blocke.scalajack.typeadapter.javaprimitives.BoxedNumberDeserializer))))")
      }
      it("Scala Primitives") {
        implicit val ops = Json4sOps
        implicit val g = SerializationGuidance()
        val tad = sj.context.typeAdapterOf[BigInt].as[TypeAdapter.=:=[BigInt]].deserializer
        tad.deserialize(Path.Root, AstDouble(123e-3D)).toString should be("DeserializationFailure(Vector(($,Can't create a BigInt from 0.123 (reported by: co.blocke.scalajack.typeadapter.BigIntDeserializer))))")

        val btad = sj.context.typeAdapterOf[Byte].as[TypeAdapter.=:=[Byte]].deserializer
        btad.deserialize(Path.Root, AstInt(123456)).toString should be("DeserializationFailure(Vector(($,Byte value out of range (reported by: co.blocke.scalajack.typeadapter.ByteDeserializer))))")

        val etad = sj.context.typeAdapterOf[Num.Value].as[EnumerationTypeAdapter[_]].deserializer
        etad.deserialize(Path.Root, AstDouble(12.34)).toString should be("DeserializationFailure(Vector(($,Expected a JSON string or int (reported by: co.blocke.scalajack.typeadapter.EnumerationValueDeserializer))))")

        val ftad = sj.context.typeAdapterOf[Float].as[TypeAdapter.=:=[Float]].deserializer
        ftad.deserialize(Path.Root, AstDecimal(1234567890123.1234567890123)).toString should be("DeserializationFailure(Vector(($,Float value out of range (reported by: co.blocke.scalajack.typeadapter.FloatDeserializer))))")
        ftad.deserialize(Path.Root, AstLong(12345)).toString should be("DeserializationSuccess(12345.0 as Float)")
        ftad.deserialize(Path.Root, AstInt(12345)).toString should be("DeserializationSuccess(12345.0 as Float)")

        val itad = sj.context.typeAdapterOf[Int].as[TypeAdapter.=:=[Int]].deserializer
        itad.deserialize(Path.Root, AstLong(123456789012345L)).toString should be("DeserializationFailure(Vector(($,Int value out of range (reported by: co.blocke.scalajack.typeadapter.IntDeserializer))))")
      }
      it("MapTypeAdapter") {
        implicit val ops = Json4sOps
        implicit val g = SerializationGuidance()
        val mtd = sj.context.typeAdapterOf[Map[String, Int]].as[MapTypeAdapter[_, _, _]].deserializer
        mtd.deserialize(Path.Root, AstString("foo")).toString should be("DeserializationFailure(Vector(($,Expected a JSON object (reported by: co.blocke.scalajack.typeadapter.MapDeserializer))))")
      }
      it("TupleTypeAdapter") {
        implicit val ops = Json4sOps
        implicit val g = SerializationGuidance()
        val tad = sj.context.typeAdapterOf[Tuple2[Option[Int], Int]].as[TupleTypeAdapter[_]].deserializer
        val ast = sj.dematerialize[Tuple2[Option[Int], Int]]((None, 5))
        tad.deserialize(Path.Root, ast).toString should be("DeserializationSuccess((None,5) as (scala.None.type, Int))")

        //        val tas = sj.context.typeAdapterOf[Tuple2[Option[Int], Int]].as[TupleTypeAdapter[_]].serializer

        tad.deserialize(Path.Root, AstInt(5)).toString should be("DeserializationFailure(Vector(($,Expected a JSON array, not JInt(5) (reported by: co.blocke.scalajack.typeadapter.TupleDeserializer))))")
      }
      it("Try") {
        implicit val ops = Json4sOps
        implicit val g = SerializationGuidance()
        val tts = sj.context.typeAdapterOf[scala.util.Try[Int]].as[TryTypeAdapter[Int]].serializer
        assertThrows[Exception](tts.serialize(TypeTagged(scala.util.Failure(new Exception("Boom")), typeOf[scala.util.Failure[Int]])))
        val ttd = sj.context.typeAdapterOf[scala.util.Try[Int]].as[TryTypeAdapter[Int]].deserializer
        ttd.deserialize(Path.Root, AstString("boom")).toString should be("DeserializationSuccess(Failure(co.blocke.scalajack.typeadapter.TryDeserializer$$anon$1: DeserializationException(1 error):\n  [$] Expected a JSON int, not JString(boom) (reported by: co.blocke.scalajack.typeadapter.IntDeserializer)) as scala.util.Failure[Int])")
      }
    }
  }
}
