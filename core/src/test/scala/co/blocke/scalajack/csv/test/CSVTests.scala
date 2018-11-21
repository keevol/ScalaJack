package co.blocke.scalajack
package csv
package test

import org.scalatest.{ FunSpec, Matchers }
import scala.reflect.runtime.universe.Type
import java.util.UUID

class CSVTests() extends FunSpec with Matchers {

  val sj = ScalaJack(CSVFlavor())

  describe("---------------\n:  CSV Tests  :\n---------------") {
    describe("Primitives (non-null):") {
      it("Writes out basic Scala primitive types") {
        val inst = BasicScala(BigDecimal(123.45), BigInt(123), true, 64.asInstanceOf[Byte],
          'Z', 12.34, Size.Large, 12.34F, 5L, 5.asInstanceOf[Short], "wow", UUID.fromString("54cab778-7b9e-4b07-9d37-87b97a011e55"))
        val csv = sj.render(inst)
        assertResult("""123.45,123,true,64,Z,12.34,Large,12.34,5,5,wow,54cab778-7b9e-4b07-9d37-87b97a011e55""") { csv }
        assertResult(inst) {
          sj.read[BasicScala](csv)
        }
      }
      it("Handles Any type") {
        val inst = HasAny(true, "hey, you", 34.56)
        val csv = sj.render(inst)
        assertResult("""true,"hey, you",34.56""") { csv }
        assertResult((classOf[java.lang.Boolean], classOf[String], classOf[java.lang.Double])) {
          val c = sj.read[HasAny](csv)
          (c.a1.getClass, c.a2.getClass, c.a3.getClass)
        }
      }
      it("Accepts value classes as CSV fields") {
        val inst = WithVC(2, VC("blip"), false)
        val csv = sj.render(inst)
        assertResult("""2,blip,false""") { csv }
        assertResult(inst) {
          sj.read[WithVC](csv)
        }
      }
      it("Fails when CSV field count doesn't match case class member count") {
        val csv = """123.45,123,true,64,Z,12.34,Large,12.34,5,5,54cab778-7b9e-4b07-9d37-87b97a011e55"""
        val msg = """ReadException(1 error):
                    |  [$.u] Required field u missing (reported by: co.blocke.scalajack.typeadapter.UUIDTypeAdapter$$anon$1)""".stripMargin
        the[ReadException] thrownBy
          sj.read[BasicScala](csv) should have message msg
      }
      it("Fails when types of CSV field don't match ordered case class constructor arguments") {
        val csv = """123.45,"m123",true,64,Z,12.34,Large,12.34,5,5,wow,54cab778-7b9e-4b07-9d37-87b97a011e55"""
        val msg = """ReadException(1 error):
                    |  [$.bi] Expected a JSON number (integer value) (reported by: co.blocke.scalajack.typeadapter.BigIntTypeAdapter$$anon$1)""".stripMargin
        the[ReadException] thrownBy
          sj.read[BasicScala](csv) should have message msg
      }
      it("Serializes from List or Seq type") {
        val list = List(1, 2, 3)
        val csv = sj.render(list)
        sj.read[List[Int]](csv) should be(list)
        val seq = List(1, 2, 3)
        val csv3 = sj.render(seq)
        sj.read[Seq[Int]](csv3) should be(seq)
      }
    }
    describe("Fancy String permutations:") {
      it("Handles fancy strings with embedded commas and quotes") {
        val inst = Strings("Hey, you", "This \"life\"", "And now, \"Mike\" will sing.")
        val csv = sj.render(inst)
        assertResult("\"Hey, you\",\"This \"\"life\"\"\",\"And now, \"\"Mike\"\" will sing.\"") { csv }
        assertResult(inst) {
          sj.read[Strings](csv)
        }
      }
      it("Handleds embedded tabs and newlines in strings") {
        val inst = Strings("Hey, you", "This \"life\"", "And\tnow, \"Mike\" will\nsing.")
        val csv = sj.render(inst)
        assertResult("\"Hey, you\",\"This \"\"life\"\"\",\"And\tnow, \"\"Mike\"\" will\nsing.\"") { csv }
        assertResult(inst) {
          sj.read[Strings](csv)
        }
      }
    }
    describe("Options and null:") {
      it("Renders Some()") {
        val inst = Maybe("yes", Some("blink"), true)
        val csv = sj.render(inst)
        assertResult("""yes,blink,true""") { csv }
        assertResult(inst) {
          sj.read[Maybe](csv)
        }
      }
      it("Renders Some() - empty string") {
        val inst = Maybe("yes", Some(""), true)
        val csv = sj.render(inst)
        assertResult("""yes,"",true""") { csv }
        assertResult(Maybe("yes", Some(""), true)) {
          sj.read[Maybe](csv)
        }
      }
      it("Renders Some() - fancy string 1") {
        val inst = Maybe("yes", Some("This,test"), true)
        val csv = sj.render(inst)
        assertResult("""yes,"This,test",true""") { csv }
        assertResult(inst) {
          sj.read[Maybe](csv)
        }
      }
      it("Renders Some() - fancy string 2") {
        val inst = Maybe("yes", Some("This \"test\""), true)
        val csv = sj.render(inst)
        assertResult("yes,\"This \"\"test\"\"\",true") { csv }
        assertResult(inst) {
          sj.read[Maybe](csv)
        }
      }
      it("Renders None (empty field)") {
        val inst = Maybe("no", None, false)
        val csv = sj.render(inst)
        assertResult("""no,,false""") { csv }
        assertResult(inst) {
          sj.read[Maybe](csv)
        }
      }
      it("Null objects") {
        val inst: Maybe = null
        val csv = sj.render(inst)
        assertResult("") { csv }
        assertResult(inst) { // null converts to None
          sj.read[Maybe](csv)
        }
      }
      it("Renders null (empty field) - Option") {
        val inst = Maybe("oops", null, false)
        val csv = sj.render(inst)
        assertResult("""oops,,false""") { csv }
        assertResult(Maybe("oops", None, false)) { // null converts to None
          sj.read[Maybe](csv)
        }
      }
      it("Renders null (empty field) - nullable 1") {
        val inst = Strings(null, "two", "three")
        val csv = sj.render(inst)
        assertResult(",two,three") { csv }
        assertResult(inst) { // null converts to empty String
          sj.read[Strings](csv)
        }
      }
      it("Renders null (empty field) - nullable 2") {
        val inst = Strings("", null, "three")
        val csv = sj.render(inst)
        assertResult(""""",,three""") { csv }
        assertResult(Strings("", null, "three")) { // null converts to empty String
          sj.read[Strings](csv)
        }
      }
      it("Renders null (empty field) - nullable 3") {
        val inst = Strings("", "two", null)
        val csv = sj.render(inst)
        assertResult(""""",two,""") { csv }
        assertResult(Strings("", "two", null)) { // null converts to empty String
          sj.read[Strings](csv)
        }
      }
      it("Reading from a non-object or non-array should fail") {
        val csv = "1,2,3"
        val msg = """ReadException(1 error):
                    |  [$] Unable to successfully deserialize this CSV (reported by: co.blocke.scalajack.typeadapter.IRParsingFallbackTypeAdapter$$anon$1)""".stripMargin
        the[ReadException] thrownBy sj.read[String](csv) should have message msg
      }
      it("Basic full-range serialization stage tests") {
        val list = List("a", "b", "c")
        val ir = sj.dematerialize(list).get
        sj.materialize[List[String]](ir).get should be(list)
        val csv = sj.emit(ir)
        csv should be("a,b,c")
        sj.parse(csv).get should be(ir)
      }
      it("Infer basic types") {
        val list: List[Any] = List(12.34, true, 123L, "wow")
        val csv = sj.render(list)
        csv should be("12.34,true,123,wow")
        sj.read[List[Any]](csv) should be(list)
      }
      it("Serializing an unsupported type should fail") {
        val msg = """WriteException(1 error):
                    |  ExceptionThrown(java.lang.UnsupportedOperationException: CSV serialization of input of type JString(wow) is unsupported.)""".stripMargin
        the[WriteException] thrownBy
          CSVOps.serialize(IRString("wow")(CSVOps), sj) should have message msg
      }
    }
    describe("Collections/nested (failures):") {
      it("Fails when given an object having a nested object (not flat)") {
        val inst = Nested("K-9", Thing("Robot dog", 1))
        val msg = """WriteException(1 error):
                    |  ExceptionThrown(java.lang.UnsupportedOperationException: CSV serialization of a field of type JObject is unsupported.)""".stripMargin
        the[WriteException] thrownBy
          sj.render(inst) should have message msg
      }
      it("Fails when given an object having a nested array (not flat)") {
        val inst = Nested2("K-9", List(Thing("Robot dog", 1)))
        val msg = """WriteException(1 error):
                    |  ExceptionThrown(java.lang.UnsupportedOperationException: CSV serialization of a field of type JArray is unsupported.)""".stripMargin
        the[WriteException] thrownBy
          sj.render(inst) should have message msg
      }
    }
    describe("ScalaJack creation 'with' modifiers (failure):") {
      it("No withTypeModifier") {
        the[java.lang.UnsupportedOperationException] thrownBy
          ScalaJack(CSVFlavor()).withTypeModifier(null.asInstanceOf[HintModifier]) should have message "Not available for CSV formatting"
      }
      it("No withAdapters") {
        the[java.lang.UnsupportedOperationException] thrownBy
          ScalaJack(CSVFlavor()).withAdapters(null.asInstanceOf[TypeAdapterFactory]) should have message "Not available for CSV formatting"
      }
      it("No withHints") {
        the[java.lang.UnsupportedOperationException] thrownBy
          ScalaJack(CSVFlavor()).withHints(null.asInstanceOf[(Type, String)]) should have message "Not available for CSV formatting"
      }
      it("No withSecondLookParsing") {
        the[java.lang.UnsupportedOperationException] thrownBy
          ScalaJack(CSVFlavor()).withSecondLookParsing() should have message "Not available for CSV formatting"
      }
      it("No withHintModifiers") {
        the[java.lang.UnsupportedOperationException] thrownBy
          ScalaJack(CSVFlavor()).withHintModifiers(null.asInstanceOf[(Type, HintModifier)]) should have message "Not available for CSV formatting"
      }
      it("No withDefaultHint") {
        the[java.lang.UnsupportedOperationException] thrownBy
          ScalaJack(CSVFlavor()).withDefaultHint("") should have message "Not available for CSV formatting"
      }
      it("No parseOrElse") {
        the[java.lang.UnsupportedOperationException] thrownBy
          ScalaJack(CSVFlavor()).parseOrElse(null.asInstanceOf[(Type, Type)]) should have message "Not available for CSV formatting"
      }
      it("No isCanonical") {
        the[java.lang.UnsupportedOperationException] thrownBy
          ScalaJack(CSVFlavor()).isCanonical(false) should have message "Not available for CSV formatting"
      }
    }
  }
}
