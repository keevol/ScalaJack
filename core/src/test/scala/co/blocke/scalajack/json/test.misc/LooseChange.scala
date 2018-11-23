package co.blocke.scalajack
package json.test.misc

import co.blocke.scalajack.json.Json4sOps
import org.scalatest.{ BeforeAndAfterAll, FunSpec, GivenWhenThen }
import org.scalatest.Matchers._

import scala.reflect.runtime.universe.typeOf
import typeadapter.{ CaseClassTypeAdapter, PlainClassTypeAdapter }
import org.json4s._

class LooseChange extends FunSpec with GivenWhenThen with BeforeAndAfterAll {

  val sj = ScalaJack()

  describe("-----------------------\n:  Lose Change Tests  :\n-----------------------") {
    it("Bijective compose") {
      val bi1 = BijectiveFunction[Int, Boolean]((i: Int) => if (i > 3) true else false, (b: Boolean) => if (b) 3 else 0)
      val bi2 = BijectiveFunction[String, Int]((s: String) => s.length, (i: Int) => "x" * i)
      val bi3 = bi1.compose[String](bi2)
      bi3("blather") should be(true)
    }
    it("Bijective double inversion") {
      val bi = BijectiveFunction[String, Int]((s: String) => s.length, (i: Int) => "x" * i)
      val x = bi.inverse
      x(3) should be("xxx")
      val y = x.inverse
      y("foo") should be(3)
    }
    it("Can find collection and key annotations on case class") {
      val adapter = sj.context.typeAdapter(typeOf[DefaultOpt]).as[CaseClassTypeAdapter[_]]
      adapter.collectionName should be(Some("myDefaults"))
      adapter.dbKeys.head.dbKeyIndex should be(Some(1))
    }
    it("Can find collection and key annotations on plain class") {
      val adapter = sj.context.typeAdapter(typeOf[Plain]).as[PlainClassTypeAdapter[_]]
      adapter.collectionName should be(Some("plains"))
      adapter.dbKeys.head.dbKeyIndex should be(Some(1))
    }
    it("Materialize from IR works") {
      import org.json4s.JsonDSL._
      val c = View1("Fred", 123L, None)
      val WriteSuccess(ir) = sj.dematerialize(c)
      val ir2 = ir.asInstanceOf[JObject] // JObject(List((name,JString(Fred)), (big,JLong(123))))
      val modded = (ir2 ~ ("maybe" -> "I'm here")).transformField {
        case JField("name", _) => ("name", JString("Sally"))
      }
      sj.materialize[View1](modded).get should be(View1("Sally", 123L, Some("I'm here")))
    }
    it("BinaryTypeAdapter") {
      implicit val ops = Json4sOps
      implicit val g = SerializationGuidance()
      val bytes = "This is a test".getBytes
      val x = sj.context.typeAdapterOf[Array[Byte]].resolved.irTransceiver
      val written = x.write(TypeTagged(bytes, typeOf[Array[Byte]]))
      written.toString should be("WriteSuccess(JString(5468697320697320612074657374))")
      val readin = x.read(Path.Root, written.get).get.toSeq
      readin.toString should be("WrappedArray(84, 104, 105, 115, 32, 105, 115, 32, 97, 32, 116, 101, 115, 116)")
      x.read(Path.Root, IRNull()).get should be(null)
      x.read(Path.Root, IRBoolean(true)).toString should be("""ReadFailure(Vector(($,Expected hex string (binary data) (reported by: co.blocke.scalajack.typeadapter.BinaryTypeAdapter$$anon$1))))""")
    }
  }
}
