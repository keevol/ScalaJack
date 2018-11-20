package co.blocke.scalajack
package json
package test.json4s

import org.json4s.JsonAST.{ JArray, JBool, JDecimal, JLong, JNull, JObject, JString }
import org.scalatest.FunSpec
import org.scalatest.Matchers._

class JsonDeserializerSpec extends FunSpec {

  val sj = ScalaJack()

  describe("-----------------------\n:  Java Number Tests  :\n-----------------------") {
    describe("Numbers") {
      it("should parseToAST a simple integer") {
        sj.parse("""1234""") should be(DeserializationSuccess(JLong(1234)))
        sj.parse(Long.MaxValue.toString) should be(DeserializationSuccess(JLong(Long.MaxValue)))
      }
      it("should parseToAST a negative integer") {
        sj.parse("""-1234""") should be(DeserializationSuccess(JLong(-1234)))
      }
      it("should parseToAST a double") {
        sj.parse("123.456") should be(DeserializationSuccess(JDecimal(123.456)))
      }
      it("should parseToAST a null") {
        sj.parse("") should be(DeserializationSuccess(JNull))
      }
      it("should parseToAST a string") {
        sj.parse("\"blather\"") should be(DeserializationSuccess(JString("blather")))
      }
      it("should parseToAST a array") {
        sj.parse("[1,2,3]") should be(DeserializationSuccess(JArray(List(JLong(1), JLong(2), JLong(3)))))
      }
      it("should parseToAST an object") {
        sj.parse("""{"a":3,"b":4}""") should be(DeserializationSuccess(JObject(List(("a", JLong(3)), ("b", JLong(4))))))
      }
      it("should parseToAST a boolean") {
        sj.parse("true") should be(DeserializationSuccess(JBool(true)))
      }
    }
  }
}
