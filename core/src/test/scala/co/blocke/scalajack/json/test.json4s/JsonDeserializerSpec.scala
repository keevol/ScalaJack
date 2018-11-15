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
        sj.parseToAST("""1234""") should be(DeserializationSuccess(JLong(1234)))
        sj.parseToAST(Long.MaxValue.toString) should be(DeserializationSuccess(JLong(Long.MaxValue)))
      }
      it("should parseToAST a negative integer") {
        sj.parseToAST("""-1234""") should be(DeserializationSuccess(JLong(-1234)))
      }
      it("should parseToAST a double") {
        sj.parseToAST("123.456") should be(DeserializationSuccess(JDecimal(123.456)))
      }
      it("should parseToAST a null") {
        sj.parseToAST("") should be(DeserializationSuccess(JNull))
      }
      it("should parseToAST a string") {
        sj.parseToAST("\"blather\"") should be(DeserializationSuccess(JString("blather")))
      }
      it("should parseToAST a array") {
        sj.parseToAST("[1,2,3]") should be(DeserializationSuccess(JArray(List(JLong(1), JLong(2), JLong(3)))))
      }
      it("should parseToAST an object") {
        sj.parseToAST("""{"a":3,"b":4}""") should be(DeserializationSuccess(JObject(List(("a", JLong(3)), ("b", JLong(4))))))
      }
      it("should parseToAST a boolean") {
        sj.parseToAST("true") should be(DeserializationSuccess(JBool(true)))
      }
    }
  }
}
