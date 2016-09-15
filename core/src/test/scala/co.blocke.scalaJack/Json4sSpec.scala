package co.blocke.scalaJack

import co.blocke.scalajack.ScalaJack
import org.json4s.{ JArray, JBool, JObject, JValue }
import org.scalatest.FunSpec

import scala.language.postfixOps

case class Json4sThing(string: String, jValue: JValue, jBool: JBool, jObject: JObject, jArray: JArray)

class Json4sSpec extends FunSpec {
  val scalaJack = ScalaJack()

  describe("thing") {
    it("fdfdf") {
      val actual = scalaJack.read[Json4sThing](
        """
          |{
          |  "string": "test",
          |  "jValue": 4,
          |  "jBool": true,
          |  "jObject": {
          |    "a": 1,
          |    "b": false,
          |    "c": {}
          |  },
          |  "jArray": [
          |    5,
          |    false,
          |    "hello"
          |  ]
          |}""".stripMargin
      )

      println(actual)
      println(scalaJack.render(actual))
    }
  }

}
