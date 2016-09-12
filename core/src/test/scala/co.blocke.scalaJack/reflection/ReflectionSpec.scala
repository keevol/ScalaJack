package co.blocke.scalajack.reflection

import co.blocke.scalajack.ScalaJack
import org.scalatest.{FunSpec, Matchers}

import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.{ClassSymbol, InstanceMirror, TypeName, typeOf}

trait Drink
case class OrangeJuice(pulp: Boolean) extends Drink
case class Milk(percent: Int) extends Drink
case class Soda(diet: Boolean) extends Drink

trait Meal[D <: Drink] {
  def drink: D
}

case class Breakfast[D <: Drink](numberOfPancakes: Int, drink: D) extends Meal[D]
case class Lunch[D <: Drink](numberOfBurgers: Int, drink: D) extends Meal[D]

trait PairOfValues[A, B]
case class PairOfMeals[D1 <: Drink, D2 <: Drink](meal1: Meal[D1], meal2: Meal[D2]) extends PairOfValues[Meal[D1], Meal[D2]]

//noinspection EmptyCheck
class ReflectionSpec extends FunSpec with Matchers {

  val scalaJack = ScalaJack()

  describe("type substitution") {
    val expected: PairOfValues[Meal[OrangeJuice], Meal[Soda]] = PairOfMeals(
      Breakfast(numberOfPancakes = 3, drink = OrangeJuice(pulp = true)),
      Lunch(numberOfBurgers = 2, drink = Soda(diet = false))
    )

    val json =
      """
        |{
        |  "_hint": "co.blocke.scalajack.reflection.PairOfMeals",
        |  "meal1": {
        |    "_hint": "co.blocke.scalajack.reflection.Breakfast",
        |    "numberOfPancakes": 3,
        |    "drink": {
        |      "_hint": "co.blocke.scalajack.reflection.OrangeJuice",
        |      "pulp": true
        |    }
        |  },
        |  "meal2": {
        |    "_hint": "co.blocke.scalajack.reflection.Lunch",
        |    "numberOfBurgers": 2,
        |    "drink": {
        |      "_hint": "co.blocke.scalajack.reflection.Soda",
        |      "diet": false
        |    }
        |  }
        |}""".stripMargin

    val actual = scalaJack.read[PairOfValues[Meal[OrangeJuice], Meal[Soda]]](json)

    it("should") {
      actual should be(expected)
    }
  }

  describe("non-erased types") {
    val breakfastType = typeOf[Breakfast[OrangeJuice]]

    assert(breakfastType.dealias =:= typeOf[Breakfast[OrangeJuice]])
    assert(breakfastType.dealias == typeOf[Breakfast[OrangeJuice]])
    assert(breakfastType.erasure =:= typeOf[Breakfast[_ <: Drink]])
    assert(breakfastType.erasure != typeOf[Breakfast[_ <: Drink]])
    assert(breakfastType.typeParams == List())
    assert(breakfastType.typeArgs == List(typeOf[OrangeJuice]))

    //    assert(breakfastType.typeConstructor.dealias == typeOf[Breakfast])
    assert(breakfastType.typeConstructor.erasure =:= typeOf[Breakfast[_ <: Drink]])
    assert(breakfastType.typeConstructor.erasure != typeOf[Breakfast[_ <: Drink]])
    assert(breakfastType.typeConstructor.typeParams.length == 1)
    assert(breakfastType.typeConstructor.typeArgs == List())

    assert(breakfastType.resultType =:= typeOf[Breakfast[OrangeJuice]])
    assert(breakfastType.resultType == typeOf[Breakfast[OrangeJuice]])

    println(breakfastType)
  }

  describe("erased types") {
    val breakfastType = typeOf[Breakfast[OrangeJuice]]
    val breakfast: Any = Breakfast[OrangeJuice](numberOfPancakes = 3, drink = OrangeJuice(pulp = false))

    val instanceMirror: InstanceMirror = currentMirror.reflect(breakfast)
    val breakfastSymbol: ClassSymbol = instanceMirror.symbol

    assert(breakfastSymbol.name == TypeName("Breakfast"))
    assert(breakfastSymbol.fullName == "co.blocke.scalajack.reflection.Breakfast")
    assert(breakfastSymbol.toType.typeSymbol eq breakfastSymbol)
    assert(breakfastSymbol.toType.typeParams == List())
    assert(breakfastSymbol.typeParams.map(_.fullName) == List("co.blocke.scalajack.reflection.Breakfast.D"))
    breakfastSymbol.toType.typeArgs
    assert(breakfastSymbol.info != breakfastSymbol.toType)
    assert(breakfastSymbol.typeSignature == breakfastSymbol.typeSignatureIn(breakfastType))
  }

}
