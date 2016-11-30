package co.blocke.scalajack
package json.test.misc

import java.time.LocalDate

import org.scalatest.{ BeforeAndAfterAll, FunSpec, GivenWhenThen }

import scala.reflect.runtime.universe.{ Type, typeOf }

case class UselessClass0()
case class UselessClass1[A](a: A)
case class UselessClass2[A, B](a: A, b: B)
case class UselessClass3[A, B, C](a: A, b: B, c: C)

trait Floor
case class TileFloor(squareFeet: Int) extends Floor
case class HardwoodFloor(squareFeet: Int) extends Floor
case class DirtFloor(color: String) extends Floor

class Types extends FunSpec with GivenWhenThen with BeforeAndAfterAll {

  val sj = ScalaJack()

  describe("-----------------------------\n:  Types  :\n-----------------------------") {
    it("0 type arguments") {
      assertResult(typeOf[UselessClass0]) {
        Types.read("co.blocke.scalajack.json.test.misc.UselessClass0", typeOf[Any])
      }
    }
    it("1 type argument") {
      assertResult(typeOf[UselessClass1[Int]]) {
        Types.read("co.blocke.scalajack.json.test.misc.UselessClass1[Int]", typeOf[Any])
      }
    }
    it("3 type arguments") {
      assertResult(typeOf[UselessClass3[UselessClass0, UselessClass1[Int], UselessClass2[String, LocalDate]]]) {
        Types.read("co.blocke.scalajack.json.test.misc.UselessClass3[co.blocke.scalajack.json.test.misc.UselessClass0, co.blocke.scalajack.json.test.misc.UselessClass1[Int], co.blocke.scalajack.json.test.misc.UselessClass2[String, java.time.LocalDate]]", typeOf[Any])
      }
    }
    it("Type bounds") {
      assertResult(typeOf[UselessClass1[HardwoodFloor]]) {
        Types.read("UselessClass1[HW]", typeOf[UselessClass1[_ <: Floor]], context => {
          import BijectiveFunction.Implicits._

          if (context <:< typeOf[UselessClass1[_]].typeConstructor) {
            val apply: String => Type = {
              case "UselessClass1" => typeOf[UselessClass1[_]].typeConstructor
            }

            val unapply: Type => String = t => {
              val fullName = t.typeSymbol.fullName
              fullName.substring(fullName.lastIndexOf('.') + 1)
            }

            (apply <=> unapply).memoized
          } else if (context <:< typeOf[Floor]) {
            val apply: String => Type = {
              case "HW" => typeOf[HardwoodFloor]
            }

            val unapply: Type => String = t => {
              if (t =:= typeOf[HardwoodFloor]) {
                "HW"
              } else {
                ???
              }
            }

            apply <=> unapply
          } else {
            BijectiveFunctions.fullNameToType
          }
        })
      }
    }
    it("Type bounds, parse-or-else") {
      assertResult(typeOf[UselessClass1[DirtFloor]]) {
        Types.read(
          string      = "UselessClass1[SomeRandomFloorThatDoesNotActuallyExist]",
          targetType  = typeOf[UselessClass1[_ <: Floor]],
          context => {
            import BijectiveFunction.Implicits._

            if (context <:< typeOf[UselessClass1[_]].typeConstructor) {
              val apply: String => Type = {
                case "UselessClass1" => typeOf[UselessClass1[_]].typeConstructor
              }

              val unapply: Type => String = t => {
                val fullName = t.typeSymbol.fullName
                fullName.substring(fullName.lastIndexOf('.') + 1)
              }

              (apply <=> unapply).memoized
            } else if (context <:< typeOf[Floor]) {
              val apply: String => Type = {
                case "HW" => typeOf[HardwoodFloor]
              }

              val unapply: Type => String = t => {
                if (t =:= typeOf[HardwoodFloor]) {
                  "HW"
                } else {
                  ???
                }
              }

              apply <=> unapply
            } else {
              BijectiveFunctions.fullNameToType
            }
          },
          parseOrElse = t => {
            if (t <:< typeOf[Floor]) {
              typeOf[DirtFloor]
            } else {
              ???
            }
          }
        )
      }
    }
  }
}
