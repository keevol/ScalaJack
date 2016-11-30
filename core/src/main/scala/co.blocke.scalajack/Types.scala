package co.blocke.scalajack

import scala.reflect.runtime.universe.{ Type, appliedType, typeOf }

object Types {

  def read(string: String, targetType: Type, f: Type => BijectiveFunction[String, Type] = _ => BijectiveFunctions.fullNameToType, parseOrElse: Type => Type = _ => ???): Type = {

    var position = 0
    val maxPosition = string.length

    def skipWhitespace(): Unit = {
      while (position < maxPosition && string.charAt(position) == ' ') {
        position += 1
      }
    }

    def readType(targetType: Type): Type = {
      val startOfName = position

      while (position < maxPosition && string.charAt(position) != '[' && string.charAt(position) != ']' && string.charAt(position) != ' ' && string.charAt(position) != ',') {
        position += 1
      }

      val endOfName = position

      val typeConstructor = string.substring(startOfName, endOfName) match {
        case "Boolean" =>
          typeOf[Boolean]

        case "Byte" =>
          typeOf[Byte]

        case "Char" =>
          typeOf[Char]

        case "Double" =>
          typeOf[Double]

        case "Float" =>
          typeOf[Float]

        case "Int" =>
          typeOf[Int]

        case "Long" =>
          typeOf[Long]

        case "Short" =>
          typeOf[Short]

        case "String" =>
          typeOf[String]

        case fullName =>
          val bijectiveFunction = f(targetType.typeConstructor)

          try {
            bijectiveFunction(fullName)
          } catch {
            case e: Exception =>
              parseOrElse(targetType)
          }
      }

      val populated = Reflection.populateChildTypeArgs(
        parentType                  = targetType,
        childTypeBeforeSubstitution = typeConstructor
      )

      println(populated)

      if (position < maxPosition && string.charAt(position) == '[') {
        position += 1

        skipWhitespace()

        var remainingTargetTypeArgs = populated.typeArgs

        val typeArgs = new scala.collection.mutable.ListBuffer[Type]

        val firstTargetTypeArg = remainingTargetTypeArgs.head
        val firstTypeArg = readType(firstTargetTypeArg)
        typeArgs += firstTypeArg
        skipWhitespace()

        remainingTargetTypeArgs = remainingTargetTypeArgs.tail

        while (position < maxPosition && string.charAt(position) != ']') {
          assert(string.charAt(position) == ',')
          position += 1
          skipWhitespace()
          val targetTypeArg = remainingTargetTypeArgs.head
          typeArgs += readType(targetTypeArg)
          remainingTargetTypeArgs = remainingTargetTypeArgs.tail
          skipWhitespace()
        }

        assert(string.charAt(position) == ']')
        position += 1

        appliedType(typeConstructor, typeArgs.toList)
      } else {
        typeConstructor
      }
    }

    readType(targetType)
  }

}
