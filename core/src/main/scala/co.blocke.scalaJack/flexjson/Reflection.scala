package co.blocke.scalajack.flexjson

import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.{ Type, appliedType }

object Reflection {

  import scala.language.reflectiveCalls

  val mirror = currentMirror.asInstanceOf[{
    def methodToJava(sym: scala.reflect.internal.Symbols#MethodSymbol): java.lang.reflect.Method
  }]

  def methodToJava(methodSymbol: scala.reflect.runtime.universe.MethodSymbol): java.lang.reflect.Method =
    mirror.methodToJava(methodSymbol.asInstanceOf[scala.reflect.internal.Symbols#MethodSymbol])

  private def solveForNeedleAfterSubstitution(
    haystackBeforeSubstitution: Type,
    haystackAfterSubstitution:  Type,
    needleBeforeSubstitution:   Type
  ): Option[Type] = {
    if (needleBeforeSubstitution == haystackBeforeSubstitution) {
      Some(haystackAfterSubstitution)
    } else {
      val pairs = haystackBeforeSubstitution.typeArgs zip haystackAfterSubstitution.typeArgs

      pairs.flatMap({ case (a, b) ⇒ solveForNeedleAfterSubstitution(a, b, needleBeforeSubstitution) }).headOption
    }
  }

  def populateChildTypeArgs(parentType: Type, childTypeBeforeSubstitution: Type): Type = {
    if (childTypeBeforeSubstitution.typeSymbol.isParameter) {
      parentType
    } else {
      val parentTypeConstructor = parentType.typeConstructor
      val parentTypeArgs = parentType.typeArgs

      val childTypeConstructor = childTypeBeforeSubstitution.typeConstructor
      val childTypeParams = childTypeConstructor.typeParams

      val childAsParentTypeBeforeSubstitution = childTypeBeforeSubstitution.baseType(parentType.typeSymbol)
      val childAsParentTypeArgsBeforeSubstitution = childAsParentTypeBeforeSubstitution.typeArgs

      val childAsParentTypeArgsAfterSubstitution =
        for ((parentTypeArg, childAsParentTypeArgBeforeSubstitution) ← parentTypeArgs zip childAsParentTypeArgsBeforeSubstitution) yield {
          populateChildTypeArgs(parentTypeArg, childAsParentTypeArgBeforeSubstitution)
        }

      val childAsParentTypeAfterSubstitution = appliedType(parentTypeConstructor, childAsParentTypeArgsAfterSubstitution)

      if (childTypeConstructor == parentTypeConstructor) {
        appliedType(parentTypeConstructor, childAsParentTypeArgsAfterSubstitution)
      } else {
        val childTypeArgs =
          for (childTypeParam ← childTypeParams.map(_.asType.toType)) yield {
            val optionalChildTypeArgAfterSubstitution = solveForNeedleAfterSubstitution(
              haystackBeforeSubstitution = childAsParentTypeBeforeSubstitution,
              haystackAfterSubstitution  = childAsParentTypeAfterSubstitution,
              needleBeforeSubstitution   = childTypeParam
            )

            optionalChildTypeArgAfterSubstitution.getOrElse(childTypeParam)
          }

        appliedType(childTypeConstructor, childTypeArgs)
      }
    }
  }

}
