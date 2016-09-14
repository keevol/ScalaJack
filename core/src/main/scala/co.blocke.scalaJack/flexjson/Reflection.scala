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

  private def solveForNeedleAfterTransformation(
    haystackBeforeTransformation: Type,
    haystackAfterTransformation:  Type,
    needleBeforeTransformation:   Type
  ): Option[Type] = {
    if (needleBeforeTransformation == haystackBeforeTransformation) {
      Some(haystackAfterTransformation)
    } else {
      val pairs = haystackBeforeTransformation.typeArgs zip haystackAfterTransformation.typeArgs

      pairs.flatMap({ case (a, b) ⇒ solveForNeedleAfterTransformation(a, b, needleBeforeTransformation) }).headOption
    }
  }

  def populateChildTypeArgs(parentType: Type, childType: Type): Type = {
    if (childType.typeSymbol.isParameter) {
      parentType
    } else {
      val parentTypeConstructor = parentType.typeConstructor
      val parentTypeArgs = parentType.typeArgs

      val childTypeConstructor = childType.typeConstructor
      val childTypeParams = childTypeConstructor.typeParams

      val childAsParentTypeBeforeSubstitution = childType.baseType(parentType.typeSymbol)
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
          for (childTypeParam ← childTypeParams) yield {
            val childTypeParamBeforeSubstitution = childTypeParam.asType.toType

            val optionalChildTypeArgAfterSubstitution = solveForNeedleAfterTransformation(
              haystackBeforeTransformation = childAsParentTypeBeforeSubstitution,
              haystackAfterTransformation  = childAsParentTypeAfterSubstitution,
              needleBeforeTransformation   = childTypeParamBeforeSubstitution
            )

            optionalChildTypeArgAfterSubstitution.getOrElse(childTypeParamBeforeSubstitution)
          }

        appliedType(childType, childTypeArgs)
      }
    }
  }

}
