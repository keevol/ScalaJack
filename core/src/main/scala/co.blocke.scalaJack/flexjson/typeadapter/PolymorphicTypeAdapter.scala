package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.FlexJsonFlavor.MemberName
import co.blocke.scalajack.flexjson.{ Context, ForwardingWriter, Reader, TokenType, TypeAdapter, TypeAdapterFactory, Writer }

import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.{ ClassSymbol, Type }
import scala.collection.mutable.{ Map ⇒ MMap }

case class PolymorphicTypeAdapterFactory(hintFieldName: String) extends TypeAdapterFactory.FromClassSymbol {

  override def typeAdapter(tpe: Type, classSymbol: ClassSymbol, context: Context, superParamTypes: List[Type]): Option[TypeAdapter[_]] =
    if (classSymbol.isTrait) {
      // println("TRAIT TYPES: " + tpe.typeArgs)
      // println("SUBS FROM DAD: " + superParamTypes)
      Some(PolymorphicTypeAdapter(hintFieldName, context.typeAdapterOf[Type], context.typeAdapterOf[MemberName], context, tpe))
    } else {
      None
    }

}

class PolymorphicWriter(
    override val delegate: Writer,
    typeFieldName:         String,
    tpe:                   Type,
    typeTypeAdapter:       TypeAdapter[Type],
    memberNameTypeAdapter: TypeAdapter[MemberName]
) extends ForwardingWriter {

  var depth = 0

  override def beginObject(): Unit = {
    depth += 1
    super.beginObject()

    if (depth == 1) {
      memberNameTypeAdapter.write(typeFieldName, this)
      typeTypeAdapter.write(tpe, this)
    }
  }

  override def endObject(): Unit = {
    depth -= 1
    super.endObject()
  }

}

object PolymorphicTypeAdapter {
  private val resolved: MMap[(Type, List[Type]), List[Type]] = MMap.empty[(Type, List[Type]), List[Type]]
}

case class PolymorphicTypeAdapter[T](
    typeMemberName:        MemberName,
    typeTypeAdapter:       TypeAdapter[Type],
    memberNameTypeAdapter: TypeAdapter[MemberName],
    context:               Context,
    polyType:              Type
) extends TypeAdapter[T] {

  // Magic that maps (known) parameter types of this polytype to the (unknown) parameter types of a value type
  // implementing this polytype.
  // private diveDeep( inMap:Map[String,Type], objType:Type ) = {
  //   ???    
  // }

  private def resolvePolyTypes(childType: Type): List[Type] = {
    PolymorphicTypeAdapter.resolved.getOrElse((childType, polyType.typeArgs), {

      // Find the "with" mixin for this polytype in the kid (there may be multiple mixin traits).
      // Then get it's type arguments, e.g. [String,P].  It's the 'P' we're interested in.
      val childTypeArgs = childType.baseClasses.find(_ == polyType.typeSymbol).map(f ⇒ childType.baseType(f)).map(_.typeArgs).getOrElse(List.empty[Type])

      // Match 'em up with dad's (this polytype) type aguments, e.g. [String,Int]
      val _argPairs = polyType.typeArgs zip childTypeArgs

      val objTypeParams = childType.typeSymbol.typeSignature.typeParams // object's type symbol list
      // TODO: Put this in some kind of loop for n-listed sets.  As-is this handles only 1-level nesting.
      // Also see if there's a better way then having to wrap NoArgs result in a List artificially so flatten will work.
      val argPairs = _argPairs.collect {
        case (known, withSymbols) if (withSymbols.typeArgs.isEmpty) => List((known, withSymbols))
        case (known, withSymbols)                                   => known.typeArgs zip withSymbols.typeArgs
      }.flatten
      println(argPairs)

      // In the next step we need to sort this list based on the argument list order in the kid, so get the ordered
      // list of kid's type arguments now.
      // (Can't assume that the parameter arg order of the parent is the same are the kid's parameter arg order!)
      val kidsParamOrder = childType.typeSymbol.asType.typeParams

      // Now pull out the ones that don't match--that need subsititution in the kid (the 'P')
      val forSubstitution = argPairs.collect {
        case (fromDad, fromKid) if (fromDad != fromKid) ⇒ (fromDad, kidsParamOrder.indexOf(fromKid.typeSymbol))
      }

      // Return sorted list
      val typeList = forSubstitution.sortWith { (a, b) ⇒ a._2 < b._2 }.map(_._1)
      PolymorphicTypeAdapter.resolved += (childType, polyType.typeArgs) → typeList
      println("Inside " + polyType + " resolved type parameters " + typeList)
      typeList
    })
  }

  override def read(reader: Reader): T = {
    if (reader.peek == TokenType.Null) {
      reader.readNull().asInstanceOf[T]
    } else {
      val originalPosition = reader.position

      reader.beginObject()

      var optionalConcreteType: Option[Type] = None

      while (optionalConcreteType.isEmpty && reader.hasMoreMembers) {
        val memberName = memberNameTypeAdapter.read(reader)

        if (memberName == typeMemberName) {
          val concreteType = typeTypeAdapter.read(reader)
          optionalConcreteType = Some(concreteType)
        } else {
          reader.skipValue()
        }
      }

      val concreteType = optionalConcreteType.getOrElse(throw new Exception(s"""Could not find type field named "$typeMemberName" """))

      val concreteTypeAdapter = context.typeAdapter(concreteType, resolvePolyTypes(concreteType))

      reader.position = originalPosition

      concreteTypeAdapter.read(reader).asInstanceOf[T]
    }
  }

  override def write(value: T, writer: Writer): Unit = {
    // TODO figure out a better way to infer the type (perhaps infer the type arguments?)
    val valueType = currentMirror.classSymbol(value.getClass).info

    val valueTypeAdapter = context.typeAdapter(valueType, resolvePolyTypes(valueType)).asInstanceOf[TypeAdapter[T]]

    val polymorphicWriter = new PolymorphicWriter(writer, typeMemberName, valueType, typeTypeAdapter, memberNameTypeAdapter)
    valueTypeAdapter.write(value, polymorphicWriter)
  }

}
