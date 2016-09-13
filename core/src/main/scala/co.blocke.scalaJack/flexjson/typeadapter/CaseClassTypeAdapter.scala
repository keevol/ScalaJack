package co.blocke.scalajack.flexjson.typeadapter

import java.lang.reflect.Method

import co.blocke.scalajack.flexjson.FlexJsonFlavor.MemberName
import co.blocke.scalajack.flexjson.typeadapter.CaseClassTypeAdapter.Member
import co.blocke.scalajack.flexjson.{ Context, EmptyReader, Reader, Reflection, TokenType, TypeAdapter, TypeAdapterFactory, Writer }

import scala.collection.mutable
import scala.language.reflectiveCalls
import scala.language.existentials
import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.{ ClassSymbol, MethodMirror, MethodSymbol, TermName, Type, typeOf }

object CaseClassTypeAdapter extends TypeAdapterFactory.FromClassSymbol {

  case class Member[T](
      index:                              Int,
      name:                               String,
      valueTypeAdapter:                   TypeAdapter[T],
      valueAccessorMethodSymbol:          MethodSymbol,
      valueAccessorMethod:                Method,
      derivedValueClassConstructorMirror: Option[MethodMirror],
      defaultValueMirror:                 Option[MethodMirror],
      outerClass:                         Option[java.lang.Class[_]]
  ) {

    def valueIn(instance: Any): T = {
      val value = valueAccessorMethod.invoke(instance)

      if (outerClass.isEmpty || outerClass.get.isInstance(value)) {
        value.asInstanceOf[T]
      } else {
        derivedValueClassConstructorMirror match {
          case Some(methodMirror) ⇒
            methodMirror.apply(value).asInstanceOf[T]

          case None ⇒
            value.asInstanceOf[T]
        }
      }
    }

    def writeValue(parameterValue: Any, writer: Writer): Unit = {
      valueTypeAdapter.asInstanceOf[TypeAdapter[Any]].write(parameterValue, writer)
    }

    def hasDefaultValue: Boolean = defaultValueMirror.isDefined

    def defaultValue: T = defaultValueMirror.get.apply().asInstanceOf[T]

  }

  override def typeAdapter(tpe: Type, classSymbol: ClassSymbol, context: Context): Option[TypeAdapter[_]] =
    if (classSymbol.isCaseClass) {
      val constructorSymbol = classSymbol.primaryConstructor.asMethod

      val constructorTypeSignature1 = constructorSymbol.typeSignature
      val constructorTypeSignature2 = constructorSymbol.typeSignatureIn(tpe)

      val params = constructorTypeSignature2.paramLists.flatten
      for (param ← params) {
        val ts = param.asTerm.typeSignature
        //        println(param)
      }

      val classMirror = currentMirror.reflectClass(classSymbol)
      val constructorMirror = classMirror.reflectConstructor(constructorSymbol)

      val companionType: Type = classSymbol.companion.typeSignature
      val companionObject = currentMirror.reflectModule(classSymbol.companion.asModule).instance
      val companionMirror = currentMirror.reflect(companionObject)

      //      val typeBeforeSubstitution = constructorSymbol.info
      //
      //      val typeAfterSubstitution =
      //        if (superParamTypes.isEmpty) {
      // tpe
      // typeBeforeSubstitution.substituteTypes(tpe.typeConstructor.typeParams, superParamTypes)
      //          typeBeforeSubstitution.substituteTypes(tpe.typeParams, tpe.typeParams.map(_ ⇒ typeOf[Any]))
      //        } else {
      //          typeBeforeSubstitution.substituteTypes(tpe.typeConstructor.typeParams, superParamTypes)
      //        }

      val memberNameTypeAdapter = context.typeAdapterOf[MemberName]

      val members = constructorSymbol.typeSignatureIn(tpe).paramLists.flatten.zipWithIndex.map({
        case (member, index) ⇒
          val memberName = member.name.encodedName.toString
          val accessorMethodSymbol = tpe.member(TermName(memberName)).asMethod
          val accessorMethod = Reflection.methodToJava(accessorMethodSymbol)

          val (derivedValueClassConstructorMirror, memberClass) =
            if (member.typeSignature.typeSymbol.isClass) {
              val memberClassSymbol = member.typeSignature.typeSymbol.asClass
              val memberClass = currentMirror.runtimeClass(memberClassSymbol)

              if (memberClassSymbol.isDerivedValueClass) {
                // The accessor will actually return the "inner" value, not the value class.
                val constructorMethodSymbol = memberClassSymbol.primaryConstructor.asMethod
                //              val innerClass = currentMirror.runtimeClass(constructorMethodSymbol.paramLists.flatten.head.info.typeSymbol.asClass)
                (Some(currentMirror.reflectClass(memberClassSymbol).reflectConstructor(constructorMethodSymbol)), Some(memberClass))
              } else {
                (None, None)
              }
            } else {
              (None, None)
            }

          val defaultValueAccessorMirror =
            if (member.typeSignature.typeSymbol.isClass) {
              val defaultValueAccessor = companionType.member(TermName("apply$default$" + (index + 1)))
              if (defaultValueAccessor.isMethod) {
                Some(companionMirror.reflectMethod(defaultValueAccessor.asMethod))
              } else {
                None
              }
            } else {
              None
            }

          val memberType = member.asTerm.typeSignature
          val memberTypeAdapter = context.typeAdapter(memberType)
          Member(index, memberName, memberTypeAdapter, accessorMethodSymbol, accessorMethod, derivedValueClassConstructorMirror, defaultValueAccessorMirror, memberClass)
      })

      Some(CaseClassTypeAdapter(tpe, constructorMirror, tpe, memberNameTypeAdapter, members))
    } else {
      None
    }

}

case class CaseClassTypeAdapter[T >: Null](
    caseClassType:         Type,
    constructorMirror:     MethodMirror,
    tpe:                   Type,
    memberNameTypeAdapter: TypeAdapter[MemberName],
    members:               List[Member[_]]
) extends TypeAdapter[T] {

  val membersByName = members.map(member ⇒ member.name → member.asInstanceOf[Member[Any]]).toMap

  override def read(reader: Reader): T =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.BeginObject ⇒
        val numberOfMembers = members.length

        val arguments = new Array[Any](numberOfMembers)
        val found = new mutable.BitSet(numberOfMembers)

        reader.peek match {
          case TokenType.Null ⇒
            reader.readNull()

          case TokenType.BeginObject ⇒
            reader.beginObject()

            while (reader.hasMoreMembers) {
              val memberName = memberNameTypeAdapter.read(reader)

              val optionalMember = membersByName.get(memberName)
              optionalMember match {
                case Some(member) ⇒
                  arguments(member.index) = member.valueTypeAdapter.read(reader)
                  found(member.index) = true

                case None ⇒
                  reader.skipValue()
              }
            }

            var index = 0
            while (index < numberOfMembers) {
              if (!found(index)) {
                val member = members(index)
                member.defaultValueMirror match {
                  case Some(mirror) ⇒
                    arguments(index) = mirror.apply()

                  case None ⇒
                    arguments(index) = member.valueTypeAdapter.read(EmptyReader)
                }
              }

              index += 1
            }

            reader.endObject()

            constructorMirror.apply(arguments: _*).asInstanceOf[T]
        }
    }

  override def write(value: T, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.beginObject()

      for (member ← members) {
        val memberValue = member.valueIn(value)

        memberNameTypeAdapter.write(member.name, writer)
        member.writeValue(memberValue, writer)
      }

      writer.endObject()
    }

}
