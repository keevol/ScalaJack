package co.blocke.scalajack
package typeadapter

import java.lang.reflect.Method

import co.blocke.scalajack.{ Symbol, TypeTag, typeOf }

import scala.reflect.runtime.universe._
import scala.language.existentials
import scala.reflect.runtime.currentMirror

object CaseClassTypeAdapter extends TypeAdapterFactory.FromClassSymbol {

  case class TypeMember[Owner](name: MemberName, typeSignature: Type, baseType: Type) extends ClassLikeTypeAdapter.TypeMember[Owner]

  case class FieldMember[Owner, T](
      index:                              Int,
      name:                               MemberName,
      valueType:                          Type,
      valueTypeAdapter:                   TypeAdapter[T],
      declaredValueType:                  Type,
      valueAccessorMethodSymbol:          MethodSymbol,
      valueAccessorMethod:                Method,
      derivedValueClassConstructorMirror: Option[MethodMirror],
      defaultValueMirror:                 Option[MethodMirror],
      outerClass:                         Option[java.lang.Class[_]],
      dbKeyIndex:                         Option[Int],
      fieldMapName:                       Option[String],
      annotations:                        List[Annotation]) extends ClassLikeTypeAdapter.FieldMember[Owner] {

    override type Value = T

    val defaultValue: Option[Value] = defaultValueMirror.map(_.apply().asInstanceOf[T]).orElse(valueTypeAdapter.defaultValue)
  }

  def getAnnotationValue[T, U](sym: Symbol, default: Option[U] = None)(implicit tt: TypeTag[T]): Option[U] = {
    val annotation = sym.annotations.find(_.tree.tpe =:= typeOf[T])
    annotation.flatMap { a =>
      if (a.tree.children.tail.size == 0)
        default
      else
        a.tree.children.tail.head.collect({
          case Literal(Constant(value)) => value
        }).headOption
    }.asInstanceOf[Option[U]]
  }

  override def typeAdapterOf[T](classSymbol: ClassSymbol, next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    if (classSymbol.isCaseClass) {
      val constructorSymbol = classSymbol.primaryConstructor.asMethod

      val classMirror = currentMirror.reflectClass(classSymbol)
      val constructorMirror = classMirror.reflectConstructor(constructorSymbol)

      val companionType: Type = classSymbol.companion.typeSignature
      val companionObject = currentMirror.reflectModule(classSymbol.companion.asModule).instance
      val companionMirror = currentMirror.reflect(companionObject)

      val isSJCapture = !(tt.tpe.baseType(typeOf[SJCapture].typeSymbol) == NoType)

      val tm = tt.tpe.members.filter(_.isType).toList
      val classTypeParamMap = tt.tpe.typeSymbol.asClass.typeParams.zip(tt.tpe.typeArgs).toMap
      val typeMembers = tm map { m =>
        CaseClassTypeAdapter.TypeMember[T](m.name.decodedName.toString, m.typeSignature, classTypeParamMap(m.typeSignature.typeSymbol))
      }

      val params1 = constructorSymbol.typeSignatureIn(tt.tpe).paramLists.flatten
      val params2 = constructorSymbol.typeSignatureIn(tt.tpe.typeSymbol.asType.toType).paramLists.flatten

      val fieldMembers = for (((member, param2), index) <- (params1 zip params2).zipWithIndex) yield {
        val memberName = member.name.encodedName.toString
        val accessorMethodSymbol = tt.tpe.member(TermName(memberName)).asMethod
        val accessorMethod = Reflection.methodToJava(accessorMethodSymbol)

        val (derivedValueClassConstructorMirror, memberClass) =
          if (member.typeSignature.typeSymbol.isClass) {
            val memberClassSymbol = member.typeSignature.typeSymbol.asClass

            if (memberClassSymbol.isDerivedValueClass) {
              val memberClass = currentMirror.runtimeClass(memberClassSymbol)
              // The accessor will actually return the "inner" value, not the value class.
              val constructorMethodSymbol = memberClassSymbol.primaryConstructor.asMethod
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

        val declaredMemberType = param2.asTerm.typeSignature

        // Exctract DBKey annotation if present
        val optionalDbKeyIndex = getAnnotationValue[DBKey, Int](member, Some(0))
        //          .annotations.find(_.tree.tpe =:= typeOf[DBKey])
        //          .map { index =>
        //            if (index.tree.children.size > 1)
        //              index.tree.children(1).productElement(1).asInstanceOf[scala.reflect.internal.Trees$Literal].value().value
        //            else
        //              0
        //          }.asInstanceOf[Option[Int]]

        // Extract MapName annotation if present
        val optionalMapName = getAnnotationValue[MapName, String](member)

        val memberTypeAdapter = context.typeAdapter(memberType).asInstanceOf[TypeAdapter[Any]]

        CaseClassTypeAdapter.FieldMember[T, Any](index, optionalMapName.getOrElse(memberName), memberType, memberTypeAdapter, declaredMemberType, accessorMethodSymbol, accessorMethod, derivedValueClassConstructorMirror, defaultValueAccessorMirror, memberClass, optionalDbKeyIndex, optionalMapName, member.annotations)
      }

      // Exctract Collection name annotation if present
      val collectionAnnotation = getAnnotationValue[Collection, String](classSymbol)

      CaseClassTypeAdapter[T](
        new ClassDeserializerUsingReflectedConstructor[T](
          context,
          constructorMirror,
          context.typeAdapterOf[Type].deserializer,
          typeMembers,
          fieldMembers,
          isSJCapture),
        new ClassSerializer[T](
          context,
          context.typeAdapterOf[Type].serializer,
          typeMembers,
          fieldMembers,
          isSJCapture),
        typeMembers,
        fieldMembers,
        collectionAnnotation)
    } else {
      next.typeAdapterOf[T]
    }

}

case class CaseClassTypeAdapter[T](
    override val deserializer: Deserializer[T],
    override val serializer:   Serializer[T],
    typeMembers:               List[ClassLikeTypeAdapter.TypeMember[T]],
    fieldMembers:              List[ClassLikeTypeAdapter.FieldMember[T]],
    collectionName:            Option[String]                            = None) extends ClassLikeTypeAdapter[T] {
}
