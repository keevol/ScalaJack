package co.blocke.scalajack
package typeadapter

import java.beans.Introspector
import java.lang.reflect.Method

import co.blocke.scalajack.typeadapter.CaseClassTypeAdapter.FieldMember

import scala.collection.immutable.List
import scala.language.existentials
import scala.reflect.ClassTag
import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe._
import scala.util.{ Failure, Success, Try }

// WARNING: This adapter should be last in the list!  This classSymbol.isClass will match pretty much
// anything all the other adapters before it failed to match, so nothing after this adapter will be
// visible/matchable!

object PlainClassTypeAdapter extends TypeAdapterFactory.FromClassSymbol {

  trait PlainFieldMember[Owner] extends ClassLikeTypeAdapter.FieldMember[Owner] {
    type Value = Any
    implicit def ownerClassTag: ClassTag[Owner]

    val valueType: Type
    val valueTypeAdapter: TypeAdapter[Value]
    val valueAccessorMethod: Method // getter method for plain classes!

    // Java & Scala need different setters.  Scala needs to properly set ValueClass values,
    // which can't be done using a Java method call.  Of course Java can *only* use a Java
    // method call, so... we have both.
    val valueSetterMethodSymbol: Option[MethodSymbol] // for Scala
    val valueSetterMethod: Option[Method] // for Java

    val derivedValueClassConstructorMirror: Option[MethodMirror]
    val outerClass: Option[java.lang.Class[_]]

    val defaultValue: Option[Value] = if (isOptional) {
      Some(None).asInstanceOf[Option[Value]]
    } else None

    def valueSet(instance: Owner, value: Value): Unit =
      valueSetterMethodSymbol match {
        case Some(vsms) => currentMirror.reflect(instance).reflectMethod(vsms)(value)
        case None       => valueSetterMethod.get.invoke(instance, value.asInstanceOf[Object])
      }
  }

  override def typeAdapterOf[T](classSymbol: ClassSymbol, next: TypeAdapterFactory)(implicit context: Context, typeTag: TypeTag[T]): TypeAdapter[T] = {
    val tpe = typeTag.tpe
    if (classSymbol.isClass) {
      val constructorSymbol = classSymbol.primaryConstructor.asMethod
      val classMirror = currentMirror.reflectClass(classSymbol)
      val constructorMirror = classMirror.reflectConstructor(constructorSymbol)
      val isSJCapture = !(tpe.baseType(typeOf[SJCapture].typeSymbol) == NoType)

      def newInstance(): T = constructorMirror.apply().asInstanceOf[T]

      def inferConstructorValFields: List[ClassLikeTypeAdapter.FieldMember[T]] =
        Try {
          // Might fail if *all* the constructor's parameters aren't val notated
          constructorSymbol.typeSignatureIn(tpe).paramLists.flatten.zipWithIndex.map({
            case (member, index) =>
              val memberName = member.name.encodedName.toString
              val accessorMethodSymbol = tpe.member(TermName(memberName)).asMethod
              val accessorMethod = Reflection.methodToJava(accessorMethodSymbol)

              val (derivedValueClassConstructorMirror, memberClass) =
                if (member.typeSignature.typeSymbol.isClass) {
                  val memberClassSymbol = member.typeSignature.typeSymbol.asClass

                  if (memberClassSymbol.isDerivedValueClass) {
                    val memberClass = currentMirror.runtimeClass(memberClassSymbol)
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

              val memberType = member.asTerm.typeSignature

              // Exctract DBKey annotation if present
              val dbkeyAnnotation = CaseClassTypeAdapter.getAnnotationValue[DBKey, Int](member, Some(0))

              // Exctract MapName annotation if present
              val mapNameAnnotation = CaseClassTypeAdapter.getAnnotationValue[MapName, String](member)

              val memberTypeAdapter = context.typeAdapter(memberType).asInstanceOf[TypeAdapter[Any]]
              FieldMember[T, Any](index, mapNameAnnotation.getOrElse(memberName), memberType, memberTypeAdapter, memberType /* FIXME */ , accessorMethodSymbol, accessorMethod, derivedValueClassConstructorMirror, None, memberClass, dbkeyAnnotation, mapNameAnnotation, member.annotations)
          })
        } match {
          case Success(m) => m
          case Failure(_) =>
            List.empty[ClassLikeTypeAdapter.FieldMember[T]]
        }

      def dontIgnore(p: Symbol) = {
        // Annoying... @Ignore may be on backing field in a superclass...so we must go find it.
        val includeSuper = tpe.members ++ tpe.typeSymbol.asClass.baseClasses.map(c => c.typeSignature.members).flatten
        val foundPrivateVar = includeSuper.filter(z => z.isPrivate && !z.isMethod && z.name.toString.trim == p.name.toString.trim).headOption
        val ignoreAnno = foundPrivateVar.flatMap(_.annotations.find(_.tree.tpe =:= typeOf[Ignore]))
        ignoreAnno.isEmpty
      }

      def reflectScalaGetterSetterFields: List[PlainFieldMember[T]] =
        tpe.members.filter(p => p.isPublic && p.isMethod).collect {
          // Scala case
          case p if (dontIgnore(p) && tpe.member(TermName(p.name.toString + "_$eq")) != NoSymbol && p.owner != typeOf[SJCapture].typeSymbol) =>
            bakeScalaPlainFieldMember(p)
          // Scala getter/setter style for private var
          case ScalaSetter(p) => bakeScalaPlainFieldMember(p, true)
        }.toList

      def bakeScalaPlainFieldMember(p: Symbol, isGetterSetter: Boolean = false): PlainFieldMember[T] = {
        val memberType = p.asMethod.returnType
        val declaredMemberType = tpe.typeSymbol.asType.toType.member(p.name).asMethod.returnType
        val memberTypeAdapter = context.typeAdapter(memberType).asInstanceOf[TypeAdapter[Any]]

        val (derivedValueClassConstructorMirror2, memberClass) =
          if (memberType.typeSymbol.isClass) {
            val memberClassSymbol = memberType.typeSymbol.asClass

            if (memberClassSymbol.isDerivedValueClass) {
              val memberClass = currentMirror.runtimeClass(memberClassSymbol)
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

        // Exctract DBKey and MapName annotations if present (Note: Here the annotation is not on the getter/setter but the private backing variable!)
        val foundPrivateVar = tpe.members.filter(z => z.isPrivate && !z.isMethod && z.name.toString.trim == p.name.toString.trim).headOption
        val dbkeyAnno = foundPrivateVar.flatMap(CaseClassTypeAdapter.getAnnotationValue[DBKey, Int](_, Some(0)))

        val mapNameAnno = foundPrivateVar.flatMap(CaseClassTypeAdapter.getAnnotationValue[MapName, String](_))

        new PlainFieldMember[T] {
          override type Value = Any
          override implicit val ownerClassTag: ClassTag[T] = ClassTag(runtimeClassOf[T])
          override val name: String = mapNameAnno.getOrElse(p.name.encodedName.toString)
          override val valueType: Type = memberType
          override val valueTypeAdapter: TypeAdapter[Value] = memberTypeAdapter
          override val declaredValueType: Type = declaredMemberType
          override val valueAccessorMethod: Method = Reflection.methodToJava(p.asMethod)
          override val valueSetterMethodSymbol: Option[MethodSymbol] =
            // Gymnatsics to handle...
            if (p.isMethod && isGetterSetter) {
              // 1) Scala-stype getters/setters
              tpe.members.filter(f => f.name.toString == p.name.toString + "_" && f.isMethod).headOption.map(_.asMethod)
                // 2) Public var (no getter/setter)
                .orElse(tpe.members.filter(f => f.name.toString == p.name.toString).headOption.map(_.asMethod))
            } else
              // 3) Constructor val
              Some(tpe.member(TermName(p.name.toString + "_$eq")).asMethod)
          override val valueSetterMethod: Option[Method] = None
          override val derivedValueClassConstructorMirror: Option[MethodMirror] = derivedValueClassConstructorMirror2
          override val outerClass: Option[java.lang.Class[_]] = memberClass
          override val dbKeyIndex: Option[Int] = dbkeyAnno
        }
      }

      object ScalaSetter {
        // Look for methats names that end with '_', canonical Scala for a setter.
        def unapply(p: Symbol): Option[Symbol] = {
          if (p.isMethod && p.name.toString.endsWith("_")) {
            // Find getter and private var
            val name = p.name.toString.reverse.tail.reverse
            val found = tpe.members.filter(_.name.toString == name)
            val dontIgnore = p.annotations.find(_.tree.tpe =:= typeOf[Ignore]).isEmpty && found.head.annotations.find(_.tree.tpe =:= typeOf[Ignore]).isEmpty
            if (found.size > 0 && dontIgnore) {
              Some(found.head) // obtain the getter method symbol
            } else
              None
          } else
            None
        }
      }

      def ignoreThisJavaProperty(pd: java.beans.PropertyDescriptor): Boolean = {
        def annoTypeMatches[A](a: Class[A]): Boolean = a.getTypeName == typeOf[Ignore].toString
        val getter = pd.getReadMethod match {
          case null => false
          case ann  => ann.getAnnotations.toList.map(a => annoTypeMatches(a.annotationType())).foldLeft(false)(_ || _)
        }
        val setter = pd.getReadMethod match {
          case null => false
          case ann  => ann.getAnnotations.toList.map(a => annoTypeMatches(a.annotationType())).foldLeft(false)(_ || _)
        }
        getter || setter
      }

      def reflectJavaGetterSetterFields: List[PlainFieldMember[T]] = {
        val clazz = currentMirror.runtimeClass(tpe.typeSymbol.asClass)

        // Figure out getters/setters, accouting for @Ignore
        Introspector.getBeanInfo(clazz).getPropertyDescriptors.toList
          .filterNot(_.getName == "class").filterNot(ignoreThisJavaProperty(_)).map { propertyDescriptor =>
            val memberType = tpe.member(TermName(propertyDescriptor.getReadMethod.getName)).asMethod.returnType
            val memberTypeAdapter = context.typeAdapter(memberType).asInstanceOf[TypeAdapter[Any]]
            val declaredMemberType = tpe.typeSymbol.asType.toType.member(TermName(propertyDescriptor.getReadMethod.getName)).asMethod.returnType
            new PlainFieldMember[T] {
              override implicit val ownerClassTag: ClassTag[T] = ClassTag(runtimeClassOf[T])
              override type Value = Any
              override val name: String = propertyDescriptor.getName
              override val valueType: Type = memberType
              override val valueTypeAdapter: TypeAdapter[Value] = memberTypeAdapter
              override val declaredValueType: Type = declaredMemberType
              override val valueAccessorMethod: Method = propertyDescriptor.getReadMethod
              override val valueSetterMethodSymbol: Option[MethodSymbol] = None
              override val valueSetterMethod: Option[Method] = Some(propertyDescriptor.getWriteMethod)
              override val derivedValueClassConstructorMirror: Option[MethodMirror] = None
              override val outerClass: Option[java.lang.Class[_]] = None
              override val dbKeyIndex: Option[Int] = None
            }
          }
      }

      // Exctract Collection name annotation if present
      val collectionAnnotation = CaseClassTypeAdapter.getAnnotationValue[Collection, String](classSymbol)

      val hasEmptyConstructor = constructorSymbol.typeSignatureIn(tpe).paramLists.flatten.isEmpty
      inferConstructorValFields match {
        case members if (!members.isEmpty) =>
          // Because all the val fields were found in the constructor we can use a normal CaseClassTypeAdapter
          val ccTransceiver = CaseClassIRTransceiver(
            context,
            constructorMirror,
            context.typeAdapterOf[Type].irTransceiver,
            Nil,
            members,
            isSJCapture,
            typeTag)
          CaseClassTypeAdapter[T](
            ccTransceiver,
            List.empty[ClassLikeTypeAdapter.TypeMember[T]], members,
            collectionAnnotation)
        case _ if (!classSymbol.isJava && hasEmptyConstructor) =>
          val members = reflectScalaGetterSetterFields
          PlainClassTypeAdapter[T](
            new PlainClassIRTransceiver[T](members, (() => newInstance()), isSJCapture), // FIXME
            members,
            collectionAnnotation)
        case _ if (classSymbol.isJava && hasEmptyConstructor) =>
          val members = reflectJavaGetterSetterFields
          PlainClassTypeAdapter[T](
            new PlainClassIRTransceiver[T](members, (() => newInstance()), isSJCapture), // FIXME
            members,
            collectionAnnotation)
        // There's no support for Java classes with non-empty constructors.  If multiple, which one to use?
        // Opens the door for uncertain results.
        case _ =>
          next.typeAdapterOf[T]
      }

    } else {
      next.typeAdapterOf[T]
    }
  }

}

case class PlainClassTypeAdapter[T](
    override val irTransceiver: IRTransceiver[T],
    fieldMembers:               List[ClassLikeTypeAdapter.FieldMember[T]],
    collectionName:             Option[String]                            = None) extends ClassLikeTypeAdapter[T] {
  val typeMembers = List.empty[ClassLikeTypeAdapter.TypeMember[T]]
}
