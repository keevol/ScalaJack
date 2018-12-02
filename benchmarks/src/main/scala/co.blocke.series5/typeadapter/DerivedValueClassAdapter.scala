package co.blocke.series5
package typeadapter

import java.lang.reflect.Method

import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.{ ClassSymbol, MethodMirror, MethodSymbol, TermName, TypeTag }

object DerivedValueClassAdapter extends TypeAdapterFactory.FromClassSymbol {

  override def typeAdapterOf[T](classSymbol: ClassSymbol, next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    if (classSymbol.isDerivedValueClass) {
      val tpe = tt.tpe

      val constructorSymbol = classSymbol.primaryConstructor.asMethod
      val constructorMirror = currentMirror.reflectClass(classSymbol).reflectConstructor(constructorSymbol)

      val (parameter :: Nil) :: Nil = constructorSymbol.paramLists
      val parameterName = parameter.name.encodedName.toString
      val accessorMethodSymbol = tpe.member(TermName(parameterName)).asMethod
      val accessorMethod = Reflection.methodToJava(accessorMethodSymbol)

      val valueType = parameter.infoIn(tpe).substituteTypes(tpe.typeConstructor.typeParams, tpe.typeArgs)
      val valueTypeAdapter = context.typeAdapter(valueType)

      DerivedValueClassAdapter(constructorMirror, accessorMethodSymbol, accessorMethod, valueTypeAdapter)
    } else {
      next.typeAdapterOf[T]
    }

}

case class DerivedValueClassAdapter[DerivedValueClass, Value](
    constructorMirror:    MethodMirror,
    accessorMethodSymbol: MethodSymbol,
    accessorMethod:       Method,
    valueTypeAdapter:     TypeAdapter[Value]) extends TypeAdapter[DerivedValueClass] {

  override def read(reader: Reader): DerivedValueClass = {
    val value = valueTypeAdapter.read(reader)
    constructorMirror.apply(value).asInstanceOf[DerivedValueClass]
  }

  override def write(value: DerivedValueClass, writer: Writer): Unit = {
    val wrappedValue = accessorMethod.invoke(value).asInstanceOf[Value]
    valueTypeAdapter.write(wrappedValue, writer)
  }

}
