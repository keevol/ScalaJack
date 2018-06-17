package co.blocke.scalajack
package typeadapter

import java.lang.reflect.Method

object DerivedValueClassAdapter extends TypeAdapterFactory.FromClassSymbol {

  override def typeAdapterOf[T](classSymbol: ClassSymbol, next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    if (classSymbol.isDerivedValueClass) {
      val tpe = tt.tpe

      val constructorSymbol = classSymbol.primaryConstructor.asMethod
      val constructorMirror = reflectClass(classSymbol).reflectConstructor(constructorSymbol)

      val (parameter :: Nil) :: Nil = constructorSymbol.paramLists
      val parameterName = parameter.name.encodedName.toString
      val accessorMethodSymbol = tpe.member(TermName(parameterName)).asMethod
      val accessorMethod = Reflection.methodToJava(accessorMethodSymbol)

      type Source = Any
      type Derived = T

      type Wrapped = T
      val wrappedTypeTag: TypeTag[Wrapped] = tt.asInstanceOf[TypeTag[Wrapped]]

      type Unwrapped = Any
      val valueType = parameter.infoIn(tpe).substituteTypes(tpe.typeConstructor.typeParams, tpe.typeArgs)
      val unwrappedTypeTag: TypeTag[Unwrapped] = TypeTags.of[Unwrapped](valueType)

      val valueTypeAdapter = context.typeAdapter(valueType).asInstanceOf[TypeAdapter[Unwrapped]]

      def wrap(unwrapped: Unwrapped): Wrapped = constructorMirror.apply(unwrapped).asInstanceOf[Wrapped]

      def unwrap(wrapped: Wrapped): Unwrapped = accessorMethod.invoke(wrapped)

      DerivedValueClassAdapter[Wrapped, Unwrapped](
        new DerivedValueClassDeserializer[Wrapped, Unwrapped](valueTypeAdapter.deserializer, wrap)(wrappedTypeTag),
        new DerivedValueClassSerializer[Wrapped, Unwrapped](unwrap, valueTypeAdapter.serializer)(unwrappedTypeTag),
        constructorMirror, accessorMethodSymbol, accessorMethod, valueTypeAdapter)
    } else {
      next.typeAdapterOf[T]
    }

}

case class DerivedValueClassAdapter[DerivedValueClass, Value](
    override val deserializer: Deserializer[DerivedValueClass],
    override val serializer:   Serializer[DerivedValueClass],
    constructorMirror:         MethodMirror,
    accessorMethodSymbol:      MethodSymbol,
    accessorMethod:            Method,
    valueTypeAdapter:          TypeAdapter[Value]) extends TypeAdapter[DerivedValueClass] {

  override def read(reader: Reader): DerivedValueClass = {
    val value = valueTypeAdapter.read(reader)
    constructorMirror.apply(value).asInstanceOf[DerivedValueClass]
  }

  override def write(value: DerivedValueClass, writer: Writer): Unit = {
    val wrappedValue = accessorMethod.invoke(value).asInstanceOf[Value]
    valueTypeAdapter.write(wrappedValue, writer)
  }

}
