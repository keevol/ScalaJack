package co.blocke.scalajack.flexjson

import co.blocke.scalajack.flexjson.typeadapter.javaprimitives.{ JavaBooleanTypeAdapter, JavaByteTypeAdapter, JavaCharacterTypeAdapter, JavaDoubleTypeAdapter, JavaFloatTypeAdapter, JavaIntegerTypeAdapter, JavaLongTypeAdapter, JavaShortTypeAdapter }
import co.blocke.scalajack.flexjson.typeadapter.joda.JodaDateTimeTypeAdapter
import co.blocke.scalajack.flexjson.typeadapter.{ AnyTypeAdapter, BigDecimalTypeAdapter, BooleanTypeAdapter, ByteTypeAdapter, CaseClassTypeAdapter, CharTypeAdapter, DerivedValueClassAdapter, DerivedValueClassCompanionTypeAdapter, DoubleTypeAdapter, EnumerationTypeAdapter, FloatTypeAdapter, IntTypeAdapter, ListTypeAdapter, LongTypeAdapter, MapTypeAdapter, OptionTypeAdapter, SetTypeAdapter, ShortTypeAdapter, StringTypeAdapter, TryTypeAdapter, TupleTypeAdapter, TypeTypeAdapter, UUIDTypeAdapter }

import scala.language.existentials
import scala.reflect.runtime.universe.{ Type, TypeTag }
import scala.util.{ Success, Try }

object Context {

  val StandardContext = Context()
    .withFactory(AnyTypeAdapter)
    .withFactory(TypeTypeAdapter)
    .withFactory(ListTypeAdapter)
    .withFactory(SetTypeAdapter)
    .withFactory(MapTypeAdapter)
    .withFactory(TupleTypeAdapter)
    .withFactory(CaseClassTypeAdapter)
    .withFactory(OptionTypeAdapter)
    .withFactory(TryTypeAdapter)
    .withFactory(BooleanTypeAdapter)
    .withFactory(CharTypeAdapter)
    .withFactory(ByteTypeAdapter)
    .withFactory(ShortTypeAdapter)
    .withFactory(IntTypeAdapter)
    .withFactory(LongTypeAdapter)
    .withFactory(FloatTypeAdapter)
    .withFactory(DoubleTypeAdapter)
    .withFactory(BigDecimalTypeAdapter)
    .withFactory(StringTypeAdapter)
    .withFactory(DerivedValueClassCompanionTypeAdapter)
    .withFactory(DerivedValueClassAdapter)
    .withFactory(EnumerationTypeAdapter)
    // FIXME    .withFactory(PolymorphicTypeAdapter)
    .withFactory(JavaBooleanTypeAdapter)
    .withFactory(JavaByteTypeAdapter)
    .withFactory(JavaCharacterTypeAdapter)
    .withFactory(JavaDoubleTypeAdapter)
    .withFactory(JavaFloatTypeAdapter)
    .withFactory(JavaIntegerTypeAdapter)
    .withFactory(JavaLongTypeAdapter)
    .withFactory(JavaShortTypeAdapter)
    .withFactory(UUIDTypeAdapter)
    .withFactory(JodaDateTimeTypeAdapter)
}

case class TypeAndTypeArgs(tpe: Type, typeArgs: List[Type])

case class Context(factories: List[TypeAdapterFactory] = Nil) {

  var resolvedTypeAdapterAttempts = Map[TypeAndTypeArgs, Try[TypeAdapter[_]]]()

  def withFactory(factory: TypeAdapterFactory): Context =
    copy(factories = factories :+ factory)

  def typeAdapter(tpe: Type, superParamTypes: List[Type] = List.empty[Type]): TypeAdapter[_] = {
    val typeAndTypeArgs = TypeAndTypeArgs(tpe, superParamTypes)

    resolvedTypeAdapterAttempts.get(typeAndTypeArgs) match {
      case Some(typeAdapterAttempt) ⇒
        typeAdapterAttempt.get

      case None ⇒
        resolvedTypeAdapterAttempts += typeAndTypeArgs → Success(LazyTypeAdapter(this, typeAndTypeArgs.tpe, typeAndTypeArgs.typeArgs))

        val typeAdapterAttempt = Try {
          var optionalTypeAdapter: Option[TypeAdapter[_]] = None

          var remainingFactories = factories
          while (optionalTypeAdapter.isEmpty && remainingFactories.nonEmpty) {
            optionalTypeAdapter = remainingFactories.head.typeAdapter(tpe, this, superParamTypes)
            remainingFactories = remainingFactories.tail
          }

          optionalTypeAdapter.getOrElse(throw new IllegalArgumentException(s"Cannot find a type adapter for $tpe"))
        }

        resolvedTypeAdapterAttempts += typeAndTypeArgs → typeAdapterAttempt

        typeAdapterAttempt.get
    }
  }

  def typeAdapterOf[T](implicit valueTypeTag: TypeTag[T]): TypeAdapter[T] =
    typeAdapter(valueTypeTag.tpe, valueTypeTag.tpe.typeArgs).asInstanceOf[TypeAdapter[T]]

}
