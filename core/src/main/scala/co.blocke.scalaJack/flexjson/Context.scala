package co.blocke.scalajack.flexjson

import java.util.concurrent.ConcurrentHashMap

import co.blocke.scalajack.flexjson.typeadapter.javaprimitives.{ JavaBooleanTypeAdapter, JavaByteTypeAdapter, JavaCharacterTypeAdapter, JavaDoubleTypeAdapter, JavaFloatTypeAdapter, JavaIntegerTypeAdapter, JavaLongTypeAdapter, JavaNumberTypeAdapter, JavaShortTypeAdapter }
import co.blocke.scalajack.flexjson.typeadapter.joda.JodaDateTimeTypeAdapter
import co.blocke.scalajack.flexjson.typeadapter.{ AnyTypeAdapter, BigDecimalTypeAdapter, BooleanTypeAdapter, ByteTypeAdapter, CaseClassTypeAdapter, CharTypeAdapter, DerivedValueClassAdapter, DerivedValueClassCompanionTypeAdapter, DoubleTypeAdapter, EnumerationTypeAdapter, FloatTypeAdapter, IntTypeAdapter, ListTypeAdapter, LongTypeAdapter, MapTypeAdapter, OptionTypeAdapter, SetTypeAdapter, ShortTypeAdapter, StringTypeAdapter, TryTypeAdapter, TupleTypeAdapter, TypeParameterTypeAdapter, TypeTypeAdapter, UUIDTypeAdapter }

import scala.language.existentials
import scala.reflect.runtime.universe.{ Type, TypeTag }
import scala.util.{ Success, Try }

object Context {

  val StandardContext = Context()
    .withFactory(TypeParameterTypeAdapter)
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
    .withFactory(JavaNumberTypeAdapter)
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

  object TypeEntryFactory extends java.util.function.Function[TypeAndTypeArgs, TypeEntry] {
    override def apply(t: TypeAndTypeArgs): TypeEntry =
      new TypeEntry(t.tpe, t.typeArgs)
  }

  class TypeEntry(tpe: Type, typeArgs: List[Type]) {

    @volatile
    private var typeAdapterAttempt: Try[TypeAdapter[_]] = _

    def typeAdapter: TypeAdapter[_] = {
      this.synchronized {
        val typeAdapterAttempt = {
          val firstTypeAdapterAttempt = this.typeAdapterAttempt
          if (firstTypeAdapterAttempt == null) {
            this.synchronized {
              val secondTypeAdapterAttempt = this.typeAdapterAttempt
              if (secondTypeAdapterAttempt == null) {
                this.typeAdapterAttempt = Success(LazyTypeAdapter(Context.this, tpe))

                val thirdTypeAdapterAttempt = Try {
                  var optionalTypeAdapter: Option[TypeAdapter[_]] = None

                  var remainingFactories = factories
                  while (optionalTypeAdapter.isEmpty && remainingFactories.nonEmpty) {
                    optionalTypeAdapter = remainingFactories.head.typeAdapter(tpe, Context.this)
                    remainingFactories = remainingFactories.tail
                  }

                  if (optionalTypeAdapter.isEmpty) {
                    println("CAN'T BE FOUND")
                  }

                  optionalTypeAdapter.getOrElse(throw new IllegalArgumentException(s"Cannot find a type adapter for $tpe"))
                }

                this.typeAdapterAttempt = thirdTypeAdapterAttempt

                thirdTypeAdapterAttempt
              } else {
                secondTypeAdapterAttempt
              }
            }
          } else {
            firstTypeAdapterAttempt
          }
        }

        typeAdapterAttempt.get
      }
    }

  }

  private val typeEntries = new ConcurrentHashMap[TypeAndTypeArgs, TypeEntry]

  var resolvedTypeAdapterAttempts = Map[TypeAndTypeArgs, Try[TypeAdapter[_]]]()

  def withFactory(factory: TypeAdapterFactory): Context =
    copy(factories = factories :+ factory)

  def typeAdapter(tpe: Type): TypeAdapter[_] = {
    val typeAndTypeArgs = TypeAndTypeArgs(tpe, null)

    typeEntries.computeIfAbsent(typeAndTypeArgs, TypeEntryFactory).typeAdapter

    //    resolvedTypeAdapterAttempts.get(typeAndTypeArgs) match {
    //      case Some(typeAdapterAttempt) ⇒
    //        typeAdapterAttempt.get
    //
    //      case None ⇒
    //        resolvedTypeAdapterAttempts += typeAndTypeArgs → Success(LazyTypeAdapter(this, typeAndTypeArgs.tpe, typeAndTypeArgs.typeArgs))
    //
    //        val typeAdapterAttempt = Try {
    //          var optionalTypeAdapter: Option[TypeAdapter[_]] = None
    //
    //          var remainingFactories = factories
    //          while (optionalTypeAdapter.isEmpty && remainingFactories.nonEmpty) {
    //            optionalTypeAdapter = remainingFactories.head.typeAdapter(tpe, this, superParamTypes)
    //            remainingFactories = remainingFactories.tail
    //          }
    //
    //          optionalTypeAdapter.getOrElse(throw new IllegalArgumentException(s"Cannot find a type adapter for $tpe"))
    //        }
    //
    //        resolvedTypeAdapterAttempts += typeAndTypeArgs → typeAdapterAttempt
    //
    //        typeAdapterAttempt.get
    //    }
  }

  def typeAdapterOf[T](implicit valueTypeTag: TypeTag[T]): TypeAdapter[T] =
    typeAdapter(valueTypeTag.tpe).asInstanceOf[TypeAdapter[T]]

}
