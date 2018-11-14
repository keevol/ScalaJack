package co.blocke.scalajack

import typeadapter._
//import typeadapter.javatime._
//import typeadapter.javaprimitives._
//import typeadapter.javacollections._

import scala.reflect.runtime.currentMirror
import scala.util.{ Success, Try }

import java.util.concurrent.ConcurrentHashMap

object Context {

  val StandardContext = Context()
    .withFactory(IRParsingFallbackTypeAdapter)
    .withFactory(TermTypeAdapterFactory)
    .withFactory(TypeParameterTypeAdapter)
    .withFactory(AnyTypeAdapter)
    .withFactory(TypeTypeAdapter)
    .withFactory(MapTypeAdapter)
    .withFactory(CanBuildFromTypeAdapter)
    .withFactory(TupleTypeAdapter)

    .withFactory(DerivedValueClassTypeAdapter) // <-- WARNING: This must preceed CaseClassTypeAdapter or all
    //              ValueClasses will be interpreted as case classes!

    .withFactory(CaseClassTypeAdapter)
    .withFactory(OptionTypeAdapter)
    .withFactory(TryTypeAdapter)
    .withFactory(EitherTypeAdapter)
    .withFactory(SealedTraitTypeAdapter)
    .withFactory(BooleanTypeAdapter)
    .withFactory(CharTypeAdapter)
    .withFactory(ByteTypeAdapter)
    .withFactory(ShortTypeAdapter)
    .withFactory(IntTypeAdapter)
    .withFactory(LongTypeAdapter)
    .withFactory(FloatTypeAdapter)
    .withFactory(DoubleTypeAdapter)
    .withFactory(BigDecimalTypeAdapter)
    .withFactory(BigIntTypeAdapter)
    .withFactory(StringTypeAdapter)
    .withFactory(EnumerationTypeAdapter)
    .withFactory(UUIDTypeAdapter)
  /*
    .withFactory(JavaNumberTypeAdapter)
    .withFactory(JavaBooleanTypeAdapter)
    .withFactory(JavaByteTypeAdapter)
    .withFactory(JavaCharacterTypeAdapter)
    .withFactory(JavaDoubleTypeAdapter)
    .withFactory(JavaFloatTypeAdapter)
    .withFactory(JavaIntegerTypeAdapter)
    .withFactory(JavaLongTypeAdapter)
    .withFactory(JavaShortTypeAdapter)
    .withFactory(JavaBigDecimalTypeAdapter)
    .withFactory(JavaBigIntegerTypeAdapter)
    .withFactory(JavaMapTypeAdapter)
    .withFactory(JavaCollectionTypeAdapter)
    .withFactory(DurationTypeAdapter)
    .withFactory(InstantTypeAdapter)
    .withFactory(LocalDateTimeTypeAdapter)
    .withFactory(LocalDateTypeAdapter)
    .withFactory(LocalTimeTypeAdapter)
    .withFactory(OffsetDateTimeTypeAdapter)
    .withFactory(OffsetTimeTypeAdapter)
    .withFactory(PeriodTypeAdapter)
    .withFactory(ZonedDateTimeTypeAdapter)
    */
}

case class Context(defaultHint: String = "", factories: List[TypeAdapterFactory] = Nil, sjFlavor: Option[ScalaJackLike[_, _]] = None) {

  sealed trait Phase
  case object Uninitialized extends Phase
  case object Initializing extends Phase
  case class Initialized(typeAdapterAttempt: Try[TypeAdapter[_]]) extends Phase

  object TypeEntryFactory extends java.util.function.Function[Type, TypeEntry] {
    override def apply(tpe: Type): TypeEntry = new TypeEntry(tpe)
  }

  class TypeEntry(tpe: Type) {

    @volatile
    private var phase: Phase = Uninitialized

    override def toString(): String = tpe.toString()

    def typeAdapter: TypeAdapter[_] = {
      val attempt =
        phase match {
          case Initialized(a) =>
            a

          case Uninitialized | Initializing =>
            synchronized {
              phase match {
                case Uninitialized =>
                  phase = Initializing

                  val typeAdapterAttempt = Try {
                    val context: Context = Context.this
                    val tt: TypeTag[Any] = TypeTags.of(currentMirror, tpe)

                    val head :: tail = factories
                    head.typeAdapterOf(next = TypeAdapterFactory(tail))(context, tt)
                  }
                  phase = Initialized(typeAdapterAttempt)

                  typeAdapterAttempt

                case Initializing =>
                  Success(LazyTypeAdapter(Context.this, tpe))

                case Initialized(a) =>
                  a
              }
            }
        }

      attempt.get
    }

  }

  private val typeEntries = new ConcurrentHashMap[Type, TypeEntry]

  def withFactory(factory: TypeAdapterFactory): Context =
    copy(factories = factories :+ factory)

  def typeAdapter(tpe: Type): TypeAdapter[_] =
    typeEntries.computeIfAbsent(tpe, TypeEntryFactory).typeAdapter

  def typeAdapterOf[T: TypeTag]: TypeAdapter[T] =
    typeAdapter(implicitly[TypeTag[T]].tpe).asInstanceOf[TypeAdapter[T]]

  def irTransceiver(tpe: Type): IRTransceiver[_] =
    typeAdapter(tpe).irTransceiver

  def irTransceiverOf[T: TypeTag]: IRTransceiver[T] =
    typeAdapterOf[T].irTransceiver

  def addTypeAdapterFactories(typeAdapterFactories: TypeAdapterFactory*): Context = copy(factories = typeAdapterFactories.toList ++ factories)

  // Unwind the wrapping magic of IRParsingFallback and Term type adapters to get to the "real" adapter.
  // (for debugging principally--assumptions made, so be warned!)
  def resolvedTypeAdapterOf[T: TypeTag]: TypeAdapter[T] =
    typeAdapterOf[T].as[IRParsingFallbackTypeAdapter[T]].decorated.as[typeadapter.TermTypeAdapter[T]].next
}
