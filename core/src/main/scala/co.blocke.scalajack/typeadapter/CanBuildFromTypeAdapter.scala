package co.blocke.scalajack
package typeadapter

import scala.collection.generic.CanBuildFrom
import scala.collection.{ GenMapLike, GenTraversableOnce, mutable }

object CanBuildFromTypeAdapter extends TypeAdapterFactory.<:<.withOneTypeParam[GenTraversableOnce] {

  override def create[E, T <: GenTraversableOnce[E]](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T], ttX: TypeTag[GenTraversableOnce[E]], ttElement: TypeTag[E]): TypeAdapter[T] = {
    val requiredClassSymbol = tt.tpe.typeSymbol.asClass

    val companionSymbol = requiredClassSymbol.companion.asModule
    val companionType = companionSymbol.info

    // Examples in comments reference Scala's List[A] type.

    val methods = for (member <- companionType.members if member.isMethod) yield member.asMethod

    // `implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, List[A]] = ...`
    val implicitConversions = for (method <- methods if method.isImplicit && method.paramLists.flatten.isEmpty && method.returnType <:< typeOf[CanBuildFrom[_, _, _]]) yield method

    val matchingTypeAdapters = implicitConversions flatMap { method =>
      // returnTypeAsCanBuildFrom == CanBuildFrom[Coll, A, List[A]]
      val returnTypeAsCanBuildFrom = method.returnType.baseType(typeOf[CanBuildFrom[_, _, _]].typeSymbol)

      // typeParam == A
      val typeParams = method.typeParams

      // toType == List[A]
      val toType = returnTypeAsCanBuildFrom.typeArgs(2)

      val typeParamSubstitutions: List[(Symbol, Type)] = typeParams flatMap { typeParam =>
        // typeParam == A
        // optionalTypeArg == Some(String)
        val optionalTypeArg = Reflection.solveForNeedleAfterSubstitution(
          haystackBeforeSubstitution = toType,
          haystackAfterSubstitution  = tt.tpe.baseType(toType.typeSymbol),
          needleBeforeSubstitution   = typeParam.asType.toType)
        optionalTypeArg.map(typeArg => typeParam -> typeArg)
      }

      // elementTypeBeforeSubstitution == A
      val elementTypeBeforeSubstitution = returnTypeAsCanBuildFrom.typeArgs(1)

      // elementTypeAfterSubstitution == String
      val elementTypeAfterSubstitution = elementTypeBeforeSubstitution.substituteTypes(typeParamSubstitutions.map(_._1), typeParamSubstitutions.map(_._2))

      // elementTypeAdapter == TypeAdapter[String]
      val elementTypeAdapter = context.typeAdapter(elementTypeAfterSubstitution)

      val companionInstance = reflectModule(companionSymbol).instance
      val canBuildFrom = reflect(companionInstance).reflectMethod(method).apply()

      if (tt.tpe <:< typeOf[GenMapLike[_, _, _]] && elementTypeAfterSubstitution <:< typeOf[(_, _)]) {
        /*
        I'm not 100% sure what this clause is supposed to do, or what case it handles!  Cutting it out doesn't
        break any tests so remvoing it for now...

        type K = Any
        type V = Any
        type M = GenMap[K, V] with GenMapLike[K, V, Any]

        val cbf = canBuildFrom.asInstanceOf[CanBuildFrom[_, (K, V), GenMapLike[K, V, M]]]

        val keyType = elementTypeAfterSubstitution.typeArgs(0)
        val keyTypeAdapter = (context.typeAdapter(keyType) match {
          case kta: OptionTypeAdapter[_] => kta.noneAsEmptyString // output "" for None for map keys
          case kta                       => kta
        }).asInstanceOf[TypeAdapter[K]]
        val valueTypeAdapter = context.typeAdapter(elementTypeAfterSubstitution.typeArgs(1)).asIn stanceOf[TypeAdapter[V]]
        */
        /*
        Some(CanBuildMapTypeAdapter(
          new MapDeserializer[K, V, M](
            keyTypeAdapter.deserializer,
            valueTypeAdapter.deserializer,
            null,
            () => ???
          )(tt),
          new MapSerializer[K, V, M](
            keyTypeAdapter.serializer,
            valueTypeAdapter.serializer
          ),
          canBuildFrom.asInstanceOf[CanBuildFrom[Any, Any, GenMapLike[Any, Any, Any]]],
          keyTypeAdapter.asInstanceOf[TypeAdapter[Any]],
          valueTypeAdapter.asInstanceOf[TypeAdapter[Any]]))
          */
        throw new java.lang.UnsupportedOperationException("This functionality has not yet been implemented")
      } else {
        def newBuilder(): mutable.Builder[E, T] = canBuildFrom.asInstanceOf[CanBuildFrom[Any, E, T]]()

        Some(CanBuildFromTypeAdapter[E, T](
          new CollectionIRTransceiver[E, T](elementTypeAdapter.irTransceiver.asInstanceOf[IRTransceiver[E]], () => newBuilder),
          canBuildFrom.asInstanceOf[CanBuildFrom[_, E, T]],
          elementTypeAdapter.asInstanceOf[TypeAdapter[E]]))
      }
    }

    matchingTypeAdapters.headOption.map(_.asInstanceOf[TypeAdapter[T]]).getOrElse(next.typeAdapterOf[T])
  }

}

case class CanBuildMapTypeAdapter[Key, Value, To <: GenMapLike[Key, Value, To]](
    override val irTransceiver: IRTransceiver[To],
    canBuildFrom:               CanBuildFrom[_, (Key, Value), To],
    keyTypeAdapter:             TypeAdapter[Key],
    valueTypeAdapter:           TypeAdapter[Value]) extends TypeAdapter[To]

case class CanBuildFromTypeAdapter[Elem, To <: GenTraversableOnce[Elem]](
    override val irTransceiver: IRTransceiver[To],
    canBuildFrom:               CanBuildFrom[_, Elem, To],
    elementTypeAdapter:         TypeAdapter[Elem])(implicit tt: TypeTag[To]) extends TypeAdapter[To]

class CollectionIRTransceiver[E, C <: GenTraversableOnce[E]](elementTransceiver: IRTransceiver[E], newBuilder: () => mutable.Builder[E, C])(implicit tt: TypeTag[C]) extends IRTransceiver[C] {

  self =>

  private val taggedNull: TypeTagged[C] = TypeTagged(null.asInstanceOf[C], tt.tpe)

  private class TaggedCollection(override val get: C, taggedElements: List[TypeTagged[E]]) extends TypeTagged[C] {
    override lazy val tpe: Type = {
      //      val elementType = lub(taggedElements.map(_.tpe))
      typeOf[C]
    }
  }

  private val GenTraversableOnceTypeSymbol: TypeSymbol = symbolOf[GenTraversableOnce[_]]

  override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[C] =
    ir match {
      case IRNull() =>
        ReadSuccess(taggedNull)

      case IRArray(elements) =>
        val elementsBuilder = newBuilder()
        val taggedElementsBuilder = List.newBuilder[TypeTagged[E]]
        val errorSequencesBuilder = Seq.newBuilder[Seq[(Path, ReadError)]]

        elements.zipWithIndex.foreach {
          case (elementIR, index) =>
            elementTransceiver.read(path \ index, elementIR) match {
              case ReadSuccess(taggedElement @ TypeTagged(element)) =>
                elementsBuilder += element
                taggedElementsBuilder += taggedElement

              case ReadFailure(errorSequence) =>
                errorSequencesBuilder += errorSequence
            }
        }

        val errorSequences: Seq[Seq[(Path, ReadError)]] = errorSequencesBuilder.result()
        if (errorSequences.isEmpty) {
          val taggedElements = taggedElementsBuilder.result()
          ReadSuccess(new TaggedCollection(elementsBuilder.result(), taggedElements))
        } else {
          ReadFailure(errorSequences.flatten.to[collection.immutable.Seq])
        }

      case _ =>
        ReadFailure(path, ReadError.Unexpected(s"Expected a JSON array, not $ir", reportedBy = self))
    }

  override def write[IR, WIRE](tagged: TypeTagged[C])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
    tagged match {
      case TypeTagged(null) =>
        WriteSuccess(IRNull())

      case TypeTagged(collection) =>
        lazy val elementType: Type = tagged.tpe.baseType(GenTraversableOnceTypeSymbol).typeArgs.head

        class TaggedElement(override val get: E) extends TypeTagged[E] {
          override def tpe: Type = elementType
        }

        WriteSuccess(IRArray(collection.asInstanceOf[GenTraversableOnce[E]].toList.map { oneElement =>
          elementTransceiver.write(new TaggedElement(oneElement))(ops, guidance.withSeq()).get
        }))
    }

}