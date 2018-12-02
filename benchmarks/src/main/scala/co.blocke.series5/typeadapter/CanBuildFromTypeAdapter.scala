package co.blocke.series5
package typeadapter

import scala.collection.generic.CanBuildFrom
import scala.collection.{ GenMapLike, GenTraversableOnce }
import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.{ Symbol, Type, TypeTag, typeOf }

object CanBuildFromTypeAdapter extends TypeAdapterFactory {

  override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    if (tt.tpe <:< typeOf[GenTraversableOnce[_]]) {
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

        val companionInstance = currentMirror.reflectModule(companionSymbol).instance
        val canBuildFrom = currentMirror.reflect(companionInstance).reflectMethod(method).apply()

        if (tt.tpe <:< typeOf[GenMapLike[_, _, _]] && elementTypeAfterSubstitution <:< typeOf[(_, _)]) {
          val keyType = elementTypeAfterSubstitution.typeArgs(0)
          val keyTypeAdapter = context.typeAdapter(keyType) match {
            case kta: OptionTypeAdapter[_] => kta.noneAsEmptyString // output "" for None for map keys
            case kta                       => kta
          }
          val valueTypeAdapter = context.typeAdapter(elementTypeAfterSubstitution.typeArgs(1))

          Some(CanBuildMapTypeAdapter(canBuildFrom.asInstanceOf[CanBuildFrom[Any, Any, GenMapLike[Any, Any, Any] with Null]], keyTypeAdapter.asInstanceOf[TypeAdapter[Any]], valueTypeAdapter.asInstanceOf[TypeAdapter[Any]]))
        } else {
          Some(CanBuildFromTypeAdapter(canBuildFrom.asInstanceOf[CanBuildFrom[Any, Any, GenTraversableOnce[Any]]], elementTypeAdapter.asInstanceOf[TypeAdapter[Any]]))
        }
      }

      matchingTypeAdapters.headOption.map(_.asInstanceOf[TypeAdapter[T]]).getOrElse(next.typeAdapterOf[T])
    } else {
      next.typeAdapterOf[T]
    }

}

case class CanBuildMapTypeAdapter[Key, Value, To >: Null <: GenMapLike[Key, Value, To]](
    canBuildFrom:     CanBuildFrom[_, (Key, Value), To],
    keyTypeAdapter:   TypeAdapter[Key],
    valueTypeAdapter: TypeAdapter[Value]) extends TypeAdapter[To] {

  override def read(reader: Reader): To =
    reader.peek match {
      case TokenType.BeginObject =>
        val builder = canBuildFrom()

        reader.beginObject()

        while (reader.hasMoreMembers) {
          val key = keyTypeAdapter.read(reader)
          val value = valueTypeAdapter.read(reader)
          builder += key -> value
        }

        reader.endObject()

        builder.result()

      case TokenType.Null =>
        reader.readNull()
    }

  override def write(map: To, writer: Writer): Unit =
    if (map == null) {
      writer.writeNull()
    } else {
      writer.beginObject()

      map foreach {
        case (key, value) =>
          keyTypeAdapter.write(key, writer)
          valueTypeAdapter.write(value, writer)
      }

      writer.endObject()
    }

}

case class CanBuildFromTypeAdapter[Elem, To >: Null <: GenTraversableOnce[Elem]](
    canBuildFrom:       CanBuildFrom[_, Elem, To],
    elementTypeAdapter: TypeAdapter[Elem]) extends TypeAdapter[To] {

  override def read(reader: Reader): To =
    reader.peek match {
      case TokenType.BeginArray =>
        val builder = canBuildFrom()

        reader.beginArray()

        while (reader.hasMoreElements) {
          val element = elementTypeAdapter.read(reader)
          builder += element
        }

        reader.endArray()

        builder.result()

      case TokenType.Null =>
        reader.readNull()
    }

  override def write(value: To, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.beginArray()

      for (element <- value) {
        elementTypeAdapter.write(element, writer)
      }

      writer.endArray()
    }

}
