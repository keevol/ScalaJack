package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.{ Context, TypeAdapter, TypeAdapterFactory }

import scala.reflect.runtime.universe.{ Type, TypeTag }

/**
 * __DO NOT__ EXTEND THIS CLASS FOR TypeAdapter OVERRIDES FOR PRIMITIVE TYPES (e.g. in VisitorContext)!
 *
 * See note in BasicTypeAdapter for details.
 */
object SimpleTypeAdapter {

  abstract class ForTypeSymbolOf[T](implicit valueTypeTag: TypeTag[T]) extends TypeAdapterFactory with TypeAdapter[T] {

    override def typeAdapter(tpe: Type, context: Context, superParamTypes: List[Type]): Option[TypeAdapter[_]] =
      if (tpe.typeSymbol == valueTypeTag.tpe.typeSymbol) {
        Some(this)
      } else {
        None
      }

  }

}

abstract class SimpleTypeAdapter[T](implicit valueTypeTag: TypeTag[T]) extends TypeAdapterFactory with TypeAdapter[T] {

  val valueType = valueTypeTag.tpe

  override def typeAdapter(tpe: Type, context: Context, superParamTypes: List[Type] = List.empty[Type]): Option[TypeAdapter[_]] =
    if (tpe =:= valueType) {
      Some(this)
    } else {
      None
    }

}
