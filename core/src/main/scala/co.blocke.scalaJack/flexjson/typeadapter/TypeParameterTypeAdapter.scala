package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.{ Context, Reader, TypeAdapter, TypeAdapterFactory, Writer }

import scala.reflect.runtime.universe.Type

object TypeParameterTypeAdapter extends TypeAdapterFactory {

  override def typeAdapter(tpe: Type, context: Context, superParamTypes: List[Type]): Option[TypeAdapter[_]] =
    if (tpe.typeSymbol.isParameter) {
      Some(TypeParameterTypeAdapter(context.typeAdapterOf[Any]))
    } else {
      None
    }
}

case class TypeParameterTypeAdapter[T](anyTypeAdapter: TypeAdapter[Any]) extends TypeAdapter[T] {

  override def read(reader: Reader): T =
    anyTypeAdapter.read(reader).asInstanceOf[T]

  override def write(value: T, writer: Writer): Unit =
    anyTypeAdapter.write(value, writer)

}
