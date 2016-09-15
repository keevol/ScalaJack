package co.blocke.scalajack.json.typeadapter.json4s

import co.blocke.scalajack.json.{ Context, Reader, TokenType, TypeAdapter, TypeAdapterFactory, Writer }
import org.json4s.{ JArray, JValue }

import scala.reflect.runtime.universe.{ Type, typeOf }

object JArrayTypeAdapter extends TypeAdapterFactory {

  override def typeAdapter(tpe: Type, context: Context): Option[TypeAdapter[_]] =
    if (tpe =:= typeOf[JArray]) {
      val jValueTypeAdapter = context.typeAdapterOf[JValue]
      Some(JArrayTypeAdapter(jValueTypeAdapter))
    } else {
      None
    }

}

case class JArrayTypeAdapter(jValueTypeAdapter: TypeAdapter[JValue]) extends TypeAdapter[JArray] {

  override def read(reader: Reader): JArray =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.BeginArray ⇒
        val builder = List.canBuildFrom[JValue]()

        reader.beginArray()

        while (reader.hasMoreElements) {
          val element = jValueTypeAdapter.read(reader)
          builder += element
        }

        reader.endArray()

        JArray(builder.result())
    }

  override def write(value: JArray, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.beginArray()

      for (element ← value.arr) {
        jValueTypeAdapter.write(element, writer)
      }

      writer.endArray()
    }

}
