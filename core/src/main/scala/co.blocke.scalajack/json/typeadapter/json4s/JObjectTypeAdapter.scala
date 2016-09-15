package co.blocke.scalajack.json.typeadapter.json4s

import co.blocke.scalajack.json.{ Context, Reader, TokenType, TypeAdapter, TypeAdapterFactory, Writer }
import org.json4s.{ JObject, JValue }

import scala.reflect.runtime.universe.{ Type, typeOf }

object JObjectTypeAdapter extends TypeAdapterFactory {

  override def typeAdapter(tpe: Type, context: Context): Option[TypeAdapter[_]] =
    if (tpe =:= typeOf[JObject]) {
      val stringTypeAdapter = context.typeAdapterOf[String]
      val jValueTypeAdapter = context.typeAdapterOf[JValue]
      Some(JObjectTypeAdapter(stringTypeAdapter, jValueTypeAdapter))
    } else {
      None
    }

}

case class JObjectTypeAdapter(
    stringTypeAdapter: TypeAdapter[String],
    jValueTypeAdapter: TypeAdapter[JValue]
) extends TypeAdapter[JObject] {

  override def read(reader: Reader): JObject =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.BeginObject ⇒
        val builder = List.canBuildFrom[(String, JValue)]()

        reader.beginObject()

        while (reader.hasMoreMembers) {
          val memberName = stringTypeAdapter.read(reader)
          val memberValue = jValueTypeAdapter.read(reader)
          builder += memberName → memberValue
        }

        reader.endObject()

        JObject(builder.result())
    }

  override def write(value: JObject, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.beginObject()

      for ((memberName, memberValue) ← value.obj) {
        stringTypeAdapter.write(memberName, writer)
        jValueTypeAdapter.write(memberValue, writer)
      }

      writer.endObject()
    }

}
