package co.blocke.scalajack.json.typeadapter.json4s

import co.blocke.scalajack.json.{ Context, Reader, TokenType, TypeAdapter, TypeAdapterFactory, Writer }
import org.json4s.{ JArray, JBool, JDecimal, JDouble, JInt, JLong, JNothing, JNull, JObject, JString, JValue }

import scala.reflect.runtime.universe.{ Type, typeOf }

object JValueTypeAdapter extends TypeAdapterFactory {

  override def typeAdapter(tpe: Type, context: Context): Option[TypeAdapter[_]] =
    if (tpe =:= typeOf[JValue]) {
      val jArrayTypeAdapter = context.typeAdapterOf[JArray]
      val jBoolTypeAdapter = context.typeAdapterOf[JBool]
      val jDecimalTypeAdapter = context.typeAdapterOf[JDecimal]
      val jDoubleTypeAdapter = context.typeAdapterOf[JDouble]
      val jIntTypeAdapter = context.typeAdapterOf[JInt]
      val jLongTypeAdapter = context.typeAdapterOf[JLong]
      val jObjectTypeAdapter = context.typeAdapterOf[JObject]
      val jStringTypeAdapter = context.typeAdapterOf[JString]

      Some(JValueTypeAdapter(jArrayTypeAdapter, jBoolTypeAdapter, jDecimalTypeAdapter, jDoubleTypeAdapter, jIntTypeAdapter, jLongTypeAdapter, jObjectTypeAdapter, jStringTypeAdapter))
    } else {
      None
    }

}

case class JValueTypeAdapter(
    jArrayTypeAdapter:   TypeAdapter[JArray],
    jBoolTypeAdapter:    TypeAdapter[JBool],
    jDecimalTypeAdapter: TypeAdapter[JDecimal],
    jDoubleTypeAdapter:  TypeAdapter[JDouble],
    jIntTypeAdapter:     TypeAdapter[JInt],
    jLongTypeAdapter:    TypeAdapter[JLong],
    jObjectTypeAdapter:  TypeAdapter[JObject],
    jStringTypeAdapter:  TypeAdapter[JString]
) extends TypeAdapter[JValue] {

  override def read(reader: Reader): JValue =
    reader.peek match {
      case TokenType.BeginObject ⇒
        jObjectTypeAdapter.read(reader)

      case TokenType.BeginArray ⇒
        jArrayTypeAdapter.read(reader)

      case TokenType.Number ⇒
        jDecimalTypeAdapter.read(reader)

      case TokenType.String ⇒
        jStringTypeAdapter.read(reader)

      case TokenType.True | TokenType.False ⇒
        jBoolTypeAdapter.read(reader)

      case TokenType.Null ⇒
        reader.readNull()
        JNull
    }

  override def write(value: JValue, writer: Writer): Unit =
    value match {
      case null | JNull ⇒
        writer.writeNull()

      case jArray: JArray ⇒
        jArrayTypeAdapter.write(jArray, writer)

      case jBool: JBool ⇒
        jBoolTypeAdapter.write(jBool, writer)

      case jDecimal: JDecimal ⇒
        jDecimalTypeAdapter.write(jDecimal, writer)

      case jDouble: JDouble ⇒
        jDoubleTypeAdapter.write(jDouble, writer)

      case jInt: JInt ⇒
        jIntTypeAdapter.write(jInt, writer)

      case jLong: JLong ⇒
        jLongTypeAdapter.write(jLong, writer)

      case JNothing ⇒
        writer.writeNothing()

      case jObject: JObject ⇒
        jObjectTypeAdapter.write(jObject, writer)

      case jString: JString ⇒
        jStringTypeAdapter.write(jString, writer)
    }

}
