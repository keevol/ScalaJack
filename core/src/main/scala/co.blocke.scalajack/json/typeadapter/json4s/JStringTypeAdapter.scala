package co.blocke.scalajack.json.typeadapter.json4s

import co.blocke.scalajack.json.typeadapter.SimpleTypeAdapter
import co.blocke.scalajack.json.{ Reader, TokenType, Writer }
import org.json4s.JString

object JStringTypeAdapter extends SimpleTypeAdapter[JString] {

  override def read(reader: Reader): JString =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.String ⇒
        JString(reader.readString())
    }

  override def write(value: JString, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(value.s)
    }

}
