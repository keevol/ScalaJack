package co.blocke.scalajack.json.typeadapter.json4s

import co.blocke.scalajack.json.typeadapter.SimpleTypeAdapter
import co.blocke.scalajack.json.{ Reader, TokenType, Writer }
import org.json4s.JBool

object JBoolTypeAdapter extends SimpleTypeAdapter[JBool] {

  override def read(reader: Reader): JBool =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.False | TokenType.True ⇒
        JBool(reader.readBoolean())
    }

  override def write(value: JBool, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeBoolean(value.value)
    }

}
