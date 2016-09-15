package co.blocke.scalajack.json.typeadapter.json4s

import co.blocke.scalajack.json.typeadapter.SimpleTypeAdapter
import co.blocke.scalajack.json.{ Reader, TokenType, Writer }
import org.json4s.JDouble

object JDoubleTypeAdapter extends SimpleTypeAdapter[JDouble] {

  override def read(reader: Reader): JDouble =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.Number ⇒
        JDouble(reader.readDouble())
    }

  override def write(value: JDouble, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeDouble(value.num)
    }

}
