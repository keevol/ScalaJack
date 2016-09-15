package co.blocke.scalajack.json.typeadapter.json4s

import co.blocke.scalajack.json.typeadapter.SimpleTypeAdapter
import co.blocke.scalajack.json.{ Reader, TokenType, Writer }
import org.json4s.JLong

object JLongTypeAdapter extends SimpleTypeAdapter[JLong] {

  override def read(reader: Reader): JLong =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.Number ⇒
        JLong(reader.readLong())
    }

  override def write(value: JLong, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeLong(value.num)
    }

}
